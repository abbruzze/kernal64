package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.{CBMComponent, Log}
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.{CPU65xx, Memory, RAMComponent}
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.mos653x.{MOS6532, MOS653X}
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceListener.{BreakType, CpuStepInfo, StepType}

import java.io.{File, ObjectInputStream, ObjectOutputStream, PrintWriter}

class D8050(val driveID:Int,
            ieeebus:IEEE488Bus,
            ledListener: DriveLedListener) extends CBMComponent with TraceListener with Drive {
  override val componentID: String = "D8050 Disk Drive " + (driveID + 8)
  override val driveType: DriveType.Value = DriveType._8050
  override val formatExtList: List[String] = List("D80")

  private var floppy : Floppy = D1581.MFMEmptyFloppy

  /************************************************************************************************************
   * UE1 - 6532 - PortA
   *
   ***********************************************************************************************************/
  private val UE1_6532_PORT_A = new MOS653X.Port {
    import IEEE488Bus.LineValue._
    import IEEE488Bus.LineType._
    var oldPa = 0
    override def read(): Int = {
      var r = 0xFF
      if (ieeebus.getLine(ATN) == RELEASED) r -= 0x80 // inverted ATN
      if (ieeebus.getLine(DAV) == PULLED) r -= 0x40
      if (ieeebus.getLine(EOI) == PULLED) r -= 0x20
      r
    }
    override def write(pa: Int): Unit = {
      import IEEE488Bus.LineValue._
      import IEEE488Bus.LineType._
      oldPa = pa

    }

    def setHandshakeLines(pa:Int): Unit = {
      /*
        IEEE handshake logic (named as in schematics):
              Inputs: /ATN    = inverted IEEE atn (true = active)
                      ATNA    = pa bit 0
                      /DACO   = pa bit 1
                      RFDO    = pa bit 2
              Output: DACO    = /DACO & (ATN | ATNA)
                      RFDO    = (/ATN == ATNA) & RFDO
      */
      // DACO = /DACO & (ATN | ATNA)
      val daco = (pa & 2) == 0 && ((ieeebus.getLine(ATN) == PULLED) || (pa & 1) == 0)
      if (daco) ieeebus.releaseLine(BUS_LISTENER,NDAC) else ieeebus.pullLine(BUS_LISTENER,NDAC)
      // RFDO    = (/ATN == ATNA) & RFDO
      val _atn = if (ieeebus.getLine(ATN) == PULLED) 0 else 1
      val rfdo = (pa & 4) == 0 && (_atn == (pa & 1))
      if (rfdo) ieeebus.releaseLine(BUS_LISTENER,NRFD) else ieeebus.pullLine(BUS_LISTENER,NRFD)
    }
  }
  /************************************************************************************************************
   * UE1 - 6532 - PortB
   *
   ***********************************************************************************************************/
  private val UE1_6532_PORT_B = new MOS653X.Port {
    import IEEE488Bus.LineValue._
    import IEEE488Bus.LineType._
    override def read(): Int = {
      var r = 0xFF - 7
      if (ieeebus.getLine(NRFD) == PULLED) r -= 0x80
      if (ieeebus.getLine(NDAC) == PULLED) r -= 0x40
      r += driveID
      r
    }
    override def write(value: Int): Unit = {
      val led = (value >> 3) & 3
      val led0 = (led & 1) > 0
      val led1 = (led & 2) > 0
      val err = (led & 4) > 0
      println(s"LED 0 ${if (led0) "ON" else "OFF"} 1 ${if (led1) "ON" else "OFF"} ERR ${if (err) "ON" else "OFF"}")
    }
  }
  /************************************************************************************************************
   * UE1 - 6532
   *
   ***********************************************************************************************************/
  private val UE1_6532 = new MOS6532("UE1-IEEE",UE1_6532_PORT_A,UE1_6532_PORT_B,handleUE1_6532_IRQ _)
  /************************************************************************************************************
   * IEEE bus listener
   *
   ***********************************************************************************************************/
  private val BUS_LISTENER : IEEE488Bus.LineListener = new IEEE488Bus.LineListener {
    override def ATNchanged(id: Long, newValue: IEEE488Bus.LineValue.Value): Unit = {
      newValue match {
        case IEEE488Bus.LineValue.PULLED =>
          UE1_6532.pa7(true)
        case IEEE488Bus.LineValue.RELEASED =>
          UE1_6532.pa7(false)
      }
      UE1_6532_PORT_A.setHandshakeLines(UE1_6532_PORT_A.oldPa)
    }
  }
  ieeebus.registerListener(BUS_LISTENER)
  /************************************************************************************************************
   * UC1 - 6532 - PortA
   *
   ***********************************************************************************************************/
  private val UC1_6532_PORT_A = new MOS653X.Port {
    override def read(): Int = ieeebus.getDIO()
    override def write(value: Int): Unit = {}
  }
  /************************************************************************************************************
   * UC1 - 6532 - PortB
   *
   ***********************************************************************************************************/
  private val UC1_6532_PORT_B = new MOS653X.Port {
    override def read(): Int = 0
    override def write(value: Int): Unit = ieeebus.setDIO(value)
  }
  /************************************************************************************************************
   * UC1 - 6532
   *
   ***********************************************************************************************************/
  private val UC1_6532 = new MOS6532("UC1",UC1_6532_PORT_A,UC1_6532_PORT_B,_ => {}) // NO IRQ
  /************************************************************************************************************
   * 4K Common memory
   *
   ***********************************************************************************************************/
   private val COMMON_MEMORY = Array.ofDim[Int](4,0x400)
  /************************************************************************************************************
   * ROMS
   * - C000 - FFFF for UN1 6502
   * - FC00 - FFFF for UH3 6502
   ***********************************************************************************************************/
  private val C000_FFFF_ROM = java.nio.file.Files.readAllBytes(new File("""C:\Users\ealeame\OneDrive - Ericsson AB\CBM-II\roms\8050\dos27.bin""").toPath).map(_.toInt & 0xFF)
  private val FC00_FFFF_ROM = java.nio.file.Files.readAllBytes(new File("""C:\Users\ealeame\OneDrive - Ericsson AB\CBM-II\roms\8050\901869-01-riot-dos2.7-mpi.bin""").toPath).map(_.toInt & 0xFF)
  /************************************************************************************************************
   * UN1 - 6502 - Memory
   *
   ***********************************************************************************************************/
   private val UN1_6502_MEM = new Memory {
     override val isRom: Boolean = false
     override val length: Int = 0
     override val startAddress: Int = 0
     override val name: String = "IEEE 6502 Memory"
     override def init(): Unit = {}
     override def isActive: Boolean = true

     override def read(address: Int, chipID: ID): Int = {
       if (address < 0x200) { // 256 bytes from riots mirrored on stack page
         val adr = address & 0xFF
         if (adr < 0x80) UE1_6532.readRAM(adr)
         else UC1_6532.readRAM(adr & 0x7F)
       }
       else if (address >= 0x200 && address < 0x208) UC1_6532.read(address)
       else if (address >= 0x280 && address < 0x288) UE1_6532.read(address)
       else if (address >= 1000 && address < 0x5000) COMMON_MEMORY(((address & 0xF000) >> 12) - 1)(address & 0x3FF)
       else if (address >= 0xC000) C000_FFFF_ROM(address & 0x3FFF)
       else {
         println(s"Read unconnected RAM at ${address.toHexString}")
         0
       }
     }

     override def write(address: Int, value: Int, chipID: ID): Unit = {
       if (address < 0x1FF) { // 256 bytes from riots mirrored on stack page
         val adr = address & 0xFF
         if (adr < 0x80) UE1_6532.writeRAM(adr,value)
         else UC1_6532.writeRAM(adr & 0x7F,value)
       }
       else if (address >= 0x200 && address < 0x208) UC1_6532.write(address,value)
       else if (address >= 0x280 && address < 0x288) UE1_6532.write(address,value)
       else if (address >= 1000 && address < 0x5000) COMMON_MEMORY(((address & 0xF000) >> 12) - 1)(address & 0x3FF) = value
       else if (address >= 0xC000) {/*ROM*/}
       else {
         println(s"Write unconnected RAM at ${address.toHexString} = $value")
       }
     }
   }
  /************************************************************************************************************
   * UN1 - 6502
   *
   ***********************************************************************************************************/
  private val UN1_6502 = CPU65xx.make(UN1_6502_MEM)
  /************************************************************************************************************
   * R/W GCR HEAD Controller
   *
   ***********************************************************************************************************/
  private[this] val RW_HEAD = new GCRRWHeadController("8050",floppy,ledListener)

  /***********************************************************************************************************/

  @inline private def handleUE1_6532_IRQ(low:Boolean): Unit = UN1_6502.irqRequest(low)

  /************************************************************************************************************
   * Initialization
   *
   ***********************************************************************************************************/
  override def reset(): Unit = {

  }
  override def init(): Unit = {
    UN1_6502.initComponent() // REMOVE IT!!
  }
  /************************************************************************************************************
   * Tracing
   *
   ***********************************************************************************************************/
  override def getRegisters(): List[TraceListener.TraceRegister] = UN1_6502.getRegisters()
  override def setTraceOnFile(out: PrintWriter, enabled: Boolean): Unit = UN1_6502.setTraceOnFile(out,enabled)

  override def setTrace(traceOn: Boolean): Unit = UN1_6502.setTrace(traceOn)

  override def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType): Unit = UN1_6502.step(updateRegisters,stepType)

  override def setBreakAt(breakType: BreakType, callback: CpuStepInfo => Unit): Unit = UN1_6502.setBreakAt(breakType, callback)

  override def jmpTo(pc: Int): Unit = UN1_6502.jmpTo(pc)

  override def disassemble(address: Int): TraceListener.DisassembleInfo = UN1_6502.disassemble(address)

  override def setCycleMode(cycleMode: Boolean): Unit = UN1_6502.setCycleMode(cycleMode)

  /************************************************************************************************************
   * Main clock
   *
   ***********************************************************************************************************/
  override def clock(cycles: Long): Unit = {
    UN1_6502.fetchAndExecute(1)
    UE1_6532.clock()
    UC1_6532.clock()
  }
  /************************************************************************************************************
   * Floppy management
   *
   ***********************************************************************************************************/
  override def getFloppy: Floppy = floppy
  override def setDriveReader(driveReader: Floppy, emulateInserting: Boolean): Unit = {
    floppy = driveReader
    RW_HEAD.setFloppy(floppy)
    RW_HEAD.setWriteProtected(floppy.isReadOnly)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = ???
}
