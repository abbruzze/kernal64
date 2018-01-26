package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.ChipID
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent

class WD1770(rwh:RWHeadController) extends RAMComponent {
  val componentID = "WD1770"
  val componentType = CBMComponentType.INTERNAL
  val isRom = false
  val length = 4
  val startAddress = 0x2000
  val name = componentID
  val isActive = true
  
  /*
   * Status bits
   * Bit 0: Busy flag. Indicates that the command is being executed.
   * Bit 1: Data request/index For all other commands this bit signals that data can be taken from register $2003 or can be written in the register.
   * Bit 2: Lost data/track0 For commands of type 1 this bit indicates that the head is on track 0. For all other commands this bit indicates that the data in register $2003 was not read or written by the program in time.
   * Bit 3: CRC error. The checksum bytes of the header or the data block decoded an error.
   * Bit 4: Record not found. The specified track or sector was not found.
   * Bit 5: Spin-up/Record type For commands of type 1 this bit specifies that 6 diskette rotations have taken place. For commands of type 2 and 3 this bit was the value of the "data mark."
   * Bit 6: Write protect. This bit indicates when writing that the write protect tab is in place.
   * Bit 7: Motor on. This bit gives the status of the motor. 0= motor off 1= motor on
   */
  private final val CMD_H = 1 << 3
  private final val CMD_V = 1 << 2
  private final val CMD_XY = 3
  private final val CMD_U = 1 << 4
  private final val CMD_M = 1 << 4
  private final val CMD_E = 1 << 2
  private final val CMD_P = 1 << 1
  private final val CMD_A = 1 << 0
  private final val CMD_K = 1 << 1
  private final val CMD_L = 1 << 0
  
  private case class Command(cmdType : Int,mask : Int,value : Int,name:String)
  
  private final val COMMANDS : Array[Command] = Array(
      Command(1,0xF0,0,"RESTORE"),
      Command(1,0xF0,0x10,"SEEK"),
      Command(1,0xE0,0x20,"STEP"),
      Command(1,0xE0,0x40,"STEP_IN"),
      Command(1,0xE0,0x60,"STEP_OUT"),
      Command(2,0xE0,0x80,"READ_SECTOR"),
      Command(2,0xE0,0xA0,"WRITE_SECTOR"),
      Command(3,0xF0,0xC0,"READ_ADDRESS"),
      Command(3,0xF0,0xE0,"READ_TRACK"),
      Command(3,0xF0,0xF0,"WRITE_TRACK"),
      Command(4,0xF0,0xD0,"FORCE_INT")
  )
  
  private def decodeCommand(byte:Int) : Option[Command] = {
    var i = 0
    while (i < COMMANDS.length) {
      val cmd = COMMANDS(i)
      if ((byte & cmd.mask) == cmd.value) return Some(cmd)
      i += 1
    }
    None
  }
  
  private[this] var cmd : Option[Command] = None
  private[this] var track,sector,data = 0
  private[this] final val clk = Clock.systemClock
  
  def reset {
    // TODO
  }
  def init {
    // TODO
  }
  
  // ==============================================================================
  @inline private def statusByte : Int = {
    if (cmd.isDefined) 1 else 0
  }
  def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    address & 3 match {
      case 0 =>
        statusByte
      case 1 =>
        track        
      case 2 =>
        sector
      case 3 =>
        data
    }
  }
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    address & 3 match {
      case 0 =>
        cmd = decodeCommand(value)
        println(s"WD1770: command $cmd")
        clk.schedule(new ClockEvent("WD1770",clk.currentCycles + 100, cycles => cmd = None))
      case 1 =>
        track = value
      case 2 =>
        sector = value
      case 3 =>
        data = value
    }
  }
  // ==============================================================================
  
  protected def saveState(out:ObjectOutputStream) {
    // TODO
  }
  protected def loadState(in:ObjectInputStream) {
    // TODO
  }
  protected def allowsStateRestoring(parent:JFrame) = true
}