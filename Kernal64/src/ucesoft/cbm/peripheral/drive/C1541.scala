package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.CPU65xx
import ucesoft.cbm.ChipID
import ucesoft.cbm.Clock
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.trace.BreakType
import java.io.PrintWriter
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.bus.IECBusListener
import ucesoft.cbm.peripheral.bus.IECBusLine
import ucesoft.cbm.ClockEvent

class C1541(val jackID: Int, bus: IECBus, ledListener: DriveLedListener) extends TraceListener with Drive {
  val driveType = DriveType._1541
  val componentID = "C1541 Disk Drive " + jackID
  val formatExtList = List("D64","D71","G64")
  override val MIN_SPEED_HZ = 985248
  override val MAX_SPEED_HZ = 1000000
  
  private[this] val LOAD_ROUTINE = 0xD7B4
  private[this] val WAIT_LOOP_ROUTINE = 0xEBFF//0xEC9B //0xEBFF//0xEC9B
  private[this] val WAIT_CYCLES_FOR_STOPPING = 2000000
  private[this] val GO_SLEEPING_MESSAGE_CYCLES = 3000000
  private[this] var CYCLE_ADJ = 0.0 //(MAX_SPEED_MHZ - MIN_SPEED_MHZ) / MIN_SPEED_MHZ.toDouble
  private[this] var currentSpeedHz = MIN_SPEED_HZ
  private[this] var cycleFrac = 0.0
  private[this] val mem = new C1541Mems.C1541_RAM
  private[this] val cpu = CPU65xx.make(mem,ChipID.CPU_1541)
  private[this] val clk = Clock.systemClock
  private[this] var running = true
  private[this] var awakeCycles = 0L
  private[this] var goSleepingCycles = 0L
  private[this] var tracing = false
  private[this] var canSleep = true
  private[this] var floppy : Floppy = EmptyFloppy
  /************************************************************************************************************
   * R/W HEAD Controller
   * 
   ***********************************************************************************************************/
  private[this] val RW_HEAD = new GCRRWHeadController("1541",floppy,ledListener)
  /************************************************************************************************************
   * VIA1 Disk Controller
   * 
   ***********************************************************************************************************/
  private[this] val viaBus = new VIA("VIA_IECBus" + jackID,0x1800,IRQSwitcher.viaBusIRQ _) with IECBusListener {
    override lazy val componentID = "VIA1 (Bus)"
    val busid = name
    private[this] val IDJACK = jackID & 0x03
    private[this] var driveEnabled = true
    
    bus.registerListener(this)
    if (jackID == 0) ParallelCable.pcCallback = onCB1 _
    
    override def reset {
      super.reset
    }
    
    def setEnabled(enabled:Boolean) = driveEnabled = enabled
    
    override def atnChanged(oldValue:Int,newValue:Int) {
      if (driveEnabled) {
        if (newValue == IECBus.GROUND) {
          irq_set(IRQ_CA1)
          awake
        }
        autoacknwoledgeData
      }
    }
    
    private def onCB1 = irq_set(IRQ_CB1)
    
    override def read(address: Int, chipID: ChipID.ID) = (address & 0x0F) match {
      case ad@(PA|PA2) =>
        super.read(address,chipID)
        if (jackID == 0 && ParallelCable.enabled) {
          val r = ParallelCable.read & ~regs(DDRA)
          if (ad == PA && (regs(PCR) & 0xE) == 0xA) ParallelCable.onCA2
          r
        }
        else 0xFF
      case PB =>
        (super.read(address,chipID) & 0x1A) | (bus.data|data_out) | (bus.clk|clock_out) << 2 | bus.atn << 7 | IDJACK << 5
        
      case ofs => super.read(address,chipID)
    }
    
    override def write(address: Int, value: Int, chipID: ChipID.ID) {
      super.write(address,value,chipID)
      (address & 0x0F) match {
        case PB|DDRB =>        
          bus.setLine(this,IECBusLine.DATA,data_out)
          bus.setLine(this,IECBusLine.CLK,clock_out)
          
          autoacknwoledgeData
        case ad@(PA|PA2) =>
          if (jackID == 0 && ParallelCable.enabled) {
            ParallelCable.write(value)
            if (ad == PA && (regs(PCR) & 0xE) == 0xA) ParallelCable.onCA2
          }
        case _ =>
      }         
    }
    
    @inline private def data_out = if ((regs(DDRB) & regs(PB) & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
    @inline private def clock_out = if ((regs(DDRB) & regs(PB) & 0x08) > 0) IECBus.GROUND else IECBus.VOLTAGE
    
    @inline private def autoacknwoledgeData {
      val atna = (regs(DDRB) & regs(PB) & 0x10) > 0
      val dataOut = (bus.atn == IECBus.GROUND) ^ atna
      if (dataOut) bus.setLine(this,IECBusLine.DATA,IECBus.GROUND) else bus.setLine(this,IECBusLine.DATA,data_out)
    }
    // state
    override protected def saveState(out:ObjectOutputStream) {
      super.saveState(out)
      //out.writeInt(data_out)
      out.writeBoolean(driveEnabled)
    }
    override protected def loadState(in:ObjectInputStream) {
      super.loadState(in)
      driveEnabled = in.readBoolean
    }
  }
  /************************************************************************************************************
   * VIA2 Disk Controller
   * 
   ***********************************************************************************************************/
  private[this] val viaDisk = new VIA("VIA1541_DiskControl", 0x1C00,IRQSwitcher.viaDiskIRQ _) {
    override lazy val componentID = "VIA1541_2 (DC)"
    private[this] val WRITE_PROTECT_SENSE = 0x10
    private[this] val WRITE_PROTECT_SENSE_WAIT = 3 * 400000L
    private[this] val REMOVING_DISK_WAIT = 500000L
    private[this] val SYNC_DETECTION_LINE = 0x80
    private[this] var isDiskChanged = true
    private[this] var isDiskChanging = false
      
    def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) {
      floppy = driveReader
      if (emulateInserting) {
        RW_HEAD.setFloppy(EmptyFloppy)
        isDiskChanged = true
        isDiskChanging = true
        viaBus.irq_set(IRQ_CA2)
        clk.schedule(new ClockEvent("DiskRemoving",Clock.systemClock.currentCycles + WRITE_PROTECT_SENSE_WAIT, cycles => {
          isDiskChanged = false
          clk.schedule(new ClockEvent("DiskWaitingInserting",cycles + REMOVING_DISK_WAIT, cycles => {
            isDiskChanged = true
            viaBus.irq_set(IRQ_CA2)
            clk.schedule(new ClockEvent("DiskWaitingClearing",cycles + WRITE_PROTECT_SENSE_WAIT, cycles => {
              isDiskChanged = false
              isDiskChanging = false
              RW_HEAD.setFloppy(floppy)
            }))
          }))
        }))
      }
      else {
        RW_HEAD.setFloppy(floppy)
      }
      awake
    }
    
    override def read(address: Int, chipID: ChipID.ID) = (address & 0x0F) match {
      case PB =>
        val wps = if (isDiskChanging) isDiskChanged else RW_HEAD.isWriteProtected
        (super.read(address, chipID) & ~(WRITE_PROTECT_SENSE | SYNC_DETECTION_LINE)) |
          (if (RW_HEAD.isSync) 0x00 else SYNC_DETECTION_LINE) |
          (if (wps) 0x00 else WRITE_PROTECT_SENSE)
      case PA|PA2 =>
        super.read(address, chipID)
        floppy.notifyTrackSectorChangeListener
        if ((regs(PCR) & 0x0C) == 0x08) RW_HEAD.canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
        RW_HEAD.getLastRead
      case ofs => super.read(address, chipID)
    }
  
    override def write(address: Int, value: Int, chipID: ChipID.ID) {
      (address & 0x0F) match {
        case PA =>
          if ((regs(PCR) & 0x0C) == 0x08) RW_HEAD.canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
          RW_HEAD.setNextToWrite(value)
        case PCR =>
          value & 0x0E match {
            case 0xE => RW_HEAD.canSetByteReady = true        // 111 => Manual output mode high
            case 0xC => RW_HEAD.canSetByteReady = false       // 110 => Manual output mode low
            case _ => RW_HEAD.canSetByteReady = true
          }
          val isWriting = (value & 0x20) == 0
          RW_HEAD.setWriting(isWriting)          
          ledListener.writeMode(isWriting)
        case PB =>
          // led
          val ledOn = (value & 0x08) > 0
          val lastMotorOn = RW_HEAD.isMotorOn
          val motorOn = (value & 0x04) > 0
          RW_HEAD.setMotor(motorOn)
          if (ledListener != null) {
            if (ledOn) ledListener.turnOn else ledListener.turnOff
            if (!motorOn) {
              ledListener.endLoading
              if (lastMotorOn && !motorOn) RW_HEAD.setCurrentFileName("")
            }
          }
          val newSpeedZone = (value & 0xFF) >> 5 & 0x3
          RW_HEAD.setSpeedZone(newSpeedZone)
          // check the head step indication
          val oldStepHead = regs(PB) & 0x03
          val stepHead = value & 0x03
          
          if (motorOn && oldStepHead == ((stepHead + 1) & 0x03)) RW_HEAD.moveHead(moveOut = true)
          else 
          if (motorOn && oldStepHead == ((stepHead - 1) & 0x03)) RW_HEAD.moveHead(moveOut = false)
        case _ =>
      }
      // write on register
      super.write(address, value, chipID)
    }
          
    final def byteReady {
      cpu.setOverflowFlag
      irq_set(IRQ_CA1)
      
      regs(PCR) & 0x0E match {
        case 0xA|0x08 => RW_HEAD.canSetByteReady = false
        case _ =>
      }
    }
    
    // state
    override protected def saveState(out:ObjectOutputStream) {
      super.saveState(out)
      out.writeBoolean(isDiskChanged)
      out.writeBoolean(isDiskChanging)
    }
    override protected def loadState(in:ObjectInputStream) {
      super.loadState(in)
      isDiskChanged = in.readBoolean
      isDiskChanging = in.readBoolean
    }
  }
  
  def getFloppy = floppy

  def setDriveReader(driveReader: Floppy,emulateInserting:Boolean) = {
    viaDisk.setDriveReader(driveReader,emulateInserting)
    RW_HEAD.setWriteProtected(floppy.isReadOnly)
  }
  override def isRunning : Boolean = running

  override def setActive(active: Boolean) = {
    viaBus.setEnabled(active)
    running = active
    runningListener(active)
  }
  override def canGoSleeping = canSleep
  override def setCanSleep(canSleep: Boolean) {
    this.canSleep = canSleep
    awake
  }
  private def awake {
    running = true
    runningListener(true)
    viaBus.setActive(true)
    viaDisk.setActive(true)
    awakeCycles = clk.currentCycles
    //viaDisk.awake
  }
  
  override def disconnect {
    bus.unregisterListener(viaBus)
  }

  override def isReadOnly: Boolean = RW_HEAD.isWriteProtected
  override def setReadOnly(readOnly:Boolean) = RW_HEAD.setWriteProtected(readOnly)
  
  override def setSpeedHz(speed:Int) {
    currentSpeedHz = speed
    CYCLE_ADJ = (speed - MIN_SPEED_HZ) / MIN_SPEED_HZ.toDouble
  }
  
  override def getSpeedHz = currentSpeedHz
  
  override def getProperties = {
    properties.setProperty("Speed Hz",currentSpeedHz.toString)
    properties.setProperty("Cycle adjustment",CYCLE_ADJ.toString)
    properties.setProperty("Running", running.toString)
    val channels = mem.getChannelsState
    for (i <- 0 to 15) {
      val opened = (channels & (1 << i)) > 0
      properties.setProperty("Channel " + i, if (opened) "open" else "inactive")
    }
    properties
  }

  def init {
    Log.info("Initializing C1541...")
    add(mem)
    add(cpu)
    add(IRQSwitcher)
    add(RW_HEAD)
    mem.addBridge(viaBus)
    mem.addBridge(viaDisk)
    if (ledListener != null) ledListener.turnOn
  }

  def reset {
    running = true
    runningListener(true)
    awakeCycles = 0
    goSleepingCycles = 0
    cycleFrac = 0.0
    if (ledListener != null) ledListener.turnOn
  }
  
  private object IRQSwitcher extends CBMComponent {
    val componentID = "IRQ Switcher (VIA1,VIA2)"
    val componentType = CBMComponentType.INTERNAL

    private[this] var viaBusIRQLow = false
    private[this] var viaDiskIRQLow = false

    private def handleIRQ = {
      Log.debug(s"Handling IRQ viaBusIRQ=${viaBusIRQLow} viaDiskIRQ=${viaDiskIRQLow}")
      cpu.irqRequest(viaBusIRQLow || viaDiskIRQLow)
    }

    override def getProperties = {
      properties.setProperty("ViaBus IRQ", viaBusIRQLow.toString)
      properties.setProperty("ViaDisk IRQ", viaDiskIRQLow.toString)

      properties
    }

    final def viaBusIRQ(low: Boolean) {
      Log.debug("VIABus setting IRQ as " + low)
      viaBusIRQLow = low
      handleIRQ
    }
    final def viaDiskIRQ(low: Boolean) {
      Log.debug("VIADisk setting IRQ as " + low)
      viaDiskIRQLow = low
      handleIRQ
    }

    def init {}

    def reset {
      viaBusIRQLow = false
      viaDiskIRQLow = false
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(viaBusIRQLow)
      out.writeBoolean(viaDiskIRQLow)
    }
    protected def loadState(in:ObjectInputStream) {
      viaBusIRQLow = in.readBoolean
      viaDiskIRQLow = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }

  override def getMem = mem

  @inline private def checkPC(cycles: Long) {
    val pc = cpu.getPC
    if (pc == LOAD_ROUTINE) setFilename
    else 
    if (pc == WAIT_LOOP_ROUTINE && canSleep && !mem.isChannelActive && !RW_HEAD.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
      running = false
      runningListener(false)
      viaBus.setActive(false)
      viaDisk.setActive(false)
      goSleepingCycles = cycles
      //ledListener.beginLoadingOf("Disk go sleeping...")
    }
  }

  def clock(cycles: Long) {
    if (running) {
      checkPC(cycles)
      if (CYCLE_ADJ > 0) {
        cycleFrac += CYCLE_ADJ
        if (cycleFrac >= 1) {
          cycleFrac -= 1
          cpu.fetchAndExecute(2)
          viaDisk.clock(cycles)
          viaBus.clock(cycles)
          //if (RW_HEAD.rotate) viaDisk.byteReady
        }
        else cpu.fetchAndExecute(1)
      }
      else cpu.fetchAndExecute(1)
      
      viaDisk.clock(cycles)
      viaBus.clock(cycles)
      if (RW_HEAD.rotate) viaDisk.byteReady
    } else if (cycles - goSleepingCycles > GO_SLEEPING_MESSAGE_CYCLES) {
      ledListener.endLoading
      goSleepingCycles = Long.MaxValue
    }
  }

  private def setFilename {
    var adr = 0x200
    val sb = new StringBuilder
    var c = mem.read(adr)
    while (c != 0) {
      sb.append(c.toChar)
      adr += 1
      c = mem.read(adr)
    }
    RW_HEAD.setCurrentFileName(sb.toString)
  }

  // ------------ TRACING -----------
  def setCycleMode(cycleMode: Boolean): Unit = {
    cpu.setCycleMode(cycleMode)
  }
  def setTraceOnFile(out:PrintWriter,enabled:Boolean) {/* ignored */}
  def setTrace(traceOn: Boolean) = {
    tracing = traceOn
    if (tracing) awake
    cpu.setTrace(traceOn)
  }
  def step(updateRegisters: (String) => Unit) = cpu.step(updateRegisters)
  def setBreakAt(breakType:BreakType,callback:(String) => Unit) = cpu.setBreakAt(breakType,callback)
  def jmpTo(pc: Int) = cpu.jmpTo(pc)
  def disassemble(mem:Memory,address:Int) = cpu.disassemble(mem, address)
  // state
  protected def saveState(out:ObjectOutputStream) {
    if (ledListener != null) out.writeBoolean(ledListener.isOn)
    out.writeDouble(CYCLE_ADJ)
    out.writeInt(currentSpeedHz)
    out.writeDouble(cycleFrac)
    out.writeBoolean(running)
    out.writeLong(awakeCycles)
    out.writeLong(goSleepingCycles)
    out.writeBoolean(canSleep)
  }
  protected def loadState(in:ObjectInputStream) {
    if (ledListener != null) {
      if (in.readBoolean) ledListener.turnOn else ledListener.turnOff
    }
    CYCLE_ADJ = in.readDouble
    currentSpeedHz = in.readInt
    cycleFrac = in.readDouble
    running = in.readBoolean
    runningListener(running)
    awakeCycles = in.readLong
    goSleepingCycles = in.readLong
    canSleep = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}