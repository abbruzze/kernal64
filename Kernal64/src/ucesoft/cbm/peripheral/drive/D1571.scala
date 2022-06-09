package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.cpu.{CPU65xx, Memory, RAMComponent, ROM}
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.trace.{BreakType, CpuStepInfo, TraceListener}

import java.io.{ObjectInputStream, ObjectOutputStream, PrintWriter}
import java.util.Properties

class D1571(val driveID: Int, 
            bus: IECBus, 
            ledListener: DriveLedListener,
            _1571Listener: Boolean => Unit) extends 
            RAMComponent with 
            TraceListener with 
            Drive {
  val driveType: DriveType.Value = DriveType._1571
  val componentID: String = "D1571 Disk Drive " + (driveID + 8)
  val formatExtList: List[String] = List("D64","D71","G64")
  override val MIN_SPEED_HZ = 985248
  override val MAX_SPEED_HZ = 1000000
  val isRom = false
  val name = "D1571"
  val startAddress = 0x0
  val length = 0x0800
  val isActive = true
  
  private[this] val _1571_LOAD_ROUTINE = 0x9088
  private[this] val _1541_LOAD_ROUTINE = 0xD7B4
  private[this] val _1541_WAIT_LOOP_ROUTINE = 0xEBFF
  private[this] val WAIT_CYCLES_FOR_STOPPING = 2000000
  private[this] val clk = Clock.systemClock
  private[this] var running = true
  private[this] var tracing = false
  private[this] var channelActive = 0
  private[this] var floppy : Floppy = EmptyFloppy
  private[this] var _1541Mode = true     // VIA1 PA5 Output  
  private[this] var busDataDirection = 0 // VIA1 PA1 Output
  private[this] var activeHead = 0       // VIA1 PA2 Output
  private[this] var awakeCycles = 0L
  private[this] var canSleep = true
  private[this] var CYCLE_ADJ = 0.0 //(MAX_SPEED_MHZ - MIN_SPEED_MHZ) / MIN_SPEED_MHZ.toDouble
  private[this] var currentSpeedHz = MIN_SPEED_HZ
  private[this] var cycleFrac = 0.0
  
  // ================== Components =================================  
  /************************************************************************************************************
   * CPU
   * 
   ***********************************************************************************************************/
  private[this] val cpu = CPU65xx.make(this,ChipID.CPU_1571)
  /**
   * IRQ Manager for VIA1,VIA2 and CIA
   */
  private[this] val IRQSwitcher = new CBMComponent {
    val componentID = "IRQ Switcher (VIA1,VIA2,CIA)"
    val componentType: Type = CBMComponentType.INTERNAL

    private[this] var viaBusIRQLow = false
    private[this] var viaDiskIRQLow = false
    private[this] var ciaIRQLow = false

    private def handleIRQ(): Unit = {
      Log.debug(s"Handling IRQ viaBusIRQ=$viaBusIRQLow viaDiskIRQ=$viaDiskIRQLow ciaIRQ=$ciaIRQLow")
      cpu.irqRequest(viaBusIRQLow || viaDiskIRQLow || ciaIRQLow)
    }

    override def getProperties: Properties = {
      properties.setProperty("ViaBus IRQ", viaBusIRQLow.toString)
      properties.setProperty("ViaDisk IRQ", viaDiskIRQLow.toString)
      properties.setProperty("CIA IRQ", ciaIRQLow.toString)

      properties
    }

    final def viaBusIRQ(low: Boolean) : Unit = {
      Log.debug("VIABus setting IRQ as " + low)
      viaBusIRQLow = low
      handleIRQ
    }
    final def viaDiskIRQ(low: Boolean) : Unit = {
      Log.debug("VIADisk setting IRQ as " + low)
      viaDiskIRQLow = low
      handleIRQ
    }
    final def ciaIRQ(low: Boolean) : Unit = {
      Log.debug("CIA setting IRQ as " + low)
      ciaIRQLow = low
      handleIRQ
    }

    def init : Unit = {}

    def reset : Unit = {
      viaBusIRQLow = false
      viaDiskIRQLow = false
      ciaIRQLow = false      
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeBoolean(viaBusIRQLow)
      out.writeBoolean(viaDiskIRQLow)
      out.writeBoolean(ciaIRQLow)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      viaBusIRQLow = in.readBoolean
      viaDiskIRQLow = in.readBoolean
      ciaIRQLow = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }
  /**
   * Empty CIA Connector
   */
  private[this] val EmptyCIAConnector = new Connector {
    val componentID = "1571_CIA_EmptyConnector"
    def read = 0xFF
    protected def performWrite(data:Int) : Unit = {}
  }
  /************************************************************************************************************
   * CIA for fast serial bus
   * 
   ***********************************************************************************************************/
  private[this] var ciaRunning = true
  private[this] val CIA = new CIA("CIA_FAST_" + driveID,0x4000,EmptyCIAConnector,EmptyCIAConnector,IRQSwitcher.ciaIRQ _,isIdle => ciaRunning = !isIdle,false) with IECBusListener {
    val busid: String = name
    setCIAModel(ucesoft.cbm.peripheral.cia.CIA.CIA_MODEL_8521)
    
    override def srqTriggered: Unit = if (busDataDirection == 0) serialIN(bus.data == IECBus.GROUND)
    
    bus.registerListener(this)
    setSerialOUT(b => {
      if (busDataDirection == 1) {
        bus.setLine(this,IECBusLine.DATA,if (b) IECBus.GROUND else IECBus.VOLTAGE)
        bus.triggerSRQ(this)
      }
    })
  }
  /************************************************************************************************************
   * R/W HEAD Controller
   * 
   ***********************************************************************************************************/
  private[this] val RW_HEAD = new GCRRWHeadController("1571",floppy,ledListener)
  /************************************************************************************************************
   * VIA1 IEC Bus Manager
   * 
   ***********************************************************************************************************/
  private[this] val VIA1 = new VIA("VIA1571_IECBus" + driveID,0x1800,IRQSwitcher.viaBusIRQ _) with IECBusListener {
    override lazy val componentID = "VIA1571_1 (Bus)"
    val busid: String = name
    private[this] val IDJACK = driveID & 0x03
    private[this] var driveEnabled = true
    
    bus.registerListener(this)
    
    def setEnabled(enabled:Boolean): Unit = driveEnabled = enabled
    
    override def atnChanged(oldValue:Int,newValue:Int) : Unit = {
      if (driveEnabled) {
        if (newValue == IECBus.GROUND) {
          irq_set(IRQ_CA1)
          awake
        }
        autoacknwoledgeData
      }
    }
        
    override def read(address: Int, chipID: ChipID.ID): Int = (address & 0x0F) match {
      case ad@(PA|PA2) =>
        super.read(address,chipID)
        (if (floppy.currentTrack != 1) 1 else 0) | busDataDirection << 1 | activeHead << 2 | (if (_1541Mode) 0 else 1 << 5) | RW_HEAD.getByteReadySignal << 7
      case PB =>
        (super.read(address,chipID) & 0x1A) | (bus.data|data_out) | (bus.clk|clock_out) << 2 | bus.atn << 7 | IDJACK << 5
        
      case ofs => super.read(address,chipID)
    }
    
    override def write(address: Int, value: Int, chipID: ChipID.ID) : Unit = {
      super.write(address,value,chipID)
      (address & 0x0F) match {
        case PB|DDRB =>        
          bus.setLine(this,IECBusLine.DATA,data_out)
          bus.setLine(this,IECBusLine.CLK,clock_out)          
          autoacknwoledgeData
        case ad@(PA|PA2) =>
          busDataDirection = (value & 2) >> 1
          if (busDataDirection == 0) bus.setLine(CIA,IECBusLine.DATA,IECBus.VOLTAGE)
          activeHead = (value & 4) >> 2
          if (floppy.side != activeHead) RW_HEAD.changeSide(activeHead)
          _1541Mode = (value & 0x20) == 0
          _1571Listener(!_1541Mode)
          //println(s"1571: 1541mode=${_1541Mode} busDirection=$busDataDirection activeHead=$activeHead")
        case _ =>
      }         
    }
    
    @inline private def data_out = if ((regs(DDRB) & regs(PB) & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
    @inline private def clock_out = if ((regs(DDRB) & regs(PB) & 0x08) > 0) IECBus.GROUND else IECBus.VOLTAGE
    
    @inline private def autoacknwoledgeData() : Unit = {
      if ((regs(DDRB) & 0x10) > 0) {
        val atna = (regs(DDRB) & regs(PB) & 0x10) > 0
        val dataOut = (bus.atn == IECBus.GROUND) ^ atna
        if (dataOut) bus.setLine(this, IECBusLine.DATA, IECBus.GROUND) else bus.setLine(this, IECBusLine.DATA, data_out)
      }
    }
    // state
    override protected def saveState(out:ObjectOutputStream) : Unit = {
      super.saveState(out)
      out.writeBoolean(driveEnabled)
    }
    override protected def loadState(in:ObjectInputStream) : Unit = {
      super.loadState(in)
      driveEnabled = in.readBoolean
    }
  }
  /************************************************************************************************************
   * VIA2 Disk Controller
   * 
   ***********************************************************************************************************/
  private[this] val VIA2 = new VIA("VIA1571_DiskControl", 0x1C00,IRQSwitcher.viaDiskIRQ _) {
    override lazy val componentID = "VIA1571_2 (DC)"
    private[this] val WRITE_PROTECT_SENSE = 0x10
    private[this] val WRITE_PROTECT_SENSE_WAIT = 3 * 400000L
    private[this] val REMOVING_DISK_WAIT = 500000L
    private[this] val SYNC_DETECTION_LINE = 0x80
    private[this] var isDiskChanged = true
    private[this] var isDiskChanging = false

    def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) : Unit = {
      floppy = driveReader
      if (emulateInserting) {
        RW_HEAD.setFloppy(EmptyFloppy)
        isDiskChanged = true
        isDiskChanging = true
        VIA1.irq_set(IRQ_CA2)
        clk.schedule(new ClockEvent("DiskRemoving",Clock.systemClock.currentCycles + WRITE_PROTECT_SENSE_WAIT, cycles => {
          isDiskChanged = false
          clk.schedule(new ClockEvent("DiskWaitingInserting",cycles + REMOVING_DISK_WAIT, cycles => {
            isDiskChanged = true
            VIA1.irq_set(IRQ_CA2)
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
    
    override def read(address: Int, chipID: ChipID.ID): Int = (address & 0x0F) match {
      case PB =>
        val wps = if (isDiskChanging) isDiskChanged else RW_HEAD.isWriteProtected
        (super.read(address, chipID) & ~(WRITE_PROTECT_SENSE | SYNC_DETECTION_LINE)) |
          (if (RW_HEAD.isSync) 0x00 else SYNC_DETECTION_LINE) |
          (if (wps) 0x00 else WRITE_PROTECT_SENSE)
      case PA|PA2 =>
        super.read(address, chipID)
        floppy.notifyTrackSectorChangeListener
        if ((regs(PCR) & 0x0C) == 0x08) RW_HEAD.canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
        RW_HEAD.resetByteReadySignal
        RW_HEAD.getLastRead
      case ofs => super.read(address, chipID)
    }
  
    override def write(address: Int, value: Int, chipID: ChipID.ID) : Unit = {
      (address & 0x0F) match {
        case PA =>
          if ((regs(PCR) & 0x0C) == 0x08) RW_HEAD.canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
          RW_HEAD.setNextToWrite(value)
          RW_HEAD.resetByteReadySignal
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
          
    final def byteReady() : Unit = {
      cpu.setOverflowFlag
      irq_set(IRQ_CA1)
      
      regs(PCR) & 0x0E match {
        case 0xA|0x08 => RW_HEAD.canSetByteReady = false
        case _ =>
      }
    }
    
    // state
    override protected def saveState(out:ObjectOutputStream) : Unit = {
      super.saveState(out)
      out.writeBoolean(isDiskChanged)
      out.writeBoolean(isDiskChanging)
    }
    override protected def loadState(in:ObjectInputStream) : Unit = {
      super.loadState(in)
      isDiskChanged = in.readBoolean
      isDiskChanging = in.readBoolean
    }
  }
  /************************************************************************************************************
   * WD1770 Controller
   * 
   ***********************************************************************************************************/
  private[this] val WD1770 = new WD1770(RW_HEAD,0x2000)
  /************************************************************************************************************
   * Memory: Kernal (32K) + RAM (2K)
   * 
   ***********************************************************************************************************/
  private[this] val ram = Array.fill(length)(0)
  private[this] val DISK_KERNEL = new ROM(null,"C1571_KERNEL",0x8000,0x8000,ROM.D1571_DOS_ROM_PROP) {
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  }
  
  final def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val address = _address & 0xFFFF
    if (address < 0x800) ram(address)
    else
    if (address >= 0x1800 && address < 0x1810) VIA1.read(address)
    else
    if (address >= 0x1C00 && address < 0x1C10) VIA2.read(address)
    else
    if (address >= 0x2000 && address < 0x2004) {
      //println("WD1770 reading from " + Integer.toHexString(address) + " " + Integer.toHexString(cpu.getPC))
      WD1770.read(address)
    }
    else
    if (address >= 0x4000 && address < 0x4010) CIA.read(address)
    else
    if (address >= 0x8000 && address < 0x10000) DISK_KERNEL.read(address)
    else 0
    
  }
  final def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    val address = _address & 0xFFFF
    if (address < 0x800) {
      ram(address) = value & 0xFF
      if (address >= 0x22B && address <= 0x239) {
        val channel = address - 0x22B
        if (value != 0xFF) channelActive |= 1 << channel else channelActive &= ~(1 << channel)
      }
    }
    else
    if (address >= 0x1800 && address < 0x1810) VIA1.write(address,value)
    else
    if (address >= 0x1C00 && address < 0x1C10) VIA2.write(address,value)
    else
    if (address >= 0x2000 && address < 0x2004) {
      //println("WD1770 writing to " + Integer.toHexString(address) + " = " + Integer.toHexString(value))
      WD1770.write(address,value)
    } //TODO WD1770
    else
    if (address >= 0x4000 && address < 0x4010) CIA.write(address,value)    
  }
  override def getMem: Memory = this
  // ===============================================================
  
  override def disconnect : Unit = {
    bus.unregisterListener(VIA1)
    bus.unregisterListener(CIA)
  }
  
  override def getProperties: Properties = {
    properties.setProperty("Speed Hz",currentSpeedHz.toString)
    properties.setProperty("Cycle adjustment",CYCLE_ADJ.toString)
    properties.setProperty("1541 mode",_1541Mode.toString)
    properties.setProperty("Running", running.toString)
    val channels = channelActive
    properties.setProperty("Total channels active:",channels.toString)
    for (i <- 0 to 15) {
      val opened = (channels & (1 << i)) > 0
      properties.setProperty("Channel " + i, if (opened) "open" else "inactive")
    }
    properties
  }
  
  def getFloppy : Floppy = floppy
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) : Unit = {
    VIA2.setDriveReader(driveReader,emulateInserting)    
    RW_HEAD.setWriteProtected(floppy.isReadOnly)
  }
  override def isRunning: Boolean = running
  override def setActive(active: Boolean) : Unit = {
    VIA1.setEnabled(active)
    running = active
    runningListener(active)
  }
  override def canGoSleeping: Boolean = this.canSleep
  override def setCanSleep(canSleep: Boolean) : Unit = {
    this.canSleep = canSleep
    awake
  }

  override def isReadOnly: Boolean = RW_HEAD.isWriteProtected
  override def setReadOnly(readOnly:Boolean): Unit = RW_HEAD.setWriteProtected(readOnly)
  
  def init : Unit = {
    Log.info("Initializing C1571...")
    add(cpu)
    add(IRQSwitcher)
    add(DISK_KERNEL)
    add(VIA1)
    add(VIA2)
    add(CIA)    
    add(RW_HEAD)
    add(WD1770)
    if (ledListener != null) ledListener.turnOn
    java.util.Arrays.fill(ram,0)
  }
  
  def reset : Unit = {
    tracing = false
    channelActive = 0
    _1541Mode = true  
    busDataDirection = 0
    activeHead = 0
    canSleep = true
    running = true
    runningListener(true)
    awakeCycles = 0
    cycleFrac = 0.0
    if (ledListener != null) ledListener.turnOn
    ciaRunning = true
  }

  override def hardReset: Unit = {
    reset
    java.util.Arrays.fill(ram,0)
  }
  
  private def setFilename() : Unit = {
    var fileNameSize = ram(0x274)
    var adr = 0x200
    val sb = new StringBuilder
    while (fileNameSize > 0) {
      val c = ram(adr)
      sb.append(c.toChar)
      adr += 1
      fileNameSize -= 1      
    }
    RW_HEAD.setCurrentFileName(sb.toString)
  }
  
  override def setSpeedHz(speed:Int) : Unit = {
    currentSpeedHz = speed
    CYCLE_ADJ = (speed - MIN_SPEED_HZ) / MIN_SPEED_HZ.toDouble
  }
  
  @inline private def checkPC(cycles: Long) : Unit = {
    val pc = cpu.getPC
    if (pc == _1541_LOAD_ROUTINE || pc == _1571_LOAD_ROUTINE) setFilename
    else 
    if (pc == _1541_WAIT_LOOP_ROUTINE && canSleep && channelActive == 0 && !RW_HEAD.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
      running = false
      runningListener(false)
      VIA1.setActive(false)
      VIA2.setActive(false)
    }
  }
  
  final def clock(cycles: Long) : Unit = {
    if (running) {
      checkPC(cycles)
      var n_cycles = if (_1541Mode) 1 else 2
      
      if (CYCLE_ADJ > 0) {
        cycleFrac += CYCLE_ADJ
        if (cycleFrac >= 1) {
          cycleFrac -= 1
          n_cycles += 1
        }
      }
      while (n_cycles > 0) {
        cpu.fetchAndExecute(1)
        if (ciaRunning) CIA.clock(false)
        VIA1.clock(cycles)
        VIA2.clock(cycles)
        n_cycles -= 1
      }
      
      if (RW_HEAD.rotate) VIA2.byteReady
      WD1770.clock
    }
  }
  
  private def awake() : Unit = {
    running = true
    runningListener(true)
    VIA1.setActive(true)
    VIA2.setActive(true)
    awakeCycles = clk.currentCycles    
  }
    
  // ================== Tracing ====================================
  def setCycleMode(cycleMode: Boolean): Unit = {
    cpu.setCycleMode(cycleMode)
  }
  def setTraceOnFile(out:PrintWriter,enabled:Boolean) : Unit = {/* ignored */}
  def setTrace(traceOn: Boolean) : Unit = {
    tracing = traceOn
    if (tracing) awake
    cpu.setTrace(traceOn)
  }
  def step(updateRegisters: CpuStepInfo => Unit): Unit = cpu.step(updateRegisters)
  def setBreakAt(breakType:BreakType,callback:CpuStepInfo => Unit): Unit = cpu.setBreakAt(breakType,callback)
  def jmpTo(pc: Int): Unit = cpu.jmpTo(pc)
  def disassemble(mem:Memory,address:Int): (String, Int) = cpu.disassemble(mem, address)
  // ================== State ======================================
  protected def saveState(out:ObjectOutputStream) : Unit = {
    if (ledListener != null) out.writeBoolean(ledListener.isOn)
    out.writeObject(ram)
    out.writeInt(channelActive)
    out.writeBoolean(_1541Mode)  
    out.writeInt(busDataDirection)
    out.writeInt(activeHead)
    out.writeBoolean(canSleep)
    out.writeBoolean(running)
    out.writeDouble(cycleFrac)
    out.writeDouble(CYCLE_ADJ)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    if (ledListener != null) {
      if (in.readBoolean) ledListener.turnOn else ledListener.turnOff
    }
    loadMemory[Int](ram,in)
    channelActive = in.readInt
    _1541Mode = in.readBoolean  
    busDataDirection = in.readInt
    activeHead = in.readInt
    canSleep = in.readBoolean
    running = in.readBoolean
    cycleFrac = in.readDouble
    CYCLE_ADJ = in.readDouble
  }
  protected def allowsStateRestoring : Boolean = true

  override protected def componentIDMismatchHandling(id:String) : Unit = {
    //throw new IOException(s"This snapshot was done with drive ${driveID + 8} of type $id. Please change drive ${driveID + 8} type")
    throw new DriveIDMismatch(driveID,id)
  }
}
  