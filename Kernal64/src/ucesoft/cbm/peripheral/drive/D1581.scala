package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.{CPU65xx, Memory, RAMComponent, ROM}
import ucesoft.cbm.formats.MFM
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceListener.{BreakType, CpuStepInfo, DisassembleInfo, StepType}
import ucesoft.cbm.{ChipID, Clock, Log}

import java.io.{ObjectInputStream, ObjectOutputStream, PrintWriter}
import java.util.Properties

object D1581 {
  object MFMEmptyFloppy extends EmptyFloppy {
    override val totalTracks = 80
    var offset = 0
    override def changeTrack(trackSteps:Int) : Unit = {
      if (trackSteps > 0) track +=1 else track -= 1
      notifyTrackSectorChangeListener()
    }
    override def nextByte: Int = {
      offset = (offset + 1) % MFM.TRACK_SIZE
      0
    }
    
    override def isOnIndexHole: Boolean = offset < 5
  }
}

class D1581(val driveID: Int, 
            bus: IECBus, 
            ledListener: DriveLedListener) extends 
            RAMComponent with 
            TraceListener with 
            Drive {
  val driveType: DriveType.Value = DriveType._1581
  val componentID: String = "D1581 Disk Drive " + (driveID + 8)
  val formatExtList: List[String] = List("D81")
  override val MIN_SPEED_HZ = 985248
  override val MAX_SPEED_HZ = 1000000
  val isRom = false
  val name = "D1581"
  val startAddress = 0x0
  val length = 0x2000
  val isActive = true  
  
  private[this] final val WD1772 = false
  private[this] final val _1581_WAIT_LOOP_ROUTINE = 0xB106
  private[this] final val WAIT_CYCLES_FOR_STOPPING = 4000000
  private[this] val clk = Clock.systemClock
  private[this] var running = true
  private[this] var tracing = false
  private[this] var floppy : Floppy = D1581.MFMEmptyFloppy
  private[this] var busDataDirection = 0
  private[this] var activeHead = 0
  private[this] var awakeCycles = 0L
  private[this] var canSleep = true
  private[this] var CYCLE_ADJ = 0.0 //(MAX_SPEED_MHZ - MIN_SPEED_MHZ) / MIN_SPEED_MHZ.toDouble
  private[this] var currentSpeedHz = MIN_SPEED_HZ
  private[this] var cycleFrac = 0.0
  private[this] var driveEnabled = true
  private[this] var diskChanged = false
  
  /************************************************************************************************************
   * CPU
   * 
   ***********************************************************************************************************/
  private[this] val cpu = CPU65xx.make(this,ChipID.CPU_1581)
  /************************************************************************************************************
   * CIA
   * 
   * PA0 = SIDE		out
   * PA1 = RDY		in
   * PA2 = MOTOR	out
   * PA3 = J1			in
   * PA4 = J2			in
   * PA5 = PLED		out
   * PA6 = ALED		out
   * PA7 = DSKCH	in
   * ----------------
   * PB0 = DATAIN in
   * PB1 = DATAOU out
   * PB2 = CLKIN	in
   * PB3 = CLKOUT out
   * PB4 = ATNACK out
   * PB5 = FSERDI out
   * PB6 = WPRT		in
   * PB7 = ATNIN	in
   ***********************************************************************************************************/
  private[this] val CIAPortAConnector = new Connector {
    val componentID = "1581_CIA_PortAConnector"
    private[this] val IDJACK = driveID & 0x03
    
    // 0x65 = 01100101 => reset latch's input pins
    final def read: Int = (latch & 0x65) | (if (floppy.isEmpty) 0x2 else 0) | IDJACK << 3 | (if (diskChanged) 0 else 0x80)
    
    final protected def performWrite(data:Int) : Unit = {
      val side0 = 1 - (data & 1)
      val motorOn = (data & 0x4) == 0
      val led = (data & 0x40) > 0 // activity led
      val powerLed = (data & 0x20) > 0
      
      if (motorOn != RW_HEAD.isMotorOn) {
        diskChanged = false
        RW_HEAD.setMotor(motorOn)
      }
      RW_HEAD.changeSide(side0)      
      if (led) ledListener.turnOn() else ledListener.turnOff()
      ledListener.turnPower(powerLed)
    }
  }
  private trait CIAPort {
    var releaseFSLine : () => Unit = _
  }
  private[this] val CIAPortBConnector = new Connector with IECBusListener with CIAPort {
    import IECBus._
    val busid: String = "CIAPortBConnector_" + driveID
    val componentID = "1581_CIA_PortBConnector"

    bus.registerListener(this)
    
    // 0x3A = 00111010 => reset latch's input pins
    final def read: Int = {
      val busbits = bus.data | bus.clk << 2 | bus.atn << 7
      (latch & 0x3A) | (if (RW_HEAD.isWriteProtected) 0 else 0x40) | busbits
    }
    
    final protected def performWrite(data:Int) : Unit = {
      bus.setLine(this,IECBusLine.DATA,data_out)
      bus.setLine(this,IECBusLine.CLK,clock_out)
      busDataDirection = (data & 0x20) >> 5
      if (busDataDirection == 0) releaseFSLine()
      autoacknwoledgeData()
    }
    
    override def atnChanged(oldValue:Int,newValue:Int) : Unit = {
      if (driveEnabled) {
        if (newValue == GROUND) awake()
        autoacknwoledgeData()
      }
    }
    
    @inline private def autoacknwoledgeData() : Unit = {
      val atna = (latch & 0x10) == 0
      val dataOut = bus.atn == IECBus.GROUND & !atna
      if (dataOut) bus.setLine(this,IECBusLine.DATA,GROUND) else bus.setLine(this,IECBusLine.DATA,data_out)
    }
    @inline private def data_out = if ((ddr & latch & 0x02) > 0) GROUND else VOLTAGE
    @inline private def clock_out = if ((ddr & latch & 0x08) > 0) GROUND else VOLTAGE
  }
  private[this] var ciaIRQ = false
  private def ciairq(low:Boolean) : Unit = {
    ciaIRQ = low
    cpu.irqRequest(low)
  }
  private[this] var ciaRunning = true
  private trait CIAFast {
    def releaseDataLine() : Unit
  }
  private[this] val CIA = new CIA("1581_CIA_FAST_" + driveID,0x4000,CIAPortAConnector,CIAPortBConnector,ciairq _, isIdle => ciaRunning = !isIdle,false) with IECBusListener with CIAFast {
    val busid: String = name
    setCIAModel(ucesoft.cbm.peripheral.cia.CIA.CIA_MODEL_8521)
    
    override def srqTriggered(): Unit = {
      if (busDataDirection == 0) serialIN(bus.data == IECBus.GROUND)
    }
    override def atnChanged(oldValue:Int,newValue:Int) : Unit = {
      if (driveEnabled) {
        if (newValue == IECBus.GROUND) setFlagLow()
      }
    }
    
    override def releaseDataLine() : Unit = {
      bus.setLine(this,IECBusLine.DATA,IECBus.VOLTAGE)
    }
    
    bus.registerListener(this)
    setSerialOUT(b => {
      if (busDataDirection == 1) {
        bus.setLine(this,IECBusLine.DATA,if (b) IECBus.GROUND else IECBus.VOLTAGE)
        bus.triggerSRQ(this)        
      }
    })
  }
  CIAPortBConnector.releaseFSLine = CIA.releaseDataLine _
  /************************************************************************************************************
   * R/W HEAD Controller
   * 
   ***********************************************************************************************************/
  private[this] val RW_HEAD = new MFMRWHeadController("1581",floppy,ledListener)
  /************************************************************************************************************
   * WD1770 Controller
   * 
   ***********************************************************************************************************/
  private[this] val WD1770 = new WD1770(RW_HEAD,0x6000,wd1772 = WD1772)
  /************************************************************************************************************
   * Memory: Kernal (32K) + RAM (8K)
   * 
   ***********************************************************************************************************/
  private[this] val ram = Array.fill(length)(0)
  private[this] val DISK_KERNEL = new ROM(null,"C1581_KERNEL",0x8000,0x8000,ROM.D1581_DOS_ROM_PROP) {
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  }
  
  final def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val address = _address & 0xFFFF
    if (address < 0x2000) ram(address)
    else
    if (address >= 0x6000 && address < 0x6004) {
      floppy.notifyTrackSectorChangeListener()
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
    if (address < 0x2000) {
      ram(address) = value & 0xFF
      //if (address == 0x914) setFilename
    }
    else
    if (address >= 0x6000 && address < 0x6004) WD1770.write(address,value)
    else
    if (address >= 0x4000 && address < 0x4010) CIA.write(address,value)    
  }
  override def getMem: Memory = this
  // ===============================================================
  
  override def disconnect() : Unit = {
    bus.unregisterListener(CIAPortBConnector)
    bus.unregisterListener(CIA)
  }
  
  override def getProperties: Properties = {
    properties.setProperty("Speed Hz",currentSpeedHz.toString)
    properties.setProperty("Cycle adjustment",CYCLE_ADJ.toString)
    properties.setProperty("Running", running.toString)
    properties.setProperty("Disk changed", diskChanged.toString)
    properties.setProperty("Write protect", RW_HEAD.isWriteProtected.toString)
    properties.setProperty("CIA Irq",ciaIRQ.toString)
 
    properties
  }
  
  def getFloppy : Floppy = floppy
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) : Unit = {
    floppy = driveReader
    RW_HEAD.setFloppy(floppy)
    RW_HEAD.setWriteProtected(floppy.isReadOnly)
    diskChanged = true
    
    awake()
  }
  override def isRunning: Boolean = running
  override def setActive(active: Boolean) : Unit = {
    driveEnabled = active
    running = active
    runningListener(active)
  }
  override def canGoSleeping: Boolean = this.canSleep
  override def setCanSleep(canSleep: Boolean) : Unit = {
    this.canSleep = canSleep
    awake()
  }

  override def isReadOnly: Boolean = RW_HEAD.isWriteProtected
  override def setReadOnly(readOnly:Boolean): Unit = RW_HEAD.setWriteProtected(readOnly)
  
  def init() : Unit = {
    Log.info("Initializing D1581...")
    add(cpu)
    add(DISK_KERNEL)
    add(CIA)    
    add(RW_HEAD)
    add(WD1770)
    if (ledListener != null) ledListener.turnOn()
    
    RW_HEAD.setSpeedZone(4)
    java.util.Arrays.fill(ram,0)
  }
  
  def reset() : Unit = {
    tracing = false
    busDataDirection = 0
    activeHead = 0
    running = true
    runningListener(true)
    awakeCycles = 0
    cycleFrac = 0.0
    diskChanged = false
    if (ledListener != null) {
      ledListener.turnOn()
      ledListener.turnPower(false)
    }

    RW_HEAD.setSpeedZone(4)
    ciaRunning = true
  }

  override def hardReset(): Unit = {
    reset()
    java.util.Arrays.fill(ram,0)
  }
  
  override def setSpeedHz(speed:Int) : Unit = {
    currentSpeedHz = speed
    CYCLE_ADJ = (speed - MIN_SPEED_HZ) / MIN_SPEED_HZ.toDouble
  }
  
  @inline private def checkPC(cycles: Long) : Unit = {
    val pc = cpu.getPC
    if (pc == _1581_WAIT_LOOP_ROUTINE && canSleep && !RW_HEAD.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
      running = false
      runningListener(false)
    }
  }
  
  final def clock(cycles: Long) : Unit = {
    if (running) {
      checkPC(cycles)
      var n_cycles = 2
      
      if (CYCLE_ADJ > 0) {
        cycleFrac += CYCLE_ADJ
        if (cycleFrac >= 1) {
          cycleFrac -= 1
          n_cycles += 1
        }
      }
      while (n_cycles > 0) { // 2Mhz stuff
        cpu.fetchAndExecute(1)
        if (ciaRunning) CIA.clock(WD1772)
        WD1770.clock()
        n_cycles -= 1
      }
      
      RW_HEAD.rotate      
    }
  }
  
  private def awake() : Unit = {
    running = true
    runningListener(true)
    awakeCycles = clk.currentCycles
  }
    
  // ================== Tracing ====================================
  override def getRegisters(): List[TraceListener.TraceRegister] = cpu.getRegisters()
  override def setCycleMode(cycleMode: Boolean): Unit = {
    cpu.setCycleMode(cycleMode)
  }
  override def setTraceOnFile(out:PrintWriter,enabled:Boolean) : Unit = {/* ignored */}
  override def setTrace(traceOn: Boolean) : Unit = {
    tracing = traceOn
    if (tracing) awake()
    cpu.setTrace(traceOn)
  }
  override def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType): Unit = cpu.step(updateRegisters,stepType)
  override def setBreakAt(breakType:BreakType,callback:CpuStepInfo => Unit): Unit = cpu.setBreakAt(breakType,callback)
  override def jmpTo(pc: Int): Unit = cpu.jmpTo(pc)
  override def disassemble(address:Int): DisassembleInfo = cpu.disassemble(address)
  // ================== State ======================================
  protected def saveState(out:ObjectOutputStream) : Unit = {
    if (ledListener != null) out.writeBoolean(ledListener.isOn)
    out.writeObject(ram)
    out.writeInt(busDataDirection)
    out.writeInt(activeHead)
    out.writeBoolean(canSleep)
    out.writeBoolean(running)
    out.writeDouble(cycleFrac)
    out.writeDouble(CYCLE_ADJ)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    if (ledListener != null) {
      if (in.readBoolean) ledListener.turnOn() else ledListener.turnOff()
    }
    loadMemory[Int](ram,in)
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