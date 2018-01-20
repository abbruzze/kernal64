package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.CPU6510
import ucesoft.cbm.ChipID
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import java.io.PrintWriter
import ucesoft.cbm.trace.BreakType
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.cpu.ROM
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.bus.IECBusListener
import ucesoft.cbm.peripheral.bus.IECBusLine
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent

class C1571(val driveID: Int, 
            bus: IECBus, 
            ledListener: DriveLedListener) extends 
            RAMComponent with 
            TraceListener with 
            Drive {
  val componentID = "C1571 Disk Drive " + driveID
  override val MIN_SPEED_HZ = 985248
  override val MAX_SPEED_HZ = 1000000
  val isRom = false
  val name = "C1571"
  val startAddress = 0x0
  val length = 0x0800
  val isActive = true
  
  private[this] val _1541_LOAD_ROUTINE = 0xD7B4
  private[this] val _1541_FORMAT_ROUTINE = 0xC8C6
  private[this] val _1541_FORMAT_ROUTINE_OK = 0xC8EF
  private[this] val _1541_FORMAT_ROUTINE_NOK = 0xC8E8
  private[this] val _1541_WAIT_LOOP_ROUTINE = 0xEBFF
  private[this] val WAIT_CYCLES_FOR_STOPPING = 2000000
  private[this] final val GO_SLEEPING_MESSAGE_CYCLES = 3000000
  private[this] val clk = Clock.systemClock
  private[this] var running = true
  private[this] var tracing = false
  private[this] var channelActive = 0
  private[this] var floppy : Floppy = EmptyFloppy
  private[this] var _1541Mode = true     // VIA1 PA5 Output  
  private[this] var busDataDirection = 0 // VIA1 PA1 Output
  private[this] var activeHead = 0       // VIA1 PA2 Output
  private[this] var byteReadySignal = 0  // VIA1 PA7 Input
  private[this] var awakeCycles = 0L
  private[this] var goSleepingCycles = 0L
  private[this] var canSleep = true
  private[this] var useTRAPFormat = false
  private[this] var CYCLE_ADJ = 0.0 //(MAX_SPEED_MHZ - MIN_SPEED_MHZ) / MIN_SPEED_MHZ.toDouble
  private[this] var currentSpeedHz = MIN_SPEED_HZ
  private[this] var cycleFrac = 0.0
  
  // ================== Components =================================  
  /************************************************************************************************************
   * CPU
   * 
   ***********************************************************************************************************/
  private[this] val cpu = CPU6510.make(this,ChipID.CPU_1571)  
  /**
   * IRQ Manager for VIA1,VIA2 and CIA
   */
  private[this] val IRQSwitcher = new CBMComponent {
    val componentID = "IRQ Switcher (VIA1,VIA2,CIA)"
    val componentType = CBMComponentType.INTERNAL

    private[this] var viaBusIRQLow = false
    private[this] var viaDiskIRQLow = false
    private[this] var ciaIRQLow = false

    private def handleIRQ = {
      Log.debug(s"Handling IRQ viaBusIRQ=${viaBusIRQLow} viaDiskIRQ=${viaDiskIRQLow} ciaIRQ=$ciaIRQLow")
      cpu.irqRequest(viaBusIRQLow || viaDiskIRQLow || ciaIRQLow)
    }

    override def getProperties = {
      properties.setProperty("ViaBus IRQ", viaBusIRQLow.toString)
      properties.setProperty("ViaDisk IRQ", viaDiskIRQLow.toString)
      properties.setProperty("CIA IRQ", ciaIRQLow.toString)

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
    final def ciaIRQ(low: Boolean) {
      Log.debug("CIA setting IRQ as " + low)
      ciaIRQLow = low
      handleIRQ
    }

    def init {}

    def reset {
      viaBusIRQLow = false
      viaDiskIRQLow = false
      ciaIRQLow = false      
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(viaBusIRQLow)
      out.writeBoolean(viaDiskIRQLow)
      out.writeBoolean(ciaIRQLow)
    }
    protected def loadState(in:ObjectInputStream) {
      viaBusIRQLow = in.readBoolean
      viaDiskIRQLow = in.readBoolean
      ciaIRQLow = in.readBoolean
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
  /**
   * Empty CIA Connector
   */
  private[this] val EmptyCIAConnector = new Connector {
    val componentID = "1571_CIA_EmptyConnector"
    def read = 0xFF
    protected def performWrite(data:Int) {}
  }
  /************************************************************************************************************
   * CIA for fast serial bus
   * 
   ***********************************************************************************************************/
  private[this] val CIA = new CIA("CIA_FAST",0x4000,EmptyCIAConnector,EmptyCIAConnector,IRQSwitcher.ciaIRQ _,false) with IECBusListener {
    val busid = name    
    
    override def srqTriggered = if (busDataDirection == 0) serialIN(bus.data == IECBus.GROUND)    
    
    bus.registerListener(this)
    setSerialOUT(b => {
      if (busDataDirection == 1) {
        bus.setLine(busid,IECBusLine.DATA,if (b) IECBus.GROUND else IECBus.VOLTAGE)
        bus.triggerSRQ(busid)
      }
    })
  }
  /************************************************************************************************************
   * VIA1 IEC Bus Manager
   * 
   ***********************************************************************************************************/
  private[this] val VIA1 = new VIA("VIA1571_IECBus" + driveID,0x1800,IRQSwitcher.viaBusIRQ _) with IECBusListener {
    override lazy val componentID = "VIA1571_1 (Bus)"
    val busid = name
    private[this] val IDJACK = driveID & 0x03
    private[this] var driveEnabled = true
    
    bus.registerListener(this)
    
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
        
    override def read(address: Int, chipID: ChipID.ID) = (address & 0x0F) match {
      case ad@(PA|PA2) =>
        super.read(address,chipID)
        (if (floppy.currentTrack == 0) 1 else 0) | busDataDirection << 1 | activeHead << 2 | (if (_1541Mode) 0 else 1 << 5) | byteReadySignal << 7
      case PB =>
        (super.read(address,chipID) & 0x1A) | (bus.data|data_out) | (bus.clk|clock_out) << 2 | bus.atn << 7 | IDJACK << 5
        
      case ofs => super.read(address,chipID)
    }
    
    override def write(address: Int, value: Int, chipID: ChipID.ID) {
      super.write(address,value,chipID)
      (address & 0x0F) match {
        case PB|DDRB =>        
          bus.setLine(busid,IECBusLine.DATA,data_out)
          bus.setLine(busid,IECBusLine.CLK,clock_out)          
          autoacknwoledgeData
        case ad@(PA|PA2) =>
          busDataDirection = (value & 2) >> 1
          if (busDataDirection == 0) bus.setLine(CIA.name,IECBusLine.DATA,IECBus.VOLTAGE)
          activeHead = (value & 4) >> 2
          if (floppy.side != activeHead) {
            floppy.side = activeHead
            sideChanged
          }
          _1541Mode = (value & 0x20) == 0          
          //println(s"1571: 1541mode=${_1541Mode} busDirection=$busDataDirection activeHead=$activeHead")
        case _ =>
      }         
    }
    
    @inline private def data_out = if ((regs(DDRB) & regs(PB) & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
    @inline private def clock_out = if ((regs(DDRB) & regs(PB) & 0x08) > 0) IECBus.GROUND else IECBus.VOLTAGE
    
    @inline private def autoacknwoledgeData {
      val atna = (regs(DDRB) & regs(PB) & 0x10) > 0
      val dataOut = (bus.atn == IECBus.GROUND) ^ atna
      if (dataOut) bus.setLine(busid,IECBusLine.DATA,IECBus.GROUND) else bus.setLine(busid,IECBusLine.DATA,data_out)
    }
    // state
    override protected def saveState(out:ObjectOutputStream) {
      super.saveState(out)
      out.writeBoolean(driveEnabled)
    }
    override protected def loadState(in:ObjectInputStream) {
      super.loadState(in)
      driveEnabled = in.readBoolean
    }
  }
  private def sideChanged {
    VIA2.sideChanged
  }
  /************************************************************************************************************
   * VIA2 Disk Controller
   * 
   ***********************************************************************************************************/
  private[this] val VIA2 = new VIA("VIA1571_DiskControl", 0x1C00,IRQSwitcher.viaDiskIRQ _) {
    val busid = "VIA1571_DiskControl"
    override lazy val componentID = "VIA1571_2 (DC)"
    private[this] val WRITE_PROTECT_SENSE = 0x10
    private[this] val WRITE_PROTECT_SENSE_WAIT = 3 * 400000L
    private[this] val REMOVING_DISK_WAIT = 500000L
    private[this] val SYNC_DETECTION_LINE = 0x80
    private[this] var isWriting = false
    private[this] var isDiskChanged = true
    private[this] var isDiskChanging = false
    private[this] var isReadOnly = false
    private[this] var motorOn = false
    private[this] var canSetByteReady = false
    private[this] var byteReady = false
    private[this] var track = 1
    private[this] var trackSteps = track << 1    
    private[this] var currentFilename = ""
    
    /**
     * Get the rotation cycles needed for read a byte for the given zone.
     *
     * Zone	Tracks	Clock rate
     * ----------------------------
     * 1		1-17	307.692 bit/s
     * 2		18-24	285.714 bit/s
     * 3		25-30	266.667 bit/s
     * 4		31-35	250.000 bit/s
     */
    private[this] val C1541_CLOCK_HZ = 1000000
    private[this] val rotationCyclesForBit = Array[Double](
        250000.0 / C1541_CLOCK_HZ,  // zone 4
        266667.0 / C1541_CLOCK_HZ,  // zone 3
        285714.0 / C1541_CLOCK_HZ,  // zone 2
        307692.0 / C1541_CLOCK_HZ   // zone 1 
    )
      
    private[this] var speedZone = 3
    private[this] var bitCycleWait = rotationCyclesForBit(speedZone)
    private[this] var rotationCycleCounter = 0.0  
    private[this] var bitCounter = 0
    private[this] var last10Bits,lastRead = 0
    private[this] var lastWrite = 0x55
    
    override def reset {
      super.reset
      resetFloppy
    }
    
    private def resetFloppy {
      isWriting = false
      isDiskChanged = true
      isDiskChanging = false
      motorOn = false
      canSetByteReady = false
      byteReady = false
      trackSteps = 2
      track = 1
      currentFilename = ""
      bitCounter = 0
      speedZone = 3
      bitCycleWait = rotationCyclesForBit(speedZone)
      rotationCycleCounter = 0.0
      last10Bits = 0
      lastRead = 0
      lastWrite = 0x55
    }
        
    override def getProperties = {
      properties.setProperty("Motor on",motorOn.toString)
      properties.setProperty("Writing",isWriting.toString)
      properties.setProperty("Track","%2d".format(track))
      properties.setProperty("Disk",floppy.toString)
      properties.setProperty("Current filename",currentFilename)
      properties.setProperty("Byte ready signal",byteReadySignal.toString)
      properties.setProperty("Last byte read",Integer.toHexString(lastRead))
      super.getProperties
    } 
    
    def awake {
      //if (isDiskChanged) diskChangedAtClockCycle = Clock.systemClock.currentCycles
    }
    
    def setReadOnly(readOnly:Boolean) = isReadOnly = readOnly
  
    def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) {
      floppy = driveReader
      track = 18
      trackSteps = track << 1
      if (emulateInserting) {
        // reset the last track
        floppy.changeTrack(trackSteps) 
        floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
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
            }))
          }))
        }))
      }
      else floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
    }
    
    def setCurrentFilename(fn:String) = currentFilename = fn
    def formatDisk = {
      try {
        println("Formatting '" + currentFilename + "'")
        floppy.format(currentFilename)
        true
      }
      catch {
        case t:IllegalArgumentException =>
          t.printStackTrace
          false
      }
    }
    
    def isMotorOn = motorOn
  
    @inline private def isSync = !isWriting && motorOn && last10Bits == 0x3FF  
  
    override def read(address: Int, chipID: ChipID.ID) = (address & 0x0F) match {
      case PB =>
        val wps = if (isDiskChanging) isDiskChanged else isReadOnly | floppy.isReadOnly
        (super.read(address, chipID) & ~(WRITE_PROTECT_SENSE | SYNC_DETECTION_LINE)) |
          (if (isSync) 0x00 else SYNC_DETECTION_LINE) |
          (if (wps) 0x00 else WRITE_PROTECT_SENSE)
      case PA|PA2 =>
        super.read(address, chipID)
        floppy.notifyTrackSectorChangeListener
        if ((regs(PCR) & 0x0C) == 0x08) canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
        byteReadySignal = 1
        lastRead
      case ofs => super.read(address, chipID)
    }
  
    override def write(address: Int, value: Int, chipID: ChipID.ID) {
      (address & 0x0F) match {
        case PA =>
          if ((regs(PCR) & 0x0C) == 0x08) canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
          byteReadySignal = 1
        case PCR =>
          value & 0x0E match {
            case 0xE => canSetByteReady = true        // 111 => Manual output mode high
            case 0xC => canSetByteReady = false       // 110 => Manual output mode low
            case _ => canSetByteReady = true
          }
          isWriting = (value & 0x20) == 0          
          ledListener.writeMode(isWriting)
        case PB =>
          // led
          val ledOn = (value & 0x08) > 0
          val lastMotorOn = motorOn
          motorOn = (value & 0x04) > 0
          if (ledListener != null) {
            if (ledOn) ledListener.turnOn else ledListener.turnOff
            if (!motorOn) {
              ledListener.endLoading
              if (lastMotorOn && !motorOn) currentFilename = ""
            }
            else if (floppy == EmptyFloppy) floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
          }
          val newSpeedZone = (value & 0xFF) >> 5 & 0x3
          if (newSpeedZone != speedZone) {
            speedZone = newSpeedZone
            bitCycleWait = rotationCyclesForBit(speedZone)
            rotationCycleCounter = 0.0
          }
          // check the head step indication
          val oldStepHead = regs(PB) & 0x03
          val stepHead = value & 0x03
          
          if (motorOn && oldStepHead == ((stepHead + 1) & 0x03) && track > floppy.minTrack) {            
            moveHead(moveOut = true)
            //println(s"Move out $track/${floppy.maxTrack}")
          }
          else 
          if (motorOn && oldStepHead == ((stepHead - 1) & 0x03) && track < floppy.maxTrack) {            
            moveHead(moveOut = false)
            //println(s"Move in $track/${floppy.maxTrack}")
          }
        case _ =>
      }
      // write on register
      super.write(address, value, chipID)
    }
    
    def sideChanged {
      track = floppy.currentTrack
      trackSteps = track << 1
      //println(s"Side changed: track=$track")
    }
  
    private def moveHead(moveOut: Boolean) {
      if (moveOut) trackSteps -= 1 else trackSteps += 1
      
      floppy.changeTrack(trackSteps)
      track = floppy.currentTrack
    }
    
    private def updateTrackSectorLabelProgress(track:Int,halfTrack:Boolean,sector:Option[Int]) {
      if (ledListener != null) {      
        sector match {
          case None =>
            val trackFormat = if (halfTrack) "%02d." else "%02d "
            ledListener.beginLoadingOf(s"%s $trackFormat".format(currentFilename,track),true)
          case Some(sec) =>
            ledListener.beginLoadingOf("%s %02d/%02d".format(currentFilename,track,sec),true)
        }
      }
    }
    
    private def rotateDisk {
      rotationCycleCounter += bitCycleWait
      if (rotationCycleCounter >= 1) {
        bitCounter += 1
        rotationCycleCounter -= 1
        if (isWriting) { // WRITING
          floppy.writeNextBit((lastWrite & 0x80) > 0)
          lastWrite <<= 1        
          if (bitCounter == 8) {
            bitCounter = 0
            lastWrite = regs(PA)
            byteReady = true
            byteReadySignal = 0
            //irq_set(IRQ_CA1)
          }
        }
        else { // READING
          val bit = floppy.nextBit
          last10Bits = ((last10Bits << 1) | bit) & 0x3FF
          if (last10Bits == 0x3FF) {
            bitCounter = 0
            byteReadySignal = 1
          }
          if (bitCounter == 8) {
            bitCounter = 0
            lastWrite = lastRead
            byteReady = true
            lastRead = last10Bits & 0xFF    
            byteReadySignal = 0
            //irq_set(IRQ_CA1)
          }
        }      
      }
    }
  
    final def rotate {
      if (motorOn) {
        rotateDisk
        if (byteReady && canSetByteReady) {
          cpu.setOverflowFlag
          irq_set(IRQ_CA1)
          byteReady = false
          byteReadySignal = 0
          regs(PCR) & 0x0E match {
            case 0xA|0x08 => canSetByteReady = false
            case _ =>
          }         
        }
      }
    }
    
    // state
    override protected def saveState(out:ObjectOutputStream) {
      super.saveState(out)
      out.writeBoolean(isWriting)
      out.writeBoolean(isDiskChanged)
      out.writeBoolean(isDiskChanging)
      out.writeBoolean(isReadOnly)
      out.writeBoolean(motorOn)
      out.writeBoolean(canSetByteReady)
      out.writeBoolean(byteReady)
      out.writeInt(trackSteps)
      out.writeInt(track)
      out.writeObject(currentFilename)
      out.writeInt(speedZone)
      out.writeDouble(bitCycleWait)
      out.writeDouble(rotationCycleCounter)
      out.writeInt(bitCounter)
      out.writeInt(last10Bits)
      out.writeInt(lastRead)
      out.writeInt(lastWrite)
    }
    override protected def loadState(in:ObjectInputStream) {
      super.loadState(in)
      isWriting = in.readBoolean
      isDiskChanged = in.readBoolean
      isDiskChanging = in.readBoolean
      isReadOnly = in.readBoolean
      motorOn = in.readBoolean
      canSetByteReady = in.readBoolean
      byteReady = in.readBoolean
      trackSteps = in.readInt
      track = in.readInt
      currentFilename = in.readObject.asInstanceOf[String]
      speedZone = in.readInt
      bitCycleWait = in.readDouble
      rotationCycleCounter = in.readDouble
      bitCounter = in.readInt
      last10Bits = in.readInt
      lastRead = in.readInt
      lastWrite = in.readInt
    }
  }
  /************************************************************************************************************
   * Memory: Kernal (32K) + RAM (2K)
   * 
   ***********************************************************************************************************/
  private[this] val ram = Array.fill(length)(0)
  private[this] val KERNAL_ROM = System.getProperty("1571kernal")
  private[this] val DISK_KERNEL = new ROM(null,"C1571_KERNEL",0x8000,0x8000,if (KERNAL_ROM != null) KERNAL_ROM else "roms/c1571.rom") {
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
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
      println("WD1770 reading from " + Integer.toHexString(address) + " " + Integer.toHexString(cpu.getPC))
      0 //TODO WD1770
    }
    else
    if (address >= 0x4000 && address < 0x4010) CIA.read(address)
    else
    if (address >= 0x8000 && address < 0x10000) DISK_KERNEL.read(address)
    else 0
    
  }
  final def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
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
      println("WD1770 writing to " + Integer.toHexString(address) + " = " + Integer.toHexString(value))
    } //TODO WD1770
    else
    if (address >= 0x4000 && address < 0x4010) CIA.write(address,value)    
  }
  override def getMem = this
  // ===============================================================
  
  override def getProperties = {
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
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) {
    useTRAPFormat = !driveReader.isFormattable
    VIA2.setDriveReader(driveReader,emulateInserting)    
  }
  override def setActive(active: Boolean) {
    VIA1.setEnabled(active)
    running = active
    isRunningListener(active)
  }
  override def setCanSleep(canSleep: Boolean) {
    this.canSleep = canSleep
    awake
  }
  override def setReadOnly(readOnly:Boolean) = VIA2.setReadOnly(readOnly)
  
  def init {
    Log.info("Initializing C1571...")
    add(cpu)
    add(IRQSwitcher)
    add(DISK_KERNEL)
    add(VIA1)
    add(VIA2)
    add(CIA)    
    //TODO
    if (ledListener != null) ledListener.turnOn
  }
  
  def reset {
    running = true
    isRunningListener(true)
    awakeCycles = 0
    goSleepingCycles = 0
    cycleFrac = 0.0
    if (ledListener != null) ledListener.turnOn
    java.util.Arrays.fill(ram,0)
  }
  
  private def setFilename {
    var fileNameSize = ram(0x274)
    var adr = 0x200
    val sb = new StringBuilder
    while (fileNameSize > 0) {
      val c = ram(adr)
      sb.append(c.toChar)
      adr += 1
      fileNameSize -= 1      
    }
    VIA2.setCurrentFilename(sb.toString)
  }
  
  override def setSpeedHz(speed:Int) {
    currentSpeedHz = speed
    CYCLE_ADJ = (speed - MIN_SPEED_HZ) / MIN_SPEED_HZ.toDouble
  }
  
  @inline private def checkPC(cycles: Long) {
    val pc = cpu.getPC
//    if (_1541Mode) {
      if (pc == _1541_LOAD_ROUTINE) setFilename
      else 
      if (pc == _1541_FORMAT_ROUTINE && useTRAPFormat) {
        setFilename
        if (VIA2.formatDisk) cpu.jmpTo(_1541_FORMAT_ROUTINE_OK) else cpu.jmpTo(_1541_FORMAT_ROUTINE_NOK)
      } 
      else 
      if (pc == _1541_WAIT_LOOP_ROUTINE && canSleep && channelActive == 0 && !VIA2.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
        running = false      
        VIA1.setActive(false)
        VIA2.setActive(false)
        goSleepingCycles = cycles
      }
//    }
  }
  
  final def clock(cycles: Long) {
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
        CIA.clock
        VIA1.clock(cycles)
        VIA2.clock(cycles)
        n_cycles -= 1
      }
      
      VIA2.rotate      
    }
    else
    if (cycles - goSleepingCycles > GO_SLEEPING_MESSAGE_CYCLES) {
      ledListener.endLoading
      goSleepingCycles = Long.MaxValue
      isRunningListener(false)
    }
  }
  
  private def awake {
    running = true
    isRunningListener(true)
    VIA1.setActive(true)
    VIA2.setActive(true)
    awakeCycles = clk.currentCycles
    VIA2.awake
  }
    
  // ================== Tracing ====================================
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
  // ================== State ======================================
  protected def saveState(out:ObjectOutputStream) {
    if (ledListener != null) out.writeBoolean(ledListener.isOn)
    //TODO
  }
  protected def loadState(in:ObjectInputStream) {
    if (ledListener != null) {
      if (in.readBoolean) ledListener.turnOn else ledListener.turnOff
    }
    //TODO
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}
  