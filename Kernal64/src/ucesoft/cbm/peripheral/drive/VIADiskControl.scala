package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.ChipID
import ucesoft.cbm.Clock
import ucesoft.cbm.cpu.CPU6510
import ucesoft.cbm.ClockEvent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

class VIADiskControl(cpu: CPU6510,
  					 irqAction: (Boolean) => Unit,
  					 ledListener: DriveLedListener) extends VIA("VIA_DiskControl", 0x1C00,irqAction) {
  override lazy val componentID = "VIA2 (DC)"
  private[this] val WRITE_PROTECT_SENSE = 0x10
  private[this] val WRITE_PROTECT_SENSE_WAIT = 3 * 400000L
  private[this] val REMOVING_DISK_WAIT = 500000L
  private[this] val SYNC_DETECTION_LINE = 0x80
  private[this] var isWriting = false
  private[this] var isDiskChanged = true
  private[this] var isDiskChanging = false
  private[this] var isReadOnly = false
  private[this] var diskChangedAtClockCycle = 0L
  private[this] var motorOn = false
  private[this] var canSetByteReady = false
  private[this] var byteReady = false
  private[this] var trackSteps = 2
  private[this] var track = 1
  private[this] var floppy : Floppy = EmptyFloppy
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
    diskChangedAtClockCycle = 0L
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
  
  def getFloppy = floppy
  
  override def getProperties = {
    properties.setProperty("Motor on",motorOn.toString)
    properties.setProperty("Writing",isWriting.toString)
    properties.setProperty("Track","%2d".format(track))
    properties.setProperty("Disk",floppy.toString)
    properties.setProperty("Current filename",currentFilename)
    super.getProperties
  } 
  
  def awake {
    if (isDiskChanged) diskChangedAtClockCycle = Clock.systemClock.currentCycles
  }
  
  def setReadOnly(readOnly:Boolean) = isReadOnly = readOnly

  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) {
    //if (!motorOn) resetFloppy
    floppy = driveReader    
    if (emulateInserting) {
      // reset the last track
      floppy.changeTrack(trackSteps) 
      floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
      isDiskChanged = true
      isDiskChanging = true
      Clock.systemClock.schedule(new ClockEvent("DiskRemoving",Clock.systemClock.currentCycles + WRITE_PROTECT_SENSE_WAIT, cycles => {
        isDiskChanged = false
        Clock.systemClock.schedule(new ClockEvent("DiskWaitingInserting",cycles + REMOVING_DISK_WAIT, cycles => {
          isDiskChanged = true
          diskChangedAtClockCycle = cycles
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
      if (isDiskChanging && isDiskChanged && Clock.systemClock.currentCycles - diskChangedAtClockCycle > WRITE_PROTECT_SENSE_WAIT) {
        isDiskChanged = false
        isDiskChanging = false
      }
      (super.read(address, chipID) & ~(WRITE_PROTECT_SENSE | SYNC_DETECTION_LINE)) |
        (if (isSync) 0x00 else SYNC_DETECTION_LINE) |
        (if (wps) 0x00 else WRITE_PROTECT_SENSE)
    case PA|PA2 =>
      super.read(address, chipID)
      floppy.notifyTrackSectorChangeListener
      if ((regs(PCR) & 0x0C) == 0x08) {
        canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
      }
      lastRead
    case ofs => super.read(address, chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID) {
    (address & 0x0F) match {
      case PA =>
        if ((regs(PCR) & 0x0C) == 0x08) {
          canSetByteReady = (regs(PCR) & 0x0E) == 0x0A
        }
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
          else if (floppy.isEmpty) floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
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
        if (motorOn && oldStepHead == ((stepHead + 1) & 0x03) && track > 1) moveHead(moveOut = true)
        else 
        if (motorOn && oldStepHead == ((stepHead - 1) & 0x03) && track < floppy.totalTracks) moveHead(moveOut = false)
      case _ =>
    }
    // write on register
    super.write(address, value, chipID)
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
          //irq_set(IRQ_CA1)
        }
      }
      else { // READING
        val bit = floppy.nextBit
        last10Bits = ((last10Bits << 1) | bit) & 0x3FF
        if (last10Bits == 0x3FF) bitCounter = 0
        if (bitCounter == 8) {
          bitCounter = 0
          lastWrite = lastRead
          byteReady = true
          lastRead = last10Bits & 0xFF      
          //irq_set(IRQ_CA1)
        }
      }      
    }
  }

  override def clock(cycles: Long) {
    super.clock(cycles)
    if (motorOn) {
      rotateDisk
      if (byteReady && canSetByteReady) {
        cpu.setOverflowFlag
        byteReady = false
        regs(PCR) & 0x0E match {
          case 0xA|0x08 => canSetByteReady = false
          case _ =>
        }         
        irq_set(IRQ_CA1)
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
    out.writeLong(diskChangedAtClockCycle)
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
    diskChangedAtClockCycle = in.readLong
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