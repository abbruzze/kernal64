package ucesoft.c64.peripheral.drive

import ucesoft.c64.ChipID
import ucesoft.c64.Clock
import ucesoft.c64.formats.D64
import ucesoft.c64.cpu.CPU6510

class VIADiskControl(cpu: CPU6510,
  					 irqAction: (Boolean) => Unit,
  					 ledListener: DriveLedListener) extends VIA("VIA_DiskControl", 0x1C00,irqAction) {
  override lazy val componentID = "VIA2 (DC)"
  private[this] val WRITE_PROTECT_SENSE = 0x10
  private[this] val WRITE_PROTECT_SENSE_WAIT = 3 * 400000L;
  private[this] val SYNC_DETECTION_LINE = 0x80
  private[this] val WAIT_CYCLES_INTER_SECTORS = 1000
  private[this] val TOTAL_TRACKS = D64.TOTAL_TRACKS
  private[this] var isWriting = false
  private[this] var isDiskChanged = true
  private[this] var diskChangedAtClockCycle = 0L
  private[this] var motorOn = false
  private[this] var trackSteps = 2
  private[this] var track = 1
  private[this] var sector = 0
  private[this] var lastSync = false
  private[this] var sectorsPerCurrentTrack = D64.TRACK_ALLOCATION(track)
  private[this] var gcrSector: Array[Int] = null
  private[this] var gcrIndex,gcrIndexFromToWrite = 0
  private[this] var sectorModified = false
  private[this] var d64: Option[D64] = None
  private[this] var currentFilename = ""
  /**
   * Get the rotation cycles needed for read a byte for the current track.
   *
   * Zone	Tracks	Clock rate
   * ----------------------------
   * 1		1-17	307.692 bit/s
   * 2		18-24	285.714 bit/s
   * 3		25-30	266.667 bit/s
   * 4		31-35	250.000 bit/s
   */
  private[this] val rotationCyclesForByte: Int => Int = {
    val C1541_CLOCK_HZ = 1000000
    val zones = Array.ofDim[Int](4)
    zones(0) = (8.0 / (307692.0 / C1541_CLOCK_HZ)).toInt
    zones(1) = (8.0 / (285714.0 / C1541_CLOCK_HZ)).toInt
    zones(2) = (8.0 / (266667.0 / C1541_CLOCK_HZ)).toInt
    zones(3) = (8.0 / (250000.0 / C1541_CLOCK_HZ)).toInt
    t: Int => {
      if (t <= 17) zones(0)
      else if (t <= 24) zones(1)
      else if (t <= 30) zones(2)
      else zones(3)
    }
  }
  private[this] var byteCycleWait = rotationCyclesForByte(1)
  
  override def getProperties = {
    properties.setProperty("Motor on",motorOn.toString)
    properties.setProperty("Writing",isWriting.toString)
    properties.setProperty("Track","%2d".format(track))
    properties.setProperty("Sector","%2d".format(sector))
    properties.setProperty("Disk",if (d64.isDefined) d64.get.file else "")
    properties.setProperty("Current filename",currentFilename)
    super.getProperties
  } 
  
  def awake {
    if (isDiskChanged) diskChangedAtClockCycle = Clock.systemClock.currentCycles
  }

  def setDriveReader(driveReader: D64) {
    d64 = Some(driveReader)
    gcrSector = driveReader.gcrImageOf(track, sector)
    isDiskChanged = true
    diskChangedAtClockCycle = Clock.systemClock.currentCycles
  }
  
  def setCurrentFilename(fn:String) = currentFilename = fn
  def formatDisk = {
    d64 match {
      case None => false
      case Some(disk) =>
        try {
          println("Formatting '" + currentFilename + "'")
          disk.format(currentFilename)
          true
        }
        catch {
          case t:IllegalArgumentException =>
            t.printStackTrace
            false
        }
    }
  }
  
  def isMotorOn = motorOn

  @inline private def isSync = d64 match {
    case None => false
    case Some(d) => motorOn && gcrSector(gcrIndex) == 0xFF && gcrIndex + 1 < gcrSector.length && gcrSector(gcrIndex + 1) != 0xFF
  }

  override def read(address: Int, chipID: ChipID.ID) = address - startAddress match {
    case PB =>
      val diskChanged = isDiskChanged
      if (diskChanged && Clock.systemClock.currentCycles - diskChangedAtClockCycle > WRITE_PROTECT_SENSE_WAIT) {
        isDiskChanged = false
      }
      (super.read(address, chipID) & ~(WRITE_PROTECT_SENSE | SYNC_DETECTION_LINE)) |
        (if (isSync) 0x00 else SYNC_DETECTION_LINE) |
        (if (diskChanged) 0x00 else WRITE_PROTECT_SENSE)
    case PA =>
      super.read(address, chipID)
      val headByte = if (gcrSector == null) 0 else gcrSector(gcrIndex)
      updateTrackSectorLabelProgress
      //println(s"READ ${Integer.toHexString(headByte)} ${track}/${sector} gcrIndex=${gcrIndex}")
      headByte
    case ofs => super.read(address, chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID) {
    address - startAddress match {
      case PCR =>
        if ((value & 0x20) == 0) {
          isWriting = true
          gcrIndexFromToWrite = gcrIndex + 1
        }
        else isWriting = false                  
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
        }
        // check the head step indication
        val oldStepHead = regs(PB) & 0x03
        val stepHead = value & 0x03
        if (oldStepHead == ((stepHead + 1) & 0x03) && track > 1) moveHead(moveOut = true)
        else if (oldStepHead == ((stepHead - 1) & 0x03) && track < TOTAL_TRACKS) moveHead(moveOut = false)
      case _ =>
    }
    // write on register
    super.write(address, value, chipID)
  }

  private def moveHead(moveOut: Boolean) {
    if (moveOut) trackSteps -= 1 else trackSteps += 1
    val isOnTrack = (trackSteps & 1) == 0
    if (isOnTrack) {
      track = trackSteps / 2
      sectorsPerCurrentTrack = D64.TRACK_ALLOCATION(track)
      sector = 0            
      byteCycleWait = rotationCyclesForByte(track)
      if (d64.isDefined) gcrSector = d64.get.gcrImageOf(track, sector)
      //println(s"Moved on track ${track}")
    }
  }
  
  private def updateTrackSectorLabelProgress {
    if (ledListener != null) ledListener.beginLoadingOf("%s %02d/%02d".format(currentFilename,track,sector),true)
  }
  
  private def rotateDisk = {
    if (gcrSector != null) {
      if (isWriting && gcrIndex >= gcrIndexFromToWrite) {    
        gcrSector(gcrIndex) = regs(PA)
        sectorModified = true
      }
      gcrIndex += 1
      if (gcrIndex == gcrSector.length) { // end of current sector
        if (sectorModified) {
          sectorModified = false
          d64.get.writeGCRSector(track,sector,gcrSector)
        }
        gcrIndex = 0
        sector = (sector + 1) % sectorsPerCurrentTrack        
        gcrSector = d64.get.gcrImageOf(track, sector)
      }

      if (isWriting || !isSync || !lastSync) cpu.setOverflowFlag // byte ready
      lastSync = isSync
      byteCycleWait
    } 
    else 0
  }

  def clock(cycles: Long) = {
    if (motorOn) rotateDisk else 0
  }
}