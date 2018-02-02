package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class RWHeadController(val name:String,
                       private var floppy:Floppy,
                       ledListener:DriveLedListener) extends CBMComponent {
  val componentID = name + " rw head controller"
  val componentType = CBMComponentType.INTERNAL
  
  private[this] var isWriting = false
  private[this] var motorOn = false
  private[this] var byteReady = false
  private[this] var byteReadySignal = 1
  private[this] var track = 1
  private[this] var trackSteps = track << 1    
  private[this] var currentFilename = ""
  
  var canSetByteReady = false
  
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
  private[this] var nextWrite = 0
  
  def init {
    
  }
  
  def reset {
    byteReadySignal = 1
    isWriting = false
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
  
  // ======================================================================
  final def getCurrentFileName = currentFilename
  final def resetByteReadySignal = byteReadySignal = 1
  final def getByteReadySignal = byteReadySignal
  final def setWriting(on:Boolean) = isWriting = on  
  final def changeSide(side:Int) {
    floppy.side = side
    track = floppy.currentTrack
    val oldTrackSteps = trackSteps
    trackSteps = (track << 1) | (oldTrackSteps & 1)
  }
  final def getLastRead : Int = lastRead
  final def setSpeedZone(newSpeedZone:Int) {
    if (speedZone != newSpeedZone) {
      speedZone = newSpeedZone
      bitCycleWait = rotationCyclesForBit(speedZone)
      rotationCycleCounter = 0.0
    }
  }
  final def setFloppy(newFloppy:Floppy) {
    floppy = newFloppy
    floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
    floppy.changeTrack(trackSteps)
  }
  final def setNextToWrite(b:Int) {
    nextWrite = b
    resetByteReadySignal
  }
  final def setCurrentFileName(fn:String) = currentFilename = fn
  final def getTrack : Int = track
  final def isMotorOn : Boolean = motorOn
  final def setMotor(on:Boolean) {
    motorOn = on
    if (on && floppy.isEmpty) floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
  }
  final def isSync : Boolean = !isWriting && motorOn && last10Bits == 0x3FF
  final def moveHead(moveOut: Boolean) {
    var trackMoved = false
    if (moveOut) {
      if (track > floppy.minTrack) {
        trackSteps -= 1
        trackMoved = true
      }
    }
    else {
      if (track < floppy.maxTrack) {
        trackSteps += 1
        trackMoved = true
      }
    }
    if (trackMoved) {
      floppy.changeTrack(trackSteps)
      track = floppy.currentTrack
    }
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
          lastWrite = nextWrite
          byteReady = true
          byteReadySignal = 0
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
        }
      }      
    }
  }
  
  final def rotate : Boolean = {
    if (motorOn) {
      rotateDisk
      if (byteReady && canSetByteReady) {
        byteReadySignal = 0
        byteReady = false
        true
      }
      else false
    }
    else false
    }
  // ======================================================================
  
  protected def saveState(out:ObjectOutputStream) {
    out.writeBoolean(isWriting)
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
  protected def loadState(in:ObjectInputStream) {
    isWriting = in.readBoolean
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
  protected def allowsStateRestoring(parent:JFrame) = true
}