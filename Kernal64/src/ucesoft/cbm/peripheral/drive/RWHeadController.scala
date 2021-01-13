package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.SwingUtilities

abstract class RWHeadController(protected var floppy:Floppy,
                       ledListener:DriveLedListener) extends CBMComponent {
  val componentType = CBMComponentType.INTERNAL
  
  protected var isWriting = false
  protected var motorOn = false
  protected var byteReady = false
  protected var byteReadySignal = 1
  protected var track = 18
  protected var currentFilename = ""
  protected var writeProtect = false
  
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
  private[this] val C1541_CLOCK_HZ = 1000000// * 1.025 // constant used to calibrate rpm to 300
  private[this] val rotationCyclesForBit = Array[Double](
      250000.0 / C1541_CLOCK_HZ,  // zone 4
      266667.0 / C1541_CLOCK_HZ,  // zone 3
      285714.0 / C1541_CLOCK_HZ,  // zone 2
      307692.0 / C1541_CLOCK_HZ,  // zone 1
      256000.0 / C1541_CLOCK_HZ   // zone 0 for 1581 256
  )
    
  protected var speedZone = 3
  protected var bitCycleWait = rotationCyclesForBit(speedZone)
  protected var rotationCycleCounter = 0.0  
  protected var bitCounter = 0
  protected var lastRead = 0
  protected var lastWrite = 0x55
  protected var nextWrite = 0
  protected var lastByteReady = false
  private[this] var lastNotifiedTrack,lastNotifiedSector = 0
  
  def init : Unit = {}
  
  def reset : Unit = {
    byteReadySignal = 1
    isWriting = false
    motorOn = false
    canSetByteReady = false
    byteReady = false
    track = 1
    currentFilename = ""
    bitCounter = 0
    setSpeedZone(3)
    rotationCycleCounter = 0.0
    lastRead = 0
    lastWrite = 0x55
    lastByteReady = false
  }
  
  final def isWriteProtected : Boolean = writeProtect | floppy.isReadOnly
  final def setWriteProtected(wp:Boolean) = writeProtect = wp
  def isOnIndexHole : Boolean = floppy.isOnIndexHole
  final def getCurrentFileName = currentFilename
  final def resetByteReadySignal = byteReadySignal = 1
  final def getByteReadySignal = byteReadySignal
  final def getLastByteReady : Boolean = {
    val lbr = lastByteReady
    lastByteReady = false
    lbr
  }
  final def setWriting(on:Boolean) : Unit = {
    isWriting = on
    ledListener.writeMode(on)
  }
  final def getLastRead : Int = lastRead
  final def setSpeedZone(newSpeedZone:Int) : Unit = {
    if (speedZone != newSpeedZone) {
      speedZone = newSpeedZone
      bitCycleWait = rotationCyclesForBit(speedZone)
      rotationCycleCounter = 0.0
    }
  }
  def setFloppy(newFloppy:Floppy) : Unit = {
    floppy = newFloppy
    floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
  }
  final def setNextToWrite(b:Int) : Unit = {
    nextWrite = b
    resetByteReadySignal
  }
  final def setCurrentFileName(fn:String) : Unit = currentFilename = fn
  def getTrack : Int = track
  final def isMotorOn : Boolean = motorOn
  final def setMotor(on:Boolean) : Unit = {
    motorOn = on
    if (on && floppy.isEmpty) floppy.setTrackChangeListener(updateTrackSectorLabelProgress _)
  }
  private def updateTrackSectorLabelProgress(track:Int,halfTrack:Boolean,sector:Option[Int]) : Unit = {
    if (ledListener != null && motorOn) {
      sector match {
        case None =>
          if (track != lastNotifiedTrack) {
            lastNotifiedTrack = track
            SwingUtilities.invokeLater(new Runnable {
              def run : Unit = {
                ledListener.moveTo(track,sector,halfTrack)
              }
            })
          }
        case Some(sec) =>
          if (track != lastNotifiedTrack || sec != lastNotifiedSector) {
            lastNotifiedTrack = track
            lastNotifiedSector = sec
            SwingUtilities.invokeLater(new Runnable {
              def run : Unit = {
                ledListener.moveTo(track,sector,halfTrack)
              }
            })
          }
      }
    }
  }
  private def rotateDisk : Unit = {
    rotationCycleCounter += bitCycleWait
    if (rotationCycleCounter >= 1) {
      bitCounter += 1
      rotationCycleCounter -= 1
      if (isWriting) writeNextBit
      else readNextBit      
    }
  }
  
  final def rotate : Boolean = {
    lastByteReady = if (motorOn) {
      rotateDisk
      if (byteReady && canSetByteReady) {
        byteReadySignal = 0
        byteReady = false
        true
      }
      else false
    }
    else false
    
    lastByteReady
  }
  
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(isWriting)
    out.writeBoolean(motorOn)
    out.writeBoolean(canSetByteReady)
    out.writeBoolean(byteReady)
    out.writeInt(track)
    out.writeObject(currentFilename)
    out.writeInt(speedZone)
    out.writeDouble(bitCycleWait)
    out.writeDouble(rotationCycleCounter)
    out.writeInt(bitCounter)
    out.writeInt(lastRead)
    out.writeInt(lastWrite)    
    out.writeBoolean(lastByteReady)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    isWriting = in.readBoolean
    motorOn = in.readBoolean
    canSetByteReady = in.readBoolean
    byteReady = in.readBoolean
    track = in.readInt
    currentFilename = in.readObject.asInstanceOf[String]
    speedZone = in.readInt
    bitCycleWait = in.readDouble
    rotationCycleCounter = in.readDouble
    bitCounter = in.readInt
    lastRead = in.readInt
    lastWrite = in.readInt
    lastByteReady = in.readBoolean
  }
  protected def allowsStateRestoring = true
 // ================== Abstract methods =============================================== 
 def changeSide(side:Int) : Unit
 def isSync : Boolean
 def moveHead(moveOut: Boolean) : Unit
 protected def readNextBit : Unit
 protected def writeNextBit : Unit
 // ===================================================================================
  
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
}