package ucesoft.c64.peripheral.drive

/**
 * @author ealeame
 */
trait Floppy {
  type TrackListener = (Int,Boolean,Option[Int]) => Unit
  val isReadOnly : Boolean
  val totalTracks : Int
  
  def nextBit : Int
  def writeByte(value:Int)
  def prepareToWrite
  def canWrite : Boolean
  
  def currentTrack : Int
  def currentSector : Option[Int]
  def changeTrack(trackSteps:Int)
  def setTrackChangeListener(l:TrackListener)
  def notifyTrackSectorChangeListener
  
  def format(diskName:String)
  
  def defaultZoneFor(track:Int) : Int = {
    if (track <= 17) 0
    else if (track <= 24) 1
    else if (track <= 30) 2
    else 3
  }
  
  def close
  def reset
}

object EmptyFloppy extends Floppy {
  val isReadOnly = false
  val totalTracks = 42
  private[this] var track = 1
  
  def nextBit = 0
  def writeByte(value:Int) {}
  def prepareToWrite {}
  def canWrite = false
  
  def currentTrack = track
  def currentSector = None
  def changeTrack(trackSteps:Int) {
    val isOnTrack = (trackSteps & 1) == 0
    if (isOnTrack) {
      track = trackSteps >> 1
    }
  }
  def setTrackChangeListener(l:TrackListener) {}
  def notifyTrackSectorChangeListener {}
  
  def format(diskName:String) = throw new IllegalArgumentException("Empty disk. Can't format")
  
  def close {}
  def reset {
    track = 1
  }
  override def toString = "Empty Floppy"
}