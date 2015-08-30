package ucesoft.c64.formats

import ucesoft.c64.peripheral.drive.Floppy
import java.io.RandomAccessFile

object G64 extends App {
  println(new G64(args(0)))
}


class G64(file:String) extends Floppy {
  private[this] var tracks : Array[Array[Int]] = _
  private[this] var speedZones : Array[Int] = _
  private[this] var trackIndex = 0
  private[this] var track = 0
  private[this] var bit = 1
  
  loadTracks
  
  val isReadOnly = true
  lazy val totalTracks = tracks.length
  
  private[this] var trackChangeListener : Floppy#TrackListener = null
  
  private def loadTracks {
    val g64 = new RandomAccessFile(file, "r")
    try {
      val header = Array.ofDim[Byte](0x0C)
      g64.readFully(header)
      val magic = "GCR-1541".toArray
      for(i <- 0 to 7) if (header(i) != magic(i)) throw new IllegalArgumentException("Bad signature")
      tracks = Array.ofDim[Array[Int]](header(0x09))
      speedZones = Array.ofDim[Int](tracks.length)
      
      val trackOffsets = Array.ofDim[Int](tracks.length)
      // read track offset
      for(i <- 0 until trackOffsets.length) {      
        val offset = g64.read | 
                     g64.read << 8 | 
                     g64.read << 16 |
                     g64.read << 24
        //println(s"Track $i offset = ${Integer.toHexString(offset)}")
        trackOffsets(i) = offset
      }
      // read speed zones
      for(i <- 0 until tracks.length) {        
        speedZones(i) = g64.read
        if (speedZones(i) > 3) {
          speedZones(i) = defaultZoneFor(i + 1)
          println(s"Track ${i + 1} has a custom speed zone: not supported. Set as ${speedZones(i)}")
        }
        g64.skipBytes(3)
      }
      // read track data
      for(i <- 0 until trackOffsets.length) {
        if (trackOffsets(i) == 0) tracks(i) = Array.fill[Int](1)(0)
        else {
          g64.seek(trackOffsets(i))
          val trackLen = g64.read | g64.read << 8
          //println(s"Track $i len=${Integer.toHexString(trackLen)}")
          tracks(i) = Array.ofDim[Int](trackLen)
          for(d <- 0 until trackLen) tracks(i)(d) = g64.read
        }
      }   
    }
    finally {
      g64.close
    }
  }
  
  def reset {
    bit = 1
    trackIndex = 0
    track = 0
  }
  
  def nextBit = {
    val b = (tracks(track)(trackIndex) >> (8 - bit)) & 1
    if (bit == 8) rotate else bit += 1
    b
  }
  def writeByte(value:Int) {}
  def prepareToWrite {}
  def canWrite = false
  
  @inline private def rotate {
    bit = 1
    trackIndex = (trackIndex + 1) % tracks(track).length
  }
  
  def notifyTrackSectorChangeListener {}
  def currentTrack = track + 1
  def currentSector = None
  def changeTrack(trackSteps:Int) {
    track = trackSteps - 2
    trackIndex = 0
    bit = 1
    if (trackChangeListener != null) trackChangeListener(trackSteps >> 1,(track & 1) == 1,None)
  }
  def setTrackChangeListener(l:TrackListener) = trackChangeListener = l
  
  def format(diskName:String) {}
  
  def close {}
  
  override def toString = s"G64 $file total tracks=$totalTracks"
}