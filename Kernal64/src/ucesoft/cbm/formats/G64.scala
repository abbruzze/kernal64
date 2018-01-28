package ucesoft.cbm.formats

import ucesoft.cbm.peripheral.drive.Floppy
import java.io.RandomAccessFile
import java.io.File
import java.nio.file._
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import ucesoft.cbm.formats.Diskette._
import ucesoft.cbm.cpu.Memory

object G64 {
  def makeEmptyDisk(file:String) {
    val emptyDisk = getClass.getResourceAsStream("/resources/emptyDisk.g64")
    if (emptyDisk == null) throw new IllegalArgumentException
    Files.copy(emptyDisk,new File(file).toPath,StandardCopyOption.REPLACE_EXISTING)
  }
}


private[formats] class G64(val file:String) extends Diskette {
  private[this] var tracks : Array[Array[Int]] = _
  private[this] var trackOffsets : Array[Int] = _
  private[this] var speedZones : Array[Int] = _
  private[this] var trackIndex = 0
  private[this] var track = 0
  private[this] var bit = 1
  private[this] val g64 = new RandomAccessFile(file, "rw")
  private[this] var trackIndexModified = false
  
  loadTracks
  val canBeEmulated = false
  val isReadOnly = false
  val isFormattable = true
  lazy val totalTracks = tracks.length >> 1
  
  private[this] var trackChangeListener : Floppy#TrackListener = null
  def TOTAL_AVAILABLE_SECTORS = throw new UnsupportedOperationException
  def bam : BamInfo = throw new UnsupportedOperationException
  def directories : List[DirEntry] = throw new UnsupportedOperationException
  def readBlock(track:Int,sector:Int) : Array[Byte] = throw new UnsupportedOperationException
  def loadInMemory(mem: Memory, fileName: String, relocate: Boolean,c64Mode:Boolean=true) : Int = throw new UnsupportedOperationException
  def load(fileName: String,fileType:FileType.Value = FileType.PRG) : FileData = throw new UnsupportedOperationException
  
  private def loadTracks {    
    val header = Array.ofDim[Byte](0x0C)
    g64.readFully(header)
    val magic = "GCR-1541".toArray
    for(i <- 0 to 7) if (header(i) != magic(i)) throw new IllegalArgumentException("Bad signature")
    tracks = Array.ofDim[Array[Int]](header(0x09))
    speedZones = Array.ofDim[Int](tracks.length)
    
    trackOffsets = Array.ofDim[Int](tracks.length)
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
        //println(s"Track ${i + 1} has a custom speed zone: not supported. Set as ${speedZones(i)}")
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
  def writeNextBit(value:Boolean) {
    trackIndexModified = true
    val mask = 1 << (8 - bit)
    if (value) tracks(track)(trackIndex) |= mask else tracks(track)(trackIndex) &= ~mask
    if (bit == 8) rotate else bit += 1
  }
  
  @inline private def rotate {
    bit = 1
    if (trackIndexModified) {
      trackIndexModified = false
      g64.seek(trackOffsets(track) + trackIndex + 2)
      g64.write(tracks(track)(trackIndex))
    }
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
  
  def close = g64.close
  
  override def toString = s"G64 $file total tracks=$totalTracks"
  // state
  def save(out:ObjectOutputStream) {
    out.writeInt(trackIndex)
    out.writeInt(track)
    out.writeInt(bit)
    out.writeBoolean(trackIndexModified)
  }
  def load(in:ObjectInputStream) {
    trackIndex = in.readInt
    track = in.readInt
    bit = in.readInt
    trackIndexModified = in.readBoolean
  }
}