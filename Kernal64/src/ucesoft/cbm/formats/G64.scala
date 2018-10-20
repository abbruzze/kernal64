package ucesoft.cbm.formats

import ucesoft.cbm.peripheral.drive.Floppy
import java.io.RandomAccessFile
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import ucesoft.cbm.formats.Diskette._

private[formats] class G64(val file:String) extends Diskette {
  val isReadOnly = !new java.io.File(file).canWrite
  private[this] var tracks : Array[Array[Int]] = _
  private[this] var trackOffsets : Array[Int] = _
  private[this] var speedZones : Array[Int] = _
  private[this] var trackIndex = 0
  private[this] var track = 0
  private[this] var bit = 1
  private[this] var sector : Option[Int] = None
  private[this] var sectorBit,sectorData = 0
  private[this] var headerFound = false
  private[this] val ungcr = new UNGCR
  protected val disk = new RandomAccessFile(file, if (isReadOnly) "r" else "rw")
  private[this] var trackIndexModified = false
  
  loadTracks
  val canBeEmulated = false  
  lazy val totalTracks = tracks.length
  
  private[this] var trackChangeListener : Floppy#TrackListener = null
  def bam : BamInfo = throw new UnsupportedOperationException
  override def directories : List[DirEntry] = throw new UnsupportedOperationException
  
  private def loadTracks {    
    val header = Array.ofDim[Byte](0x0C)
    disk.readFully(header)
    val magic = "GCR-1541".toArray
    for(i <- 0 to 7) if (header(i) != magic(i)) throw new IllegalArgumentException("Bad signature")
    tracks = Array.ofDim[Array[Int]](header(0x09))
    speedZones = Array.ofDim[Int](tracks.length)
    
    trackOffsets = Array.ofDim[Int](tracks.length)
    // read track offset
    for(i <- 0 until trackOffsets.length) {      
      val offset = disk.read | 
                   disk.read << 8 | 
                   disk.read << 16 |
                   disk.read << 24
      //println(s"Track $i offset = ${Integer.toHexString(offset)}")
      trackOffsets(i) = offset
    }
    // read speed zones
    for(i <- 0 until tracks.length) {        
      speedZones(i) = disk.read
      if (speedZones(i) > 3) {
        speedZones(i) = defaultZoneFor(i + 1)
        //println(s"Track ${i + 1} has a custom speed zone: not supported. Set as ${speedZones(i)}")
      }
      disk.skipBytes(3)
    }
    // read track data
    for(i <- 0 until trackOffsets.length) {
      if (trackOffsets(i) == 0) tracks(i) = Array.fill[Int](1)(0)
      else {
        disk.seek(trackOffsets(i))
        val trackLen = disk.read | disk.read << 8
        //println(s"Track $i len=${Integer.toHexString(trackLen)}")
        tracks(i) = Array.ofDim[Int](trackLen)
        for(d <- 0 until trackLen) tracks(i)(d) = disk.read
      }
    }   
  }
  
  def reset {
    bit = 1
    trackIndex = 0
    track = 0
  }
  
  @inline private def checkSector(b:Int) {
    sectorData <<= 1
    sectorData |= b
    if (headerFound) {
      sectorBit += 1
      if (sectorBit == 20) { // sector found
        headerFound = false
        val sector = ungcr.gcr2Byte(sectorData & 0x3FF)
        this.sector = Some(sector)
      }
    }
    else
    if ((sectorData & 0xFFFFF) == 1047881) { // found header
      headerFound = true
      sectorBit = 0
    }
  }
  
  final def nextBit = {
    val b = (tracks(track)(trackIndex) >> (8 - bit)) & 1
    if (bit == 8) rotate else bit += 1
    checkSector(b)
    b
  }
  final def writeNextBit(value:Boolean) {
    checkSector(if (value) 1 else 0)
    trackIndexModified = true
    val mask = 1 << (8 - bit)
    if (value) tracks(track)(trackIndex) |= mask else tracks(track)(trackIndex) &= ~mask
    if (bit == 8) rotate else bit += 1
  }
  final def nextByte : Int = tracks(track)(trackIndex)
  def writeNextByte(b:Int) = tracks(track)(trackIndex) = b & 0xFF
  
  @inline private def rotate {
    bit = 1
    if (trackIndexModified && canWriteOnDisk) {
      trackIndexModified = false
      disk.seek(trackOffsets(track) + trackIndex + 2)
      disk.write(tracks(track)(trackIndex))
    }
    trackIndex = (trackIndex + 1) % tracks(track).length
  }
  
  def notifyTrackSectorChangeListener {
    if (trackChangeListener != null) trackChangeListener((track >> 1) + 1,(track & 1) == 1,sector)
  }
  def currentTrack = track + 1
  def currentSector = sector
  def changeTrack(trackSteps:Int) {
    track = trackSteps - 2
    trackIndex = 0
    bit = 1
    notifyTrackSectorChangeListener
  }
  def setTrackChangeListener(l:TrackListener) = trackChangeListener = l
    
  def close = disk.close
  
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