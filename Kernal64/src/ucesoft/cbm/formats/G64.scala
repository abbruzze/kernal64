package ucesoft.cbm.formats

import ucesoft.cbm.peripheral.drive.Floppy
import java.io.RandomAccessFile
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import ucesoft.cbm.formats.Diskette._

import scala.collection.mutable.ListBuffer

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

  def bam : BamInfo = {
    var bamSector : Array[Int] = null
    GCR.GCR2track(tracks((DIR_TRACK - 1) * 2),-1,(tr,se,data) => if (se == BAM_SECTOR) bamSector = data)

    var offset = 0x90
    val diskName = new StringBuilder
    var i = 0
    while (i < 16) {
      val c = bamSector(offset) & 0xFF
      if (c != 0xA0) diskName.append(c.toChar)
      i += 1
      offset += 1
    }
    offset += 2
    val diskID = "" + bamSector(offset).toChar + bamSector(offset + 1).toChar
    offset += 3
    val dosType = "" + bamSector(offset).toChar + bamSector(offset + 1).toChar

    offset = 4
    var free = 0
    for(t <- 1 to 35) {
      val f = bamSector(offset) & 0xFF
      free += (if (t == 18) 0 else f)
      offset += 4
    }
    BamInfo(diskName.toString, diskID, dosType,true,free)
  }

  override def directories : List[DirEntry] = {
    var t = DIR_TRACK
    var s = DIR_SECTOR
    var dirs = new ListBuffer[DirEntry]
    var readNextSector = true
    val buffer = Array.ofDim[Int](0x20)
    val sectors = new collection.mutable.HashMap[(Int,Int),Array[Int]]

    while (readNextSector) {
      val sector : Array[Int] = sectors get (t,s) match {
        case None =>
          GCR.GCR2track(tracks((t - 1) * 2),-1,(tr,se,data) => { sectors += (tr,se) -> data })
          sectors(t,s)
        case Some(s) => s
      }

      var firstEntryOfSector = true
      var entryIndex = 0
      var readNextEntry = true
      var sectorPos = 0
      while (readNextEntry) {
        System.arraycopy(sector,sectorPos,buffer,0,0x20)
        sectorPos += 0x20
        if (firstEntryOfSector) {
          firstEntryOfSector = false
          val nextT = buffer(0)
          val nextS = buffer(1)
          if (nextT != 0) {
            t = nextT
            s = nextS
          }
          else readNextSector = false
        }
        entryIndex += 1
        if (sectorPos + 0x20 > sector.length) {
          readNextEntry = false
          readNextSector = false
        }

        if (entryIndex == 9 || buffer.forall(_ == 0)) {
          readNextEntry = false // last+1 entry of this sector
        }
        else dirs += makeDirEntryFromBuffer(buffer map { _.toByte })
      }
    }
    dirs.toList
  }
  
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
        //for(d <- 0 until trackLen) tracks(i)(d) = disk.read
        val buf = Array.ofDim[Byte](trackLen)
        disk.readFully(buf)
        var b = 0
        while (b < trackLen) {
          tracks(i)(b) = buf(b).toInt & 0xFF
          b += 1
        }
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