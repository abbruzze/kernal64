package ucesoft.cbm.formats

import ucesoft.cbm.formats.Diskette._
import ucesoft.cbm.peripheral.drive.Floppy

import java.io.{ObjectInputStream, ObjectOutputStream, RandomAccessFile}
import scala.collection.mutable.ListBuffer

object G64 {
  def DEFAULT_TRACK_LENGTH(track:Int) : Int = {
    if (track <= 17) 7692
    else if (track <= 24) 7142
    else if (track <= 30) 6666
    else 6250
  }
}

private[formats] class G64(val file:String) extends Diskette {
  import G64._

  val isReadOnly: Boolean = !new java.io.File(file).canWrite
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
  private[this] var _side = 0
  private[this] var trackSideOffset = 0
  private[this] var _singleSide = true

  loadTracks
  val canBeEmulated = false
  lazy val totalTracks: Int = tracks.length

  private[this] var trackChangeListener : Floppy#TrackListener = null

  override lazy val singleSide = _singleSide

  override def side_=(newSide: Int): Unit = {
    _side = newSide
    trackSideOffset = if (newSide == 1) 84 else 0
  }
  override def side: Int = _side

  def bam : BamInfo = {
    var bamSector : Array[Int] = null
    GCR.GCR2track(tracks((DIR_TRACK - 1) * 2),-1,(_,se,data) => if (se == BAM_SECTOR) bamSector = data)

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
    for (t <- 1 to 35) {
      val f = bamSector(offset) & 0xFF
      free += (if (t == 18) 0 else f)
      offset += 4
    }
    if (!_singleSide) {
      offset = 0
      for (t <- 0 until 35) {
        val f = bamSector(0xDD + t) & 0xFF
        free += f
      }
    }
    BamInfo(diskName.toString, diskID, dosType,true,free)
  }

  override def directories : List[DirEntry] = {
    var t = DIR_TRACK
    var s = DIR_SECTOR
    val dirs = new ListBuffer[DirEntry]
    var readNextSector = true
    val buffer = Array.ofDim[Int](0x20)
    val sectors = new collection.mutable.HashMap[(Int,Int),Array[Int]]

    while (readNextSector) {
      val currentT = t
      val currentS = s
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
        else dirs += makeDirEntryFromBuffer(buffer map { _.toByte },currentT,currentS,entryIndex - 1)
      }
    }
    dirs.toList
  }

  private def loadTracks()  : Unit = {
    val header = Array.ofDim[Byte](0x0C)
    disk.readFully(header)
    val magic = new String(header,0,8)
    if (magic != "GCR-1541" && magic != "GCR-1571") throw new IllegalArgumentException("Bad signature")

    val totalTracks = header(0x09).toInt & 0xFF
    _singleSide = totalTracks != 0xA8
    tracks = Array.ofDim[Array[Int]](totalTracks)
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
      if (trackOffsets(i) == 0) tracks(i) = Array(0)
      else {
        disk.seek(trackOffsets(i))
        val trackLen = disk.read | disk.read << 8
        //println(s"Track $i len=$trackLen offset=${trackOffsets(i).toHexString}")
        tracks(i) = Array.ofDim[Int](trackLen)
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

  def reset  : Unit = {
    bit = 1
    trackIndex = 0
    track = 0
    _side = 0
    trackSideOffset = 0
  }

  @inline private def checkSector(b:Int) : Unit = {
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

  final def nextBit: Int = {
    val b = (tracks(track + trackSideOffset)(trackIndex) >> (8 - bit)) & 1
    if (bit == 8) rotate else bit += 1
    checkSector(b)
    b
  }
  private def writeNewTrack() : Unit = {
    val trackLen = DEFAULT_TRACK_LENGTH(track >> 1)
    tracks(track + trackSideOffset) = Array.fill[Int](trackLen)(0x55)
    if (canWriteOnDisk) {
      trackOffsets(track + trackSideOffset) = disk.length.toInt
      // updating track data position
      disk.seek(0x0C + 4 * (track + trackSideOffset))
      disk.write(trackOffsets(track + trackSideOffset) & 0xFF)
      disk.write((trackOffsets(track + trackSideOffset) >> 8) & 0xFF)
      disk.write((trackOffsets(track + trackSideOffset) >> 16) & 0xFF)
      disk.write((trackOffsets(track + trackSideOffset) >> 24) & 0xFF)
      // writing track length
      disk.seek(trackOffsets(track + trackSideOffset))
      disk.write(trackLen & 0xFF)
      disk.write(trackLen >> 8)
      // writing track data
      disk.write(Array.fill[Byte](trackLen)(0x55))
    }
  }

  final def writeNextBit(value:Boolean) : Unit = {
    if (trackOffsets(track + trackSideOffset) == 0) writeNewTrack // writing on an empty track

    checkSector(if (value) 1 else 0)
    trackIndexModified = true
    val mask = 1 << (8 - bit)
    if (value) tracks(track + trackSideOffset)(trackIndex) |= mask else tracks(track + trackSideOffset)(trackIndex) &= ~mask
    if (bit == 8) rotate else bit += 1
  }
  final def nextByte : Int = tracks(track + trackSideOffset)(trackIndex)
  def writeNextByte(b:Int): Unit = tracks(track + trackSideOffset)(trackIndex) = b & 0xFF

  @inline private def rotate()  : Unit = {
    bit = 1
    if (trackIndexModified && canWriteOnDisk) {
      trackIndexModified = false
      disk.seek(trackOffsets(track + trackSideOffset) + trackIndex + 2)
      disk.write(tracks(track + trackSideOffset)(trackIndex))
    }
    trackIndex = (trackIndex + 1) % tracks(track + trackSideOffset).length
  }

  def notifyTrackSectorChangeListener  : Unit = {
    if (trackChangeListener != null) trackChangeListener(((track + trackSideOffset) >> 1) + 1,(track & 1) == 1,sector)
  }
  def currentTrack: Int = track + 1
  def currentSector: Option[Int] = sector
  def changeTrack(trackSteps:Int) : Unit = {
    track = trackSteps - 2
    trackIndex = trackIndex % tracks(track + trackSideOffset).length
    bit = 1
    notifyTrackSectorChangeListener
  }
  def setTrackChangeListener(l:TrackListener): Unit = trackChangeListener = l

  def close: Unit = disk.close()

  override def toString = s"G64 $file total tracks=$totalTracks"
  // state
  def save(out:ObjectOutputStream) : Unit = {
    out.writeInt(trackIndex)
    out.writeInt(track)
    out.writeInt(bit)
    out.writeBoolean(trackIndexModified)
  }
  def load(in:ObjectInputStream) : Unit = {
    trackIndex = in.readInt
    track = in.readInt
    bit = in.readInt
    trackIndexModified = in.readBoolean
  }
}