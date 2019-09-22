package ucesoft.cbm.formats
import java.io.RandomAccessFile
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import ucesoft.cbm.peripheral.drive.Floppy

private[formats] class D64_D71(val file: String) extends Diskette {
  import Diskette._
  val canBeEmulated = true
  // D64
  private[this] final  val DISK_SIZE_40_TRACKS = 196608
  private[this] final  val DISK_SIZE_35_TRACKS = 174848
  private[this] final  val DISK_SIZE_35_TRACKS_WITH_ERRORS = 175531
  private[this] final  val DISK_SIZE_40_TRACKS_WITH_ERRORS = 197376
  // D71
  private[this] final  val D71_DISK_SIZE_70_TRACKS = 349696
  private[this] final  val D71_DISK_SIZE_70_TRACKS_WITH_ERRORS = 351062
    
  /*
    Track allocation tables.

    .D64 ======================================================

        Track   Sectors/track   # Sectors   Storage in Bytes
        -----   -------------   ---------   ----------------
         1-17        21            357           7820
        18-24        19            133           7170
        25-30        18            108           6300
        31-40(*)     17             85           6020
                                   ---
                                   683 (for a 35 track image)
    .D71 ======================================================
         Track       Sec/trk   # Sectors
        --------------  -------   ---------
         1-17 (side 0)    21         357
        18-24 (side 0)    19         133
        25-30 (side 0)    18         108
        31-35 (side 0)    17          85
        36-52 (side 1)    21         357
        53-59 (side 1)    19         133
        60-65 (side 1)    18         108
        66-70 (side 1)    17          85
                                     ---
                              total 1366
   */
  private[this] final val TRACK_ALLOCATION = // Key = #track => Value = #sectors per track
    if (file.toUpperCase.endsWith(".D64"))
      (for (t <- 0 to 40) yield {
        if (t <= 17) (t, 21)
        else if (t <= 24) (t, 19)
        else if (t <= 30) (t, 18)
        else (t,17)
      }).toMap
    else
    (for (t <- 0 to 70) yield {
      if (t <= 17) (t, 21)
      else if (t <= 24) (t, 19)
      else if (t <= 30) (t, 18)
      else if (t <= 35) (t, 17)
      else if (t <= 52) (t, 21)
      else if (t <= 59) (t, 19)
      else if (t <= 65) (t, 18)
      else (t,17)
    }).toMap
    
  val isReadOnly = !new java.io.File(file).canWrite
  
  protected final val disk = new RandomAccessFile(file, if (isReadOnly) "r" else "rw")
  private[this] val absoluteSectorCache = new collection.mutable.HashMap[Int,Int]
  private[this] var _bam : BamInfo = bamInfo
  private[this] val trackChangeBitMap = Array.ofDim[Long](70)
  
  @inline private def trackSectorModified(t:Int,s:Int) = trackChangeBitMap(t - 1) |= 1 << s
  @inline private def isTrackModified(t:Int) = trackChangeBitMap(t - 1) > 0
  @inline private def clearModification = java.util.Arrays.fill(trackChangeBitMap,0)
  
  TOTAL_TRACKS // just to check for a valid track format
  
  private[this] val GCRImage = {
    val gcr = Array.ofDim[Array[Array[Int]]](TOTAL_TRACKS)
    for(i <- 0 until gcr.length) {
      gcr(i) = Array.ofDim[Array[Int]](TRACK_ALLOCATION(i + 1))
    }
    gcr
  }  
  
  override protected def absoluteSector(t: Int, s: Int) = {
    val cacheIndex = t << 8 | s
    absoluteSectorCache get cacheIndex match {
      case None =>
        val pos = (1 until t).foldLeft(0) { (acc, t) => acc + TRACK_ALLOCATION(t) } + s
        absoluteSectorCache += cacheIndex -> pos
        pos
      case Some(pos) =>
        pos
    }    
  }
  
  private def TOTAL_TRACKS = disk.length match {
    case DISK_SIZE_35_TRACKS|DISK_SIZE_35_TRACKS_WITH_ERRORS => 35
    case DISK_SIZE_40_TRACKS|DISK_SIZE_40_TRACKS_WITH_ERRORS => 40
    case D71_DISK_SIZE_70_TRACKS | D71_DISK_SIZE_70_TRACKS_WITH_ERRORS => if (_bam != null && _bam.singleSide) 35 else 70
    case _ => throw new IllegalArgumentException("Unsupported file format. size is " + disk.length)
  }
  
  def bam = _bam
  
  // CONSTRUCTOR
  loadGCRImage
  
  private def gcrImageOf(t:Int,s:Int) = if (t == 0 || t > GCRImage.length) GCR.EMPTY_GCR_SECTOR else GCRImage(t - 1)(s)
  
  @inline private def getSectorError(t:Int,s:Int) : Option[Int] = {
    disk.length match {
      case DISK_SIZE_35_TRACKS|DISK_SIZE_40_TRACKS => None
      case DISK_SIZE_35_TRACKS_WITH_ERRORS =>
        disk.seek(DISK_SIZE_35_TRACKS + absoluteSector(t,s))
        Some(disk.read)
      case DISK_SIZE_40_TRACKS_WITH_ERRORS =>
        disk.seek(DISK_SIZE_40_TRACKS + absoluteSector(t,s))
        Some(disk.read)
      case D71_DISK_SIZE_70_TRACKS => None
      case D71_DISK_SIZE_70_TRACKS_WITH_ERRORS => 
        disk.seek(D71_DISK_SIZE_70_TRACKS + absoluteSector(t,s))
        Some(disk.read)
    }
  }
  
  private def loadGCRImage {
    for(t <- 1 to TOTAL_TRACKS;
    	s <- 0 until TRACK_ALLOCATION(t)) GCRImage(t - 1)(s) = GCR.sector2GCR(s,t,readBlock(t,s),bam.diskID,getSectorError(t,s),s == TRACK_ALLOCATION(t) - 1)
  }
  
  override def flush {
    if (sectorModified && canWriteOnDisk) {
      sectorModified = false
      flushListener.flushing(file.toString,{
        val gcrTrack = new collection.mutable.ArrayBuffer[Int](400 * 20)
        val tracks = TOTAL_TRACKS
        for(t <- 1 to tracks) {
          if (isTrackModified(t)) {
            gcrTrack.clear
            val ss = TRACK_ALLOCATION(t)        
            for(s <- 0 until ss) {
              gcrTrack.appendAll(gcrImageOf(t, s))
            }
            GCR.GCR2track(gcrTrack.toArray,ss,(track,sector,data) => {
              //println(s"Writing back track $track sector $sector") 
              disk.seek(absoluteSector(track,sector) * BYTES_PER_SECTOR)
              val buf = Array.ofDim[Byte](data.length)
              var b = 0
              while (b < buf.length) {
                buf(b) = data(b).toByte
                b += 1
              }
              disk.write(buf)
            })
          }
          flushListener.update((t * 100.0 / tracks).toInt)
        }
        clearModification
      })
    }
  }
  
  def close = {
    flush    
    disk.close
  }
  
  private def bamInfo = {    
    //disk.seek(absoluteSector(DIR_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 3)
    val singleSide = file.toUpperCase.endsWith(".D64")// || disk.read != 0x80
    disk.seek(absoluteSector(DIR_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 0x90)
    val diskName = new StringBuilder
    var i = 0
    while (i < 16) {
      val c = disk.readByte.toInt & 0xFF
      if (c != 0xA0) diskName.append(c.toChar)
      i += 1
    }
    disk.skipBytes(2)
    val diskID = "" + disk.readByte.toChar + disk.readByte.toChar
    disk.skipBytes(1)
    val dosType = "" + disk.readByte.toChar + disk.readByte.toChar
    
    disk.seek(absoluteSector(DIR_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 4)
    var free = 0
    for(t <- 1 to 35) {
      val f = disk.read & 0xFF
      free += (if (t == 18) 0 else f)
      disk.skipBytes(3)
    }
    if (!singleSide) {
      disk.seek(absoluteSector(DIR_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 0xDD)
      for(t <- 1 to 35) free += disk.read & 0xFF      
    }
    BamInfo(diskName.toString, diskID, dosType,singleSide,free)
  }
  // --------------------- Floppy -------------------------  
  val totalTracks = TOTAL_TRACKS
  override lazy val singleSide = bamInfo.singleSide
  
  private[this] var track = 1
  private[this] var sector = 0
  private[this] var gcrSector = gcrImageOf(track, sector)
  private[this] var sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
  private[this] var gcrIndex = 0
  private[this] var sectorModified = false
  private[this] var trackChangeListener : Floppy#TrackListener = null
  private[this] var bit = 1
  private[this] var _side = 0
  private[this] var trackSideBase = 0
  
  override def isOnIndexHole = gcrIndex > 0 && gcrIndex < 3
  
  override def minTrack = _side match {
    case 0 =>
      1
    case 1 =>
      (totalTracks >> 1) + 1
  }
  override def maxTrack = _side match {
    case 0 =>
      if (bam.singleSide) totalTracks else totalTracks >> 1
    case 1 =>
      totalTracks
  }
  
  override def side = _side
  override def side_=(newSide:Int) {
    val oldT = track
    newSide match {
      case 0 if _side == 1 =>
          track -= (if (bam.singleSide) TOTAL_TRACKS else TOTAL_TRACKS >> 1)
          sector = 0
          gcrIndex = 0
          sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
          gcrSector = gcrImageOf(track, sector)
      case 1 if _side == 0 =>        
          track += (if (bam.singleSide) TOTAL_TRACKS else TOTAL_TRACKS >> 1)
          sector = 0
          gcrIndex = 0
          sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
          gcrSector = gcrImageOf(track, sector)
      case _ =>
    }
    //println(s"Side changed: ${_side} -> $newSide $oldT => $track")
    _side = newSide    
  }
  
  def reset {
    _side = 0
    track = 1
    sector = 0
    gcrSector = gcrImageOf(track, sector)
    sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
    gcrIndex = 0
    sectorModified = false
    bit = 1
  }
  
  final def nextBit = {
    val b = (gcrSector(gcrIndex) >> (8 - bit)) & 1
    if (bit == 8) rotate else bit += 1
    b
  }
  final def writeNextBit(value:Boolean) {
    sectorModified = true
    trackSectorModified(track,sector)
    val mask = 1 << (8 - bit)
    if (value) gcrSector(gcrIndex) |= mask else gcrSector(gcrIndex) &= ~mask
    if (bit == 8) rotate else bit += 1
  }
  final def nextByte : Int = gcrSector(gcrIndex)
  final def writeNextByte(b:Int) = gcrSector(gcrIndex) = b & 0xFF  
  
  @inline private def rotate {
    bit = 1
    gcrIndex += 1
    if (gcrIndex >= gcrSector.length) { // end of current sector
      gcrIndex = 0
      sector = (sector + 1) % sectorsPerCurrentTrack        
      gcrSector = gcrImageOf(track, sector)  
    }
  }
  
  def notifyTrackSectorChangeListener = if (trackChangeListener != null) trackChangeListener(track,false,Some(sector))
  def currentTrack = track
  def currentSector = Some(sector)
  /**
   * tracksteps & 1 == 0 are valid tracks, the others are half tracks not used
   * in the D64 format.
   */
  def changeTrack(trackSteps:Int) {
    val isOnTrack = (trackSteps & 1) == 0    
    if (isOnTrack) {
      val newTrack = trackSteps >> 1
      if (track != newTrack) {
        track = trackSteps >> 1
        sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
        sector = 0   
        bit = 1
        gcrSector = gcrImageOf(track, sector)
        gcrIndex = gcrIndex % gcrSector.length
        notifyTrackSectorChangeListener
      }
    }
  }
  
  def setTrackChangeListener(l:Floppy#TrackListener) = trackChangeListener = l
  
  override def toString = s"Disk fileName=$file totalTracks=$TOTAL_TRACKS t=$track s=$sector"
  // state
  def save(out:ObjectOutputStream) {
    out.writeInt(_side)
    out.writeInt(track)
    out.writeInt(sector)
    out.writeInt(gcrIndex)
    out.writeInt(bit)
    out.writeBoolean(sectorModified)
  }
  def load(in:ObjectInputStream) {
    _side = in.readInt
    track = in.readInt
    sector = in.readInt
    gcrIndex = in.readInt
    bit = in.readInt
    sectorModified = in.readBoolean
    sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
    gcrSector = gcrImageOf(track, sector)
  }
}