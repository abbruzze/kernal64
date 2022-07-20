package ucesoft.cbm.formats
import ucesoft.cbm.formats.TestD64Editor.d64
import ucesoft.cbm.peripheral.drive.Floppy

import java.io.{ObjectInputStream, ObjectOutputStream, RandomAccessFile}
import scala.collection.mutable.ListBuffer

class D64_D71(val file: String,loadImage:Boolean = true) extends Diskette {
  import Diskette._
  val canBeEmulated = true
  // D64
  private[this] final  val DISK_SIZE_40_TRACKS = 196608
  private[this] final  val DISK_SIZE_42_TRACKS = 205312
  private[this] final  val DISK_SIZE_35_TRACKS = 174848
  private[this] final  val DISK_SIZE_35_TRACKS_WITH_ERRORS = 175531
  private[this] final  val DISK_SIZE_40_TRACKS_WITH_ERRORS = 197376
  private[this] final  val DISK_SIZE_42_TRACKS_WITH_ERRORS = 206114
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
  protected lazy val TRACK_ALLOCATION: Map[Int, Int] = // Key = #track => Value = #sectors per track
    if (file.toUpperCase.endsWith(".D64"))
      (for (t <- 0 to 42) yield {
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
    
  val isReadOnly: Boolean = !new java.io.File(file).canWrite
  
  protected final val disk = new RandomAccessFile(file, if (isReadOnly) "r" else "rw")
  private[this] val absoluteSectorCache = new collection.mutable.HashMap[Int,Int]
  private[this] val _bam : BamInfo = bamInfo
  private[this] val trackChangeBitMap = Array.ofDim[Long](70)
  
  @inline private def trackSectorModified(t:Int,s:Int): Unit = trackChangeBitMap(t - 1) |= 1 << s
  @inline private def isTrackModified(t:Int) = trackChangeBitMap(t - 1) > 0
  @inline private def clearModification(): Unit = java.util.Arrays.fill(trackChangeBitMap,0)
  
  TOTAL_TRACKS // just to check for a valid track format
  
  private[this] val GCRImage = {
    val gcr = Array.ofDim[Array[Array[Int]]](TOTAL_TRACKS)
    for(i <- 0 until gcr.length) {
      gcr(i) = Array.ofDim[Array[Int]](TRACK_ALLOCATION(i + 1))
    }
    gcr
  }  
  
  override protected def absoluteSector(t: Int, s: Int): Int = {
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
  
  protected def TOTAL_TRACKS: Int = disk.length match {
    case DISK_SIZE_35_TRACKS|DISK_SIZE_35_TRACKS_WITH_ERRORS => 35
    case DISK_SIZE_40_TRACKS|DISK_SIZE_40_TRACKS_WITH_ERRORS => 40
    case DISK_SIZE_42_TRACKS|DISK_SIZE_42_TRACKS_WITH_ERRORS => 42
    case D71_DISK_SIZE_70_TRACKS | D71_DISK_SIZE_70_TRACKS_WITH_ERRORS => if (_bam != null && _bam.singleSide) 35 else 70
    case _ => throw new IllegalArgumentException("Unsupported file format. size is " + disk.length)
  }

  def isValidTrackAndSector(t:Int,s:Int): Boolean =
    TRACK_ALLOCATION.get(t) match {
      case Some(sectors) =>
        s <= sectors
      case None =>
        false
    }
  
  def bam: BamInfo = _bam
  
  // CONSTRUCTOR
  if (loadImage) loadGCRImage
  
  private def gcrImageOf(t:Int,s:Int) = if (t == 0 || t > GCRImage.length) GCR.EMPTY_GCR_SECTOR else GCRImage(t - 1)(s)
  
  protected def getSectorError(t:Int,s:Int) : Option[Int] = {
    disk.length match {
      case DISK_SIZE_35_TRACKS|DISK_SIZE_40_TRACKS|DISK_SIZE_42_TRACKS => None
      case DISK_SIZE_35_TRACKS_WITH_ERRORS =>
        disk.seek(DISK_SIZE_35_TRACKS + absoluteSector(t,s))
        Some(disk.read)
      case DISK_SIZE_40_TRACKS_WITH_ERRORS =>
        disk.seek(DISK_SIZE_40_TRACKS + absoluteSector(t,s))
        Some(disk.read)
      case DISK_SIZE_42_TRACKS_WITH_ERRORS =>
        disk.seek(DISK_SIZE_42_TRACKS + absoluteSector(t,s))
        Some(disk.read)
      case D71_DISK_SIZE_70_TRACKS => None
      case D71_DISK_SIZE_70_TRACKS_WITH_ERRORS => 
        disk.seek(D71_DISK_SIZE_70_TRACKS + absoluteSector(t,s))
        Some(disk.read)
    }
  }
  
  private def loadGCRImage()  : Unit = {
    for(t <- 1 to TOTAL_TRACKS;
    	s <- 0 until TRACK_ALLOCATION(t)) GCRImage(t - 1)(s) = GCR.sector2GCR(s,t,readBlock(t,s),bam.diskID,getSectorError(t,s),s == TRACK_ALLOCATION(t) - 1)
  }
  
  override def flush  : Unit = {
    if (sectorModified && canWriteOnDisk) {
      sectorModified = false
      flushListener.flushing(file,{
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
  
  def close: Unit = {
    flush    
    disk.close()
  }
  
  protected def bamInfo: BamInfo = {
    //disk.seek(absoluteSector(DIR_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 3)
    val singleSide = file.toUpperCase.endsWith(".D64")// || disk.read != 0x80
    disk.seek(absoluteSector(BAM_TRACK, BAM_SECTOR) * BYTES_PER_SECTOR + 0x90)
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
  val totalTracks: Int = TOTAL_TRACKS
  override lazy val singleSide: Boolean = bamInfo.singleSide
  
  private[this] var track = 1
  private[this] var sector = 0
  private[this] var gcrSector = gcrImageOf(track, sector)
  private[this] var sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
  private[this] var gcrIndex = 0
  private[this] var sectorModified = false
  private[this] var trackChangeListener : Floppy#TrackListener = null
  private[this] var bit = 1
  private[this] var _side = 0

  override def isOnIndexHole: Boolean = gcrIndex > 0 && gcrIndex < 3
  
  override def minTrack: Int = _side match {
    case 0 =>
      1
    case 1 =>
      (totalTracks >> 1) + 1
  }
  override def maxTrack: Int = _side match {
    case 0 =>
      if (bam.singleSide) totalTracks else totalTracks >> 1
    case 1 =>
      totalTracks
  }

  def isModified : Boolean = sectorModified
  
  override def side: Int = _side
  override def side_=(newSide:Int) : Unit = {
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
    _side = newSide
  }
  
  def reset  : Unit = {
    _side = 0
    track = 1
    sector = 0
    gcrSector = gcrImageOf(track, sector)
    sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
    gcrIndex = 0
    sectorModified = false
    bit = 1
  }
  
  final def nextBit: Int = {
    val b = (gcrSector(gcrIndex) >> (8 - bit)) & 1
    if (bit == 8) rotate else bit += 1
    b
  }
  def writeNextBit(value:Boolean) : Unit = {
    sectorModified = true
    trackSectorModified(track,sector)
    val mask = 1 << (8 - bit)
    if (value) gcrSector(gcrIndex) |= mask else gcrSector(gcrIndex) &= ~mask
    if (bit == 8) rotate else bit += 1
  }
  final def nextByte : Int = gcrSector(gcrIndex)
  def writeNextByte(b:Int): Unit = {
    gcrSector(gcrIndex) = b & 0xFF
    sectorModified = true
  }
  
  @inline private def rotate()  : Unit = {
    bit = 1
    gcrIndex += 1
    if (gcrIndex >= gcrSector.length) { // end of current sector
      gcrIndex = 0
      sector = (sector + 1) % sectorsPerCurrentTrack        
      gcrSector = gcrImageOf(track, sector)  
    }
  }
  
  def notifyTrackSectorChangeListener: Unit = if (trackChangeListener != null) trackChangeListener(track,false,Some(sector))
  def currentTrack: Int = track
  def currentSector: Option[Int] = Some(sector)
  /**
   * tracksteps & 1 == 0 are valid tracks, the others are half tracks not used
   * in the D64 format.
   */
  def changeTrack(trackSteps:Int) : Unit = {
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
  
  def setTrackChangeListener(l:Floppy#TrackListener): Unit = trackChangeListener = l
  
  override def toString = s"Disk fileName=$file totalTracks=$TOTAL_TRACKS t=$track s=$sector"
  // state
  def save(out:ObjectOutputStream) : Unit = {
    out.writeInt(_side)
    out.writeInt(track)
    out.writeInt(sector)
    out.writeInt(gcrIndex)
    out.writeInt(bit)
    out.writeBoolean(sectorModified)
  }
  def load(in:ObjectInputStream) : Unit = {
    _side = in.readInt
    track = in.readInt
    sector = in.readInt
    gcrIndex = in.readInt
    bit = in.readInt
    sectorModified = in.readBoolean
    sectorsPerCurrentTrack = TRACK_ALLOCATION(track)
    gcrSector = gcrImageOf(track, sector)
  }

  // ================================== EDITOR ====================================================
  protected def BAM_HEADER_SIZE = 4
  protected def BAM_SECTORS : Int = 1
  protected def BAM_INTERLEAVE : Int = 0
  protected def BAM_ENTRY_SIZE : Int = 4
  protected def BAM_TRACKS : Array[Int] = Array(TOTAL_TRACKS)

  protected def readBams(): Array[Array[Byte]] = {
    var bamSector = BAM_SECTOR
    val buffers = Array.ofDim[Array[Byte]](BAM_SECTORS)
    for(b <- 0 until BAM_SECTORS) {
      buffers(b) = readBlock(BAM_TRACK,bamSector)
      bamSector += BAM_INTERLEAVE
    }
    buffers
  }

  override def formatDirectory(): Option[FileData] = {
    val buffer = new ListBuffer[Int]
    val dir = readBlock(DIR_TRACK,BAM_SECTOR)
    buffer.addAll(dir.drop(2).map(_.toInt & 0xFF)) // BAM sector without 1st and 2nd bytes
    Some(FileData("$",0,buffer.toArray,FileType.SEQ))
  }

  def addPRG(file:Array[Byte],fileName:String,startAddress:Int) : Boolean = addFile(file,fileName,startAddress,true)
  def addSEQ(file:Array[Byte],fileName:String) : Boolean = addFile(file,fileName,-1,false)

  protected def addFile(file:Array[Byte],fileName:String,startAddress:Int,isPRG:Boolean = true) : Boolean = {
    def asByte(a:Array[Byte],p:Int) : Int = a(p).toInt & 0xFF
    for(b <- file) println(s"Writing ${b.toInt & 0xFF} ${b.toChar}")
    val fileBlocks = math.ceil(file.length / 256.0).toInt
    if (fileBlocks > bam.freeSectors) return false
    // load bam in buffer
    val bamsBuffer = readBams()
    var dirSector = readBlock(DIR_TRACK,DIR_SECTOR)
    val dirSectorsToUpdate = new ListBuffer[(Int,Int,Array[Byte])]

    var t = DIR_TRACK
    var s = DIR_SECTOR
    var lt = DIR_TRACK
    var ls = DIR_SECTOR

    // search en empty entry in the directory chain
    var dirEntryFound = false
    var dirEntry = 2
    while (t != 0 && !dirEntryFound) {
      // check if there is an empty entry in current sector
      dirEntry = 2
      while (dirEntry < 256 && !dirEntryFound) {
        if (asByte(dirSector,dirEntry) == 0) dirEntryFound = true
        else dirEntry += 32
      }
      if (!dirEntryFound) {
        lt = t
        ls = s
        t = asByte(dirSector, 0)
        s = asByte(dirSector, 1)
        if (t != 0) dirSector = readBlock(t, s)
      }
    }
    if (dirEntryFound) dirSectorsToUpdate.addOne((t,s,dirSector)) // modify current one
    else dirSectorsToUpdate.addOne((lt,ls,dirSector)) // modify last one to point eventually to the new one

    if (!dirEntryFound) { // no entry found on current sector
      findOrAllocateFreeSector(bamsBuffer(0),true) match {
        case None =>
          return false
        case Some((nt,ns)) =>
          // change next pointer
          dirSector(0) = nt.toByte
          dirSector(1) = ns.toByte
          dirSector = Array.ofDim[Byte](256)
          dirSectorsToUpdate.addOne((nt,ns,dirSector))
          dirEntry = 2
      }
    }
    //dirSector(1) = 0xFF.toByte // last sector
    dirSector(dirEntry) = if (isPRG) 0x82.toByte else 0x81.toByte // PRG or SEQ
    for(i <- 0 until 16) dirSector(dirEntry + 3 + i) = if (i < fileName.length) fileName.charAt(i).toByte else 0xA0.toByte

    // find space for file
    var firstTrackSector = true
    var fileOffset = 0
    var sectors = new ListBuffer[(Int,Int)]
    while (fileOffset < file.length) {
      findFreeSector(bamsBuffer) match {
        case None =>
          return false
        case Some(ts@(nt,ns)) =>
          if (firstTrackSector) {
            firstTrackSector = false
            dirSector(dirEntry + 1) = nt.toByte
            dirSector(dirEntry + 2) = ns.toByte
            fileOffset += 252
          }
          else fileOffset += 254
          sectors += ts
      }
    }
    dirSector(dirEntry + 0x1C) = (sectors.length & 0xFF).toByte
    dirSector(dirEntry + 0x1D) = ((sectors.length >> 8) & 0xFF).toByte
    // ok, space found, write file content
    firstTrackSector = true
    fileOffset = 0
    while (!sectors.isEmpty) {
      val (t,s) = sectors.head
      disk.seek(absoluteSector(t, s) * BYTES_PER_SECTOR)
      val diff = file.length - fileOffset
      if (sectors.tail.isEmpty) {
        disk.write(0)
        val lastByte = if (firstTrackSector) 3 + diff else 1 + diff
        disk.write(lastByte)
      }
      else {
        val (nt,ns) = sectors.tail.head
        disk.write(nt)
        disk.write(ns)
      }

      val len = if (firstTrackSector && isPRG) {
        firstTrackSector = false
        disk.write(startAddress & 0xFF)
        disk.write((startAddress >> 8) & 0xFF)
        252
      }
      else 254

      disk.write(file,fileOffset,if (diff < len) diff else len)
      fileOffset += len

      sectors = sectors.tail
    }
    // write directory sectors
    for((t,s,buffer) <- dirSectorsToUpdate) {
      writeBlock(t,s,buffer)
    }
    // write bam
    var bamSector = BAM_SECTOR
    for(b <- 0 until BAM_SECTORS) {
      writeBlock(BAM_TRACK, bamSector, bamsBuffer(b))
      bamSector += BAM_INTERLEAVE
    }
    true
  }

  protected def findFreeSector(bams:Array[Array[Byte]],allocate:Boolean = true): Option[(Int,Int)] = {
    var trackAllocationOffset = 0
    for(b <- 0 until bams.length) {
      findOrAllocateFreeSector(bams(b),false,trackAllocationOffset,BAM_TRACKS(b),allocate) match {
        case s@Some(_) =>
          return s
        case None =>
          trackAllocationOffset += BAM_TRACKS(b)
      }
    }
    None
  }

  def findOrAllocateFreeSector(bam:Array[Byte], onDirTrack:Boolean,trackAllocationOffset : Int = 0,bamTrackLimit : Int = TOTAL_TRACKS,allocate:Boolean = true) : Option[(Int,Int)] = {
    def asByte(p:Int) : Int = bam(p).toInt & 0xFF
    var track = if (onDirTrack) DIR_TRACK else 1
    val trackLimit = if (onDirTrack) DIR_TRACK + 1 else bamTrackLimit + 1
    val jump = 1
    var trackCount = 0
    val bamEntrySize = BAM_ENTRY_SIZE
    while (trackCount < trackLimit) {
      if (track != DIR_TRACK || onDirTrack) {
        val sectorPerTrack = TRACK_ALLOCATION(track + trackAllocationOffset)
        val pos = BAM_HEADER_SIZE + bamEntrySize * (track - 1)
        if (asByte(pos) > 0) {
          val sectorsMap = {
            var map = 0L
            var i = 1
            while (i < bamEntrySize) {
              map |= (bam(pos + i).toLong & 0xFF) << ((i - 1) << 3)
              i += 1
            }
            map
          }
          var mask = 1L
          var s = 0
          var i = 0
          while (i < sectorPerTrack) {
            if ((sectorsMap & mask) == mask) { // found
              if (allocate) {
                var modSectorsMap = sectorsMap & ~mask
                bam(pos) = (bam(pos) - 1).toByte
                var j = 0
                while (j < BAM_ENTRY_SIZE - 1) {
                  bam(pos + 1 + j) = (modSectorsMap & 0xFF).toByte
                  modSectorsMap >>= 8
                  j += 1
                }
              }

              return Some((track + trackAllocationOffset,s))
            }
            s = (s + jump) % sectorPerTrack
            mask = 1L << s
            i += 1
          }
        }
      }
      trackCount += 1
      track = if (track == 1)
        bamTrackLimit
      else track - 1
    }
    None
  }

  def deleteFile(entry:DirEntry) : Unit = {
    val bamsBuffer = readBams()
    // DEL on directory
    val dir = readBlock(entry.entryTrack,entry.entrySector)
    dir(2 + entry.entryPos * 32) = 0 // DEL
    // delete file
    var t = entry.t
    var s = entry.s
    while (t != 0) {
      val (bamSector,track) = getBAMForTrack(bamsBuffer,t)
      makeFreeSector(bamSector,track,s)
      disk.seek(absoluteSector(t,s) * BYTES_PER_SECTOR)
      // read next t,s
      t = disk.read
      s = disk.read
    }
    writeBlock(entry.entryTrack,entry.entrySector,dir)
    // write bam
    var bamSector = BAM_SECTOR
    for(b <- 0 until BAM_SECTORS) {
      writeBlock(BAM_TRACK, bamSector, bamsBuffer(b))
      bamSector += BAM_INTERLEAVE
    }
  }

  protected def getBAMForTrack(bams:Array[Array[Byte]],t:Int): (Array[Byte],Int) = {
    var tracks = 0
    for(b <- 0 until bams.length) {
      if (t - tracks <= BAM_TRACKS(b)) return (bams(b),t - tracks)
      tracks += BAM_TRACKS(b)
    }
    throw new IllegalArgumentException(s"Error while getting BAM for track $t")
  }

  def reloadGCR() : Unit = loadGCRImage

  /*
    0 = OK
    1 = newName already exists
    2 = oldName doesn't exist
   */
  def renameFile(oldName:String,newName:String): Int = {
    val dirs = directories
    if (dirs.exists(_.fileName == newName)) 1
    else {
      dirs.find(_.fileName == oldName) match {
        case Some(e) =>
          disk.seek(absoluteSector(e.entryTrack, e.entrySector) * BYTES_PER_SECTOR + e.entryPos * 0x20 + 5) // file name
          var i = 0
          while (i < 16) {
            if (i < newName.length) disk.write(newName.charAt(i))
            else disk.write(0xA0)
            i += 1
          }
          0
        case None =>
          2
      }
    }
  }

  def rename(name:String) : Unit = {
    disk.seek(absoluteSector(DIR_TRACK,BAM_SECTOR) * BYTES_PER_SECTOR + 0x90)
    for(i <- 0 to 15) {
      val c = if (i < name.length) name.charAt(i).toInt else 0xA0
      disk.write(c)
    }
  }

  def makeFreeSector(bam:Array[Byte],t:Int,s:Int) : Unit = {
    val bamEntrySize = BAM_ENTRY_SIZE
    val pos = BAM_HEADER_SIZE + bamEntrySize * (t - 1)
    bam(pos) = (bam(pos) + 1).toByte
    val sectorsMap = {
      var map = 0L
      var i = 1
      while (i < bamEntrySize) {
        map |= (bam(pos + i).toLong & 0xFF) << ((i - 1) << 3)
        i += 1
      }
      map
    }
    var modSectorsMap = sectorsMap | 1L << s
    var j = 0
    while (j < BAM_ENTRY_SIZE - 1) {
      bam(pos + 1 + j) = (modSectorsMap & 0xFF).toByte
      modSectorsMap >>= 8
      j += 1
    }
  }
}

object TestD64Editor extends App {
  val d64 = new D80("/Users/ealeame/Desktop/empty.d80",false)

  d64.rename("PIPPO") ; d64.close ; sys.exit(0)
  d64.directories foreach { d64.deleteFile } ; d64.close ; sys.exit(0)

  val in = new java.io.FileInputStream("/Users/ealeame/Desktop/bruce.prg")
  val file = in.readAllBytes()
  val startAddress = file(0) | file(1) << 8
  val content = file.drop(2)

  val N = scala.io.StdIn.readLine("How many?:").toInt
  val from = scala.io.StdIn.readLine("From:").toInt
  for(i <- 1 to N)
    if (!d64.addPRG(content,s"BRUCE${from + i - 1}",startAddress)) println("NO SPACE") else println("OK, added")


  //d64.directories find { e =>e.fileName == "BRUCE LEE2" } foreach { d64.deleteFile }
  //d64.rename("AUTOSTART")
  d64.reloadGCR
  d64.close
}