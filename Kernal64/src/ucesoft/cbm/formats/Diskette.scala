package ucesoft.cbm.formats

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.bus.BusDataIterator
import ucesoft.cbm.peripheral.drive.Floppy

import java.io.{File, FileNotFoundException, IOException, RandomAccessFile}
import java.nio.file.{Files, StandardCopyOption}
import java.util.StringTokenizer
import scala.collection.mutable.ListBuffer

object Diskette {
  object FileType extends Enumeration {
    val DEL, SEQ, PRG, USR, REL, CBM = Value

    def fromString(s:String): FileType.Value = {
      if (s == null) PRG
      else
      s.toUpperCase() match {
        case null|"P"|"PRG" => PRG
        case "D"|"DEL" => DEL
        case "S"|"SEQ" => SEQ
        case "R"|"REL" => REL
        case "U"|"USR" => USR
      }
    }
  }
  object FileMode extends Enumeration {
    val READ ,WRITE, APPEND = Value

    def fromString(s:String): FileMode.Value = {
      if (s == null) READ
      else
      s.toUpperCase() match {
        case "R" => READ
        case "W" => WRITE
        case "A" => APPEND
      }
    }
  }
  case class DirEntry(fileType: FileType.Value, fileName: String, t: Int, s: Int, sizeInSectors: Int,entryTrack:Int,entrySector:Int,entryPos:Int)
  case class BamInfo(diskName: String, diskID: String, dosType: String,singleSide:Boolean,freeSectors:Int)
  case class FileData(fileName: String, startAddress: Int, data: Array[Int],fileType:FileType.Value) {
    def iterator: BusDataIterator = {
      val buffer = if (startAddress != -1) Array.ofDim[Int](data.length + 2) else data
      if (startAddress != -1) {
        buffer(0) = startAddress % 256
        buffer(1) = startAddress / 256
        Array.copy(data, 0, buffer, 2, data.length)
      }
      new BusDataIterator {
        private[this] var index = 0
        override def hasNext: Boolean = index < buffer.length
        override def next: Int = {
          val value = buffer(index)
          index += 1
          value
        }
        def isLast: Boolean = index == buffer.length - 1
        def getPerc: Int = (100 * index.toFloat / buffer.length).toInt
        def goto(pos:Int) : Unit = {
          index = pos
        }
      }
    }
  }

  sealed trait FileName
  case class StandardFileName(name:String,fileType:FileType.Value,mode:FileMode.Value,overwrite:Boolean) extends FileName
  case class DirectoryFileName(pattern:Option[String],filterOnType:Option[FileType.Value]) extends FileName

  private val FILENAME_RE = """(@?\d:)?([^,]+)(,(s|seq|S|SEQ|p|prg|P|PRG|u|usr|U|USR|r|rel|R|REL))?(,(r|w|a|R|W|A))?""".r
  private val DIRECTORY_RE = """\$(\d:([^=]+)(=([s|S|r|R|p|P|u|U]))?)?""".r

  def parseFileName(fn:String): Option[FileName] = {
    fn match {
      case DIRECTORY_RE(_,pattern,_,ftype) =>
        Some(DirectoryFileName(Option(pattern),Option(ftype).map(FileType.fromString)))
      case FILENAME_RE(ovr,pattern,_,ftype,_,mode) =>
        Some(StandardFileName(pattern,FileType.fromString(ftype),FileMode.fromString(mode),ovr != null && ovr.startsWith("@")))
      case _ =>
        None
    }
  }
  
  def fileNameMatch(fileNameToSearch:String,fileName:String) : Boolean = {
    var i = 0
    while (i < fileNameToSearch.length) {
      val a = fileNameToSearch.charAt(i)
      if (a == '*') return true      
      if (i >= fileName.length) return false
      
      val b = fileName.charAt(i)
      if (a == '?') i += 1      
      else
      if (a == b) i += 1
      else return false
    }
    
    fileNameToSearch.length == fileName.length
  }  
  
  // factory method
  def apply(fileName:String,load : Boolean = true) : Diskette = {
    val upper = fileName.toUpperCase
    if (upper.endsWith(".D64") || upper.endsWith(".D71")) new D64_D71(fileName,load)
    else
    if (upper.endsWith(".D81") ) new D81(fileName)
    else
    if (upper.endsWith(".G64") ) new G64(fileName)
    else throw new IllegalArgumentException("Unsupported file format")
  }
  
  def makeEmptyDisk(file:String) : Unit = {
    val p = file.lastIndexOf('.')
    if (p == -1) throw new IllegalArgumentException("File name must have a valid extension")
    val ext = file.substring(p + 1).toUpperCase    
    val diskRes = ext match {
      case "G64" =>
        "emptyDisk.g64"
      case "D64" =>
        "emptyDisk.d64"
      case "D71" =>
        "emptyDisk.d71"
      case "D81" =>
        "emptyDisk.d81"
      case _ =>
        throw new IllegalArgumentException(s"Unsupported disk format: $ext")
    }
    val emptyDisk = getClass.getResourceAsStream(s"/resources/$diskRes")
    if (emptyDisk == null) throw new IllegalArgumentException(s"Cannot find '$diskRes'")
    Files.copy(emptyDisk,new File(file).toPath,StandardCopyOption.REPLACE_EXISTING)
  }
}

abstract class Diskette extends Floppy {
  import Diskette._
  
  protected val BYTES_PER_SECTOR = 256
  protected def DIR_TRACK = 18
  protected def DIR_SECTOR = 1
  protected def BAM_SECTOR = 0
  
  val canBeEmulated : Boolean
  protected val disk : RandomAccessFile
  
  protected def absoluteSector(t:Int,s:Int) : Int = 0

  protected def makeDirEntryFromBuffer(buffer:Array[Byte],t:Int,s:Int,pos:Int) : DirEntry = {
    val fileType = FileType(buffer(2) & 7)
    val track = buffer(3)
    val sector = buffer(4)
    val fileName = new StringBuilder
    var a0Found = false
    var i = 5
    val a0 = 0xA0.toByte
    while (i < 0x15 && !a0Found) {
      if (buffer(i) == a0) a0Found = true
      else fileName.append((buffer(i) & 0xFF).toChar)
      i += 1
    }
    val size = buffer(0x1E).toInt & 0xFF + (buffer(0x1F).toInt & 0xFF) * 256
    DirEntry(fileType, fileName.toString, track, sector, size,t,s,pos)
  }
  
  def directories : List[DirEntry] = {
    var t = DIR_TRACK
    var s = DIR_SECTOR
    val dirs = new ListBuffer[DirEntry]
    var readNextSector = true
    val buffer = Array.ofDim[Byte](0x20)
    while (readNextSector) {
      val currentT = t
      val currentS = s
      disk.seek(absoluteSector(t, s) * BYTES_PER_SECTOR)
      var firstEntryOfSector = true
      var entryIndex = 0
      var readNextEntry = true
      while (readNextEntry) {
        disk.read(buffer)
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
        if (entryIndex == 9 || buffer.forall(_ == 0)) {
          readNextEntry = false // last+1 entry of this sector
        }
        else dirs += makeDirEntryFromBuffer(buffer,currentT,currentS,entryIndex - 1)
      }
    }
    dirs.toList
  }
  
  // optional
  def bam : BamInfo  
  
  def readBlock(track:Int,sector:Int): Array[Byte] = {
    disk.seek(absoluteSector(track,sector) * BYTES_PER_SECTOR)
    val buffer = Array.ofDim[Byte](BYTES_PER_SECTOR)
    disk.read(buffer)
    buffer
  }
  
  // =======================================================================
  def loadInMemory(mem: Memory, fileName: String, relocate: Boolean,c64Mode:Boolean,drive:Int) : Unit = {
    load(fileName) match {
      case FileData(fn, startAddress, data,_) =>
        val (start,end) = ProgramLoader.loadPRG(mem,data,if (relocate) Some(startAddress) else None,c64Mode,drive,fileName)
        println(s"Loaded $fn from $start to $end")
      case _ =>
    }
  }
  
  protected def loadPRG(entry: DirEntry): FileData = {
    val buffer = Array.ofDim[Byte](BYTES_PER_SECTOR)
    val data = new ListBuffer[Int]
    var lastChunk = false
    var isFirstChunk = true
    var startAddress = 0
    var t = entry.t
    var s = entry.s
    while (!lastChunk) {
      disk.seek(absoluteSector(t, s) * BYTES_PER_SECTOR)
      disk.read(buffer)
      var chunkIndex = 0
      t = buffer(0)
      s = buffer(1).toInt & 0xFF
      lastChunk = t == 0
      if (isFirstChunk) {
        isFirstChunk = false
        startAddress = (buffer(2).toInt & 0xFF) + (buffer(3).toInt & 0xFF) * 256
        chunkIndex += 4
      } else chunkIndex += 2
      val lastByte = if (lastChunk) s else 255
      while (chunkIndex <= lastByte) {
        data += buffer(chunkIndex).toInt & 0xFF
        chunkIndex += 1
      }
    }
    FileData(entry.fileName, startAddress, data.toArray,entry.fileType)
  }
  
  protected def loadSEQ(entry: DirEntry): FileData = {
    val buffer = Array.ofDim[Byte](BYTES_PER_SECTOR)
    val data = new ListBuffer[Int]
    var lastChunk = false
    var t = entry.t
    var s = entry.s
    while (!lastChunk) {
      disk.seek(absoluteSector(t, s) * BYTES_PER_SECTOR)
      disk.read(buffer)
      var chunkIndex = 0
      t = buffer(0)
      s = buffer(1).toInt & 0xFF
      lastChunk = t == 0
      chunkIndex += 2
      val lastByte = if (lastChunk) s else 255
      while (chunkIndex <= lastByte) {
        data += buffer(chunkIndex).toInt & 0xFF
        chunkIndex += 1
      }
    }
    FileData(entry.fileName, -1, data.toArray,entry.fileType)
  }
  
  def load(fileName: String,fileType:FileType.Value = FileType.PRG): FileData = {
    if (fileName.startsWith("$")) formatDirectoriesAsPRG(DirectoryFileName(Some(fileName),None))
    else {
      val dpos = fileName.indexOf(":")
      val st = new StringTokenizer(if (dpos != -1) fileName.substring(dpos + 1) else fileName,",")
      val fn = {
        val fname = st.nextToken
        if (fname.length < 17) fname else fname.substring(0,16)
      }
      val ft = if (st.hasMoreTokens && st.nextToken == "S") FileType.SEQ else fileType
      
      directories find { e =>
        /*ft == e.fileType &&*/ fileNameMatch(fn,e.fileName)
      } match {
        case None => throw new FileNotFoundException(fileName)
        case Some(entry) =>
          ft match {
            case FileType.PRG => loadPRG(entry)
            case FileType.SEQ => loadSEQ(entry)
            case _ => throw new IOException("Bad file type: " + entry.fileType)
          }
      }
    }
  }

  def load(fileName:FileName): Option[FileData] = {
    fileName match {
      case StandardFileName(name, fileType, mode, overwrite) =>
        val fn = if (name.length < 17) name else name.substring(0,16)
        directories.find(e => fileNameMatch(fn,e.fileName)).map { e =>
          fileType match {
            case FileType.PRG => loadPRG(e)
            case FileType.SEQ => loadSEQ(e)
            case _ => throw new IOException("Bad file type: " + e.fileType)
          }
        }
      case d@DirectoryFileName(_,_) =>
        Some(formatDirectoriesAsPRG(d))
    }
  }

  protected def getDirectoryStartAddress: Int = 0x801
  
  protected def formatDirectoriesAsPRG(dir:DirectoryFileName,showDEL:Boolean = false): FileData = {
    /*val colonPos = fileName.indexOf(":")
    val dirs = if (colonPos == -1) directories else {
      val filter = fileName.substring(colonPos + 1)
      val asteriskPos = filter.indexOf('*')
      directories filter { fn =>
        if (asteriskPos == -1) fn.fileName == filter else fn.fileName.startsWith(filter.substring(0,asteriskPos))
      }      
    }
     */
    val dirs = directories filter { d =>
      dir.pattern match {
        case Some(p) =>
          fileNameMatch(p,d.fileName)
        case None =>
          true
      }
    } filter { d =>
      dir.filterOnType match {
        case Some(t) =>
          d.fileType == t
        case None =>
          true
      }
    } filter { d =>
      if (!showDEL && d.fileType == FileType.DEL) false
      else true
    }

    val out = new ListBuffer[Int]
    val _bam = bam
    
    // set start address to $0801
    var ptr = getDirectoryStartAddress
    // write next line address
    ptr += 30
    out.append(ptr & 0xFF) 	// L
    out.append(ptr >> 8)	// H
    // write label
    out.append(0) // drive L
    out.append(0) // drive H
    out.append(0x12) // RVS ON
    out.append(0x22) // "
    for(i <- 0 until 16) {
      if (i < _bam.diskName.length) out.append(_bam.diskName.charAt(i)) else out.append(0x20)
    }
    out.append(0x22) // "
    out.append(0x20)
    out.append(_bam.diskID(0))
    out.append(_bam.diskID(1))
    out.append(0x20)
    out.append(_bam.dosType(0))
    out.append(_bam.dosType(1))
    out.append(0x00)	// EOL
    //val filteredDirs = if (showDEL) dirs else dirs.filterNot(_.fileType == FileType.DEL)
    for(dir <- dirs) {
      val blanks = if (dir.sizeInSectors < 10) 3 
      	else
        if (dir.sizeInSectors < 100) 2
        else 1
      // write next line address
      ptr += blanks + 2 + 2 + 18 + 5
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      out.append(ptr & 0xFF) 	// L
      out.append(ptr >> 8)	// H
      // write blocks
      out.append(dir.sizeInSectors & 0xFF)
      out.append(dir.sizeInSectors >> 8)
      // blanks after blocks      
      for(i <- 1 to blanks) out.append(0x20)
      out.append(0x22) // "
      for(i <- 0 until dir.fileName.length) out.append(dir.fileName.charAt(i))
      out.append(0x22) // "
      for(i <- 1 to 16 - dir.fileName.length) out.append(0x20)
      out.append(0x20) // "
      val fileType = dir.fileType.toString
      for(i <- 0 until fileType.length) out.append(fileType.charAt(i))
      for(i <- 1 to endBlanks) out.append(0x20)
      out.append(0x00) // EOL
    }
    
    val blocksFreeText = "BLOCKS FREE."
    // write next line address
    ptr += 2 + 2 + blocksFreeText.length + 1
    out.append(ptr & 0xFF) 	// L
    out.append(ptr >> 8)	// H
    val blocksFree = bam.freeSectors
    // write block free
    out.append(blocksFree & 0xFF) 	// L
    out.append(blocksFree >> 8)		// H    
    for(i <- 0 until blocksFreeText.length) out.append(blocksFreeText.charAt(i))
    out.append(0x00) // EOL
    
    out.append(0x00)
    out.append(0x00)
    FileData("$",getDirectoryStartAddress,out.toArray,FileType.PRG)
  }
}
