package ucesoft.cbm.formats

import ucesoft.cbm.peripheral.bus.BusDataIterator
import ucesoft.cbm.peripheral.drive.Floppy
import ucesoft.cbm.cpu.Memory
import java.io.RandomAccessFile
import scala.collection.mutable.ListBuffer
import java.util.StringTokenizer
import java.io.FileNotFoundException
import java.io.IOException
import java.nio.file.Files
import java.io.File
import java.nio.file.StandardCopyOption

object Diskette {
  object FileType extends Enumeration {
    val DEL, SEQ, PRG, USR, REL, CBM = Value
  }
  case class DirEntry(fileType: FileType.Value, fileName: String, t: Int, s: Int, sizeInSectors: Int)
  case class BamInfo(diskName: String, diskID: String, dosType: String,singleSide:Boolean,freeSectors:Int)
  case class FileData(fileName: String, startAddress: Int, data: Array[Int]) {
    def iterator = {
      val buffer = if (startAddress != -1) Array.ofDim[Int](data.length + 2) else data
      if (startAddress != -1) {
        buffer(0) = startAddress % 256
        buffer(1) = startAddress / 256
        Array.copy(data, 0, buffer, 2, data.length)
      }
      new BusDataIterator {
        private[this] var index = 0
        override def hasNext = index < buffer.length
        override def next = {
          val value = buffer(index)
          index += 1
          value
        }
        def isLast = index == buffer.length - 1
        def getPerc = (100 * index.toFloat / buffer.length).toInt
        def goto(pos:Int) {
          index = pos
        }
      }
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
  def apply(fileName:String) : Diskette = {
    val upper = fileName.toUpperCase
    if (upper.endsWith(".D64") || upper.endsWith(".D71")) new D64_D71(fileName)
    else
    if (upper.endsWith(".D81") ) new D81(fileName)
    else
    if (upper.endsWith(".G64") ) new G64(fileName)
    else throw new IllegalArgumentException("Unsupported file format")
  }
  
  def makeEmptyDisk(file:String) {
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
  protected val DIR_TRACK = 18
  protected val DIR_SECTOR = 1
  protected val BAM_SECTOR = 0
  
  val canBeEmulated : Boolean
  protected val disk : RandomAccessFile
  
  protected def absoluteSector(t:Int,s:Int) : Int = 0
  
  def directories : List[DirEntry] = {
    var t = DIR_TRACK
    var s = DIR_SECTOR
    var dirs = new ListBuffer[DirEntry]
    var readNextSector = true
    val buffer = Array.ofDim[Byte](0x20)
    while (readNextSector) {      
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
        else {                              
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
          val entry = DirEntry(fileType, fileName.toString, track, sector, size)
          dirs += entry
        }
      }
    }
    dirs.toList
  }
  
  // optional
  def bam : BamInfo  
  
  def readBlock(track:Int,sector:Int) = {
    disk.seek(absoluteSector(track,sector) * BYTES_PER_SECTOR)
    val buffer = Array.ofDim[Byte](BYTES_PER_SECTOR)
    disk.read(buffer)
    buffer
  }
  
  // =======================================================================
  def loadInMemory(mem: Memory, fileName: String, relocate: Boolean,c64Mode:Boolean=true) = {
    load(fileName) match {
      case FileData(fn, startAddress, data) =>
        val initialAddress = relocate match {
          case true => startAddress
          case false => ProgramLoader.startBASICAddress(mem,c64Mode)
        }
        for (m <- initialAddress until initialAddress + data.length) mem.write(m, data(m - initialAddress))
        val endAddress = initialAddress + data.length
        println("Loaded " + fn + " from " + initialAddress + " to " + endAddress)
        ProgramLoader.updateBASICPointers(mem,initialAddress, endAddress,c64Mode)
        endAddress
    }
  }
  
  protected def loadPRG(entry: DirEntry) = {
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
    FileData(entry.fileName, startAddress, data.toArray)
  }
  
  protected def loadSEQ(entry: DirEntry) = {
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
    FileData(entry.fileName, -1, data.toArray)
  }
  
  def load(fileName: String,fileType:FileType.Value = FileType.PRG) = {
    if (fileName.startsWith("$")) formatDirectoriesAsPRG(fileName)
    else {
      val dpos = fileName.indexOf(":")
      val st = new StringTokenizer(if (dpos != -1) fileName.substring(dpos + 1) else fileName,",")
      val fn = st.nextToken
      val ft = if (st.hasMoreTokens && st.nextToken == "S") FileType.SEQ else fileType
      
      directories find { e =>
        ft == e.fileType && fileNameMatch(fn,e.fileName)
      } match {
        case None => throw new FileNotFoundException(fileName)
        case Some(entry) =>
          entry.fileType match {
            case FileType.PRG => loadPRG(entry)
            case FileType.SEQ => loadSEQ(entry)
            case _ => throw new IOException("Bad file type: " + entry.fileType)
          }
      }
    }
  }  
  
  private def formatDirectoriesAsPRG(fileName:String) = {
    val colonPos = fileName.indexOf(":")
    val dirs = if (colonPos == -1) directories else {
      val filter = fileName.substring(colonPos + 1)
      val asteriskPos = filter.indexOf('*')
      directories filter { fn =>
        if (asteriskPos == -1) fn.fileName == filter else fn.fileName.startsWith(filter.substring(0,asteriskPos))
      }      
    }
    val out = new ListBuffer[Int]
    val _bam = bam
    
    // set start address to $0801
    var ptr = 0x801
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
    FileData("$",0x801,out.toArray)
  }
}
