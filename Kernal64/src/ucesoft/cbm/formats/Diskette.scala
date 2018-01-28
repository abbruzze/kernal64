package ucesoft.cbm.formats

import ucesoft.cbm.peripheral.bus.BusDataIterator
import ucesoft.cbm.peripheral.drive.Floppy
import ucesoft.cbm.cpu.Memory

object Diskette {
  object FileType extends Enumeration {
    val DEL, SEQ, PRG, USR, REL = Value
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
  def apply(fileName:String,empty:Boolean = false) : Diskette = {
    val upper = fileName.toUpperCase
    if (upper.endsWith(".D64") || upper.endsWith(".D71")) new D64_D71(fileName,empty)
    else
    if (upper.endsWith(".G64") ) new G64(fileName)
    else throw new IllegalArgumentException("Unsupported file format")
  }
}

trait Diskette extends Floppy {
  import Diskette._
  def TOTAL_AVAILABLE_SECTORS : Int
  
  val canBeEmulated : Boolean  
  
  // optional
  def bam : BamInfo
  def directories : List[DirEntry]
  def readBlock(track:Int,sector:Int) : Array[Byte]
  def loadInMemory(mem: Memory, fileName: String, relocate: Boolean,c64Mode:Boolean=true) : Int
  def load(fileName: String,fileType:FileType.Value = FileType.PRG) : FileData  
}
