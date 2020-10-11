package ucesoft.cbm.formats

import java.io._
import java.nio.file.Files

import scala.collection.mutable.ListBuffer

class TAP(file:String) extends Iterator[Int] {
  private[this] val MAGIC = "C64-TAPE-RAW"
  private[this] val UNDEFINED_GAP = 20000
  private[this] val tape = new RandomAccessFile(file, "rw")
  private[this] var _version = 0
  
  loadHeader
  
  private def read = tape.read & 0xFF
  
  private def loadHeader  : Unit = {
    for(i <- MAGIC) {
      if (read.toChar != i) throw new IllegalArgumentException("Bad TAP file")
    }
    _version = read
    // ignore length
    rewind
  }
  
  def getFilename = file
  def close = tape.close
  def version = _version
  def tapeLength = tape.length
  def getOffset = tape.getFilePointer
  def rewind = tape.seek(0x14)
  def goTo(pos:Long) : Unit = tape.seek(pos)
  
  def hasNext = tape.getFilePointer < tape.length
  def next: Int = {
    val gap = read
    if (gap == 0) {
      if (version == 0) UNDEFINED_GAP
      else {
        val newGap = read | read << 8 | read << 16
        if (newGap != 0) newGap else UNDEFINED_GAP 
      }
    }
    else gap << 3
  }
  
  def write(_value:Int) : Unit = {
    var value = _value
    if (value > 0xFF) {
      tape.write(0)
      tape.write(value & 0xFF)
      value >>= 8
      tape.write(value & 0xFF)
      value >>= 8
      tape.write(value & 0xFF)
    }
    else {
      tape.write(value)
    }    
  }
}

object TAP {
  private val Z = 0x30
  private val O = 0x42
  private val N = 0x56
  private val ZERO = Z << 8 | O
  private val ONE = O << 8 | Z
  private val NEW_DATA = N << 8 | O
  private val END_DATA = N << 8 | Z
  private val TOL = 5

  case class TAPHeader(fileType:String,fileName:String,tapOffset:Long,blocks:Int = 0) {
    override def toString : String = fileName
  }

  private def isSync(sync:Long,byte:Int) : Boolean = sync == 0x8988878685848382L && byte == 0x81

  def anaylize(tapFile:File) : List[TAPHeader] = {
    var lastTwo = 0x0000
    val files = new ListBuffer[TAPHeader]

    val tapBuffer = Files.readAllBytes(tapFile.toPath)

    var inData = false
    var byte = 0
    var pulses = 0
    var sync = 0L
    var header = false
    var headerCounter = 0
    var fileType = 0
    var tapOffset = 0L
    val fileName = new StringBuilder
    var i = 0x14
    while (i < tapBuffer.length) {
      var read = tapBuffer(i).toInt
      if (read >= Z - TOL && read <= Z + TOL) read = Z
      else
      if (read >= O - TOL && read <= O + TOL) read = O
      else
      if (read >= N - TOL && read <= N + TOL) read = N

      lastTwo = (lastTwo << 8 | read) & 0xFFFF
      if (inData) {
        pulses -= 1
        val bit = if (lastTwo == ONE) 1 else 0
        if ((pulses % 2) == 0) byte = bit << 15 | byte >> 1
        if (pulses == 0) {
          inData = false
          byte = (byte >> 7) & 0xFF // ignore checkbit
          if (header) {
            headerCounter += 1
            if (headerCounter == 1) {
              tapOffset = i
              fileType = byte
              if (fileType != 1 && fileType != 3 && fileType != 4) header = false
            }
            else
            if (headerCounter >= 6 && headerCounter <= 22) {
              fileName.append(byte.toChar)
              if (headerCounter == 22) {
                val fileTypeName = fileType match {
                  case 1|3 => "PRG"
                  case 4 => "SEQ"
                }
                files += TAPHeader(fileTypeName,fileName.toString,tapOffset - 0x6A00)
              }
            }
          }
          if (isSync(sync,byte)) {
            header = true
            headerCounter = 0
            fileName.clear()
          }
          else sync = sync << 8 | byte
        }
      }
      else
      if (lastTwo == NEW_DATA) {
        inData = true
        pulses = 18
        byte = 0
      }
      else
      if (lastTwo == END_DATA) inData = false

      i += 1
    }
    files.toList
  }
}