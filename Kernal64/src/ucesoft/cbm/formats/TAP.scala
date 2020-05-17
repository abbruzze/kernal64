package ucesoft.cbm.formats

import java.io._

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
  
  def hasNext = tape.getFilePointer < tape.length
  def next = {
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