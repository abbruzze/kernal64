package ucesoft.c64.formats

import java.io._

class TAP(file:String) extends Iterator[Int] {
  private[this] val MAGIC = "C64-TAPE-RAW"
  private[this] val UNDEFINED_GAP = 20000
  private[this] val tape = new RandomAccessFile(file, "r")
  private[this] var _version = 0
  private[this] var _length = 0L
  private[this] var offset = 0L
  
  loadHeader
  
  private def read = tape.read & 0xFF
  
  private def loadHeader {
    for(i <- MAGIC) {
      if (read.toChar != i) throw new IllegalArgumentException("Bad TAP file")
    }
    _version = read
    tape.seek(0x10)
    _length = read | read << 8 | read << 16 | read << 24
  }
  
  def getFilename = file
  def close = tape.close
  def version = _version
  def tapeLength = _length
  def getOffset = offset
  def setOffset(newOffset:Long) {
    offset = newOffset
    tape.seek(offset + 0x14)
  }
  def hasNext = offset < _length
  def next = {
    offset += 1
    val gap = read
    if (gap == 0) {
      if (version == 0) UNDEFINED_GAP
      else {
        offset += 3
        val newGap = read | read << 8 | read << 16
        if (newGap != 0) newGap else UNDEFINED_GAP 
      }
    }
    else gap << 3//(gap * 8.119783039397187).toInt
  }
}