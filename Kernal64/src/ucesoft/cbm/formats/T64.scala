package ucesoft.cbm.formats

import ucesoft.cbm.cpu.Memory

import java.io.{IOException, RandomAccessFile}

case class T64Entry(fileName: String, startAddress: Int, offset: Int, length: Int, blocks: Int) {
  override def toString: String = fileName
}

class T64(file: String) {
  private[this] val t64 = new RandomAccessFile(file, "r")
  private[this] var _tapeName = ""
  val entries: Array[T64Entry] = loadEntries

  def close: Unit = t64.close()
  def tapeName: String = _tapeName

  def loadInMemory(mem: Memory, entry: T64Entry,c64Mode:Boolean=true) : Unit = {
    t64.seek(entry.offset)
    for (m <- entry.startAddress until entry.startAddress + entry.length) mem.write(m, read)
    
    val endAddress = entry.startAddress + entry.length
    println("Loaded " + entry.fileName + " from " + entry.startAddress + " to " + endAddress)
    ProgramLoader.updateBASICPointers(mem,entry.startAddress,endAddress,c64Mode,1,entry.fileName)
  }

  private def read = t64.read & 0xFF

  private def readName(len: Int) = {
    val sb = new StringBuilder
    for (i <- 1 to len) sb.append(read.toChar)
    sb.toString.trim
  }

  private def loadEntries = {
    // check magic
    if (read != 0x43 || read != 0x36 || read != 0x34) throw new IOException("T64 Bad format")
    // read total number of entries
    t64.seek(0x24)
    val totalEntries = read | read << 8
    // read tape name
    t64.seek(0x28)
    _tapeName = readName(24)
    var offset = 0x40
    val entries = for (e <- 1 to totalEntries) yield {
      t64.seek(offset)
      offset += 0x20
      if (read == 0x00 || read == 0x00) None // skip empty entries and not PRG files
      else {
        val startAddress = read | read << 8
        val length = (read | read << 8) - startAddress
        t64.skipBytes(2)
        val off = (read | read << 8) | (read | read << 8) << 16
        t64.skipBytes(4)
        val fileName = readName(16)
        Some(T64Entry(fileName, startAddress, off, length, length / 256))
      }
    }
    entries.flatten.toArray
  }
}