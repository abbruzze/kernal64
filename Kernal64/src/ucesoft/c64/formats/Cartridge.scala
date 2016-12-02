package ucesoft.c64.formats

import java.io._
import ucesoft.c64.cpu.Memory
import ucesoft.c64.ChipID
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.Log
import scala.language.postfixOps

class Cartridge(file:String) {
  class Chip {
    var bankNumber = 0
    var startingLoadAddress = 0
    var romSize = 0
    
    var romData : Array[Int] = null
    
    def load(in:RandomAccessFile) {
      if (in.readByte != 'C' || in.readByte != 'H' || in.readByte != 'I' || in.readByte != 'P') throw new IOException("CHIP signature not found")
      in.skipBytes(6)
      bankNumber = in.readByte * 256 + in.readByte
      startingLoadAddress = (in.readByte * 256 + in.readByte) & 0xFFFF
      romSize = (in.readByte * 256 + in.readByte) & 0xFFFF
      romData = Array.ofDim(romSize)
      for(i <- 0 until romSize) romData(i) = in.readByte.toInt & 0xFF
    }
    
    override def toString = s"CHIP bank=${bankNumber} loadAddress=${Integer.toHexString(startingLoadAddress)} romSize=${romSize}"
  }
  private val CRT_SIGN = "C64 CARTRIDGE   "
  var name = ""
  var ctrType = 0 
  var EXROM,GAME = false
  var chips : Array[Chip] = null
  lazy val kbSize = (chips map { _.romSize } sum) / 1024
  
  load 
  
  def load {
    println("Opening file " + file)
    val in = new RandomAccessFile(file,"r")
    try {
      for(i <- 0 to 15) if (in.readByte != CRT_SIGN(i)) throw new IOException("Signature not found on index " + i)
      in.seek(0x16)
      ctrType = in.readByte * 256 + in.readByte
      EXROM = in.readByte == 1
      GAME = in.readByte.toInt == 1
      in.seek(0x20)
      val sb = new StringBuilder
      var i = 0
      var zeroFound = false
      while (i < 32 && !zeroFound) {
        val c = in.readByte
        if (c == 0) zeroFound = true else sb.append(c.toChar)
        i += 1
      }
      name = sb.toString
      in.seek(0x40)
      var tmp : List[Chip] = Nil
      while (in.getFilePointer < in.length) {        
        val chip = new Chip
        chip.load(in)
        tmp = chip :: tmp
      }
      chips = tmp.reverse.toArray
    }
    finally {
      in.close
    }
  }
  
  override def toString = s"Cartridge ${name} type=${ctrType} EXROM=${EXROM} GAME=${GAME} CHIPS=${chips.map{_.toString} mkString("[",",","]")}"
}