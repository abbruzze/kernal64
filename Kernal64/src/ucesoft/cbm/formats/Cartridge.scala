package ucesoft.cbm.formats

import java.io._
import scala.language.postfixOps

object Cartridge {
  def createCRTFileFromState(in:ObjectInputStream) : File = {
    val tmpFile = File.createTempFile("CRT",null)
    tmpFile.deleteOnExit()
    val tmpOut = new FileOutputStream(tmpFile)
    val len = in.readInt
    val buffer = Array.ofDim[Byte](len)
    in.readFully(buffer)
    tmpOut.write(buffer)
    tmpOut.close()
    tmpFile
  }

  def main(args:Array[String]): Unit = {
    println(new Cartridge(args(0)))
  }

  object CBMType extends Enumeration {
    val C64   = Value("C64 CARTRIDGE   ")
    val C128  = Value("C128 CARTRIDGE  ")
    val VIC20 = Value("VIC20 CARTRIDGE ")
    val CBMII = Value("CBM2 CARTRIDGE  ")
  }
}

class Cartridge(val file:String) {
  import Cartridge._

  class Chip {
    var bankNumber = 0
    var startingLoadAddress = 0
    var romSize = 0
    
    var romData : Array[Int] = null
    
    def load(in:RandomAccessFile) : Unit = {
      def nextByte : Int = in.readByte.toInt & 0xFF
      if (in.readByte != 'C' || in.readByte != 'H' || in.readByte != 'I' || in.readByte != 'P') throw new IOException("CHIP signature not found")
      in.skipBytes(6)
      bankNumber = (nextByte << 8) + nextByte
      startingLoadAddress = ((nextByte << 8) + nextByte) & 0xFFFF
      romSize = ((nextByte << 8) + nextByte) & 0xFFFF
      val buffer = Array.ofDim[Byte](romSize)
      in.read(buffer)
      romData = Array.ofDim(romSize)
      for(i <- 0 until romSize) romData(i) = buffer(i).toInt & 0xFF
    }
    
    override def toString = s"CHIP bank=$bankNumber loadAddress=${Integer.toHexString(startingLoadAddress)} romSize=$romSize"
  }
  var name = ""
  var ctrType = 0
  var cbmType : CBMType.Value = _
  var EXROM,GAME = false
  var chips : Array[Chip] = null
  lazy val kbSize: Int = (chips map { _.romSize } sum) / 1024
  
  load

  def saveState(out:ObjectOutputStream) : Unit = {
    val f = new File(file)
    out.writeInt(f.length.toInt)
    java.nio.file.Files.copy(f.toPath,out)
  }
  
  def load()  : Unit = {
    val in = new RandomAccessFile(file,"r")
    try {
      val nameBuffer = Array.ofDim[Byte](16)
      in.readFully(nameBuffer)
      val signature = new String(nameBuffer)
      try {
        cbmType = CBMType.withName(signature)
      }
      catch {
        case _:Exception =>
          throw new IOException(s"Cart signature not supported: $signature")
      }
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
      in.close()
    }
  }
  
  override def toString = s"Cartridge[$cbmType] $name type=$ctrType EXROM=$EXROM GAME=$GAME CHIPS=${chips.map{_.toString} mkString("[",",","]")}"
}

class CartridgeBuilder(crt:String,name:String,crtType:Int,exrom:Boolean,game:Boolean) {
  private val out = new FileOutputStream(crt)

  // signature
  for(i <- 0 to 15) out.write("C64 CARTRIDGE   ".charAt(i))
  // header length
  out.write(0) ;out.write(0) ;out.write(0) ; out.write(0x40)
  // version
  out.write(1) ; out.write(0)
  // type
  out.write(crtType >> 8) ; out.write(crtType & 0xFF)
  // EXROM
  out.write(if (exrom) 0 else 1)
  // GAME
  out.write(if (game) 0 else 1)
  // reserved
  for(_ <- 0x1A to 0x1F) out.write(0)
  // name
  for(i <- 0 to 31) out.write(if (i < name.length) name.charAt(i).toInt else 0)

  def addChip(startAddress:Int,chipType:Int,bank:Int,data:Array[Int]) : Unit = {
    // signature
    for(i <- 0 to 3) out.write("CHIP".charAt(i))
    // total packet length
    val tpl = 0x10 + data.length
    out.write(tpl >> 24) ; out.write(tpl >> 16) ; out.write(tpl >> 8) ; out.write(tpl & 0xFF)
    // type
    out.write(chipType >> 8) ; out.write(chipType & 0xFF)
    // bank number
    out.write(bank >> 8) ; out.write(bank & 0xFF)
    // load address
    out.write(startAddress >> 8) ; out.write(startAddress & 0xFF)
    // image size
    out.write(data.length >> 8) ; out.write(data.length & 0xFF)
    // data
    val bdata = Array.ofDim[Byte](data.length)
    var i = 0
    while (i < data.length) {
      bdata(i) = data(i).toByte
      i += 1
    }
    out.write(bdata)
  }

  def finish() : Unit = out.close()
}