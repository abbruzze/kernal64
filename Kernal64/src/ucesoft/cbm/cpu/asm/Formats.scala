package ucesoft.cbm.cpu.asm

import ucesoft.cbm.cpu.asm.AsmEvaluator._

import scala.collection.mutable

object Formats {
  private val SID_STRUCT_FIELDS = List("magicID","version","init","play","load","songs","startSong","size","data","speed","name","author","released","flags")

  def analyzeSID(sid:ListVal) : StructVal = {
    def error(msg:String) = throw new EvaluationError(s"Error while analyzing SID content, invalid format: $msg")
    def int(i:RuntimeValue) : Int = i match {
      case NumberVal(n) => n.toInt
      case _ => error("expected number")
    }
    def read0String(b:mutable.Buffer[RuntimeValue],offset:Int,len:Int) : StringVal = {
      var p = offset
      var c = int(b(p))
      val sb = new StringBuilder()
      var rem = len
      while (rem > 0 && c != 0) {
        sb.append(c.toChar)
        rem -= 1
        p += 1
        c = int(b(p))
      }
      StringVal(sb.toString)
    }

    val list = sid.list
    if (list.length < 76) error("bad header length")
    try {
      val map = new collection.mutable.HashMap[String,RuntimeValue]
      // header
      if (int(list(0)) == 0x50 && int(list(1)) == 0x53 && int(list(2)) == 0x49 && int(list(3)) == 0x44) map += "magicID" -> StringVal("PSID")
      else if (int(list(0)) == 0x52 && int(list(1)) == 0x53 && int(list(2)) == 0x49 && int(list(3)) == 0x44) map += "magicID" -> StringVal("RSID")
      else error("Bad magicID")
      // version
      val version = int(list(4)) << 8 | int(list(5))
      map += "version" -> NumberVal(version)
      // init address
      val initAddress = int(list(0x0A)) << 8 | int(list(0x0B))
      map += "init" -> NumberVal(initAddress)
      // play address
      val playAddress = int(list(0x0C)) << 8 | int(list(0x0D))
      map += "play" -> NumberVal(playAddress)
      // load address
      var dataOffsetBytesToSkip = 0
      val dataOffset = int(list(0x06)) << 8 | int(list(0x07))
      var loadAddress = int(list(0x08)) << 8 | int(list(0x09))
      if (loadAddress == 0) {
        loadAddress = int(list(dataOffset + 1)) << 8 | int(list(dataOffset))
        dataOffsetBytesToSkip = 2
      }
      map += "load" -> NumberVal(loadAddress)
      // songs
      val songs = int(list(0x0E)) << 8 | int(list(0x0F))
      map += "songs" -> NumberVal(songs)
      // startSong
      val startSong = int(list(0x10)) << 8 | int(list(0x11))
      map += "startSong" -> NumberVal(startSong)
      // size
      val size = list.length - dataOffset - dataOffsetBytesToSkip
      map += "size" -> NumberVal(size)
      // data
      val data = list.takeRight(size)
      map += "data" -> ListVal(data)
      // speed
      val speed = int(list(0x12)) << 24 | int(list(0x13)) << 16 | int(list(0x14)) << 8 | int(list(0x15))
      map += "speed" -> NumberVal(speed)
      // name
      val name = read0String(list,0x16,32)
      map += "name" -> name
      // author
      val author  = read0String(list,0x36,32)
      map += "author" -> author
      // released
      val released  = read0String(list,0x56,32)
      map += "released" -> released
      // flags
      val flags = if (version != 1) int(list(0x76)) << 8 | int(list(0x77)) else 0
      map += "flags" -> NumberVal(flags)

      StructVal("SID",None,SID_STRUCT_FIELDS,map)

    }
    catch {
      case e:EvaluationError => throw e
      case _:Exception =>
        error(" internal error")
    }
  }
}
