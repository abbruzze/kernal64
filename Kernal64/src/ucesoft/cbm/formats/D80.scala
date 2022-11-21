package ucesoft.cbm.formats

import ucesoft.cbm.formats.Diskette.{BamInfo, FileData, FileType}

import scala.collection.mutable.ListBuffer

class D80(override val file: String,loadImage:Boolean = true) extends D64_D71(file,loadImage) {
  override protected def BAM_HEADER_SIZE = 6
  override protected def BAM_TRACK = 38
  override protected def DIR_TRACK = 39
  override protected def DIR_SECTOR = 1
  override protected def BAM_SECTOR = 0
  protected val DIR_HEADER_SECTOR = 0

  override protected def BAM_SECTORS : Int = 2
  override protected def BAM_INTERLEAVE : Int = 3
  override protected def BAM_ENTRY_SIZE : Int = 5
  override protected def BAM_TRACKS : Array[Int] = Array(50,27)

  override protected def EMPTY_DISK() : String = "/resources/emptyDisk.d80"

  private[this] final val DISK_SIZE_77_TRACKS = 533248

  override protected def TOTAL_TRACKS : Int = 77


  override protected lazy val TRACK_ALLOCATION: Map[Int, Int] = {
    (for (t <- 0 to 77) yield {
      if (t <= 39) (t, 29)
      else if (t <= 53) (t, 27)
      else if (t <= 64) (t, 25)
      else (t, 23)
      //else if (t <= 116) (t, 29)
      //else if (t <= 130) (t, 27)
      //else if (t <= 141) (t, 25)
      //else (t,23)
    }).toMap
  }

  override protected def getSectorError(t:Int,s:Int) : Option[Int] = None

  override def formatDirectory(): Option[FileData] = {
    val buffer = new ListBuffer[Int]
    val dir = readBlock(DIR_TRACK,DIR_HEADER_SECTOR)
    buffer.addAll(dir.drop(2).map(_.toInt & 0xFF)) // DIR sector without 1st and 2nd bytes
    for(bam <- readBams()) {
      buffer.addAll(bam.drop(2).map(_.toInt & 0xFF)) // BAM sector without 1st and 2nd bytes
    }
    Some(FileData("$",0,buffer.toArray,FileType.SEQ))
  }

  override def reloadBam: BamInfo = bamInfo

  override protected def bamInfo: BamInfo = {
    // go to 39/0 to read header
    disk.seek(absoluteSector(DIR_TRACK, DIR_HEADER_SECTOR) * BYTES_PER_SECTOR)
    var bamTrack = disk.readByte().toInt & 0xFF
    var bamSector = disk.readByte().toInt & 0xFF

    disk.skipBytes(4)
    val diskName = new StringBuilder
    var i = 0
    while (i < 16) {
      val c = disk.readByte.toInt & 0xFF
      if (c != 0xA0) diskName.append(c.toChar)
      i += 1
    }
    disk.seek(absoluteSector(DIR_TRACK, DIR_HEADER_SECTOR) * BYTES_PER_SECTOR + 0x18)
    val id1 = disk.readByte.toInt & 0xFF
    val id2 = disk.readByte.toInt & 0xFF
    val diskID = "" + id1.toChar + id2.toChar
    disk.skipBytes(1)
    val dos1 = disk.readByte.toInt & 0xFF
    val dos2 = disk.readByte.toInt & 0xFF
    val dosType = "" + dos1.toChar + dos2.toChar

    var bamSectorCount = 0
    var free = 0
    while (bamSectorCount < 2) {
      disk.seek(absoluteSector(bamTrack, bamSector) * BYTES_PER_SECTOR)
      // next bam coordinates
      bamTrack = disk.readByte().toInt & 0xFF
      bamSector = disk.readByte().toInt & 0xFF
      bamSectorCount += 1

      disk.skipBytes(2)
      val lowTrack = disk.readByte().toInt & 0xFF
      val hiTrack = disk.readByte().toInt & 0xFF

      for(t <- lowTrack until hiTrack) {
        val f = disk.readByte().toInt & 0xFF
        free += (if (t == DIR_TRACK) 0 else f)
        disk.skipBytes(4)
      }
    }
    BamInfo(diskName.toString, diskID, dosType,true,free)
  }

  override def rename(name:String,id:String) : Unit = {
    disk.seek(absoluteSector(DIR_TRACK,BAM_SECTOR) * BYTES_PER_SECTOR + 0x06)
    for(i <- 0 to 17) {
      val c = if (i < name.length) name.charAt(i).toInt else 0xA0
      disk.write(c)
    }
    disk.write(id.charAt(0))
    disk.write(id.charAt(1))
  }

}
