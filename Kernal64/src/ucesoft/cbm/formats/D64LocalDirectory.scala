package ucesoft.cbm.formats

import ucesoft.cbm.formats.Diskette.DirEntry

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import scala.collection.mutable

object D64LocalDirectory {
  private final val MAX_FILE_SIZE = 664 * 256

  def createDiskFromLocalDir(dir:File) : D64LocalDirectory = {
    val disk = File.createTempFile(s"localdisk_${dir.getName}",".d64")
    disk.deleteOnExit()
    Diskette.makeEmptyDisk(disk.toString)
    val d64 = new D64LocalDirectory(disk.toString,dir)
    val files = dir.listFiles() filterNot { f => f.isDirectory || f.length >= MAX_FILE_SIZE } iterator
    var overflow = false
    val fileMap = new mutable.HashMap[String,File]

    while (!overflow && files.hasNext) {
      val f = files.next()
      val name = f.getName.toUpperCase
      val content = Files.readAllBytes(f.toPath)
      if (name.endsWith(".SEQ")) {
        val c64Name = name.dropRight(4)
        overflow = !d64.addSEQ(content,c64Name)
        if (!overflow) fileMap += c64Name -> f
      }
      else {
        val c64Name = if (name.endsWith(".PRG")) name.dropRight(4) else name
        val startAddress = content(0) | content(1) << 8
        overflow = !d64.addPRG(content.drop(2),c64Name,startAddress)
        if (!overflow) fileMap += c64Name -> f
      }
    }
    d64.setFileMap(fileMap.toMap)
    d64.rename(s"/${dir.getName.toUpperCase}","  ")
    d64.reloadGCR
    d64
  }
}

class D64LocalDirectory(override val file:String,val dir:File) extends D64_D71(file,false)  {
  private[this] var initialDirEntries : List[DirEntry] = Nil
  private[this] var diskModified = false
  private[this] var fileMap : Map[String,File] = Map.empty

  private[formats] def setFileMap(map:Map[String,File]) : Unit = {
    fileMap = map
    initialDirEntries = directories
  }

  override def writeNextBit(value: Boolean): Unit = {
    super.writeNextBit(value)
    diskModified = true
  }

  override def writeNextByte(b: Int): Unit = {
    super.writeNextByte(b)
    diskModified = true
  }

  override def close: Unit = {
    flush
    if (diskModified) syncWithLocalDir
    disk.close()
  }

  private def syncWithLocalDir() : Unit = {
    val dirEntries = directories
    val toDelete = initialDirEntries filter { e => !dirEntries.contains(e) }
    val toModify = dirEntries
    // remove
    for(e <- toDelete) {
      fileMap get e.fileName match {
        case Some(f) =>
          f.delete()
        case None =>
      }
    }
    for(e <- toModify filter { f => f.fileType == Diskette.FileType.PRG || f.fileType == Diskette.FileType.SEQ }) {
      fileMap get e.fileName match {
        case Some(f) =>
          updateFile(f,e)
        case None =>
          val newFile = new File(dir,s"${e.fileName.toLowerCase}.${e.fileType.toString.toLowerCase}")
          updateFile(newFile,e)
      }
    }
  }

  private def updateFile(f:File,e:DirEntry) : Unit = {
    var t = e.t
    var s = e.s
    val out = new FileOutputStream(f)
    try {
      do {
        val sector = readBlock(t, s)
        t = sector(0)
        s = sector(1).toInt & 0xFF
        if (t == 0) out.write(sector, 2, s)
        else out.write(sector, 2, sector.length - 2)
      }
      while (t != 0)
    }
    finally out.close()
  }
}
