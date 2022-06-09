package ucesoft.cbm.formats

import ucesoft.cbm.Log

import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import scala.util.Try

object ZIP {
  private val allowedArchiveExtentions = Set("PRG","D64","G64","CRT","TAP","T64")
  case class ArchiveEntry(name:String,zipFile:File,private[ZIP] entry:String) {
    val isPRG: Boolean = name.toUpperCase.endsWith(".PRG")
    val isDisk: Boolean = name.toUpperCase.endsWith(".D64") || name.toUpperCase.endsWith(".G64")
    val isCRT: Boolean = name.toUpperCase.endsWith(".CRT")
    val isTape: Boolean = name.toUpperCase.endsWith(".TAP")
    override def toString: String = name
  }
  
  def zipEntries(file:File) : Try[List[ArchiveEntry]] = {
    Try {
        val zip = new ZipFile(file)
      import scala.jdk.CollectionConverters._
        val entries = zip.entries.asScala
        val validEntries = entries filter { e =>
          val name = e.getName
          val dot = name.lastIndexOf(".")
          dot != -1 && allowedArchiveExtentions.contains(name.substring(dot + 1).toUpperCase)
        }
        val list = validEntries map { e => ArchiveEntry(new File(e.getName).getName,file,e.getName) } toList
        
        zip.close()
        list      
    }
  }
  def extractEntry(entry:ArchiveEntry,dir:File) : Option[File] = {
    val zip = new ZipFile(entry.zipFile)
    try {
      try {
        val item = zip.getEntry(entry.entry)
        if (item == null) None
        else {
          val in = zip.getInputStream(item)
          val file = new File(dir,entry.name)
          if (file.exists) Some(file)
          else {
            file.deleteOnExit()
            Files.copy(in,file.toPath)
            Some(file)
          }
        }
      }
      finally {
        zip.close()
      }   
    }
    catch {
      case t:Throwable =>
        Log.info(s"Can't open zip archive: ${entry.zipFile}: " + t)
        None
    }
  }
  def createZIPWith(zipArchive:File,files:File*) : Unit = {
    val out = new ZipOutputStream(new FileOutputStream(zipArchive))
    for(f <- files) {
      val entry = new ZipEntry(f.getName)
      out.putNextEntry(entry)
      Files.copy(f.toPath,out)
      out.closeEntry()
    }    
    out.close()
  } 
}