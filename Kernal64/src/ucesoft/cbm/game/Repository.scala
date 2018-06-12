package ucesoft.cbm.game

import java.io.File
import scala.util.Properties
import java.io.IOException
import ucesoft.cbm.Log
import java.util.zip.GZIPOutputStream
import java.io.FileOutputStream
import java.io.PrintWriter
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.net.URL
import javax.swing.ImageIcon
import java.nio.file.Files
import javax.imageio.ImageIO
import ucesoft.cbm.formats.ZIP
import scala.util.Success
import scala.util.Failure
import java.net.HttpURLConnection
import java.io.InputStream
import java.net.URLConnection

class Repository(provider:GameProvider) {
  import ZIP._
  
  private val home = new File(new File(Properties.userHome,".kernal64_game_repo"),provider.name)
  private val icon = new File(home,"icon")
  private val repository = new File(home,"repository.gz")
  private val version = new File(home,"version")
  private val iconExt = ".icon"
  private val fileNameExt = ".filename"
  private val SEP = "||"
  private val UN_SEP = """\|\|"""
  private var iconImage : Option[ImageIcon] = None
  private var versionID = ""
  
  init
  
  private def init {
    if (!home.exists) {
      if (!home.mkdirs) throw new IOException(s"Cannot create repository directory $home")
    }
    
    try {
      if (!icon.exists && provider.iconURL.isDefined) {
        val iconURL = provider.iconURL.get
        Log.info(s"Downloading icon for ${provider.name} [$iconURL] ...")
        val in = iconURL.openStream
        Files.copy(in,icon.toPath)
        in.close
      }
      iconImage = Some(new ImageIcon(ImageIO.read(icon)))
    }
    catch {
      case t:Throwable =>
        t.printStackTrace
    }
    if (!version.exists) writeVersion 
    else versionID = {
      val src = io.Source.fromFile(version)
      val v = src.getLines.next
      src.close
      v
    }
  }
  
  private def writeVersion {
    val v = new PrintWriter(new FileOutputStream(version))
    v.print(provider.version)
    versionID = provider.version
    v.close
  }
  
  final def currentVersion = versionID
  
  final def getIcon : Option[ImageIcon] = iconImage
  final def getIconFor(game:Game) : Option[ImageIcon] = {
    val iconFile = iconid(game) 
    if (iconFile.exists) Some(new ImageIcon(iconFile.toString)) else None
  }
  
  final def exists : Boolean = repository.exists
  
  final def load : List[Game] = {
    if (!exists) return Nil
    
    val in = io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(repository)))
    val lines = in.getLines
    try {
      val games = for(l <- lines) yield {
        l.split(UN_SEP) match {
          case Array(name,imageURL,downloadPageURL,year,genre,softwareHouse) =>
            Game(name,if (imageURL.isEmpty) None else Some(new URL(imageURL)),if (downloadPageURL.isEmpty) None else Some(new URL(downloadPageURL)),year,genre,softwareHouse)
          case _ => throw new IOException(s"Repository of ${provider.name} seems to be corrupted on line: $l")
        }
      }
      games.toList 
    }
    finally {
      in.close
    }    
  }
  
  final def save(games:List[Game]) {
    val out = new PrintWriter(new GZIPOutputStream(new FileOutputStream(repository)))
    try {
      for(g <- games) {
        import g._
        val sb = new StringBuilder
        sb.append(name)
        sb.append(SEP)
        sb.append(imageURL.getOrElse(""))
        sb.append(SEP)
        sb.append(downloadPageURL.getOrElse(""))
        sb.append(SEP)
        sb.append(date)
        sb.append(SEP)
        sb.append(genre)
        sb.append(SEP)
        sb.append(softwareHouse)
        out.println(sb.toString)
      }      
    }
    finally {
      out.close
    }
  }
  
  def clearCache {
    for(file <- home.listFiles) {
      if (file.getName != repository.getName) file.delete
    }
  }
  
  @inline private def gameid(game:Game) : File = new File(home,game.id)
  @inline private def iconid(game:Game) : File = new File(home,game.id  + iconExt)
  @inline private def fileNameid(game:Game) : File = new File(home,game.id  + fileNameExt)
  
  def existsInCache(game:Game) : Boolean = gameid(game).exists
  private def getArchiveFor(game:Game) : Option[File] = if (existsInCache(game)) Some(gameid(game)) else None
  private def getOrLoadArchiveFor(game:Game) : Option[File] = {
    getArchiveFor(game) match {
      case f@Some(_) => f
      case None =>
        saveInCache(game)
        getArchiveFor(game)
    }
  }
  protected def extractFileName(connection:URLConnection) : String = {
    var fileName = ""
    if (connection.isInstanceOf[HttpURLConnection]) {
      val cd = connection.getHeaderField("content-disposition")
      if (cd != null) {
        val FILENAME = """.*filename=\"(.+)\".*""".r
        cd match {
          case FILENAME(fn) =>
            fileName = fn
            println("FILENAME=" + fn)
          case _ =>
        }
      }
      else {
        val file = new File(connection.getURL.getFile)
        fileName = file.getName
        println(s"FILE=$fileName")
      }
    }
    
    fileName
  }
  protected def openDownloadURL(game:Game) : (InputStream,String) = {
    var connection = game.downloadPageURL.get.openConnection
    var fileName = ""
    if (connection.isInstanceOf[HttpURLConnection]) {
      fileName = extractFileName(connection)
      var keepRedirecting = true
      while (keepRedirecting) {
        keepRedirecting = false
        val hc = connection.asInstanceOf[HttpURLConnection]
        var status = hc.getResponseCode
        if (status != HttpURLConnection.HTTP_OK) {
          if (status == HttpURLConnection.HTTP_MOVED_TEMP || 
              status == HttpURLConnection.HTTP_MOVED_PERM ||            
  				    status == HttpURLConnection.HTTP_SEE_OTHER) {
            val newUrl = new URL(hc.getHeaderField("Location"))
            game.downloadPageURL = Some(newUrl)
            connection.getInputStream.close
            connection = newUrl.openConnection
            fileName = extractFileName(connection)   
            keepRedirecting = true
          }
        }
      }
    }
    (connection.getInputStream,fileName)
  }
  def saveInCache(game:Game) {
    if (game.downloadPageURL.isDefined) {    
      val (in,fileName) = openDownloadURL(game)
      val copied = Files.copy(in,gameid(game).toPath)
      in.close
      Files.write(fileNameid(game).toPath,java.util.Arrays.asList(fileName))
      writeVersion
    }
    saveGameIconInCache(game)
  }
  def saveGameIconInCache(game:Game) {    
    if (game.imageURL.isDefined) {
      getIconFor(game) match {
        case None =>
          val in = game.imageURL.get.openStream
          Files.copy(in,iconid(game).toPath)
          in.close
        case _ =>
      }      
    }
  }
  def getArchiveItemsFor(game:Game) : List[ArchiveEntry] = {
    getOrLoadArchiveFor(game) match {
      case None => 
        Nil
      case Some(file) =>
        zipEntries(file) match {
          case Success(entries) =>
            entries
          case Failure(t) =>
            val in = io.Source.fromFile(fileNameid(game))
            val fileName = in.getLines.mkString
            in.close
            if (fileName.isEmpty) return Nil
            
            val renamed = new File(home,fileName)
            file.renameTo(renamed)
            createZIPWith(file,renamed)
            renamed.delete
            zipEntries(file) match {
              case Success(entries) =>
                entries
              case Failure(_) =>
                Nil
            }
        }
    }
  }
  def extractEntry(entry:ArchiveEntry) : Option[File] = ZIP.extractEntry(entry,home)  
}