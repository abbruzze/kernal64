package ucesoft.c64.peripheral.drive

import ucesoft.c64.peripheral.bus.IECBus
import com.dropbox.core.DbxClient
import ucesoft.c64.Log
import ucesoft.c64.peripheral.bus.BusDataIterator
import ucesoft.c64.formats.D64
import java.io.IOException
import java.io.ByteArrayOutputStream
import java.io.File
import language.postfixOps
import java.io.InputStream
import com.dropbox.core.DbxWriteMode

class DropboxDrive(bus: IECBus,dbxClient : DbxClient,device: Int = 9) extends AbstractDrive(bus,device) {
  val componentID = "Dropbox Drive"
  override val busid = "Dropbox_" + device
  final private[this] val STATUS_OK = 0 
  final private[this] val STATUS_WELCOME = 1
  final private[this] val STATUS_IO_ERROR = 2
  final private[this] val STATUS_CHANNEL_ERROR = 3
  final private[this] val STATUS_FILE_NOT_FOUND = 4
  final private[this] val STATUS_SYNTAX_ERROR = 5
  
  protected val ERROR_CODES = Map (STATUS_OK -> "OK",
                                   STATUS_WELCOME -> "KERNAL64 DROPBOX DOS",
                                   STATUS_IO_ERROR -> "IO ERROR",
                                   STATUS_CHANNEL_ERROR -> "CHANNEL NOT PERMITTED",
                                   STATUS_FILE_NOT_FOUND -> "FILE NOT FOUND",
                                   STATUS_SYNTAX_ERROR -> "SYNTAX ERROR")
  private[this] var loading = true
  private[this] var currentPath = "/"
  
  status = STATUS_WELCOME
  
  protected def getDirectoryEntries(path:String) : List[DirEntry] = {
    try {
      val meta = dbxClient.getMetadataWithChildren(currentPath)
      import collection.JavaConversions._
      meta.children map { c => DirEntry(c.name,if (c.isFile) c.asFile.numBytes.toInt else 0,c.isFolder) } toList
    }
    catch {
      case t:Throwable =>
        Log.info("Dropbox error while getting directory's files: " + t)
        setStatus(STATUS_IO_ERROR)
        Nil
    }
  }
  
  override protected def loadData(fileName: String) : Option[BusDataIterator] = {
    if (fileName == "$") return Some(loadDirectory(currentPath,new File(currentPath).getName))

    val found = getDirectoryEntries(currentPath) find { f => D64.fileNameMatch(fileName,f.name.toUpperCase) }
    found match {
      case Some(file) =>
        val buffer = new ByteArrayOutputStream 
        try {
          dbxClient.getFile(currentPath + "/" + file.name,null,buffer)
          setStatus(STATUS_OK)
          import BusDataIterator._
          Some(new ArrayDataIterator(buffer.toByteArray))
        }
        catch {
          case io:IOException =>
            setStatus(STATUS_IO_ERROR)
            None
        }
      case None => None
    }
  }
  
  override def open {
    super.open
    channel match {
      case 0 => // LOADING FILE
        loading = true
        channels(channel).readDirection = true
      case 1 => // SAVING FILE
        loading = false
        channels(channel).readDirection = false
      case 15 =>
      case _ =>
        setStatus(STATUS_CHANNEL_ERROR)
    }
  }
  
  override def open_channel {
    super.open_channel
    channels(channel).open
    channel match {
      case 15 => handleChannel15
      case 0 => load(channels(channel).fileName.toString)
      case 1 => // saving ...
      case _ =>
        setStatus(STATUS_CHANNEL_ERROR)
    }
  }
  
  override def close {
    super.close
    if (!loading) {
      try {
        val buffer = channels(channel).buffer.toArray
        val in = new InputStream {          
          var index = 0
          def read = if (index < buffer.length) {
            val v = buffer(index)
            index += 1
            v
          } else -1
        }
        dbxClient.uploadFile(currentPath + "/" + channels(channel).fileName.toString,DbxWriteMode.add,buffer.length,in)
      }
      catch {
        case io:IOException =>
          io.printStackTrace
          setStatus(STATUS_IO_ERROR)
      }
    }
    setStatus(STATUS_OK)
  }  
  
  override def dataNotFound {
    super.dataNotFound
    setStatus(STATUS_FILE_NOT_FOUND)
  }
  
  protected def handleChannel15 {
    val cmd = if (channels(channel).fileName.length > 0) channels(channel).fileName.toString else channels(channel).bufferToString
    if (cmd.length > 0) {
      executeCommand(cmd)
      channels(channel).buffer.clear      
    }
    sendStatus
  }
  
  private def executeCommand(cmd: String) {
    val command = if (cmd.charAt(cmd.length - 1) == 13) cmd.substring(0, cmd.length - 1) else cmd
    // TODO ...
    setStatus(STATUS_SYNTAX_ERROR)
  }
}