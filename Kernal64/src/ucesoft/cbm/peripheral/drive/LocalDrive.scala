package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Diskette
import ucesoft.cbm.peripheral.bus.{BusDataIterator, IECBus}
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceListener.{BreakType, CpuStepInfo, StepType}

import java.io._
import scala.language.postfixOps

class LocalDrive(bus: IECBus, device: Int = 9) extends AbstractDrive(bus, device) with TraceListener {
  val driveType: DriveType.Value = DriveType.LOCAL
  val componentID = "Local Drive"
  val formatExtList: List[String] = Nil
  override val busid: String = "LocalDrive_" + device
  
  bus.registerListener(this)
  
  final private[this] val STATUS_OK = 0
  final private[this] val STATUS_SYNTAX_ERROR = 30
  final private[this] val STATUS_FILE_NOT_FOUND = 62
  final private[this] val STATUS_WELCOME = 73
  final private[this] val STATUS_IO_ERROR = 74
  final private[this] val STATUS_CHANNEL_ERROR = 1
  protected val ERROR_CODES: Map[Int, String] = Map(
    STATUS_OK -> "OK",
    STATUS_FILE_NOT_FOUND -> "FILE NOT FOUND",
    STATUS_WELCOME -> "KERNAL64 LOCAL DRIVE DOS",
    STATUS_SYNTAX_ERROR -> "SYNTAX ERROR",
    STATUS_IO_ERROR -> "I/O ERROR",
    STATUS_CHANNEL_ERROR -> "CHANNEL NOT PERMITTED")
  
  private[this] var currentDir = new File(util.Properties.userDir)
  private[this] var loading = true
  
  status = STATUS_WELCOME
  
  def reset : Unit = {}
  
  def getCurrentDir: File = currentDir
  def setCurrentDir(dir:File): Unit = currentDir = dir
  
  protected def getDirectoryEntries(path:String) : List[DirEntry] = {
    new File(path).listFiles map { f => DirEntry(f.getName,f.length.toInt,f.isDirectory) } toList
  }
    
  override protected def loadData(fileName: String) : Option[BusDataIterator] = {
    //println(s"Loading $fileName ...")
    if (fileName.startsWith("$")) return Some(loadDirectory(currentDir.toString,currentDir.getName))
    val dp = fileName.indexOf(":")
    val fn = (if (dp != -1) fileName.substring(dp + 1) else fileName) map { c => if (c < 128) c else (c - 128).toChar }
    val found = currentDir.listFiles find { f => Diskette.fileNameMatch(fn,f.getName.toUpperCase) }
    found match {
      case Some(file) =>
        val buffer = Array.ofDim[Byte](file.length.toInt)
        try {
          val in = new DataInputStream(new FileInputStream(file))
          in.readFully(buffer)
          in.close()
          setStatus(STATUS_OK)
          import BusDataIterator._
          Some(new ArrayDataIterator(buffer))
        }
        catch {
          case _:IOException =>
            setStatus(STATUS_IO_ERROR)
            None
        }
      case None => None
    }
  }
  
  override def open : Unit = {
    super.open
    channel match {
      case 0 => // LOADING FILE
        loading = true
        channels(channel).readDirection = true
      case 1 => // SAVING FILE
        loading = false
        channels(channel).open
        channels(channel).readDirection = false
      case 15 =>
      case _ =>
        setStatus(STATUS_CHANNEL_ERROR)
    }
  }

  override def byteJustRead(byte: Int, isLast: Boolean): Unit = {
    super.byteJustRead(byte, isLast)
  }
  
  override def open_channel : Unit = {
    super.open_channel
    if (!channels(channel).isOpened) {
      channels(channel).open
      channel match {
        case 15 => handleChannel15
        case 0 => load(channels(channel).fileName.toString)
        case 1 => // saving ...
        case _ =>
          setStatus(STATUS_CHANNEL_ERROR)
      }
    }
  }
  
  override def close : Unit = {
    super.close
    if (!loading) {
      var out : FileOutputStream = null
      try {
        out = new FileOutputStream(new File(currentDir,channels(channel).fileName.toString))
        for(b <- channels(channel).buffer.toArray) out.write(b)
      }
      catch {
        case io:IOException =>
          io.printStackTrace()
          setStatus(STATUS_IO_ERROR)
      }
      finally {
        if (out != null) out.close()
      }
    }
    setStatus(STATUS_OK)
  }  
  
  override def dataNotFound : Unit = {
    super.dataNotFound
    setStatus(STATUS_FILE_NOT_FOUND)
  }
  
  protected def handleChannel15 : Unit = {
    val cmd = if (channels(channel).fileName.length > 0) channels(channel).fileName.toString else channels(channel).bufferToString
    if (cmd.length > 0) {
      executeCommand(cmd)
      channels(channel).buffer.clear      
    }
    sendStatus
  }
  
  private def executeCommand(cmd: String) : Unit = {
    //println(s"Executing CMD $cmd")
    val command = if (cmd.charAt(cmd.length - 1) == 13) cmd.substring(0, cmd.length - 1) else cmd
    if (command.startsWith("CD")) {
      val cd = command.substring(2).trim
      //println(s"Change dir to $cd")
      val newDir = new File(currentDir,cd)
      if (newDir.isDirectory) currentDir = newDir
      else setStatus(STATUS_IO_ERROR)
    }
    else
    if (command.startsWith("I")) { setStatus(STATUS_OK)/* do nothing */ }
    else setStatus(STATUS_SYNTAX_ERROR)
  }

  // fake trace listener implementation
  override def getRegisters(): List[TraceListener.TraceRegister] = Nil
  override def setTraceOnFile(out: PrintWriter, enabled: Boolean): Unit = {}
  override def setTrace(traceOn: Boolean): Unit = {}
  override def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType): Unit = {}
  override def setBreakAt(breakType: BreakType, callback: CpuStepInfo => Unit): Unit = {}
  override def jmpTo(pc: Int): Unit = {}
  override def disassemble(address: Int): TraceListener.DisassembleInfo = throw new UnsupportedOperationException("Disassembling is not supported on local drive")
  override def setCycleMode(cycleMode: Boolean): Unit = {}
}