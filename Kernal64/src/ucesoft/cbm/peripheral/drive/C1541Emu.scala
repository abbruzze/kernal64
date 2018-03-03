package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.peripheral.bus._
import java.io.FileNotFoundException
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.JOptionPane
import ucesoft.cbm.formats.Diskette

object C1541Emu {
  private val BLINK_TIMEOUT = 100000
  private val DEBUG = false

  private object Job extends Enumeration {
    val LOAD_FILE = Value
    val SAVE_FILE = Value
    val LOAD_CHANNEL = Value
    val SAVE_CHANNEL = Value
    val EMPTY = Value
  }

  private val STATUS_OK = 0
  private val STATUS_SYNTAX_ERROR = 30
  private val STATUS_FILE_NOT_FOUND = 62
  private val STATUS_NO_CHANNEL = 70
  private val STATUS_WELCOME = 73
  private val STATUS_DRIVE_NOT_READY = 74
  private val ERROR_CODES = Map(
    STATUS_OK -> "OK",
    STATUS_FILE_NOT_FOUND -> "FILE NOT FOUND",
    STATUS_WELCOME -> "CBM DOS V2.6 1541",
    STATUS_SYNTAX_ERROR -> "SYNTAX ERROR",
    STATUS_NO_CHANNEL -> "NO CHANNEL",
    STATUS_DRIVE_NOT_READY -> "DRIVE NOT READY")
}

class C1541Emu(bus: IECBus, ledListener: DriveLedListener, device: Int = 8) extends IECBusDevice(bus, device) with Drive {
  val driveType = DriveType.OTHER
  val componentID = "C1541 Emu"
  val formatExtList = List("D64")
  override val busid = "C1541Emu_" + device

  // register itself to bus
  bus.registerListener(this)

  import C1541Emu._
  import Job._

  private[this] var driveReader: Option[Diskette] = None
  private[this] var job = EMPTY

  private[this] var status = STATUS_WELCOME
  private[this] var errorTimeout = 0L

  override protected def isDeviceReady = driveReader.isDefined

  override protected def loadData(fileName: String): Option[BusDataIterator] = {
    try {
      if (DEBUG) println("Loading " + fileName)
      ledListener.beginLoadingOf(fileName)
      Some(driveReader.get.load(fileName.toString).iterator)
    } catch {
      case e: FileNotFoundException => None
    }
  }

  override def getProperties = {
    properties.setProperty("Status", status.toString)
    properties
  }

  def init {}
  
  def getFloppy = driveReader.getOrElse(EmptyFloppy)

  def setDriveReader(driveReader: Floppy,emulateInserting:Boolean) {
    this.driveReader = Some(driveReader.asInstanceOf[Diskette])
  }

  private def blinkLed(cycles: Long) {
    if (errorTimeout == 0) {
      ledListener.turnOn
      errorTimeout = cycles + BLINK_TIMEOUT
    } else if (errorTimeout < cycles) {
      if (ledListener.isOn) ledListener.turnOff else ledListener.turnOn
      errorTimeout = cycles + BLINK_TIMEOUT
    }
  }

  override protected def setMode(newMode: IECBusDevice.Mode.Value) {
    super.setMode(newMode)
    if (channels.exists(_.isOpened)) ledListener.turnOn else ledListener.turnOff
  }

  override def clocked(cycles: Long) {
    super.clocked(cycles)
    if (status != STATUS_OK && status != STATUS_WELCOME) blinkLed(cycles)
  }

  def reset {
    job = EMPTY
  }

  override def unlisten {
    super.unlisten
    if (channel == 15) handleChannel15
    resetSignals
  }

  override def open {
    super.open
    channel match {
      case 0 => // LOADING FILE
        job = LOAD_FILE
        channels(channel).readDirection = true
      case 1 => // SAVING FILE
        job = SAVE_FILE
        channels(channel).readDirection = false
      case 15 => // SPECIAL CASE
        if (DEBUG) println("Channel 15!!!")
      case _ =>
        if (DEBUG) println("Using logical channel " + channel)
        // TODO read/write direction
        channels(channel).readDirection = true
        // TODO LOAD or SAVE CHANNEL
        job = LOAD_CHANNEL
    }
  }

  override def open_channel {
    super.open_channel
    if (!channels(channel).isOpened) {
      channels(channel).open
      channel match {
        case 15 => handleChannel15
        case _ => load(channels(channel).fileName.toString)
      }
    }
  }

  override protected def untalk {
    resetSignals
  }

  override def close {
    super.close
    job match {
      case SAVE_FILE =>
        if (DEBUG) println("Saving file " + channels(channel).fileName + " " + channels(channel).buffer.length)
        ledListener.endSaving
      case _ =>
        ledListener.endLoading
      // TODO
    }
    setStatus(STATUS_OK)
  }

  override def fileNameReceived {
    super.fileNameReceived
    if (channels(channel).fileName.toString.startsWith("#")) channels(channel).open
    if (job == SAVE_FILE) ledListener.beginSavingOf(channels(channel).fileName.toString)
    if (channel == 15 && channels(channel).fileName.length > 0) handleChannel15
  }

  override def dataNotFound {
    super.dataNotFound
    setStatus(STATUS_FILE_NOT_FOUND)
    ledListener.endLoading
  }

  override def byteJustWritten(isLast: Boolean) {
    super.byteJustWritten(isLast)
    ledListener.updateLoading(channels(channel).dataToSend.get.getPerc)
  }

  override protected def load(fileName: String) {
    if (fileName != "#") {
      super.load(fileName)
      ledListener.beginLoadingOf(fileName)
    } else if (DEBUG) println("Loading from #")
  }

  private def handleChannel15 {
    val cmd = if (channels(channel).fileName.length > 0) channels(channel).fileName.toString else channels(channel).bufferToString
    if (cmd.length > 0) {
      executeCommand(cmd)
      channels(channel).buffer.clear      
    }
    sendStatus
  }

  private def sendStatus {
    import BusDataIterator._
    channels(channel).dataToSend = Some(new StringDataIterator("%02d,%s,00,00".format(status, ERROR_CODES(status)) + 13.toChar))
  }

  private def setStatus(code: Int) {
    status = code
  }

  private def executeCommand(cmd: String) {
    val command = if (cmd.charAt(cmd.length - 1) == 13) cmd.substring(0, cmd.length - 1) else cmd
    println("Executing command " + command)
    try {
      command.charAt(0) match {
        case 'I' => initialize
        case 'U' =>
          if (command.startsWith("U1:")) memoryRead(command.substring(3))
          else if (command.startsWith("U1")) memoryRead(command.substring(2))
          else setStatus(STATUS_SYNTAX_ERROR)
        case 'B' =>
          if (command.startsWith("B-P:")) bufferPointer(command.substring(4))
          else if (command.startsWith("B-P")) bufferPointer(command.substring(3))
          else setStatus(STATUS_SYNTAX_ERROR)
        case 'M' =>
          if (command.startsWith("M-R:")) m_r(command.substring(4))
          else if (command.startsWith("M-R")) m_r(command.substring(3))
          else if (command.startsWith("M-W:")) m_w(command.substring(4))
          else if (command.startsWith("M-W")) m_w(command.substring(3))
          else if (command.startsWith("M-E:")) m_e(command.substring(4))
          else if (command.startsWith("M-E")) m_e(command.substring(3))
        case _ =>
          setStatus(STATUS_SYNTAX_ERROR)
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  private def m_e(cmd: String) {
    val lo = cmd.charAt(0).toInt
    val hi = cmd.charAt(1).toInt
    val address = hi * 256 + lo
    println("M-E from " + Integer.toHexString(address))
  }

  private def m_w(cmd: String) {
    val lo = cmd.charAt(0).toInt
    val hi = cmd.charAt(1).toInt
    val len = cmd.charAt(2).toInt
    val address = hi * 256 + lo
    println("M-W to " + Integer.toHexString(address) + " len=" + len)
  }

  private def m_r(cmd: String) {
    if (cmd.length == 2 || cmd.length == 3) {
      val lo = cmd.charAt(0).toInt
      val hi = cmd.charAt(1).toInt
      val len = if (cmd.length == 3) cmd.charAt(2).toInt else 1
      val address = hi * 256 + lo
      println("M-R from " + Integer.toHexString(address) + " len=" + len)
    } else setStatus(STATUS_SYNTAX_ERROR)
  }

  private def initialize {
    setStatus(STATUS_OK)
    for (c <- channels) {
      c.close
      c.dataToSend = None
    }
    if (DEBUG) println("Initialized!!")
  }

  private def bufferPointer(cmd: String) {
    val st = new java.util.StringTokenizer(cmd.trim, " ,")
    if (st.countTokens == 2) {
      try {
        val channel = st.nextToken.toInt
        val pos = st.nextToken.toInt
        if (DEBUG) println(s"Buffer pointer on channel=${channel} pos=${pos}")
        if (channel > 15 || !channels(channel).isOpened) setStatus(STATUS_NO_CHANNEL)
        else channels(channel).dataToSend.get.goto(pos)
      } catch {
        case _: NumberFormatException => setStatus(STATUS_SYNTAX_ERROR)
      }
    } else setStatus(STATUS_SYNTAX_ERROR)
  }

  private def memoryRead(cmd: String) {
    val st = new java.util.StringTokenizer(cmd.trim, " ,")
    if (st.countTokens == 4) {
      try {
        val channel = st.nextToken.toInt
        val drive = st.nextToken.toInt
        val track = st.nextToken.toInt
        val sector = st.nextToken.toInt
        if (DEBUG) println(s"Memory read on channel=${channel} drive=${drive} track=${track} sector=${sector}")
        if (channel > 15 || !channels(channel).isOpened) setStatus(STATUS_NO_CHANNEL)
        else if (drive != 0) setStatus(STATUS_DRIVE_NOT_READY)
        else {
          // read block
          import BusDataIterator._
          channels(channel).dataToSend = Some(new ArrayDataIterator(driveReader.get.readBlock(track, sector), Some(256)))
          println(driveReader.get.readBlock(track, sector) mkString ",")
        }
      } catch {
        case _: NumberFormatException => setStatus(STATUS_SYNTAX_ERROR)
      }
    } else setStatus(STATUS_SYNTAX_ERROR)
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) {}
  protected def loadState(in:ObjectInputStream) {}
  protected def allowsStateRestoring(parent:JFrame) : Boolean = {
    driveReader match {
      case Some(d64) =>
        JOptionPane.showMessageDialog(parent,s"Warning: 1541 emulation is active with $d64. The disk will not be saved/loaded.","State warning",JOptionPane.WARNING_MESSAGE)
      case None =>
    }
    true
  }
}