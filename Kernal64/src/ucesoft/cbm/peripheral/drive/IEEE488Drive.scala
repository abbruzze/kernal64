package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.formats.Diskette.{FileMode, FileType, StandardFileName}
import ucesoft.cbm.formats.{D80, Diskette}
import ucesoft.cbm.peripheral.bus.{IEEE488Bus, IEEE488BusCommand}
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceListener.{BreakType, CpuStepInfo, StepType}
import ucesoft.cbm.{CBMComponentType, ClockEvent}

import java.io.{IOException, ObjectInputStream, ObjectOutputStream, PrintWriter}
import scala.collection.mutable.ListBuffer

class IEEE488Drive(override val name:String,
                   override val deviceID:Int,
                   bus: IEEE488Bus,
                   driveLedListener: DriveLedListener) extends IEEE488BusCommand(name,deviceID, bus) with Drive with TraceListener {
  override val supportTracing = false
  override val componentType: CBMComponentType.Type = CBMComponentType.DISK
  override val driveType: DriveType.Value = DriveType._8050
  override val formatExtList: List[String] = List("D80")

  protected case class Status(var st:Int = 0,var t:Int = 0,var s:Int = 0,var filesScratched:Int = 0)

  protected var status: Status = Status()

  protected final val STATUS_OK = 0
  protected final val STATUS_SYNTAX_ERROR = 30
  protected final val STATUS_FILENOTFOUND = 62
  protected final val STATUS_FILETYPEMISMATCH = 64
  protected final val STATUS_POWERUP = 73
  protected final val STATUS_DISK_FULL = 72
  protected final val STATUS_NO_CHANNEL = 70
  protected final val STATUS_ILLEGAL_TRACK_AND_SECTOR = 66
  protected final val STATUS_FILE_EXISTS = 63
  protected final val STATUS_FILES_SCRATCHED = 1
  protected final val STATUS_DRIVE_NOT_READY = 74

  override protected val buffersAddress : Array[Int] = Array(
    0x1100,0x1200,0x1300,         // #0,#1,#2
    0x2000,0x2100,0x2200,0x2300,  // #3,#4,#5,#6
    0x3000,0x3100,0x3200,0x3300,  // #7,#8,#9,#a
    0x4000,0x4100,0x4200,0x4300   // #b,#c,#d,#e
  )

  protected val STATUS_CODES: Map[Int, String] = Map(
    STATUS_OK -> "OK",
    STATUS_SYNTAX_ERROR -> "SYNTAX ERROR",
    STATUS_FILENOTFOUND -> "FILE NOT FOUND",
    STATUS_FILETYPEMISMATCH -> "FILE TYPE MISMATCH",
    STATUS_POWERUP -> "CBM DOS V2.7",
    STATUS_DISK_FULL -> "DISK FULL",
    STATUS_NO_CHANNEL -> "NO CHANNEL",
    STATUS_ILLEGAL_TRACK_AND_SECTOR -> "ILLEGAL TRACK AND SECTOR",
    STATUS_FILE_EXISTS -> "FILE EXISTS",
    STATUS_FILES_SCRATCHED -> "FILES SCRATCHED",
    STATUS_DRIVE_NOT_READY -> "DRIVE NOT READY"
  )

  protected val letLedBlinkOnError = false
  protected var ledBlinking = false
  protected var ledBlinkingOn = false

  protected val RENAME_CMD_RE = """R(ENAME)?\d?:([^=]+=.+)""".r
  protected val SCRATCH_CMD_RE = """S(CRATCH)?(.+)""".r
  protected val NEW_CMD_RE = """N(EW)?\d?:([^,]+)(,..)?""".r

  // INIT
  setStatus(STATUS_POWERUP)

  override def disconnect(): Unit = {
    bus.unregisterListener(listener)
  }

  override def reset: Unit = {
    super.reset
    driveLedListener.turnOff()
    ledBlinking = false
  }

  // Drive stuff
  override def setDriveReader(driveReader: Floppy, emulateInserting: Boolean): Unit = {
    if (driveReader != EmptyFloppy) {
      d80 = driveReader.asInstanceOf[D80]
      d80.setTrackSectorListener((t, s) => {
        var lastTrack = 0
        driveLedListener.moveTo(t, Some(s), false)
        if (lastTrack != t) {
          lastTrack = t
          //Clock.systemClock.waitFor(10)
        }
      })
      emptyFloppy = false
    }
    else emptyFloppy = true
  }

  override def clock(cycles: Long): Unit = {}

  override def getFloppy: Floppy = if (emptyFloppy) EmptyFloppy else d80

  private var d80 : D80 = null
  private var emptyFloppy = true

  override def commandReceived(cmd:Command): Unit = {
    val channelOpened = channels.exists(_.isOpened())
    if (channelOpened) driveLedListener.turnOn() else driveLedListener.turnOff()

    super.commandReceived(cmd)
  }

  protected def formatST(): String = {
    val text = STATUS_CODES.getOrElse(status.st,"CODE NOT FOUND")
    val sb = new StringBuilder
    // status code
    if (status.st < 10) sb.append("0")
    sb.append(s"${status.st},")
    // status message
    sb.append(s"$text,")
    // optional scratch info
    if (status.st == STATUS_FILES_SCRATCHED) {
      if (status.filesScratched < 10) sb.append("0")
      sb.append(s"${status.filesScratched},")
    }
    // track
    if (status.t < 10) sb.append("0")
    sb.append(s"${status.t},")
    // sector
    if (status.s < 10) sb.append("0")
    sb.append(s"${status.s},")
    // end
    sb.append(s"0${13.toChar}")
    //s"${if (status.st < 10) "0" else ""}${status.st}, $text,${if (status.t < 10) "0" else ""}${status.t},${if (status.s < 10) "0" else ""}${status.s},0${13.toChar}"
    sb.toString
  }

  override protected def openChannel(): Unit = {
    //if (secondaryAddress == 15 && channels(15).name().isEmpty && !channels(15).hasMoreOutData()) {
    if (secondaryAddress == 15 && !channels(15).isCommandOutputReady()) {
      //println(s"Setting status to ${status.st}")
      channels(15).setOutData(formatST().toArray.map(_.toInt & 0xFF))
    }
  }

  override protected def openNamedChannel(): Boolean = {
    if (secondaryAddress == 15) {
      return true
    }

    if (emptyFloppy) {
      setStatus(STATUS_DRIVE_NOT_READY)
      clk.cancel("IEEE488Talk")
      return false
    }

    val name = channels(secondaryAddress).name()

    // Buffers management
    if (name.startsWith("#")) {
      if (name.length == 1) {
        findFreeBuffer() match {
          case Some(b) =>
            channels(secondaryAddress).setBuffer(b)
            return true
          case None =>
            setStatus(STATUS_NO_CHANNEL)
            clk.cancel("IEEE488Talk")
            return false
        }
      }
      else {
        try {
          val index = name.substring(1).toInt
          if (index > buffers.length || buffers(index).isUsed) {
            setStatus(STATUS_NO_CHANNEL)
            clk.cancel("IEEE488Talk")
            return false
          }
          else {
            buffers(index).setUsed(true)
            channels(secondaryAddress).setBuffer(buffers(index))
            return true
          }
        }
        catch {
          case _:NumberFormatException =>
            setStatus(STATUS_SYNTAX_ERROR)
            clk.cancel("IEEE488Talk")
            return false
        }
      }
    }

    // special secondary address
    if (secondaryAddress == 1) {
      Diskette.parseFileName(name.toUpperCase()) match {
        case Some(fn@StandardFileName(name, ftype, _, _)) =>
          if (ftype == FileType.PRG) {
            println(s"Saving $name")
            return saveChannel(fn)
          }
          else {
            setStatus(STATUS_FILETYPEMISMATCH)
            clk.cancel("IEEE488Talk")
            return false
          }
        case _ =>
          setStatus(STATUS_FILETYPEMISMATCH)
          clk.cancel("IEEE488Talk")
          return false
      }
    }
    else if (secondaryAddress == 0) {
      println(s"Loading $name")
      Diskette.parseFileName(name.toUpperCase()) match {
        case Some(fn) =>
          println(s"Loading $name")
          return loadChannel(fn)
        case None =>
          setStatus(STATUS_SYNTAX_ERROR)
          clk.cancel("IEEE488Talk")
          return false
      }
    }
    // others
    Diskette.parseFileName(name.toUpperCase()) match {
      case Some(fn@StandardFileName(name, _,FileMode.WRITE,_)) =>
        println(s"Saving $name")
        saveChannel(fn)
      case Some(fn) =>
        println(s"Loading $name")
        loadChannel(fn)
      case None =>
        setStatus(STATUS_SYNTAX_ERROR)
        clk.cancel("IEEE488Talk")
        false
    }
  }

  override protected def closeChannel(channel: Int): Unit = {
    if (channels(secondaryAddress).isWriteMode()) {
      Diskette.parseFileName(channels(secondaryAddress).name()) match {
        case Some(StandardFileName(name, ftype, _, overwrite)) =>
          println(s"Saving $channel name = $name type = $ftype overwrite = $overwrite data size = ${channels(secondaryAddress).getInData().length}")
          ftype.getOrElse(FileType.PRG) match {
            case FileType.PRG =>
              val data = channels(secondaryAddress).getInData()
              val startAddress = data(0) << 8 | data(1)
              if (!d80.addPRG(data.drop(2).map(_.toByte),name,startAddress)) setStatus(STATUS_DISK_FULL)
              else setStatus(STATUS_OK)
            case FileType.SEQ =>
              if (!d80.addSEQ(channels(secondaryAddress).getInData().map(_.toByte),name)) setStatus(STATUS_DISK_FULL)
              else setStatus(STATUS_OK)
            case ft =>
              println(s"File type $ft not supported")
          }

      }
    }
  }

  protected def saveChannel(fn:Diskette.StandardFileName): Boolean = {
    channels(secondaryAddress).setWriteMode(true)
    true
  }

  protected def loadChannel(fn:Diskette.FileName): Boolean = {
    if (emptyFloppy) {
      setStatus(STATUS_DRIVE_NOT_READY)
      clk.cancel("IEEE488Talk")
      return false
    }

    d80.load(fn,secondaryAddress) match {
      case Some(data) if data.fileType != FileType.DEL =>
        if ((secondaryAddress == 0 || secondaryAddress == 1) && data.fileType != FileType.PRG) {
          setStatus(STATUS_FILETYPEMISMATCH)
          clk.cancel("IEEE488Talk")
          false
        }
        else {
          val startData = if (secondaryAddress == 0 && data.fileType == FileType.PRG) {//if (data.fileType == FileType.PRG) {
            val buffer = Array.ofDim[Int](data.data.length + 2)
            buffer(0) = data.startAddress & 0xFF
            buffer(1) = data.startAddress >> 8
            System.arraycopy(data.data, 0, buffer, 2, data.data.length)
            buffer
          } else data.data

          channels(secondaryAddress).setWriteMode(false)
          channels(secondaryAddress).setOutData(startData)
          setStatus(STATUS_OK)
          true
        }
      case _ =>
        setStatus(STATUS_FILENOTFOUND)
        clk.cancel("IEEE488Talk")
        false
    }
  }

  protected def setStatus(st:Int,t:Int = 0,s:Int = 0,sf:Int = 0): Unit = {
    status.st = st
    status.t = t
    status.s = s
    status.filesScratched = sf
    channels(15).rewind()

    if (letLedBlinkOnError) {
      if (st != STATUS_OK && st != STATUS_POWERUP) {
        clk.cancel(s"IEEE488LedBlink$deviceID")
        ledBlinking = true
        clk.schedule(new ClockEvent(s"IEEE488LedBlink$deviceID", clk.nextCycles, ledBlink _))
      }
      else {
        clk.cancel(s"IEEE488LedBlink$deviceID")
        ledBlinking = false
      }
    }
  }

  protected def ledBlink(cycles:Long): Unit = {
    if (ledBlinkingOn) driveLedListener.turnOn() else driveLedListener.turnOff()
    ledBlinkingOn ^= true
    if (ledBlinking) clk.schedule(new ClockEvent(s"IEEE488LedBlink$deviceID",cycles + clk.getClockHz.toLong / 10,ledBlink _))
  }

  override def executeCommand(): Unit = {
    if (channels(15).name().nonEmpty || channels(15).getInData().length > 0) { // some command to execute
      var cmd = if (channels(15).name().nonEmpty) channels(15).name() else String.valueOf(channels(15).getInData().map(_.toChar))
      cmd = cmd.filterNot(_ == 13)
      println(s"Executing command '$cmd' ${cmd.toCharArray.map(_.toInt).mkString(",")}")
      if (RENAME_CMD_RE.matches(cmd)) renameFile(cmd)
      else if (SCRATCH_CMD_RE.matches(cmd)) scratchFile(cmd)
      else if (NEW_CMD_RE.matches(cmd)) formatDisk(cmd)
      else if (cmd.startsWith("I")) executeInitialize()
      else if (cmd.startsWith("B-P")) executeBP(cmd.substring(3))
      else if (cmd.startsWith("U1")) executeBlockRead(cmd.substring(2),true)
      else if (cmd.startsWith("B-R")) executeBlockRead(cmd.substring(3),true)
      else if (cmd.startsWith("M-R")) executeMemoryRead(cmd.substring(3))
      else if (cmd.startsWith("U2")) executeBlockWrite(cmd.substring(2))
      else if (cmd.startsWith("M-W")) executeMemoryWrite(cmd.substring(3))
      else if (cmd.startsWith("B-A")) executeBlockAllocate(cmd.substring(3))


      channels(15).resetInput()
    }
  }

  protected def parseCommand(cmd:String): Array[String] = {
    val pars = new ListBuffer[String]
    var i = 0
    val sb = new StringBuilder()
    var lastWasPar = false
    while (i < cmd.length) {
      val c = cmd.charAt(i)
      if (c != ':' && c != ' ' && c != ',' && c.toInt != 0x1d) {
        sb.append(c)
        lastWasPar = true
      }
      else {
        if (lastWasPar) {
          lastWasPar = false
          pars += sb.toString()
          sb.clear()
        }
      }
      i += 1
    }
    if (lastWasPar) pars += sb.toString()
    pars.toArray
  }

  protected def formatDisk(cmd:String): Unit = {
    cmd match {
      case NEW_CMD_RE(_,name,id) =>
        println(s"Formatting name=$name id=$id")
        if (emptyFloppy) {
          setStatus(STATUS_DRIVE_NOT_READY)
          return
        }
        d80.formatDisk(name,Option(id).map(_.substring(1)).getOrElse(0xA0.toChar.toString + 0xA0.toChar))
        setStatus(STATUS_OK)
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def scratchFile(cmd:String): Unit = {
    cmd match {
      case SCRATCH_CMD_RE(_,files) =>
        if (emptyFloppy) {
          setStatus(STATUS_DRIVE_NOT_READY)
          return
        }
        val dirs = d80.directories
        for(f <- files.split(",")) {
          val fileName = f.substring(f.indexOf(":") + 1)
          println(s"Deleting pattern $fileName")
          var deleted = 0
          dirs.filter(e => Diskette.fileNameMatch(fileName,e.fileName)).foreach { del =>
            println(s"\tDeleting ${del.fileName}")
            try {
              d80.deleteFile(del)
              deleted += 1
            }
            catch {
              case _:IOException =>
                // seems no error is given
            }
          }
          setStatus(STATUS_FILES_SCRATCHED,0,0,deleted)
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def renameFile(cmd:String): Unit = {
    cmd match {
      case RENAME_CMD_RE(_,files) =>
        if (emptyFloppy) {
          setStatus(STATUS_DRIVE_NOT_READY)
          return
        }
        val Array(newFile,oldFile) = files.split("=")
        println(s"Renaming file $oldFile to $newFile")
        try {
          d80.renameFile(oldFile, newFile) match {
            case 0 =>
              setStatus(STATUS_OK)
            case 1 =>
              setStatus(STATUS_FILE_EXISTS)
            case 2 =>
              setStatus(STATUS_FILENOTFOUND)
          }
        }
        catch {
          case _:IOException =>
            setStatus(STATUS_OK) // seems no error is given
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeInitialize(): Unit = {
    for(c <- channels) c.reset()
    for(b <- buffers) b.setUsed(false)
    setStatus(STATUS_OK)
  }

  protected def executeBP(cmd:String): Unit = {
    parseCommand(cmd) match {
      case Array(ch, pos) =>
        try {
          val channel = ch.trim.toInt
          val position = pos.trim.toInt
          if (channel > buffers.length || !channels(channel).hasBuffer())
            setStatus(STATUS_NO_CHANNEL)
          else {
            // EXECUTE COMMAND
            channels(channel).getBuffer().setBP(position)
            setStatus(STATUS_OK)
          }
        }
        catch {
          case _:NumberFormatException =>
            setStatus(STATUS_SYNTAX_ERROR)
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeBlockRead(cmd: String,u1:Boolean): Unit = {
    parseCommand(cmd) match {
      case Array(ch,_,t,s) =>
        if (emptyFloppy) {
          setStatus(STATUS_DRIVE_NOT_READY)
          return
        }
        try {
          val channel = ch.trim.toInt
          val track = t.trim.toInt
          val sector = s.trim.toInt
          if (channel > buffers.length || !channels(channel).hasBuffer())
            setStatus(STATUS_NO_CHANNEL)
          else if (d80 == null || !d80.isValidTrackAndSector(track,sector))
            setStatus(STATUS_ILLEGAL_TRACK_AND_SECTOR,track,sector)
          else {
            // EXECUTE COMMAND
            if (u1) {
              channels(channel).getBuffer().set(d80.readBlock(track, sector).map(_.toInt & 0xFF))
              println(s"U1 => #$channel $track $sector")
            }
            else {
              val data = d80.readBlock(track, sector).map(_.toInt & 0xFF)
              val size = data(0)
              val buffer = Array.ofDim[Int](size)
              System.arraycopy(data,1,buffer,0,size)
              channels(channel).getBuffer().set(buffer)
              println(s"B-R => #$channel $track $sector")
            }

            setStatus(STATUS_OK)
          }
        }
        catch {
          case _:NumberFormatException =>
            setStatus(STATUS_SYNTAX_ERROR)
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeBlockWrite(cmd: String): Unit = {
    parseCommand(cmd) match {
      case Array(ch,_,t,s) =>
        try {
          if (emptyFloppy) {
            setStatus(STATUS_DRIVE_NOT_READY)
            return
          }
          val channel = ch.trim.toInt
          val track = t.trim.toInt
          val sector = s.trim.toInt
          if (channel > buffers.length || !channels(channel).hasBuffer())
            setStatus(STATUS_NO_CHANNEL)
          else if (d80 == null || !d80.isValidTrackAndSector(track,sector))
            setStatus(STATUS_ILLEGAL_TRACK_AND_SECTOR,track,sector)
          else {
            // EXECUTE COMMAND
            println(s"U2 => #$channel $track $sector")
            try {
              d80.writeBlock(track, sector, channels(channel).getBuffer().getBuffer().map(_.toByte))
            }
            catch {
              case _:IOException =>
              // seems no error is given
            }

            setStatus(STATUS_OK)
          }
        }
        catch {
          case _:NumberFormatException =>
            setStatus(STATUS_SYNTAX_ERROR)
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeMemoryRead(cmd: String): Unit = {
    parseCommand(cmd) match {
      case Array(lh) if lh.length == 2 =>
        val low = lh.charAt(0).toInt
        val high = lh.charAt(1).toInt
        val address = low | high << 8
        channels(15).setCommandOutputReady()
        findBufferForAddress(address) match {
          case Some(b) =>
            val offset = address - b.memoryAddress
            channels(15).setOutData(Array(b.get(offset)))
          case None =>
            channels(15).setOutData(Array(0))
        }
        println(s"M-R ${address.toHexString}")
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeMemoryWrite(cmd: String): Unit = {
    parseCommand(cmd) match {
      case Array(lh) if lh.length > 2 =>
        val low = lh.charAt(0).toInt
        val high = lh.charAt(1).toInt
        val address = low | high << 8
        findBufferForAddress(address) match {
          case Some(b) =>
            val offset = address - b.memoryAddress
            val size = lh.charAt(2).toInt
            val data = lh.substring(3).getBytes.map(_.toInt & 0xFF)
            if (size == data.length) b.set(data,offset)
            println(s"M-W ${address.toHexString} size = $size data size = ${data.length}")
          case None =>
            println(s"M-W ${address.toHexString} not found")
        }

        setStatus(STATUS_OK)
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  protected def executeBlockAllocate(cmd: String): Unit = {
    parseCommand(cmd) match {
      case Array(_,t,s) =>
        try {
          val track = t.trim.toInt
          val sector = s.trim.toInt
          if (d80 == null || !d80.isValidTrackAndSector(track,sector))
            setStatus(STATUS_ILLEGAL_TRACK_AND_SECTOR,track,sector)
          else {
            // EXECUTE COMMAND
            println(s"B-A => $track $sector")
            // TODO: missing API to block allocate
            setStatus(STATUS_OK)
          }
        }
        catch {
          case _:NumberFormatException =>
            setStatus(STATUS_SYNTAX_ERROR)
        }
      case _ =>
        setStatus(STATUS_SYNTAX_ERROR)
    }
  }

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeInt(status.st)
  }
  override protected def loadState(in: ObjectInputStream): Unit = {
    status.st = in.readInt()
  }
  // Trace listener
  override def setTraceOnFile(out: PrintWriter, enabled: Boolean): Unit = {}
  override def setTrace(traceOn: Boolean): Unit = {}
  override def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType): Unit = {}
  override def setBreakAt(breakType: BreakType, callback: CpuStepInfo => Unit): Unit = {}
  override def jmpTo(pc: Int): Unit = {}
  override def disassemble(address: Int): TraceListener.DisassembleInfo = throw new UnsupportedOperationException("Disassembling is not supported on IEEE488 drive")
  override def setCycleMode(cycleMode: Boolean): Unit = {}
}
