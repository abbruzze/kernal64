package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.formats.Diskette
import ucesoft.cbm.formats.Diskette.FileType
import ucesoft.cbm.peripheral.bus.{IEEE488Bus, IEEE488BusCommand}

import java.io.{ObjectInputStream, ObjectOutputStream}

class IEEE488Drive(override val name:String,override val deviceID:Int,bus: IEEE488Bus) extends IEEE488BusCommand(name,deviceID, bus) {
  override val componentType: CBMComponentType.Type = CBMComponentType.DISK

  protected case class Status(var st:Int = 0,t:Int = 0,s:Int = 0)

  protected var status: Status = Status()

  protected final val STATUS_OK = 0
  protected final val STATUS_SYNTAX_ERROR = 30
  protected final val STATUS_FILENOTFOUND = 62
  protected final val STATUS_FILETYPEMISMATCH = 64
  protected final val STATUS_POWERUP = 73

  protected val STATUS_CODES: Map[Int, String] = Map(
    STATUS_OK -> "OK",
    STATUS_SYNTAX_ERROR -> "SYNTAX ERROR",
    STATUS_FILENOTFOUND -> "FILE NOT FOUND",
    STATUS_FILETYPEMISMATCH -> "FILE TYPE MISMATCH",
    STATUS_POWERUP -> "CBM DOS V2.7"
  )

  // INIT
  setStatus(STATUS_POWERUP)

  private val d80 = new ucesoft.cbm.formats.D80("""C:\Users\ealeame\OneDrive - Ericsson AB\CBM-II\software\test-pass-fail.d80""")

  protected def formatST(): String = {
    val text = STATUS_CODES.getOrElse(status.st,"CODE NOT FOUND")
    s"${(if (status.st < 10) "0" else "")}${status.st}, $text,${(if (status.t < 10) "0" else "")}${status.t},${(if (status.s < 10) "0" else "")}${status.s},0${13.toChar}"
  }

  override protected def openChannel(): Unit = {
    if (secondaryAddress == 15 && channels(15).name().isEmpty) {
      channels(15).setData(formatST().toArray.map(_.toInt & 0xFF))
    }
  }

  override protected def openNamedChannel(): Boolean = {
    if (secondaryAddress == 15 && channels(15).name().isEmpty) {
      channels(15).setData(formatST().toArray.map(_.toInt & 0xFF))
      return true
    }

    val name = channels(secondaryAddress).name()
    println(s"Loading $name")

    Diskette.parseFileName(name.toUpperCase()) match {
      case Some(fn) =>
        d80.load(fn) match {
          case Some(data) =>
            if ((secondaryAddress == 0 || secondaryAddress == 1) && data.fileType != FileType.PRG) {
              setStatus(STATUS_FILETYPEMISMATCH)
              clk.cancel("IEEE488Talk")
              false
            }
            else {
              val startData = if (data.fileType == FileType.PRG) {
                val buffer = Array.ofDim[Int](data.data.length + 2)
                buffer(0) = data.startAddress & 0xFF
                buffer(1) = data.startAddress >> 8
                System.arraycopy(data.data, 0, buffer, 2, data.data.length)
                buffer
              } else data.data
              channels(secondaryAddress).setData(startData)
              status.st = STATUS_OK
              true
            }
          case None =>
            setStatus(STATUS_FILENOTFOUND)
            clk.cancel("IEEE488Talk")
            false
        }
      case None =>
        setStatus(STATUS_SYNTAX_ERROR)
        clk.cancel("IEEE488Talk")
        false
    }
  }

  protected def setStatus(st:Int): Unit = {
    status.st = st
    channels(15).rewind()
  }

  override def executeCommand(): Unit = {
    println(s"Executing command '${channels(15).name()}'")
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???
  override protected def loadState(in: ObjectInputStream): Unit = ???
  override protected def allowsStateRestoring: Boolean = ???
}
