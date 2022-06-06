package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.formats.Diskette.FileType

import java.io.FileNotFoundException
import scala.collection.mutable.ListBuffer

abstract class IEEE488BusCommand(override val name:String,val deviceID:Int,bus: IEEE488Bus) extends IEEE488BusHandshake(name,bus) {
  import Role._
  import IEEE488Bus.LineType._

  protected sealed trait Command

  protected case class LISTEN(device:Int) extends Command
  protected case object UNLISTEN extends Command
  protected case class TALK(device:Int) extends Command
  protected case object UNTALK extends Command
  protected case class SECONDARY_ADDRESS(address:Int) extends Command
  protected case class CLOSE(address:Int) extends Command
  protected case class OPEN(address:Int) extends Command
  protected case class GLOBAL(cmd:Int) extends Command
  protected object UNKNOWN extends Command

  protected object Command {
    def fromByte(b:Int): Command = {
      if (b == 0x3F) return UNLISTEN
      if (b == 0x5F) return UNTALK

      (b & 0xE0) >> 5 match {
        case 0 => GLOBAL(b & 0x1F)
        case 1 => LISTEN(b & 0x1F)
        case 2 => TALK(b & 0x1F)
        case 3 => SECONDARY_ADDRESS(b & 0x1F)
        case 7 =>
          b >> 4 match {
            case 0xE => CLOSE(b & 0xF)
            case 0xF => OPEN(b & 0xF)
            case _ => UNKNOWN
          }
        case _ => UNKNOWN
      }
    }
  }

  protected class Channel {
    private var opened = false
    private val _name = new StringBuilder()
    private val buffer = new ListBuffer[Int]
    private var bufferIndex = 0
    private var notFound = false

    def isOpened(): Boolean = opened
    def isNotFound(): Boolean = notFound
    def setNotFound(): Unit = notFound = true

    def open(): Unit = {
      opened = true
      _name.clear()
      bufferIndex = 0
      notFound = false
    }
    def close(): Unit = {
      opened = false
      bufferIndex = 0
    }
    def name(): String = _name.toString
    def addNameChar(c:Int): Unit = _name.append(c.toChar)
    def setData(data:Array[Int]): Unit = {
      buffer.clear()
      buffer.addAll(data)
      bufferIndex = 0
    }
    def addData(data:Int): Unit = {
      buffer += data
    }
    def hasMoreData(): Boolean = bufferIndex < buffer.length
    def nextData(): Int = {
      if (bufferIndex < buffer.length) {
        val data = buffer(bufferIndex)
        bufferIndex += 1
        data
      }
      else 0
    }
    def reset(): Unit = {
      opened = false
      _name.clear()
      buffer.clear()
      bufferIndex = 0
    }
    def getBuffer(): Array[Int] = buffer.toArray
    def index(): Int = bufferIndex
    def rewind(): Unit = bufferIndex = 0
  }

  protected var lastCommand : Command = UNKNOWN
  protected var secondaryAddress = 0
  protected val channels = Array.fill(16)(new Channel)


  protected def closeChannel(channel:Int): Unit = {}
  protected def letsTalk(): Unit = {
    role = TALKER_READY
  }
  protected def checkData(data:Int): Boolean = true

  override def reset: Unit = {
    super.reset
    for(c <- channels) c.reset()
  }

  override protected def sendData(): Boolean = {
    val ch = channels(secondaryAddress)
    /*
    if (ch.index() == 0) {
      if (!loadChannel()) return false
    }
     */
    if (ch.isNotFound()) return false

    bus.setDIO(ch.nextData())
    //println(s"Sending '${bus.getDIO().toChar}''")
    if (!ch.hasMoreData()) {
      bus.pullLine(listener,EOI)
      eoiLow = true
      ch.rewind()
    }
    true
  }

  protected def openChannel(): Unit = {}

  // TO BE IMPLEMENTED BY SUBCLASSES
  //protected def loadChannel(): Boolean
  protected def openNamedChannel(): Boolean = false

  protected def receiveData(data:Int): Unit = {
    //println(s"Data received: ${data.toHexString} ${data.toChar} EOI=$eoiLow")
    lastCommand match {
      case OPEN(_) =>
        channels(secondaryAddress).addNameChar(data)
        //println(s"Channel($secondaryAddress) name become ${channels(secondaryAddress).name()} EOI=$eoiLow")
        if (eoiLow) {
          if (!openNamedChannel()) {
            channels(secondaryAddress).setNotFound()
          }
        }
      case _ =>
        if (checkData(data)) {
          channels(secondaryAddress).addData(data)
          //println(s"New data[$secondaryAddress]: ${channels(secondaryAddress).getBuffer().mkString("[",",","]")}")
        }
    }
  }

  protected def executeCommand(): Unit = {}

  protected def isThisDevice(device:Int): Boolean = device == deviceID

  override protected def dataAvailable(): Unit = {
    val data = bus.getDIO() ^ 0xFF
    atnLow match {
      case true =>
        lastCommand = Command.fromByte(data)
        println(s"Command received: $lastCommand [$role]")
        lastCommand match {
          case LISTEN(device) =>
            if (isThisDevice(device)) role = LISTENER
          case UNLISTEN =>
            if (secondaryAddress == 15 && channels(15).isOpened() && channels(15).name().length > 0) executeCommand()
            role = IDLE
          case SECONDARY_ADDRESS(sa) =>
            if (role != IDLE) {
              secondaryAddress = sa
              //channels(sa).open()
              openChannel()
            }
            if (role == TALKER_READY) {
              role = TALKER
            }
          case UNTALK =>
            role = IDLE
          case OPEN(address) =>
            if (role != IDLE) {
              secondaryAddress = address
              channels(address).open()
            }
          case CLOSE(address) =>
            if (role != IDLE) {
              channels(address).close()
              closeChannel(address)
            }
          case TALK(device) =>
            if (isThisDevice(device)) {
              letsTalk()
            }
        }
      case false =>
        if (role != IDLE) {
          receiveData(data)
        }
    }
    //println("----------------------------------------")
  }
}
