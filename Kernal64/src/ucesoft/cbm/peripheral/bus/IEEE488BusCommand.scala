package ucesoft.cbm.peripheral.bus

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.mutable.ListBuffer

abstract class IEEE488BusCommand(override val name:String,val deviceID:Int,bus: IEEE488Bus) extends IEEE488BusHandshake(name,bus) {
  import IEEE488Bus.LineType._
  import Role._

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

  protected class DirectAccessBuffer(val index:Int,val memoryAddress:Int) {
    val addresses = memoryAddress until (memoryAddress + 0xFF)
    private var used = false
    private val buffer = Array.ofDim[Int](256)
    private var bp = 0

    def isUsed: Boolean = used
    def setUsed(used:Boolean): Unit = {
      this.used = used
    }

    def getBuffer(): Array[Int] = buffer

    def setBP(bp:Int): Unit = this.bp = bp % 256
    def get(pos:Int): Int = {
      if (pos < buffer.length) buffer(pos) else 0
    }
    def get(): Int = {
      val v = buffer(bp)
      bp = (bp + 1) % buffer.length
      v
    }

    def set(value:Int): Unit = {
      if (used) {
        buffer(bp) = value
        bp = (bp + 1) % buffer.length
      }
    }
    def set(value:Array[Int]): Unit = {
      System.arraycopy(value,0,buffer,0,value.length)
      bp = 0
    }
    def set(value:Array[Int],pos:Int): Unit = {
      if (used) {
        for (i <- 0 until value.length) {
          buffer((pos + i) % buffer.length) = value(i)
        }
      }
    }
  }

  protected class Channel {
    private var opened = false
    private val _name = new StringBuilder()
    private val inBuffer = new ListBuffer[Int]
    private var outBuffer : Array[Int] = Array()
    private var outBufferIndex = 0
    private var notFound = false
    private var writeMode = false
    private var bufferPtr : DirectAccessBuffer = _
    private var commandOutputReady = false

    def setCommandOutputReady(): Unit = commandOutputReady = true
    def isCommandOutputReady(): Boolean = commandOutputReady

    def isOpened(): Boolean = opened
    def isNotFound(): Boolean = notFound
    def setNotFound(): Unit = notFound = true

    def setBuffer(buffer:DirectAccessBuffer): Unit = bufferPtr = buffer
    def hasBuffer(): Boolean = bufferPtr != null
    def getBuffer(): DirectAccessBuffer = bufferPtr

    def open(): Unit = {
      opened = true
      _name.clear()
      outBufferIndex = 0
      notFound = false
    }
    def close(): Unit = {
      opened = false
      outBuffer = Array()
      outBufferIndex = 0
      if (bufferPtr != null) bufferPtr.setUsed(false)
      bufferPtr = null
      inBuffer.clear()
      commandOutputReady = false
    }
    def name(): String = _name.toString
    def addNameChar(c:Int): Unit = _name.append(c.toChar)
    def setOutData(data:Array[Int]): Unit = {
      /*
      inBuffer.clear()
      inBuffer.addAll(data)*/
      outBuffer = data
      outBufferIndex = 0
    }
    def addInData(data:Int): Unit = {
      if (bufferPtr != null)
        bufferPtr.set(data)
      else inBuffer += data
    }
    def getInData(): Array[Int] = inBuffer.toArray
    def getOutData(): Array[Int] = outBuffer
    def hasMoreOutData(): Boolean = bufferPtr != null || outBufferIndex < outBuffer.length
    def nextOutData(): Int = {
      if (bufferPtr != null) bufferPtr.get()
      else
      if (outBufferIndex < outBuffer.length) {
        val data = outBuffer(outBufferIndex)
        outBufferIndex += 1
        data
      }
      else 0
    }
    def reset(): Unit = {
      opened = false
      _name.clear()
      inBuffer.clear()
      outBuffer = Array()
      outBufferIndex = 0
      writeMode = false
      bufferPtr = null
      commandOutputReady = false
    }
    def resetInput(): Unit = {
      inBuffer.clear()
      _name.clear()
    }
    def setWriteMode(wm:Boolean): Unit = writeMode = wm
    def isWriteMode(): Boolean = writeMode
    def rewind(): Unit = outBufferIndex = 0
  }

  protected var lastCommand : Command = UNKNOWN
  protected var secondaryAddress = 0
  protected val channels: Array[Channel] = Array.fill(16)(new Channel)
  protected val buffersAddress : Array[Int] = Array()
  protected lazy val buffers: Array[DirectAccessBuffer] = {
    for(a <- buffersAddress.zipWithIndex) yield {
      new DirectAccessBuffer(a._2,a._1)
    }
  }


  protected def findFreeBuffer(): Option[DirectAccessBuffer] = {
    val buf = buffers.find(!_.isUsed)
    buf.foreach(_.setUsed(true))
    buf
  }

  protected def maskAddressForBuffer(address:Int): Int = address

  protected def findBufferForAddress(address:Int): Option[DirectAccessBuffer] = {
    val maskedAdr = maskAddressForBuffer(address)
    buffers.find(_.addresses.contains(maskedAdr))
  }


  protected def closeChannel(channel:Int): Unit = {}
  protected def checkData(data:Int): Boolean = true

  override def reset: Unit = {
    super.reset
    for(c <- channels) c.reset()
  }

  override protected def sendData(): Boolean = {
    val ch = channels(secondaryAddress)
    if (ch.isNotFound()) return false

    bus.setDIO(ch.nextOutData())
    //println(s"Sending '${bus.getDIO().toChar}''")
    if (!ch.hasMoreOutData()) {
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
          channels(secondaryAddress).addInData(data)
          if (secondaryAddress == 15 && data == 13) executeCommand()
          //println(s"New data[$secondaryAddress]: ${channels(secondaryAddress).getBuffer.mkString("[",",","]")}")
        }
    }
  }

  protected def executeCommand(): Unit = {}

  protected def isThisDevice(device:Int): Boolean = device == deviceID

  override protected def dataAvailable(): Unit = {
    val data = bus.getDIO() ^ 0xFF
    if (atnLow) {
      lastCommand = Command.fromByte(data)
      //println(s"Command received: $lastCommand [$role]")
      lastCommand match {
        case LISTEN(device) =>
          if (isThisDevice(device)) setRole(LISTENER)
        case UNLISTEN =>
          if (secondaryAddress == 15 /*&& channels(15).isOpened()*/ && (channels(15).name().length > 0 || channels(15).getInData().length > 0)) executeCommand()
          setRole(IDLE)
        case SECONDARY_ADDRESS(sa) =>
          if (role != IDLE) {
            secondaryAddress = sa
            //channels(sa).open()
            openChannel()
          }
          if (role == TALKER_READY) {
            setRole(TALKER)
          }
        case UNTALK =>
          setRole(IDLE)
        case OPEN(address) =>
          if (role != IDLE) {
            secondaryAddress = address
            channels(address).open()
          }
        case CLOSE(address) =>
          if (role != IDLE) {
            closeChannel(address)
            channels(address).close()
          }
        case TALK(device) =>
          if (isThisDevice(device)) {
            setRole(TALKER_READY)
          }
      }
      commandReceived(lastCommand)
    } else {
      if (role != IDLE) {
        receiveData(data)
      }
    }
    //println("----------------------------------------")
  }

  protected def commandReceived(cmd:Command): Unit = {}

  override protected def saveState(out: ObjectOutputStream): Unit = {
   super.save(out)
    out.writeInt(secondaryAddress)
    out.writeObject(channels)
    out.writeObject(buffers)
  }

  override protected def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    secondaryAddress = in.readInt()
    loadMemory(channels,in)
    loadMemory(buffers,in)
  }
}
