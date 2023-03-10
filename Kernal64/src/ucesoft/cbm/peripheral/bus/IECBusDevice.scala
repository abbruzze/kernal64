package ucesoft.cbm.peripheral.bus

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object IECBusDevice {
  private val WAIT_BIT_TIMEOUT = 300
  private val EOI_TIMEOUT = 80
  private val WAIT_BETWEEN_BYTES = 100
  private val WRITE_KEEP_BIT_TIMEOUT = 100
  private val DEBUG = false
  
  object Mode extends Enumeration {
    val IDLE: Mode.Value = Value
    val ATN_SEEN: Mode.Value = Value
    val READ: Mode.Value = Value
    val WRITE: Mode.Value = Value
    val TURNAROUND: Mode.Value = Value
    val INIT_WRITE: Mode.Value = Value
  }
  
  private object WriteMode extends Enumeration {
    val WAIT_LISTENER_READY: WriteMode.Value = Value
    val WAIT_LISTENER_EOI_HANDSHAKE: WriteMode.Value = Value
    val WRITE_BIT_CLK1: WriteMode.Value = Value
    val WRITE_BIT_CLK2: WriteMode.Value = Value
    val WRITE_LAST_BIT: WriteMode.Value = Value
    val WRITE_END: WriteMode.Value = Value
  }
  
  private object ReadMode extends Enumeration {
    val WAIT_BIT: ReadMode.Value = Value
    val READ_BIT: ReadMode.Value = Value
  }
  
  object Role extends Enumeration {
    val NONE: Role.Value = Value
    val READY_TO_BE_TALKER: Role.Value = Value
    val TALKER: Role.Value = Value
    val LISTENER: Role.Value = Value
  }
  
  object Command extends Enumeration {
    val TALK: Command.Value = Value(0x40)
    val LISTEN: Command.Value = Value(0x20)
    val UNTALK: Command.Value = Value(0x5F)
    val UNLISTEN: Command.Value = Value(0x3F)
    val OPEN_CHANNEL: Command.Value = Value(0x60)
    val CLOSE: Command.Value = Value(0xE0)
    val OPEN: Command.Value = Value(0xF0)
  }
  
  protected class Channel {
    var dataToSend : Option[BusDataIterator] = None
    var readDirection = true
    private val _buffer = new ListBuffer[Int]
    val fileName = new StringBuilder
    private var opened = false
    
    def clear()  : Unit = {
      buffer.clear()
      fileName.clear()
    }
    def isOpened: Boolean = opened
    def open(): Unit = opened = true
    def close(): Unit = {
      opened = false
      buffer.clear()
      fileName.clear()
    }
    def addToBuffer(value:Int) : Unit = {
      _buffer += value
    }
    def buffer: ListBuffer[Int] = _buffer
    def bufferToString: String = buffer map { _.toChar } mkString
  }
}

abstract class IECBusDevice(bus: IECBus,device: Int = 8) extends IECBusListener {
  import IECBus._
  import IECBusDevice._
  import IECBusLine._
  import Mode._
  import ReadMode._
  import Role._
  import WriteMode._
  
  protected val channels: Array[Channel] = {
    val chs = Array.ofDim[Channel](31)
    for(i <- 0 until chs.length) chs(i) = new Channel
    chs
  }
  private[this] var mode = IDLE
  private[this] var lastAtn = false
  private[this] var byteRead = 0
  private[this] var byteReadCount = 0
  private[this] var byteReadLastChar = false
  private[this] var readEOI = false
  private[this] var readMode = WAIT_BIT
  private[this] var waitTimeout = 0L
  private[this] var isReadingFileName = false
  protected[this] var role: IECBusDevice.Role.Value = NONE
  protected[this] var channel = 0
  private[this] var writeMode = WAIT_LISTENER_READY
  private[this] var byteToSend = 0
  private[this] var dataToSendBitIndex = 0
  private[this] var writeEOI = false
  private[this] var writeCyclesWait = 0L
  private[this] var initNextByte = true
  
  protected def setMode(newMode:Mode.Value) : Unit = {
    if (DEBUG) println(s"$busid - $newMode FROM $mode")
    mode = newMode
  } 
  
  // ABSTRACT or HOOK METHODS
  protected def clocked(cycles:Long) : Unit = {}
  protected def isDeviceReady : Boolean
  protected def loadData(fileName:String) : Option[BusDataIterator]
  
  protected def byteJustRead(byte:Int,isLast:Boolean) : Unit = {}
  protected def byteJustWritten(isLast:Boolean) : Unit = {}
  protected def fileNameReceived()  : Unit = {}
  protected def dataNotFound()  : Unit = {}
  protected def listen()  : Unit = {}
  protected def unlisten()  : Unit = {}
  protected def open()  : Unit = {}
  protected def open_channel()  : Unit = {}
  protected def talk()  : Unit = {}
  protected def untalk()  : Unit = {}
  protected def close()  : Unit = {}
  protected def onCommand(cmd:Command.Value,secondaryAddress:Int) : Unit = {}
  
  // -------------------------------
  
  final def clock(cycles: Long) : Unit = {
    val atn = bus.atn == GROUND
    val clk = bus.clk == GROUND
    val data = bus.data == GROUND
    val timeout = waitTimeout > 0 && waitTimeout < cycles
    
    clocked(cycles)
    
    mode match {
      case TURNAROUND =>
        if (!clk) {
          //if (DEBUG) println("CLK -> true")
          set(DATA,VOLTAGE)
          set(CLK,GROUND)
          setMode(WRITE)
        }
      case INIT_WRITE =>
        setMode(WRITE)
        initByteToWrite(cycles)
      case IDLE =>
      	if (atn && clk && isDeviceReady) {      
      	  set(CLK,VOLTAGE) 
      	  set(DATA,GROUND)
      	  setMode(ATN_SEEN)
      	}
      	else 
      	if (!atn && role == LISTENER && !clk) {
      	  setMode(READ)
      	  readByte(cycles,true,false)
      	}
      case ATN_SEEN =>
        if (atn && !clk) {	// READY_TO_SEND
          set(DATA,VOLTAGE)	// READY_TO_DATA
          setMode(READ)
          readByte(cycles,true,timeout)
        }
        else
        if (!atn) {
          if (role == LISTENER) setMode(IDLE)
          else resetSignals()
        }
      case READ =>
        if (atn && !lastAtn) {
          setMode(IDLE)
        }
        else {
          readByte(cycles,false,timeout) match {
            case None =>
            case Some(r) =>
              setMode(IDLE)
              if (DEBUG) println("RECEIVED " + Integer.toHexString(r))
              if (atn) decodeATNCommand(cycles,r)
              else {
                if (isReadingFileName) {
                  channels(channel).fileName.append(r.toChar)
                  if (byteReadLastChar) {
                    if (DEBUG) println("File name is " + channels(channel).fileName) //; readLine
                    fileNameReceived()
                  }
                }
                else 
                if (role == LISTENER) {
                  channels(channel).addToBuffer(r)
                  byteJustRead(r,byteReadLastChar)
                }                
              }
          }
        }
      case WRITE =>
        if (atn && !lastAtn) {
          setMode(IDLE)
          initNextByte = true
        }
        else {
          if (initNextByte) initByteToWrite(cycles)
          if (writeByte(cycles,timeout)) resetSignals()
        }
    }
    
    lastAtn = atn
  }
  
  private def set(line:IECBusLine.Line,value:Int) : Unit = {
    bus.setLine(this,line,value)
  }
  
  protected def resetSignals()  : Unit = {
    if (DEBUG) println("resetting...")
    initNextByte = true
    setMode(IDLE)
    role = NONE
    readMode = WAIT_BIT 
    set(CLK,VOLTAGE)
    set(DATA,VOLTAGE)
  }
  
  private def decodeATNCommand(cycles:Long,data:Int) : Unit = {
    import Command._
    val (cmd,devOrSecAddr) =
    if (data < LISTEN.id + 31) (LISTEN.id,data - LISTEN.id)
    else
    if (data == UNLISTEN.id) (data,0)
    else
    if (data < TALK.id + 31) (TALK.id,data - TALK.id)
    else
    if (data == UNTALK.id) (data,0)
    else
    if (data <  OPEN_CHANNEL.id + 16) (OPEN_CHANNEL.id,data - OPEN_CHANNEL.id)
    else
    if (data < CLOSE.id + 16) (CLOSE.id,data - CLOSE.id)
    else
    if (data < OPEN.id + 16) (OPEN.id,data - OPEN.id)
    else throw new IllegalArgumentException("Unknown ATN command: " + data)
    val CMD = Command(cmd)
    if (DEBUG) println(s"Dev/SecA=$devOrSecAddr cmd=$CMD")
    
    onCommand(CMD,devOrSecAddr)
    CMD match {
      case LISTEN =>
        if (devOrSecAddr == device) role = LISTENER
        else resetSignals()
        listen()
      case UNLISTEN =>
        set(DATA,VOLTAGE)
        set(CLK,VOLTAGE)
        role = NONE
        unlisten()//if (channel == 15) handleChannel15(true)
      case OPEN =>
        if (role == LISTENER) {
          channel = devOrSecAddr        
          channels(channel).fileName.clear()
          isReadingFileName = true       
          open()
        }        
        else resetSignals()
      case OPEN_CHANNEL =>
        channel = devOrSecAddr
        isReadingFileName = false
        if (role == READY_TO_BE_TALKER) {
          role = TALKER
          setMode(TURNAROUND)     
          open_channel()
        }
        if (role == LISTENER) open_channel()
      case TALK =>
        if (devOrSecAddr == device) role = READY_TO_BE_TALKER
        talk()
      case UNTALK =>
        role = NONE
        untalk()
      case CLOSE =>
        channel = devOrSecAddr
        if (role != NONE) {
          close()
          channels(channel).close()
          resetSignals()
        }
    }
  }
  
  private def readByte(cycles:Long,restart:Boolean,timeout:Boolean) : Option[Int] = {
    val data = bus.data == GROUND
    val clk = bus.clk == GROUND
    val atn = bus.atn == GROUND
    
    if (restart) {
      if (DEBUG) println("Start reading byte...")
      byteReadCount = 0
      readMode = WAIT_BIT
      byteRead = 0
      byteReadLastChar = false
      readEOI = false
      set(DATA,VOLTAGE)
      waitTimeout = cycles + WAIT_BIT_TIMEOUT
      return None
    }
    else 
    if (!atn && timeout) {      
      if (!readEOI) {
        //if (DEBUG) println("EOI timeout detected (phase 1)")
        set(DATA,GROUND)
        waitTimeout = cycles + EOI_TIMEOUT
        readEOI = true
      }
      else {        
        //if (DEBUG) println("EOI timeout detected (phase 2)")
        set(DATA,VOLTAGE)
        waitTimeout = 0
        byteReadLastChar = true
      }
    }
        
    readMode match {
      case WAIT_BIT =>
        if (clk && byteReadCount == 8) {
          byteReadCount += 1
        }
        else
        if (clk) readMode = READ_BIT
      case READ_BIT =>
        if (!clk) {
          val bit = if (data) 0 else (1 << byteReadCount)
          byteRead |= bit
          readMode = WAIT_BIT
          byteReadCount += 1
          waitTimeout = 0
        }
    }
    
    if (byteReadCount == 9) {
      set(DATA,GROUND) // ACK
      Some(byteRead)
    }
    else None
  }  
  
  private def writeByte(cycles:Long,timeout:Boolean) : Boolean = {
    if (writeCyclesWait > cycles) return false
    
    val data = bus.data == GROUND
    
    writeMode match {
      case WAIT_LISTENER_READY =>
        set(CLK,VOLTAGE)
        if (!data) {
          if (DEBUG) println("LISTENER READY FOR DATA")          
          if (!channels(channel).dataToSend.isDefined) { // FILE NOT FOUND
            waitTimeout = 0
            if (DEBUG) println("FILE NOT FOUND DETECTED for channel " + channel)
            channels(channel).close()
            dataNotFound()//setStatus(STATUS_FILE_NOT_FOUND)
            true
          }
          else {
            if (channels(channel).dataToSend.get.hasNext) loadByte(cycles)
	        if (!writeEOI) {
	          writeMode = WRITE_BIT_CLK2
	          false
	        }
	        else {
	          writeMode = WAIT_LISTENER_EOI_HANDSHAKE
	          false
	        }      
          }
        }
        else false
      case WRITE_BIT_CLK2 =>
        set(CLK,GROUND)
        set(DATA,VOLTAGE)
        writeMode = WRITE_BIT_CLK1
        writeCyclesWait = cycles + WRITE_KEEP_BIT_TIMEOUT
        false
      case WRITE_BIT_CLK1 =>
        val bit = byteToSend & (1 << dataToSendBitIndex)
        //if (DEBUG) println("WRITE BIT=" + bit)
        if (bit == 0) set(DATA,GROUND) else set(DATA,VOLTAGE)
        dataToSendBitIndex += 1
        set(CLK,VOLTAGE)
        writeCyclesWait = cycles + WRITE_KEEP_BIT_TIMEOUT
        if (dataToSendBitIndex < 8) writeMode = WRITE_BIT_CLK2 else writeMode = WRITE_LAST_BIT
        false
      case WRITE_LAST_BIT =>
        set(CLK,GROUND)
        set(DATA,VOLTAGE)
        writeMode = WRITE_END
        false
      case WRITE_END =>        
        dataToSendBitIndex = 0
        //if (DEBUG) println("BYTE_END...")        
        //ledListener.updateLoading(channels(channel).dataToSend.get.getPerc)
        set(CLK,GROUND)
        if (data) {
          val isLast = !channels(channel).dataToSend.get.hasNext
          byteJustWritten(isLast)
          writeMode = WAIT_LISTENER_READY
          if (DEBUG) println("RECEIVED ACK")
          if (isLast) {
            if (DEBUG) println("WRITE FINISHED: ")
            true
          }
          else {
            //initByteToWrite(cycles)
            initNextByte = true
            false
          }
        }
        else false
      case WAIT_LISTENER_EOI_HANDSHAKE =>
        if (data) {
          if (DEBUG) println("EOI Handshake")
          writeEOI = false
          writeMode = WAIT_LISTENER_READY
        }
        false
    }
  }
  
  private def loadByte(cycles:Long) : Unit = {
    channels(channel).dataToSend match {
      case Some(ds) =>
        if (ds.isLast) {
          if (DEBUG) println("Last char, setting EOI")
          writeCyclesWait = cycles + EOI_TIMEOUT
          writeEOI = true
        }
        byteToSend = ds.next()
        if (DEBUG) println("Next byte to send: " + Integer.toHexString(byteToSend) + " " + ds.getPerc + "%")                
      case None =>
    }
  }
  
  private def initByteToWrite(cycles:Long) : Unit = {
    initNextByte = false
    writeEOI = false
    dataToSendBitIndex = 0
    writeCyclesWait = cycles + WAIT_BETWEEN_BYTES
    writeMode = WAIT_LISTENER_READY    
  }
  
  protected def load(fileName: String) : Unit = {
    if (DEBUG) println("Loading " + fileName)      
    channels(channel).dataToSend = loadData(fileName)
  }
}
