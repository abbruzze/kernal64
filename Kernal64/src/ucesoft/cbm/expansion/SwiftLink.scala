package ucesoft.cbm.expansion

import org.apache.commons.net.telnet.TelnetClient
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.rs232._
import ucesoft.cbm._

import java.io._

object SwiftLink {
  def getSL(nmiHandler: (Boolean) => Unit,reu:Option[ExpansionPort]) : ExpansionPort with RS232 = {
    new SwiftLink(nmiHandler,reu)
  }
}

private class SwiftLink(nmiHandler: (Boolean) => Unit,reu:Option[ExpansionPort]) extends ExpansionPort with RS232 with ModemCommandListener {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.SWIFTLINK
  override val name = "SwiftLink"
  override val componentID = "SwiftLink"
  override val componentType: CBMComponentType.Value = CBMComponentType.MEMORY
  final private[this] val RX_EVENT = "SL_RX_EVENT"
  final private[this] val TX_EVENT = "SL_TX_EVENT"
  
  // CMD
  final private[this] val DATA_TERMINAL_READY = 0x1
  final private[this] val RECEIVER_INTERRUPT_ENABLED = 0x2
  final private[this] val TRANSMITTER_INTERRUPT_CONTROL = 0x0C
  final private[this] val ECHO_MODE = 0x10
  // STATUS
  final private[this] val OVERRUN = 0x2
  final private[this] val DCD_DSR = 0x20 | 0x40
  final private[this] val RECEIVER_DATA_REGISTER_EMPTY = 0x0
  final private[this] val RECEIVER_DATA_REGISTER_FULL = 0x8
  final private[this] val TRANSMITTER_DATA_REGISTER_EMPTY = 0x10
  final private[this] val TRANSMITTER_DATA_REGISTER_FULL = 0x0
  final private[this] val DATA_CARRIER_DETECTED = 0x20
  final private[this] val DATA_SET_READY = 0x40
  final private[this] val IRQ = 0x80
  
  final private[this] val DATA = if (reu.isDefined) 0xDE00 else 0xDF00
  final private[this] val STATUS = if (reu.isDefined) 0xDE01 else 0xDF01
  final private[this] val CMD = if (reu.isDefined) 0xDE02 else 0xDF02
  final private[this] val CTRL = if (reu.isDefined) 0xDE03 else 0xDF03
  private[this] var status = 0
  private[this] var cmd = 0
  private[this] var ctrl = 8
  private[this] var connected = DATA_CARRIER_DETECTED | DATA_SET_READY
  private[this] var overrun = 0
  private[this] var receiverDataRegister = RECEIVER_DATA_REGISTER_EMPTY
  private[this] var transmitterDataRegister = TRANSMITTER_DATA_REGISTER_EMPTY
  private[this] var irq = 0
  private[this] var byteReceived = 0
  private[this] var clockTicks = 1000000L
  private[this] val clk = Clock.systemClock
  private[this] val telnetClient : TelnetClient = new TelnetClient
  private[this] var host = ""
  private[this] var port = 0
  private[this] var lastBaud = 0.0
  private[this] val modem = new Modem(this,s"WELCOME ON KERNAL64'S SWIFTLINK IMPL.${13.toChar}TYPE 'ATDT <HOST>:<PORT> TO CONNECT ${13.toChar}")
  private[this] var statusListener : RS232StatusListener = _
  private[this] var hostAndConf = ""
  
  final private[this] val SPEEDS = Array(10, 50, 75, 109.92, 134.58, 150, 300, 600, 1200, 1800,2400, 3600, 4800, 7200, 9600, 19200) map { _ * 2 } // SL
  
  val EXROM = true
  val GAME = true
  val ROML: Memory = null
  val ROMH: Memory = null

  // Modem interface
  def hangUp() : Unit = {
    Log.info("SwiftLink hanged up")
    connected = DATA_SET_READY
    disconnect()
    nmi()
  }
  def commandMode(on:Boolean): Unit = {}
  def connectTo(address:String): Unit = {
    Log.info(s"SwiftLink - Connecting to $address")
    setConfiguration(address)
    setEnabled(true)
  }
  def ring(ringing:Boolean) : Unit = {}
  
  // RS-232 interface
  def setTXD(high:Int) : Unit = {}
  def getTXD = 0
  def setOthers(value:Int) : Unit = {}
  def getOthers = 0
  def setConfiguration(conf:String) : Unit = {
    if (conf != "") {
      hostAndConf = conf
      val pars = conf.split(":")
      if (pars.length != 2) throw new IllegalArgumentException("Bad configuration string, expected host:port")
      host = pars(0)
      port = pars(1).toInt
    }
    else host = ""
  }
  
  def connectionInfo: String = hostAndConf
  
  def getConfiguration: String = host + ":" + port
  
  def isEnabled: Boolean = connected == 0
  
  def setEnabled(enabled:Boolean) : Unit = {
    eject()
    if (enabled) {      
      if (host != "") connect()
      connected = 0
      clk.pause()
      clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,readCycle _))
      clk.play()
      nmi()
      ExpansionPort.setExpansionPort(this)
    }
  }
  
  private def connect()  : Unit = {
    try {
      telnetClient.connect(host, port)
      modem.setStreams(telnetClient.getInputStream,telnetClient.getOutputStream)
      Log.info(s"SwiftLink connected to $host:$port")
      statusListener.setRS232Enabled(true)
      statusListener.connectedTo(s"$host:$port")
    }
    catch {
      case io:IOException =>
        Log.info("SwiftLink connection error: " + io)
        disconnect()
    }
  }
  
  private def disconnect()  : Unit = {
    try {
      connected = DATA_SET_READY
      if (telnetClient != null) telnetClient.disconnect()
      modem.setStreams(null,null)
      Log.info(s"SwiftLink disconnected from $host:$port")
      modem.commandModeMessage(HayesResultCode.NO_CARRIER)
    }
    catch {
      case _:Throwable =>
    }
    finally {
      statusListener.setRS232Enabled(false)
      statusListener.connectedTo("")
    }
  }
  
  def setBitReceivedListener(l: () => Unit) : Unit = {}
  
  def getDescription = "<html>SwiftLink TCP/IP cartridge.<br>Connection string: <i>host:port</i> or leave empty to connect later via <i>atdt host:port</i> modem command</html>"
  
  def setRS232Listener(l:RS232StatusListener): Unit = statusListener = l
  
  override def toString: String = componentID + (if (isEnabled) "(enabled)" else "") + (if (reu.isDefined) " $DE00 + REU" else " $DF00")
  // ----------------
  
  override def reset(): Unit = {
    disconnect()
    ctrl = 8
    cmd = 0xE0 // found into desterm 128 
    status = TRANSMITTER_DATA_REGISTER_EMPTY
    nmiHandler(false)
    clk.cancel(RX_EVENT)
    clk.cancel(TX_EVENT)
    reu match {
      case Some(r) => r.reset()
      case _ =>
    }
  }
  
  override def eject(): Unit = {
    disconnect()
    clk.cancel(RX_EVENT)
    clk.cancel(TX_EVENT)
    ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)    
  }
  @inline private def isRTS = ((cmd >> 2) & 3) != 0
  @inline private def isDataTerminalReady = ((cmd & DATA_TERMINAL_READY) > 0)
  @inline private def isReceiverInterruptEnabled = (cmd & RECEIVER_INTERRUPT_ENABLED) == 0
  @inline private def isTransmitterInterruptEnabled = (cmd & TRANSMITTER_INTERRUPT_CONTROL) == 0x04
  @inline private def nmi()  : Unit = {
    nmiHandler(true)
    nmiHandler(false)
    irq = IRQ
  }
  
  private def readCycle(cycles:Long) : Unit = {
    try {
      if (isRTS && isDataTerminalReady && modem.inputStream != null && modem.inputStream.available > 0) {
        statusListener.update(RS232.RXD,1)
        byteReceived = modem.inputStream.read
        if (receiverDataRegister == RECEIVER_DATA_REGISTER_FULL) overrun = OVERRUN else overrun = 0
        receiverDataRegister = RECEIVER_DATA_REGISTER_FULL
        if (isReceiverInterruptEnabled) nmi()
        //println(s"RECEIVED $byteReceived ${byteReceived.toChar}")
      }
      else statusListener.update(RS232.RXD,0)
    }
    catch {
      case t:Throwable =>
        socketError(t)
    }
    clk.schedule(new ClockEvent(RX_EVENT,cycles + clockTicks,readCycle _))
  }  
  
  private def socketError(t:Throwable) : Unit = {
    Log.info("Error while reading/writing byte from/to SwiftLink: " + t)
    t.printStackTrace()
    hangUp()
  }
  
  override def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val address = (_address & 0xFF00) | (_address & 3) 
    address match {  
      case STATUS => 
        val status = overrun | receiverDataRegister | transmitterDataRegister | connected | irq
        irq = 0
        status
      case CTRL =>
        ctrl
      case CMD =>
        cmd
      case DATA =>
        receiverDataRegister = RECEIVER_DATA_REGISTER_EMPTY
        byteReceived
      case _ =>
        reu match {
          case Some(r) =>
            r.read(_address, chipID)
          case _ => 
            0
        }      
    }    
  }
  override def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    val address = (_address & 0xFF00) | (_address & 3) 
    
    address match {  
      case STATUS =>
        reset()
      case CTRL =>
        ctrl = value
        updateClockTicks()
      case CMD =>
        cmd = value
        statusListener.update(RS232.DTR,if (isDataTerminalReady) 1 else 0)
        statusListener.update(RS232.RTS,if (((cmd >> 2) & 3) != 0) 1 else 0)
        //println(s"CMD= $cmd DTR=${isDataTerminalReady} RX_IRQ=${isReceiverInterruptEnabled} TX_IRQ=${isTransmitterInterruptEnabled}")
        updateClockTicks()
      case DATA =>      
        statusListener.update(RS232.RTS,value & TRANSMITTER_INTERRUPT_CONTROL)
        if (transmitterDataRegister == TRANSMITTER_DATA_REGISTER_EMPTY) {
          statusListener.update(RS232.TXD,1)
          //println(s"Sending $value ${value.toChar}")
          transmitterDataRegister = TRANSMITTER_DATA_REGISTER_FULL
          clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,cycles => {
            statusListener.update(RS232.TXD,0)
            try {
              if (modem.outputStream != null) {
                modem.outputStream.write(value)
                modem.outputStream.flush()
              }
            }
            catch {
              case t:Throwable =>
                socketError(t)
            }          
            finally {
              transmitterDataRegister = TRANSMITTER_DATA_REGISTER_EMPTY
              if (isTransmitterInterruptEnabled) nmi()
            }
          }))
        }      
      case _ =>
        reu match {
          case Some(r) =>
            r.write(_address,value,chipID)
          case _ => 
        }
    }
  }
  
  private def updateClockTicks()  : Unit = {
    val stopBits = 1 + ((ctrl & 0x80) >> 7)
    val wordLength = 8 - ((ctrl & 0x60) >> 5)
    val parityBit = (cmd & 0x20) >> 5
    
    val baud = SPEEDS(ctrl & 0x0f)
    modem.setBaud(baud.toInt)
    clockTicks = math.round(clk.getClockHz / baud * (1 + stopBits + wordLength + parityBit)).toInt

    if (baud != lastBaud) {
      lastBaud = baud
      clk.cancel(RX_EVENT)
      clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,readCycle _))
      Log.info(s"SwiftLink clockTicks=$clockTicks stopBits=$stopBits wordLength=$wordLength parityBit=$parityBit baud=$baud")
    }
  }
  // state
  override protected def saveState(out:ObjectOutputStream) : Unit = {}
  override protected def loadState(in:ObjectInputStream) : Unit = {}
  override protected def allowsStateRestoring : Boolean = {
    showError("State error","Storing SwiftLink's state is not supported.")
    false
  }
}