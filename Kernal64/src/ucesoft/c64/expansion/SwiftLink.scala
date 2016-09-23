package ucesoft.c64.expansion

import ucesoft.c64.Clock
import ucesoft.c64.ChipID
import java.io.InputStream
import java.io.OutputStream
import ucesoft.c64.Log
import ucesoft.c64.ClockEvent
import org.apache.commons.net.telnet.TelnetClient
import ucesoft.c64.peripheral.rs232.RS232
import ucesoft.c64.peripheral.cia.CIA
import ucesoft.c64.peripheral.rs232.RS232StatusListener
import ucesoft.c64.C64ComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.JOptionPane

object SwiftLink {
  def getSL(nmiHandler: (Boolean) => Unit,reu:Option[ExpansionPort]) : ExpansionPort with RS232 = {
    new SwiftLink(nmiHandler,reu)
  }
}

private class SwiftLink(nmiHandler: (Boolean) => Unit,reu:Option[ExpansionPort]) extends ExpansionPort with RS232 {
  override val name = "SwiftLink"
  override val componentID = "SwiftLink"
  override val componentType = C64ComponentType.MEMORY
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
  private[this] var ctrl = 0
  private[this] var connected = DATA_CARRIER_DETECTED | DATA_SET_READY
  private[this] var overrun = 0
  private[this] var receiverDataRegister = RECEIVER_DATA_REGISTER_EMPTY
  private[this] var transmitterDataRegister = TRANSMITTER_DATA_REGISTER_EMPTY
  private[this] var irq = 0
  private[this] var byteReceived = 0
  private[this] var clockTicks = 1000000L
  private[this] val clk = Clock.systemClock
  private[this] var in : InputStream = _
  private[this] var out : OutputStream = _
  private[this] var telnetClient : TelnetClient = _
  private[this] var host = ""
  private[this] var port = 0
  private[this] var lastBaud = 0.0
  private[this] var isModemMode = false
  private[this] val modemIn = new ModemInputStream(s"WELCOME ON KERNA64'S SWIFTLINK IMPL.${13.toChar}TYPE 'AD <HOST>:<PORT> TO CONNECT ${13.toChar}")
  private[this] var modemCmd = ""
  private[this] val inHpChecker = new HangupChecker
  private[this] val outHPChecker = new HangupChecker
  private[this] var statusListener : RS232StatusListener = _
  private[this] var hostAndConf = ""
  
  final private[this] val SPEEDS = Array(10, 50, 75, 109.92, 134.58, 150, 300, 600, 1200, 1800,2400, 3600, 4800, 7200, 9600, 19200) map { _ * 2 } // SL
  
  private[this] class ModemInputStream(welcomeMessage:String) extends InputStream {
    var msg = welcomeMessage
    
    override def available = msg.length
    def read = {
      if (msg.length > 0) {
        val b = msg.charAt(0).toInt
        msg = msg.substring(1)
        b
      }
      else -1
    }
    def append(s:String) = msg += s
  }
  
  private[this] class HangupChecker {
    final val HANGUP = "+++ATH"
    var index = 0
    
    def check(c:Char) : Boolean = if (c.toUpper == HANGUP.charAt(index)) {
      index += 1
      if (index == HANGUP.length) {
        index = 0
        true
      }     
      else false
    }
    else {
      index = 0
      false
    }
  }
  
  val EXROM = true
  val GAME = true
  val ROML = null
  val ROMH = null
  
  // RS-232 interface
  def setTXD(high:Int) {}
  def getTXD = 0
  def setOthers(value:Int) {}
  def getOthers = 0
  def setConfiguration(conf:String) {
    hostAndConf = conf
    if (conf != "modem") {
      val pars = conf.split(":")
      if (pars.length != 2) throw new IllegalArgumentException("Bad configuration string, expected host:port")
      host = pars(0)
      port = pars(1).toInt
      isModemMode = false
    }
    else {
      isModemMode = true
      in = modemIn
    }
  }
  
  def connectionInfo = hostAndConf
  
  def getConfiguration = host + ":" + port
  
  def isEnabled = connected == 0
  
  def setEnabled(enabled:Boolean) {
    statusListener.setRS232Enabled(enabled)
    eject
    if (enabled) {      
      if (!isModemMode) connect    
      connected = 0
      clk.pause
      clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,readCycle _))
      clk.play
      nmi
      ExpansionPort.setExpansionPort(this)
    }
  }
  
  private def connect {
    telnetClient = new TelnetClient
    telnetClient.connect(host,port)
    in = telnetClient.getInputStream
    out = telnetClient.getOutputStream
  }
  
  private def disconnect {
    try {
      connected = DATA_SET_READY
      if (telnetClient != null) telnetClient.disconnect
      Log.info(s"SwiftLink disconnected from $host:$port")
    }
    catch {
      case t:Throwable =>
    }
  }
  
  def setCIA(cia2:CIA) {}
  
  def getDescription = "SwiftLink TCP/IP cartridge. Connection string: 'host:port' or 'modem' to connect later via 'ad host:port' modem command"
  
  def setRS232Listener(l:RS232StatusListener) = statusListener = l
  
  override def toString = componentID + (if (isEnabled) "(enabled)" else "") + (if (reu.isDefined) " $DE00 + REU" else " $DF00")
  // ----------------
  
  override def reset {
    ctrl = 0
    cmd = RECEIVER_INTERRUPT_ENABLED
    status = TRANSMITTER_DATA_REGISTER_EMPTY
    nmiHandler(false)
    clk.cancel(RX_EVENT)
    clk.cancel(TX_EVENT)
    reu match {
      case Some(r) => r.reset
      case _ =>
    }
  }
  
  override def eject {
    disconnect
    clk.cancel(RX_EVENT)
    clk.cancel(TX_EVENT)
    ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)    
  }
  
  @inline private def isDataTerminalReady = (cmd & DATA_TERMINAL_READY) > 0
  @inline private def isReceiverInterruptEnabled = (cmd & RECEIVER_INTERRUPT_ENABLED) == 0
  @inline private def isTransmitterInterruptEnabled = (cmd & TRANSMITTER_INTERRUPT_CONTROL) == 0x04
  @inline private def nmi {    
    nmiHandler(true)
    nmiHandler(false)
    irq = IRQ
  }
  
  private def hangUp {
    println("SwiftLink hanged up")
    connected = DATA_SET_READY
    isModemMode = true
    modemIn.append(13.toChar + "HANGED UP" + 13.toChar)
    in = modemIn
    disconnect
    nmi
  }
  
  private def readCycle(cycles:Long) {
    try {
      if (isDataTerminalReady && in.available > 0) {
        statusListener.update(RS232.RXD,1)
        byteReceived = in.read
        if (receiverDataRegister == RECEIVER_DATA_REGISTER_FULL) overrun = OVERRUN else overrun = 0
        receiverDataRegister = RECEIVER_DATA_REGISTER_FULL
        if (isReceiverInterruptEnabled) nmi
        //println(s"RECEIVED $byteReceived ${byteReceived.toChar}")
        if (inHpChecker.check(byteReceived.toChar)) hangUp
      }
      else statusListener.update(RS232.RXD,0)
    }
    catch {
      case t:Throwable =>
        socketError(t)
    }
    clk.schedule(new ClockEvent(RX_EVENT,cycles + clockTicks,readCycle _))
  }  
  
  private def socketError(t:Throwable) {
    Log.info("Error while reading/writing byte from/to SwiftLink: " + t)
    t.printStackTrace
    hangUp
    modemIn.append("CONNECTION CLOSED!!" + 13.toChar)
  }
  
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = address match {
    case STATUS => 
      val status = overrun | receiverDataRegister | transmitterDataRegister | connected | irq
      // maybe ?
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
          r.read(address, chipID)
        case _ => 
          0
      }      
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = address match {
    case STATUS =>
      reset
    case CTRL =>
      ctrl = value
      updateClockTicks
    case CMD =>
      cmd = value
      statusListener.update(RS232.DTR,if (isDataTerminalReady) 1 else 0)
      //println(s"CMD= $cmd DTR=${isDataTerminalReady} RX_IRQ=${isReceiverInterruptEnabled} TX_IRQ=${isTransmitterInterruptEnabled}")
      updateClockTicks
    case DATA =>      
      statusListener.update(RS232.RTS,value & TRANSMITTER_INTERRUPT_CONTROL)
      if (isModemMode) {
        if (value != 13) {
          if (value == 20) modemCmd = modemCmd.dropRight(1)
          else modemCmd += value.toChar
        }
        else processModemCommand
      }
      else
      if (transmitterDataRegister == TRANSMITTER_DATA_REGISTER_EMPTY) {
        statusListener.update(RS232.TXD,1)
        //println(s"Sending $value ${value.toChar}")
        transmitterDataRegister = TRANSMITTER_DATA_REGISTER_FULL
        clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,cycles => {
          statusListener.update(RS232.TXD,0)
          try {
            out.write(value)
            out.flush
            if (outHPChecker.check(value.toChar)) hangUp                          
          }
          catch {
            case t:Throwable =>
              socketError(t)
          }          
          finally {
            transmitterDataRegister = TRANSMITTER_DATA_REGISTER_EMPTY
            if (isTransmitterInterruptEnabled) nmi
          }
        }))
      }      
    case _ =>
      reu match {
        case Some(r) =>
          r.write(address,value,chipID)
        case _ => 
      }
  }
  
  private def updateClockTicks {
    val stopBits = 1 + ((ctrl & 0x80) >> 7)
    val wordLength = 8 - ((ctrl & 0x60) >> 5)
    val parityBit = (cmd & 0x20) >> 5
    
    val baud = SPEEDS(ctrl & 0x0f)
    clockTicks = (985248.0 / baud * (1 + stopBits + wordLength + parityBit)).toInt
    
    if (baud != lastBaud) {
      lastBaud = baud
      clk.cancel(RX_EVENT)
      clk.schedule(new ClockEvent(RX_EVENT,clk.currentCycles + clockTicks,readCycle _))      
    }
    
    //println(s"clockTicks=$clockTicks stopBits=$stopBits wordLength=$wordLength parityBit=$parityBit baud=${SPEEDS(ctrl & 0x0f)}")
  }
  
  private def processModemCommand {    
    val cmd = modemCmd.trim filterNot { c => c.isControl }
    modemCmd = ""
    println(s"Processing modem command: $cmd")
    if (cmd == "") return
    
    if (cmd.toUpperCase.startsWith("AD") && cmd.length > 4) {
      val conf = cmd.substring(3).trim
      setConfiguration(conf)
      try {
        connect
      }
      catch {
        case t:Throwable =>
          modemIn.append("CONNECTION ERROR" + 13.toChar)
          Log.info("SwiftLink connection error: " + t)
      }
    }
    else modemIn.append("MODEM COMMAND ERROR" + 13.toChar)
  }
  // state
  override protected def saveState(out:ObjectOutputStream) {}
  override protected def loadState(in:ObjectInputStream) {}
  override protected def allowsStateRestoring(parent:JFrame) : Boolean = {
    JOptionPane.showMessageDialog(parent,"Loading/storing of cartridge's state is not supported [SwiftLink].","State error",JOptionPane.ERROR_MESSAGE)
    false
  }
}