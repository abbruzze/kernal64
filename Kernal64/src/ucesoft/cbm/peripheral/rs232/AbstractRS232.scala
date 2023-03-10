package ucesoft.cbm.peripheral.rs232

import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.rs232.RS232._
import ucesoft.cbm.{Clock, ClockEvent, Log}

import java.util.Properties

abstract class AbstractRS232 extends RS232 with ModemCommandListener {
  protected var bitReceivedListener : () => Unit = _
  private[this] var txd,others = 0
  private[this] var stop,parity,bits,length = 0
  protected var dsr: Int = DSR
  protected var cts,dcd,ri = 0
  protected var rts,dtr = false
  private[this] var rxd = RXD
  private[this] var outbuffer = 0
  private[this] var bitreceived,bitsent,tmpParity = 0
  protected var byteToSend: Int = -1
  private[this] var totalByteSent,totalByteReceived = 0
  private[this] var configurationString = ""
  private[this] var enabled = false
  private[this] var statusListener : RS232StatusListener = _
  private[this] var baudCycles = 0
  private[this] val clk = Clock.systemClock
  private[this] var sendState = 0

  protected val modem = new Modem(this)

  def hangUp() : Unit = {}
  def commandMode(on:Boolean): Unit = {}
  def connectTo(address:String): Unit = {
    Log.info(s"RS232 - Connecting to $address")
    setConfiguration(address + "," + configurationString)
    setEnabled(true)
    connect(address)
  }
  def ring(ringing:Boolean): Unit = {
    if (ringing) ri = RI else ri = 0
    println("Ringing..")
  }
  
  def isEnabled: Boolean = enabled
  def setEnabled(enabled:Boolean): Unit = {
    this.enabled = enabled
    if (enabled) {
      clk.schedule(new ClockEvent("RS232-readCycle",clk.currentCycles + baudCycles,_ => readCycle()))
      dcd = RS232.DCD
    }
    else {
      clk.cancel("RS232-readCycle")
      dcd = 0
      disconnect()
    }

  }
  def setRS232Listener(l:RS232StatusListener): Unit = statusListener = l
  
  def init() : Unit = {}
  
  def reset() : Unit = {
    disconnect()
    bitreceived = 0
    outbuffer = 0
    byteToSend = -1
    dcd = 0
    sendState = 0
    modem.reset()
  }
  
  override def getProperties: Properties = {
    properties.setProperty("Total bytes received",totalByteReceived.toString)
    properties.setProperty("Total bytes sent",totalByteSent.toString)
    properties
  }

  def setBitReceivedListener(listener: () => Unit) : Unit = bitReceivedListener = listener
  
  def setTXD(high:Int) : Unit = {
    txd = high
    //println(s"TXD: $txd rts=$rts n=$bitreceived buffer=$outbuffer bits=$bits length=$length")
    if ((!flowControlEnabled || rts) && bitreceived == 0) {
      if (high == 0) { // consumes start bit
        //println("TXD: consumed start bit")
        bitreceived = 1
        if (statusListener != null) statusListener.update(TXD, 1)
      }
      // else ignore stop bit
    }
    else
    if ((!flowControlEnabled || rts) && bitreceived > 0) {
      if (bitreceived < bits + 1) { // ignore parity & stops
        outbuffer |= high << (bitreceived - 1)  
        bitreceived += 1
      }
      else
      if (bitreceived == length - 1) {
        bitreceived = 0
        totalByteSent += 1
        sendOutByte(outbuffer)
        //println("OUTBUFFER => " + outbuffer)
        if (statusListener != null) statusListener.update(TXD,0)
        outbuffer = 0
      }
      else bitreceived += 1
    }
  }

  protected def sendOutByte(byte:Int) : Unit = {
    try {
      if (modem.outputStream != null) {
        modem.outputStream.write(byte)
        //print(byte.toChar)
        modem.outputStream.flush()
      }
    }
    catch {
      case t:Throwable =>
        Log.info(s"I/O error while writing from rs-232 ($componentID): " + t)
        t.printStackTrace()
        disconnect()
    }
  }
  
  def getTXD : Int = txd
  
  protected def checkCTS() : Unit = {
    // auto set cs
    cts = if (!flowControlEnabled || rts) CTS else 0
  }

  def setOthers(value:Int) : Unit = {
    others = value
    rts = (others & RTS) > 0
    dtr = (others & DTR) > 0

    checkCTS()
    //println(s"RTS=$rts DTR=$dtr DCD=$dcd CTS=$cts")
    
    if (statusListener != null) {
      statusListener.update(RTS,others & RTS)
      statusListener.update(DTR,others & DTR)
    }    
  }

  def getOthers : Int = {
    rxd | others & RTS | others & DTR | ri | dcd | cts | dsr
  }

  def getConfiguration: String = configurationString

  /**
   * Syntax: <baud>,<bits>,<parity>,<stops>
   * 
   * parity can be:
   * n no parity
   * e even parity
   * o odd parity
   * m mark parity
   * s space parity 
   */
  def setConfiguration(conf:String) : Unit = {
    configurationString = conf
    val parts = conf.split(",")
    if (parts.length != 4) throw new IllegalArgumentException("Bad configuration string")

    //println(s"BAUD => ${parts(0)} $baudCycles")
    bits = parts(1).toInt
    stop = parts(3).toInt
    modem.setBaud(parts(0).toInt)
    baudCycles = math.round(clk.getClockHz / parts(0).toDouble).toInt
    if (stop != 0 && stop != 1 && stop != 2) throw new IllegalArgumentException("Stop bits must be 0 or 1 or 2")
    parity = parts(2).toUpperCase match {
      case "N" => NO_PARITY
      case "E" => EVEN_PARITY
      case "O" => ODD_PARITY
      case "M" => MARK_PARITY
      case "S" => SPACE_PARITY
      case _ => NO_PARITY
    }
    length = 1 + bits + stop + (if (parity != NO_PARITY) 1 else 0)
    //println(s"RS-232 configured with bits=$bits stop=$stop parity=$parity")
  }

  protected def connect(address:String) : Unit = {
    if (statusListener != null) {
      statusListener.setRS232Enabled(true)
      statusListener.connectedTo(address)
    }
  }

  protected def disconnect() : Unit = {
    if (statusListener != null) {
      statusListener.setRS232Enabled(false)
      statusListener.disconnected()
    }
    modem.commandModeMessage(HayesResultCode.NO_CARRIER)
  }

  protected def isByteAvailable : Boolean = modem.inputStream.available() > 0
  protected def getByte : Int = modem.inputStream.read()
  protected def canSend: Boolean = !flowControlEnabled || (rts && dtr)
  protected def sendRXD(rxdHigh:Boolean) : Unit = {}

  protected def readCycle(): Unit = {
    try {
      if (isByteAvailable && canSend) {
        sendInByte(getByte)
      }
      else clk.schedule(new ClockEvent("RS232-readCycle",clk.currentCycles + baudCycles,_ => readCycle()))
    }
    catch {
      case e:Exception =>
        Log.info(s"RS232 - Error while reading from stream: $e")
        e.printStackTrace()
        disconnect()
        clk.schedule(new ClockEvent("RS232-readCycle",clk.currentCycles + baudCycles,_ => readCycle()))
    }
  }

  protected def sendInByte(byte:Int) : Unit = {
    totalByteReceived += 1
    byteToSend = byte
    sendState = 0
    // send start bit
    rxd = 0
    bitReceivedListener()
    //sendRXD(false)
    bitsent = 1
    tmpParity = 0
    //println(s"Sent start bit ($byteToSend) ${byteToSend.toChar}")
    clk.schedule(new ClockEvent("RS232-in",clk.currentCycles + baudCycles,_ => sendBit()))
    if (statusListener != null) statusListener.update(RXD,1)
  }

  protected def stopin: Int = stop

  protected def sendBit(): Unit = {
    val scheduleNextBit = sendState match {
      case 0 => // DATA BITS
        if ((byteToSend & 1) > 0) rxd = RXD else rxd = 0
        //println(s"Sent ($bitsent/$byteToSend)" + (if (rxd > 0) "1" else "0"))
        byteToSend >>= 1
        if (rxd > 0) tmpParity ^= 1
        bitsent += 1
        if (bitsent == bits + 1) { // DATA BITS FINISHED
          sendState += 1
          if (parity == NO_PARITY) {
            if (stopin == 0) sendState = 4
            else sendState += 1
          } // skip parity
        }
        if (rxd == 0) bitReceivedListener()
        sendRXD(rxd > 0)
        true
      case 1 => // PARITY
        rxd = parity match {
          case ODD_PARITY => if (tmpParity == 0) RXD else 0
          case EVEN_PARITY => if (tmpParity == 1) RXD else 0
          case MARK_PARITY => RXD
          case SPACE_PARITY => 0
        }
        if (stopin == 0) sendState = 4
        else sendState += 1
        if (rxd == 0) bitReceivedListener()
        sendRXD(rxd > 0)
        true
      case 2 => // STOP #1
        rxd = RXD
        sendState += 1
        if (stop == 1) sendState += 1
        sendRXD(rxd > 0)
        //println("Sent stop #1")
        true
      case 3 => // STOP #2
        rxd = RXD
        sendState += 1
        sendRXD(rxd > 0)
        //println("Sent stop #2")
        true
      case 4 => // END
        if (statusListener != null) statusListener.update(RXD,0)
        false
    }
    if (scheduleNextBit) clk.schedule(new ClockEvent("RS232-in",clk.currentCycles + baudCycles,_ => sendBit()))
    else clk.schedule(new ClockEvent("RS232-readCycle",clk.currentCycles + baudCycles,_ => readCycle()))
  }

  def connectionInfo: String = getConfiguration
}