package ucesoft.c64.peripheral.rs232

import ucesoft.c64.peripheral.cia.CIA
import RS232._
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent

abstract class AbstractRS232 extends RS232 with Runnable {
  private[this] var cia1,cia2 : CIA = _
  private[this] var txd,others = 0
  private[this] var stop,parity,bits,length = 0
  protected var dsr = DSR
  protected var cts,dcd,ri = 0
  protected var rts,dtr = false
  @volatile private[this] var rxd = RXD
  private[this] var outbuffer = 0
  @volatile private[this] var bitreceived,bitsent,tmpParity = 0
  @volatile private[this] var byteToSend = -1
  private[this] var totalByteSent,totalByteReceived = 0
  private[this] var configurationString = ""
  private[this] var enabled = false
  private[this] var statusListener : RS232StatusListener = _
  private[this] val sendingLock = new Object
  private[this] var baudCycles = 0
  private[this] val clk = Clock.systemClock
  
  def isEnabled = enabled
  def setEnabled(enabled:Boolean) = {
    this.enabled = enabled
    if (statusListener != null) statusListener.setRS232Enabled(enabled)
  }
  def setRS232Listener(l:RS232StatusListener) = statusListener = l
  
  def init {}
  
  def reset {
    setEnabled(false)
    bitreceived = 0
    outbuffer = 0
    byteToSend = -1
    dcd = 0
  }
  
  override def getProperties = {
    properties.setProperty("Total bytes received",totalByteReceived.toString)
    properties.setProperty("Total bytes sent",totalByteSent.toString)
    properties
  }
  
  def setCIA(cia2:CIA) {
    this.cia2 = cia2
  }
  
  def setTXD(high:Int) {
    txd = high
    //println(s"TXD: $txd rts=$rts n=$bitreceived buffer=$outbuffer bits=$bits length=$length")
    if (rts && high == 0 && bitreceived == 0) {
      // consumes start bit
      bitreceived = 1    
      if (statusListener != null) statusListener.update(TXD,1)
    }
    else
    if (rts && bitreceived > 0) {
      if (bitreceived < bits + 1) { // ignore parity & stops
        outbuffer |= high << (bitreceived - 1)  
        bitreceived += 1
      }
      else
      if (bitreceived == length - 1) {
        bitreceived = 0
        totalByteSent += 1
        sendOutByte(outbuffer)
        if (statusListener != null) statusListener.update(TXD,0)
        outbuffer = 0
      }
      else bitreceived += 1
    }
  }
  
  def getTXD : Int = {
    txd
  }
  
  protected def checkRTSandDTRReset {
    if (rts && dtr) { // reset
      bitreceived = 0
      outbuffer = 0
      byteToSend = -1
//      dcd = 0
    }
  }
  
  protected def checkCTS {
    // auto set cs
    cts = if (rts) CTS else 0
  }
  
  def setOthers(value:Int) {
    others = value
    rts = (others & RTS) > 0
    dtr = (others & DTR) > 0
    
    checkRTSandDTRReset
    checkCTS   
    //println(s"RTS=$rts DTR=$dtr DCD=$dcd CTS=$cts")
    
    if (statusListener != null) {
      statusListener.update(RTS,others & RTS)
      statusListener.update(DTR,others & DTR)
    }    
  }
  
  def getOthers : Int = {        
    //println(s"Read rxd=" + (if (rxd > 0) "1" else "0"))
    val ret = rxd | others & RTS | others & DTR | ri | dcd | cts | dsr
    //checkSendBit
    ret
  }
  
  def getConfiguration = configurationString
  
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
  def setConfiguration(conf:String) {
    configurationString = conf
    val parts = conf.split(",")
    if (parts.length != 4) throw new IllegalArgumentException("Bad configuration string")
    
    baudCycles = (clk.getClockHz / parts(0).toInt).toInt
    //println(s"BAUD => ${parts(0)} $baudCycles")
    bits = parts(1).toInt
    stop = parts(3).toInt    
    if (stop != 1 && stop != 2) throw new IllegalArgumentException("Stop bits must be 1 or 2")
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
  
  protected def isSending = byteToSend != -1
  protected def waitingSending {
    if (isSending) sendingLock.synchronized {
      sendingLock.wait
      while (isSending) sendingLock.wait
    }
  }
    
  protected def sendOutByte(byte:Int)
  
  protected def sendInByte(byte:Int) = sendingLock.synchronized {
    totalByteReceived += 1
    byteToSend = byte
    // send start bit
    rxd = 0
//    dcd = DCD
    cia2.setFlagLow    
    bitsent = 1
    tmpParity = 0      
    //println("Sent start bit")
    clk.scheduleExternal(new ClockEvent("RS232-in",clk.currentCycles + baudCycles,c => sendBit))    
    if (statusListener != null) statusListener.update(RXD,1)
  }
  
  protected def sendBit {    
    if (bitsent == bits + 1 && parity != NO_PARITY) { // parity
      rxd = parity match {
        case ODD_PARITY => if (tmpParity == 0) RXD else 0
        case EVEN_PARITY => if (tmpParity == 1) RXD else 0
        case MARK_PARITY => RXD
        case SPACE_PARITY => 0
      }
    }
    else
    if (bitsent >= bits + 1) { // stop bits
      rxd = RXD
      //println("Sent stop bit")
    }
    else {
      if ((byteToSend & 1) > 0) rxd = RXD else rxd = 0
      byteToSend >>= 1
      if (rxd > 0) tmpParity ^= 1
      //println("Sent " + (if (rxd > 0) "1" else "0"))
    }
    bitsent += 1
    if (bitsent == length + 1) {
//      dcd = 0      
      sendingLock.synchronized {        
        byteToSend = -1
        sendingLock.notify
      }
      if (statusListener != null) statusListener.update(RXD,0)     
    }
    else clk.scheduleExternal(new ClockEvent("RS232-in",clk.currentCycles + baudCycles,c => sendBit))
  }
  
  def connectionInfo = getConfiguration
}