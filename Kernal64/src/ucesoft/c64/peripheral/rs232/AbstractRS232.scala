package ucesoft.c64.peripheral.rs232

import ucesoft.c64.peripheral.cia.CIA

abstract class AbstractRS232 extends RS232 with Runnable {
  private[this] var cia2 : CIA = _
  private[this] var txd,others = 0
  private[this] var stop,parity,bits,length = 0
  protected var dsr = DSR
  protected var cts,dcd,ri = 0
  protected var rts,dtr = false
  private[this] var rxd = RXD
  private[this] var outbuffer = 0
  private[this] var bitreceived,bitsent,byteToSend,tmpParity = 0
  private[this] var totalByteSent,totalByteReceived = 0
  private[this] var configurationString = ""
  private[this] var enabled = false
  
  def isEnabled = enabled
  def setEnabled(enabled:Boolean) = this.enabled = enabled
  
  def init {}
  
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
    //println(s"TXD: $txd rts=$rts n=$bitreceived buffer=$outbuffer")
    if (rts && high == 0 && bitreceived == 0) {
      // consumes start bit
      bitreceived = 1      
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
        outbuffer = 0
      }
      else bitreceived += 1
    }
  }
  
  def getTXD : Int = {
    txd
  }
  
  def setOthers(value:Int) {
    others = value
    rts = (others & RTS) > 0
    dtr = (others & DTR) > 0
    
    if (rts && dtr) { // reset
      bitreceived = 0
      outbuffer = 0
      byteToSend = -1
      dcd = 0
    }
    
    // auto set cs
    cts = if (rts) CTS else 0 
    //println(s"RTS=$rts DTR=$dtr")
  }
  
  def getOthers : Int = {        
    //println(s"Read rxd=" + (if (rxd > 0) "1" else "0"))
    val ret = rxd | others & RTS | others & DTR | ri | dcd | cts | dsr
    if (byteToSend != -1) sendBit
    ret
  }
  
  def getConfiguration = configurationString
  
  /**
   * Syntax: <bits>,<parity>,<stops>
   * 
   * parity can be:
   * n no parity
   * e even parity
   * o odd parity
   * m mark parity
   * s space parity 
   */
  def setConfiguration(conf:String) {
    val parts = conf.split(",")
    if (parts.length != 3) throw new IllegalArgumentException("Bad configuration string")
    
    bits = parts(0).toInt
    stop = parts(2).toInt    
    parity = parts(1).toUpperCase match {
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
  
  protected def sendOutByte(byte:Int)
  
  protected def sendInByte(byte:Int) {
    totalByteReceived += 1
    byteToSend = byte
    // send start bit
    rxd = 0
    dcd = DCD
    cia2.setFlagLow
    bitsent = 1
    tmpParity = 0
    //println("Sent start bit")
  }
  
  private def sendBit {
    val old_rxd = rxd
    if (bitsent == bits + 1 && parity != NO_PARITY) { // parity
      rxd = parity match {
        case ODD_PARITY => if (tmpParity == 0) RXD else 0
        case EVEN_PARITY => if (tmpParity == 1) RXD else 0
        case MARK_PARITY => RXD
        case SPACE_PARITY => 0
      }
      println("Sent parity " + (if (rxd > 0) "1" else "0"))
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
    //if (old_rxd > 0 && rxd == 0) cia2.setFlagLow
    if (bitsent == length) {
      dcd = 0      
      byteToSend = -1
      //println("BYTE FINISHED")
    }
  }
}