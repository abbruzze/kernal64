package ucesoft.c64.peripheral.rs232

import ucesoft.c64.peripheral.cia.CIA

object BridgeRS232 extends RS232 {
  val componentID = "RS-232 on UserPort"
  private[this] var rs232 : RS232 = _
  private[this] var txd,others = 0
  private[this] var cia2 : CIA = _
  private[this] var statusListener : RS232StatusListener = _
  
  def init {}
  def reset = if (rs232 != null) rs232.reset
  
  def setRS232Listener(l:RS232StatusListener) = statusListener = l
  
  override def getProperties = {
    if (rs232 == null) {
      properties.setProperty("Total bytes received","-")
      properties.setProperty("Total bytes sent","-")
      properties
    }
    else rs232.getProperties
  }
  
  def setTXD(high:Int) = if (rs232 != null) rs232.setTXD(high) else txd = high
  def getTXD : Int = if (rs232 == null) txd else rs232.getTXD  
  def setOthers(value:Int) = if (rs232 != null) rs232.setOthers(value) else others = value
  def getOthers : Int = if (rs232 == null) others else rs232.getOthers
  def setConfiguration(conf:String) = if (rs232 != null) rs232.setConfiguration(conf)
  def getConfiguration : String = if (rs232 == null) "" else rs232.getConfiguration
  def isEnabled : Boolean = if (rs232 != null) rs232.isEnabled else false
  def setEnabled(enabled:Boolean) = if (rs232 != null) rs232.setEnabled(enabled)
  def setCIA(cia2:CIA) = this.cia2 = cia2
  def getDescription : String = "No RS-232 attached"
  def connectionInfo = if (rs232 != null) rs232.connectionInfo else ""
  
  def setRS232(rs232:RS232) {
    this.rs232 = rs232
    rs232.setRS232Listener(statusListener)
    rs232.setCIA(cia2)
    rs232.setOthers(others)    
  }    
}