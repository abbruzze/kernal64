package ucesoft.c64.peripheral.rs232

import org.apache.commons.net.telnet.TelnetClient
import ucesoft.c64.Log

object TelnetRS232 extends StreamRS232 {
  val componentID = "Telnet RS-232"
  
  private[this] var client = new TelnetClient
  private[this] var host = ""
  private[this] var port = 0
  private[this] var config = ""
  
  def getDescription = "Connects to a telnet server. Connection String syntax: host:port,baud,bits,parity,stops"
  
  /**
   * Syntax: host:baud,port,bits,parity,stops
   */
  override def setConfiguration(conf:String) {
    val parts = conf.split(",")
    if (parts.length != 5) throw new IllegalArgumentException("Bad Telnet RS-232 configuration string. Expected <host>:<port>,<baud>,<bits>,<parity>,<stops>")
    
    super.setConfiguration(conf.substring(conf.indexOf(",") + 1))

    val pars = parts(0).split(":")
    if (pars.length != 2) throw new IllegalArgumentException("Bad Telnet RS-232 configuration string. Bad host:port parameter")
    
    host = pars(0)
    port = pars(1).toInt
    config = conf
  }
  
  override def connectionInfo = config
  
  override def setEnabled(enabled:Boolean) {
    val lastEnabled = isEnabled    
    
    if (enabled) {
      if (lastEnabled) {
        Log.info(s"Disconnecting to $host...")
        try { client.disconnect } catch { case _:Throwable => }
        super.setEnabled(false)
      }
      
      Log.info(s"Connecting to $host:$port...")
      client = new TelnetClient
      client.connect(host,port)
      setStreams(client.getInputStream,client.getOutputStream)
    }
    else {
      try { client.disconnect } catch { case _:Throwable => }
      Log.info(s"Disconnected from $host")
    }
    
    super.setEnabled(enabled)
  }
  
  override def toString = componentID + (if (isEnabled) "(enabled)" else "")
}