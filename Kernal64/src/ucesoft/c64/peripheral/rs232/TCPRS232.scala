package ucesoft.c64.peripheral.rs232

import ucesoft.c64.Log
import java.net.Socket

object TCPRS232 extends TCPRS232

class TCPRS232 extends StreamRS232 {
  val componentID = "TCP RS-232"
  
  private[this] var host = ""
  private[this] var port = 0
  private[this] var socket : Socket = _
  private[this] var hostAndConf = ""
  
  def getDescription = "Connects to a tcp server. Connection String syntax: host:port,bits,parity,stops"
  
  override def connectionInfo = hostAndConf
  
  /**
   * Syntax: host:port,bits,parity,stops
   */
  override def setConfiguration(conf:String) {
    hostAndConf = conf
    val parts = conf.split(",")
    if (parts.length != 4) throw new IllegalArgumentException("Bad TCP RS-232 configuration string. Expected <host>:<port>,<bits>,<parity>,<stops>")
    
    super.setConfiguration(conf.substring(conf.indexOf(",") + 1))

    val pars = parts(0).split(":")
    if (pars.length != 2) throw new IllegalArgumentException("Bad TCP RS-232 configuration string. Bad host:port parameter")
    
    host = pars(0)
    port = pars(1).toInt
  }
  
  override def setEnabled(enabled:Boolean) {
    if (!enabled) {
      super.setEnabled(enabled)
      Log.info(s"Disconnecting from $host...")
      try { socket.close } catch { case _t:Throwable => }
    }
    else {      
      Log.info(s"Connecting to $host:$port ...")
      socket = new Socket(host,port)
      setStreams(socket.getInputStream,socket.getOutputStream)
      super.setEnabled(enabled)
    }
  }
  
  override def toString = componentID + (if (isEnabled) "(enabled)" else "")
}