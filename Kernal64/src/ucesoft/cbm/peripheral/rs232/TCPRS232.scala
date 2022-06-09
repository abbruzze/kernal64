package ucesoft.cbm.peripheral.rs232

import ucesoft.cbm.Log

import java.io.IOException
import java.net.Socket

object TCPRS232 extends TCPRS232

class TCPRS232 extends StreamRS232 {
  val componentID = "TCP RS-232"
  
  private[this] var host = ""
  private[this] var port = 0
  private[this] var socket : Socket = _
  private[this] var hostAndConf = ""
  
  def getDescription = "<html><b>Connects to a TCP server</b>.<br>Connection String syntax: <i>host:port,baud,bits,parity,stops</i> to connect or<br><i>baud,bits,parity,stops</i> to use 'at' modem commands</html>"
  
  override def connectionInfo: String = hostAndConf
  
  /**
   * Syntax: host:port,baud,bits,parity,stops
   */
  override def setConfiguration(conf:String) : Unit = {
    hostAndConf = conf
    val parts = conf.split(",")
    val confString = parts.length match {
      case 4 =>
        conf
      case 5 =>
        conf.substring(conf.indexOf(",") + 1)
      case _ =>
        throw new IllegalArgumentException("Bad Telnet RS-232 configuration string. Expected [<host>:<port>,<baud>,]<bits>,<parity>,<stops>")
    }
    super.setConfiguration(confString)

    if (parts.length == 5) {
      val pars = parts(0).split(":")
      if (pars.length != 2) throw new IllegalArgumentException("Bad Telnet RS-232 configuration string. Bad host:port parameter")

      host = pars(0)
      port = pars(1).toInt
    }
    else {
      host = ""
    }
  }
  
  override def setEnabled(enabled:Boolean) : Unit = {
    if ((isEnabled || !enabled) && host != "") {
      Log.info(s"Disconnecting from $host...")
      try {
        if (socket != null) socket.close()
      } catch {
        case _: Throwable =>
      }
      disconnect
      super.setEnabled(false)
    }
    if (enabled && host != "") {
      Log.info(s"Connecting to $host:$port ...")
      try {
        socket = new Socket(host, port)
        setStreams(socket.getInputStream, socket.getOutputStream,s"$host:$port")
        super.setEnabled(enabled)
      }
      catch {
        case io:IOException =>
          Log.info(s"Cannot connect to $host:$port. " + io)
          disconnect
      }
    }
  }
  
  override def toString: String = componentID + (if (isEnabled) "(enabled)" else "")
}