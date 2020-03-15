package ucesoft.cbm.peripheral.rs232

import java.io.IOException

import org.apache.commons.net.telnet.{TelnetClient, TelnetInputListener, TelnetNotificationHandler}
import ucesoft.cbm.{Clock, ClockEvent, Log}

object TelnetRS232 extends TelnetRS232

class TelnetRS232 extends StreamRS232 {
  val componentID = "Telnet (BBS) RS-232"
  
  private[this] var client = new TelnetClient
  private[this] var host = ""
  private[this] var port = 0
  private[this] var config = ""
  private[this] val clk = Clock.systemClock

  def getDescription = "<html><b>Connects to a telnet server (like BBS)</b>.<br>Connection String syntax: <i>host:port,baud,bits,parity,stops</i> to connect or<br><i>baud,bits,parity,stops</i> to use 'at' modem commands</html>"
  
  /**
   * Syntax: host:baud,port,bits,parity,stops
   */
  override def setConfiguration(conf:String) {
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
    config = conf
  }
  
  override def connectionInfo = config
  
  override def setEnabled(enabled:Boolean) {
    val lastEnabled = isEnabled    
    
    if (enabled) {
      if (lastEnabled) {
        Log.info(s"Disconnecting from $host...")
        try { client.disconnect } catch { case _:Throwable => }
        super.setEnabled(false)
      }
      if (host != "") {
        Log.info(s"Connecting to $host:$port...")
        client = new TelnetClient
        try {
          client.connect(host, port)
          // wait 1 sec to show CONNECT message
          modem.commandModeMessage(HayesResultCode.CONNECT)
          clk.schedule(new ClockEvent("RS232-waitConnect", clk.currentCycles + 1000000, _ => {
            setStreams(client.getInputStream, client.getOutputStream,s"$host:$port")
          }))
        }
        catch {
          case io:IOException =>
            Log.info(s"Telnet: Cannot connect to $host:$port. " + io)
            disconnect
        }
      }
    }
    else {
      disconnect
      Log.info(s"Disconnected from $host")
    }
    
    super.setEnabled(enabled)
  }

  override def disconnect : Unit = {
    try { client.disconnect } catch { case _:Throwable => }
    super.disconnect
  }
  
  override def toString = componentID + (if (isEnabled) "(enabled)" else "")
}