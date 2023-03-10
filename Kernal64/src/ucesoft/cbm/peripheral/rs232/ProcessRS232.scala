package ucesoft.cbm.peripheral.rs232

import ucesoft.cbm.Log

object ProcessRS232 extends StreamRS232 {
  val componentID = "Process RS-232"
  private[this] var process : Process = _
  private[this] var processStr : String = _ 
  private[this] var config = ""
  
  def getDescription = "Connects to an external process and uses its standard input/output as channels. Connection String syntax: external process,baud,bits,parity,stops"
  
  /**
   * Syntax: external process,bits,parity,stops
   */
  override def setConfiguration(conf:String) : Unit = {
    val parts = conf.split(",")
    if (parts.length != 5) throw new IllegalArgumentException("Bad Process RS-232 configuration string. Expected <external process>,<baud>,<bits>,<parity>,<stops>")
    
    super.setConfiguration(conf.substring(conf.indexOf(",") + 1))
    processStr = parts(0)
    config = conf
  }
  
  override def connectionInfo: String = config
  
  override def setEnabled(enabled:Boolean) : Unit = {
    val lastEnabled = isEnabled
    
    if (enabled) {
      if (lastEnabled) disconnect()
      val pars = processStr.split(" ")
      val pb = new ProcessBuilder(pars:_*)
      process = pb.start
      Log.info(s"$processStr started")
      setStreams(process.getInputStream,process.getOutputStream,processStr)
    }
    else disconnect()
    super.setEnabled(enabled)
  }
  
  override def disconnect() : Unit = {
    try {
      if (process != null) process.destroy()
      super.disconnect()
      Log.info(s"Process $processStr terminated")
    }
    catch {
      case _:Throwable =>
    }
  }
  
  override def toString: String = componentID + (if (isEnabled) "(enabled)" else "")
}