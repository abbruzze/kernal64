package ucesoft.cbm.peripheral.rs232

/**
  * To be investigated ...
  */
object UP9600 extends TelnetRS232 {
  override val componentID = "UP9600"
  override def setEnabled(enabled:Boolean): Unit = {
    super.setEnabled(enabled)
    if (enabled) {
      cia1.setSerialOUT(bit => {
        setTXD(if (bit) 1 else 0)
        //println("OUT: " + (if (bit) "1" else 0))
      })
    }
    else {
      cia1.setSerialOUT(null)
    }
  }

  override def sendRXD(rxdHigh:Boolean): Unit = {
    cia2.serialIN(rxdHigh)
  }

  override protected def stopin = 0

  override def setConfiguration(conf:String) : Unit = {
    super.setConfiguration(conf + ",8,n,1")
  }

  override def getDescription = "<html><b>Connects to a telnet server using UP9600 hack</b>.<br>Connection String syntax: <i>host:port,baud</i> to connect or<br><i>baud</i> to use 'at' modem commands</html>"
}
