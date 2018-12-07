package ucesoft.cbm.peripheral.rs232

import java.io.OutputStream
import java.io.InputStream
import ucesoft.cbm.Log
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent

abstract class StreamRS232 extends AbstractRS232 {

  override def hangUp : Unit = {
    disconnect
  }

  override protected def disconnect: Unit = {
    setStreams(null,null,null)
    super.disconnect
  }

  def setStreams(in:InputStream,out:OutputStream,address:String) {
    modem.setStreams(in,out)
    if (in == null) {
      dcd = RS232.DCD
      super.disconnect
    }
    else {
      dcd = 0
      connect(address)
    }
  }
  
  def getStreams = (modem.inputStream,modem.outputStream)
}