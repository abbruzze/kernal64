package ucesoft.cbm.peripheral.rs232

trait RS232StatusListener {
  def update(signal:Int,value:Int) : Unit
  def setRS232Enabled(enabled:Boolean) : Unit
  def connectedTo(address:String) : Unit
  def disconnected : Unit
}