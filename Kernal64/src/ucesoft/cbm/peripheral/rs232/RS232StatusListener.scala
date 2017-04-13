package ucesoft.cbm.peripheral.rs232

trait RS232StatusListener {
  def update(signal:Int,value:Int)
  def setRS232Enabled(enabled:Boolean)
}