package ucesoft.cbm.peripheral.drive

trait DriveLedListener {
  def writeMode(enabled:Boolean) : Unit
  def setPowerLedMode(on:Boolean) : Unit
  def turnPower(on:Boolean) : Unit
  def turnOn : Unit
  def turnOff : Unit
  def isOn : Boolean
  def moveTo(track:Int,sector:Option[Int],halfTrack:Boolean) : Unit
}