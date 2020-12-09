package ucesoft.cbm.peripheral.drive

trait DriveLedListener {
  def writeMode(enabled:Boolean) : Unit
  def setPowerLedMode(on:Boolean) : Unit
  def turnPower(on:Boolean) : Unit
  def turnOn : Unit
  def turnOff : Unit
  def isOn : Boolean
  def beginLoadingOf(fileName:String,indeterminate:Boolean=false) : Unit
  def updateLoading(perc:Int) : Unit
  def endLoading : Unit
  def beginSavingOf(fileName:String) : Unit
  def endSaving : Unit
}