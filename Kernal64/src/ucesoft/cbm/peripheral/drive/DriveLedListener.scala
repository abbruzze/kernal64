package ucesoft.cbm.peripheral.drive

trait DriveLedListener {
  def writeMode(enabled:Boolean)
  def setPowerLedMode(on:Boolean)
  def turnPower(on:Boolean)
  def turnOn
  def turnOff
  def isOn : Boolean
  def beginLoadingOf(fileName:String,indeterminate:Boolean=false)
  def updateLoading(perc:Int)
  def endLoading
  def beginSavingOf(fileName:String)
  def endSaving
}