package ucesoft.c64.peripheral.drive

trait DriveLedListener {
  def turnOn
  def turnOff
  def isOn : Boolean
  def beginLoadingOf(fileName:String,indeterminate:Boolean=false)
  def updateLoading(perc:Int)
  def endLoading
  def beginSavingOf(fileName:String)
  def endSaving
}