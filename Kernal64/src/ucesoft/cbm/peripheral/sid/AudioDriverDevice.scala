package ucesoft.cbm.peripheral.sid

trait AudioDriverDevice {
  def getMasterVolume : Int
  def setMasterVolume(v:Int)
  def setSoundOn(on:Boolean)
  def addSample(sample:Int)
  def reset
  def discard
}