package ucesoft.c64.peripheral.sid

trait AudioDriverDevice {
  def getMasterVolume : Int
  def setMasterVolume(v:Int)
  def setSoundOn(on:Boolean)
}