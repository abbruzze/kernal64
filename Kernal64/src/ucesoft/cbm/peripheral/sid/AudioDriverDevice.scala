package ucesoft.cbm.peripheral.sid

trait AudioDriverDevice {
  def getMasterVolume : Int
  def setMasterVolume(v:Int)
  def setSoundOn(on:Boolean)
  def setMuted(muted:Boolean)
  def isMuted : Boolean
  def isSoundOn : Boolean
  def addSample(sample:Int)
  def reset
  def discard
}