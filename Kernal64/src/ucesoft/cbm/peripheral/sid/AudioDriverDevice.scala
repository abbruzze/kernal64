package ucesoft.cbm.peripheral.sid

trait AudioDriverDevice {
  def getMasterVolume : Int
  def setMasterVolume(v:Int) : Unit
  def setSoundOn(on:Boolean) : Unit
  def setMuted(muted:Boolean) : Unit
  def isMuted : Boolean
  def isSoundOn : Boolean
  def addSample(sample:Int) : Unit
  def reset() : Unit
  def discard() : Unit
}