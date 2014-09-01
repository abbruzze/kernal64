package ucesoft.c64.peripheral.sid

trait AudioDriver {
  def write(buffer:Array[Byte])
  def getMicros : Long
  def hasSound : Boolean
  def available : Int
  def getMasterVolume : Int
  def setMasterVolume(v:Int)
  def shutdown
  def setSoundOn(on:Boolean)
  def setFullSpeed(full:Boolean)
  def fullSpeed : Boolean
}