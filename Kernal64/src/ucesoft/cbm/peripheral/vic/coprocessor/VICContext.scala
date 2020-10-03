package ucesoft.cbm.peripheral.vic.coprocessor

trait VICContext {
  def turnOnInterruptControlRegisterBits(value:Int) : Unit
  def isAECAvailable : Boolean
}
