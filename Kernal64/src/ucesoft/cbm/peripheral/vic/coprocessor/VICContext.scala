package ucesoft.cbm.peripheral.vic.coprocessor

trait VICContext {
  def turnOnInterruptControlRegisterBits(value:Int) : Unit
  def isBadlineOnRaster(rasterLine:Int) : Boolean
  def isAECAvailable : Boolean
}
