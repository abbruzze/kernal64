package ucesoft.cbm.peripheral.vic.coprocessor

trait VICContext {
  def turnOnInterruptControlRegisterBits(value:Int) : Unit
  def isBadlineOnRaster(rasterLine:Int) : Boolean
  def currentRasterLine : Int
  def currentRasterCycle : Int
  def baState : Boolean
  def forceBadLine(bad:Int) : Unit
}
