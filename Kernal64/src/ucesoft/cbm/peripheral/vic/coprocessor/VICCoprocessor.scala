package ucesoft.cbm.peripheral.vic.coprocessor

trait VICCoprocessor {
  val readOffset : Int
  val controlRegisterMask : Int
  val interruptMaskRegisterMask : Int

  def isActive : Boolean
  def cycle(rasterLine:Int,rasterCycle:Int) : Unit
  def readReg(reg:Int) : Int
  def writeReg(reg:Int,value:Int) : Unit
  def reset : Unit
  def g_access(rasterCycle:Int) : Int
}
