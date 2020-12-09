package ucesoft.cbm.peripheral.vic.coprocessor

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.vic.VICModel

trait VICContext extends Memory {
  def turnOnInterruptControlRegisterBits(value:Int) : Unit
  def isAECAvailable : Boolean
  def getVICModel : VICModel
}
