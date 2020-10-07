package ucesoft.cbm.peripheral.vic.coprocessor

import ucesoft.cbm.cpu.Memory

trait VICContext extends Memory {
  def turnOnInterruptControlRegisterBits(value:Int) : Unit
  def isAECAvailable : Boolean
}
