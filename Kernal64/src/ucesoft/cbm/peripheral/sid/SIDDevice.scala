package ucesoft.cbm.peripheral.sid

import ucesoft.cbm.cpu.RAMComponent

trait SIDDevice extends RAMComponent {
  def start() : Unit
  def stop() : Unit
  def setFullSpeed(full:Boolean) : Unit
  def getDriver : AudioDriverDevice
  def clock() : Unit
  def setCycleExact(ce:Boolean): Unit
}