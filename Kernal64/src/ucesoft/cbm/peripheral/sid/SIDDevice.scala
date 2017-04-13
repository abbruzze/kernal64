package ucesoft.cbm.peripheral.sid

import ucesoft.cbm.cpu.RAMComponent

trait SIDDevice extends RAMComponent {
  def start
  def stop
  def setFullSpeed(full:Boolean)
  def getDriver : AudioDriverDevice
}