package ucesoft.c64.peripheral.sid

import ucesoft.c64.cpu.RAMComponent

trait SIDDevice extends RAMComponent {
  def start
  def stop
  def setFullSpeed(full:Boolean)
  def getDriver : AudioDriverDevice
}