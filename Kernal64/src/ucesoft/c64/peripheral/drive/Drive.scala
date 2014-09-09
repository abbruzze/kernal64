package ucesoft.c64.peripheral.drive

import ucesoft.c64.formats.D64
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

trait Drive extends C64Component {
  val componentType = C64ComponentType.DISK
  
  def setActive(active:Boolean) {}
  def setCanSleep(canSleep:Boolean) {}
  def setDriveReader(driveReader:D64)
  def clock(cycles:Long)
}