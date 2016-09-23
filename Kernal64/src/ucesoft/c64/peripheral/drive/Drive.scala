package ucesoft.c64.peripheral.drive

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

trait Drive extends C64Component {
  val componentType = C64ComponentType.DISK
  protected var isRunningListener : (Boolean) => Unit = x => {}
  
  def setActive(active:Boolean) {}
  def setCanSleep(canSleep:Boolean) {}
  def setIsRunningListener(listener: (Boolean) => Unit) = isRunningListener = listener
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean)
  def clock(cycles:Long)
  def changeCPU(cycleExact:Boolean) {}
  def setReadOnly(readOnly:Boolean) {}
  def getFloppy : Floppy
  
  def getSpeedHz = -1  
  def setSpeedHz(speed:Int) {}
  val MIN_SPEED_HZ = 0
  val MAX_SPEED_HZ = 0
}