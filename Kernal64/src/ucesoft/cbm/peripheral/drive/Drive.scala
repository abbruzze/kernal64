package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType

trait Drive extends CBMComponent {
  val componentType = CBMComponentType.DISK
  protected var isRunningListener : (Boolean) => Unit = x => {}
  
  def setActive(active:Boolean) {}
  def setCanSleep(canSleep:Boolean) {}
  def setIsRunningListener(listener: (Boolean) => Unit) = isRunningListener = listener
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean)
  def clock(cycles:Long)
  def setReadOnly(readOnly:Boolean) {}
  def getFloppy : Floppy
  
  def getSpeedHz = -1  
  def setSpeedHz(speed:Int) {}
  val MIN_SPEED_HZ = 0
  val MAX_SPEED_HZ = 0
}