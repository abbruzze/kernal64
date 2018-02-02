package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.cpu.Memory

object DriveType extends Enumeration {
  val _1541 = Value
  val _1571 = Value
  val _1581 = Value
  val OTHER = Value
  val LOCAL = Value
}

trait Drive extends CBMComponent {
  val componentType = CBMComponentType.DISK
  val driveType : DriveType.Value
  protected var isRunningListener : (Boolean) => Unit = x => {}
  
  def disconnect {}
  def setActive(active:Boolean) {}
  def setCanSleep(canSleep:Boolean) {}
  def setIsRunningListener(listener: (Boolean) => Unit) = isRunningListener = listener
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean)
  def clock(cycles:Long)
  def setReadOnly(readOnly:Boolean) {}
  def getFloppy : Floppy
  def getMem : Memory = Memory.empty
  
  def getSpeedHz = -1  
  def setSpeedHz(speed:Int) {}
  val MIN_SPEED_HZ = 0
  val MAX_SPEED_HZ = 0
}