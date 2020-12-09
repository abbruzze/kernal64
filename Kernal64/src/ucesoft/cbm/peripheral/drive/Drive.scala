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
  val formatExtList : List[String]
  var runningListener : (Boolean) => Unit = _
  
  def disconnect : Unit = {}
  def setActive(active:Boolean) : Unit = {}
  def isRunning : Boolean = false
  def setCanSleep(canSleep:Boolean) : Unit = {}
  def canGoSleeping : Boolean = false
  def setDriveReader(driveReader:Floppy,emulateInserting:Boolean) : Unit
  def clock(cycles:Long) : Unit
  def setReadOnly(readOnly:Boolean) : Unit = {}
  def isReadOnly : Boolean = false
  def getFloppy : Floppy
  def getMem : Memory = Memory.empty
  
  def getSpeedHz = -1  
  def setSpeedHz(speed:Int) : Unit = {}
  val MIN_SPEED_HZ = 0
  val MAX_SPEED_HZ = 0
}