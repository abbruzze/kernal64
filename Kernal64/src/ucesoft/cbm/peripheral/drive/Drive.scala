package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.{CBMComponent, CBMComponentType}

object DriveType extends Enumeration {
  val _1541: DriveType.Value = Value("1541")
  val _1571: DriveType.Value = Value("1571")
  val _1581: DriveType.Value = Value("1581")
  val OTHER: DriveType.Value = Value("OTHER")
  val LOCAL: DriveType.Value = Value("LOCAL")
}

trait Drive extends CBMComponent {
  val componentType: Type = CBMComponentType.DISK
  val driveType : DriveType.Value
  val formatExtList : List[String]
  var runningListener : (Boolean) => Unit = _
  
  def disconnect() : Unit = {}
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
  
  def getSpeedHz: Int = -1
  def setSpeedHz(speed:Int) : Unit = {}
  val MIN_SPEED_HZ = 0
  val MAX_SPEED_HZ = 0
}