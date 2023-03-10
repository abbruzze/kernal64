package ucesoft.cbm.c64

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.{Memory, RAMComponent}
import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.{CBMComponentType, ChipID}

import java.io.{ObjectInputStream, ObjectOutputStream}

class ExtendedROM(ram: Memory,val name:String,val startAddress:Int) extends RAMComponent {    
  import ExpansionPort._
  val componentID: String = "Extended " + name
  val componentType: Type = CBMComponentType.MEMORY
  val length = 8192
  val isRom = true
  
  private[this] var active = false
  final private[this] val isROML = name == "ROML"
    
  final def isActive: Boolean = active
  def setActive(active:Boolean): Unit = this.active = active
  def init() : Unit = {}
  def reset() : Unit = active = false
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
    if (selectedROM != null) selectedROM.read(address,chipID)
    else ram.read(address,chipID)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
    if (selectedROM == null) ram.write(address,value,chipID)
    else selectedROM.write(address,value)
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
}