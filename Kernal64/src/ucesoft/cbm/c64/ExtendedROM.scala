package ucesoft.cbm.c64

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class ExtendedROM(ram: Memory,val name:String,val startAddress:Int) extends RAMComponent {    
  import ExpansionPort._
  val componentID = "Extended " + name
  val componentType = CBMComponentType.MEMORY
  val length = 8192
  val isRom = true
  
  private[this] var active = false
  final private[this] val isROML = name == "ROML"
    
  final def isActive = active
  def setActive(active:Boolean) = this.active = active
  def init : Unit = {}
  def reset : Unit = active = false
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
    if (selectedROM != null) selectedROM.read(address,chipID)
    else ram.read(address,chipID)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {
    val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
    if (selectedROM == null) ram.write(address,value,chipID)
    else selectedROM.write(address,value)
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
}