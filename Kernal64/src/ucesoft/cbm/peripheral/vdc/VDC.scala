package ucesoft.cbm.peripheral.vdc

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.Log

class VDC extends RAMComponent {
  val name = "C128 VDC"
  val componentID = "C128_VDC"
  val isRom = false
  val startAddress = 0xD600
  val length = 0x2
  val componentType = CBMComponentType.MEMORY
  val isActive = true
  
  final private[this] val RAM_SIZE = 0x10000 
  private[this] val ram = Array.ofDim[Int](RAM_SIZE)
  private[this] var address_reg = 0
  private[this] val regs = Array.ofDim[Int](0x24)
  final private[this] val VDC_VERSION = 0
  private[this] var vblank = 0
  
  final def init {
    
  }
  
  final def reset {
    
  }
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    // TODO
    address & 1 match {
      case 0 => 0x80 | vblank << 5 | VDC_VERSION // always status = 1
      case 1 => regs(address_reg)
    }
  }
  
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    // TODO
    address & 1 match {
      case 0 => 
        address_reg = value & 0x3F
        Log.debug(s"VDC register set to ${Integer.toHexString(address_reg)}")
      case 1 => 
        regs(address_reg) = value
        Log.debug(s"VDC writing ${Integer.toHexString(value)} to register ${Integer.toHexString(address_reg)}")
    }
  }
  
  // state -----------------------------------------------  
  protected def saveState(out:ObjectOutputStream) {
    // TODO
  }
  protected def loadState(in:ObjectInputStream) {
    // TODO
  }
  protected def allowsStateRestoring(parent:JFrame) = {
    true
    // TODO
  }
}