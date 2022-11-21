package ucesoft.cbm.c128

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponentType, ChipID, Log}
import ucesoft.cbm.cpu.RAMComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

class ColorRAM extends RAMComponent {
  val componentID = "128 COLOR RAM"
  val componentType: Type = CBMComponentType.MEMORY
  
  val isRom = false
  val name = "128_COLOR_RAM"
  val startAddress = 0xD800
  val length = 0x400

  private[this] val mem = Array.ofDim[Int](2,length)
  private[this] var processorBank,vicBank = 0
  
  final val isActive = true
  final def init  : Unit = {}
  final def reset  : Unit = {
    for(b <- 0 until 2;i <- 0 until length) mem(b)(i) = 0xFF
    processorBank = 0
    vicBank = 0
  }
  
  /**
   * Set the color bank seen by the processor and by the VIC
   * 
   * $01 (bit 0-1)
   */
  final def setProcessorAndVICColorBanks(pvBank:Int): Unit = {
    processorBank = pvBank & 1
    vicBank = (pvBank & 2) >> 1
    Log.debug(s"Color banks: set $processorBank for processor and $vicBank for VIC")
  }
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val bank = if (chipID == ChipID.VIC) vicBank else processorBank
    mem(bank)(address & 0x3FF)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    val bank = if (chipID == ChipID.VIC) vicBank else processorBank
    mem(bank)(address & 0x3FF) = value & 0xff
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeObject(mem(0))
    out.writeObject(mem(1))
    out.writeInt(processorBank)
    out.writeInt(vicBank)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    loadMemory[Int](mem(0),in)
    loadMemory[Int](mem(1),in)
    processorBank = in.readInt
    vicBank = in.readInt
  }
  protected def allowsStateRestoring : Boolean = true
}
 