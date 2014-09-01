package ucesoft.c64.peripheral.drive

import ucesoft.c64.cpu.CPU6510Mems.ROM
import ucesoft.c64.ChipID
import ucesoft.c64.cpu.Memory
import ucesoft.c64.Log
import ucesoft.c64.cpu.BridgeMemory
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.cpu.RAMComponent

object C1541Mems {
  val KERNEL_M = 0xC000
  
  private class DISK_KERNEL extends ROM(null,"C1541_KERNEL",KERNEL_M,16384,"roms/c1541.rom") {
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
  }
  
  private class RAM extends RAMComponent {
    val componentID = "DISK RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_RAM"
    val startAddress = 0x0
    val length = 0x0800

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    
    def init {
      Log.info("Initialaizing C1541 RAM memory ...")
    }
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0
    }
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address & 0xFFFF)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address & 0xFFFF) = value & 0xff    
  }
  
  class C1541_RAM extends BridgeMemory {
    val componentID = "MAIN DISK RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_MAIN_RAM"
    val startAddress = 0x0
    val length = 0xFFFF
    final val isActive = true
    
    private[this] val KERNEL = new DISK_KERNEL
    private[this] val RAM = new RAM
    
    def init {
      addBridge(KERNEL)
      addBridge(RAM)
    }
    
    def reset {}
    
    override def defaultValue(address:Int) = Some(address >> 8)
  }
}