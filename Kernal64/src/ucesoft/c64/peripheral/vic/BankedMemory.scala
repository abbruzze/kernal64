package ucesoft.c64.peripheral.vic

import ucesoft.c64.cpu.Memory
import ucesoft.c64.Log
import ucesoft.c64.ChipID
import ucesoft.c64.cpu.CPU6510Mems

class BankedMemory(mem: Memory,charROM:Memory,colorRam:Memory) extends Memory {
  val name = "VIC-Memory"
  val isRom = false
  val startAddress = 0
  val length = 16384
  
  private var bank = 0
  private var baseAddress = 0
  
  def getBank = bank
  def getBankAddress = bank * 16384
  
  def init {
    Log.info("Initialaizing banked memory ...")
  }
  val isActive = true

  def setBank(bank: Int) {
    this.bank = 3 - (bank & 3)
    baseAddress = this.bank * 16384
    Log.debug(s"Set VIC bank to ${bank}. Internal bank is ${this.bank}")
  }
  
  def read(address: Int, chipID: ChipID.ID = ChipID.VIC): Int = { 
    // check if the VIC is accessing COLOR area
    if (address >= 0xD800 && address <= 0xDBFF) colorRam.read(address,chipID)
    else
    // check if we have to read CHAR ROM
    if (address >= 4096 && address < 8192 && (bank == 0 || bank == 2)) {
      val offset = address - 4096 + CPU6510Mems.M_CHARACTERS
      Log.fine("VIC reading character ROM at offset %4X".format(offset))
      charROM.read(offset,chipID)
    }
    else {      
      val realAddress = baseAddress + address
      Log.fine("VIC reading RAM at %4X".format(realAddress))
      mem.read(realAddress,chipID)
    }
  }
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = { /* ignored for VIC */}
}