package ucesoft.c64.peripheral.vic

import ucesoft.c64.cpu.Memory
import ucesoft.c64.Log
import ucesoft.c64.ChipID
import ucesoft.c64.cpu.CPU6510Mems
import ucesoft.c64.expansion.LastByteReadMemory
import ucesoft.c64.expansion.ExpansionPortConfigurationListener

class BankedMemory(mem: Memory,charROM:Memory,colorRam:Memory) extends Memory with LastByteReadMemory with ExpansionPortConfigurationListener {
  val name = "VIC-Memory"
  val isRom = false
  val startAddress = 0
  val length = 16384
  
  private[this] var bank = 0
  private[this] var baseAddress = 0
  private[this] var memLastByteRead = 0
  private[this] var ultimax = false
  
  def getBank = bank
  def getBankAddress = bank << 14
  
  def init {
    Log.info("Initialaizing banked memory ...")
  }
  val isActive = true

  def setBank(bank: Int) {
    this.bank = ~bank & 3
    baseAddress = this.bank << 14
    Log.debug(s"Set VIC bank to ${bank}. Internal bank is ${this.bank}")
  }
  
  def lastByteRead = memLastByteRead
  
  def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
    ultimax = !game && exrom
  }
  
  def read(address: Int, chipID: ChipID.ID = ChipID.VIC): Int = { 
    /*
    memLastByteRead =
    // check if we have to read CHAR ROM
    if (address >= 4096 && address < 8192 && (bank == 0 || bank == 2)) {
      val offset = address + 0xC000 //- 4096 + CPU6510Mems.M_CHARACTERS
      //Log.fine("VIC reading character ROM at offset %4X".format(offset))
      charROM.read(offset,chipID)
    }
    else {      
      val realAddress = baseAddress | address
      //Log.fine("VIC reading RAM at %4X".format(realAddress))
      mem.read(realAddress,chipID)
    }
    * 
    */
    
    val realAddress = baseAddress | address
    memLastByteRead =
    if ((realAddress & 0x7000) == 0x1000 && !ultimax) charROM.read(0xD000 | (address & 0x0FFF),chipID)//(address + 0xC000,chipID)
    else mem.read(realAddress,chipID)
    
    memLastByteRead
  }
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = { /* ignored for VIC */}
}