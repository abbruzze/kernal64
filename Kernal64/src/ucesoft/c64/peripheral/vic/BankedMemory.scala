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

  final def setBank(bank: Int) {
    this.bank = ~bank & 3
    baseAddress = this.bank << 14
    //Log.debug(s"Set VIC bank to ${bank}. Internal bank is ${this.bank}")
  }
  
  final def lastByteRead = memLastByteRead
  
  def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
    ultimax = !game && exrom
  }
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.VIC): Int = {     
    memLastByteRead = readPhi1(address)    
    memLastByteRead
  }
  
  final def readPhi2(address:Int) : Int = readPhi1(address)
  
  @inline private def readPhi1(address: Int) : Int = {
    val realAddress = baseAddress | address
    if ((realAddress & 0x7000) == 0x1000 && !ultimax) charROM.read(0xD000 | (address & 0x0FFF),ChipID.VIC)
    else mem.read(realAddress,ChipID.VIC)
  }
  
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = { /* ignored for VIC */}
}