package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.Log
import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.CPU6510Mems
import ucesoft.cbm.expansion.LastByteReadMemory
import ucesoft.cbm.expansion.ExpansionPortConfigurationListener
import ucesoft.cbm.cpu.RAMComponent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.CBMComponentType

class BankedMemory(mem: Memory,charROM:Memory,colorRam:Memory) extends RAMComponent with LastByteReadMemory with ExpansionPortConfigurationListener {
  val componentID = "VIC Banked Memory"
  val componentType = CBMComponentType.MEMORY
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
  def reset {
    bank = 0
    baseAddress = 0
    memLastByteRead = 0
    ultimax = false
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
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeInt(bank)
    out.writeInt(baseAddress)
    out.writeInt(memLastByteRead)
    out.writeBoolean(ultimax)
    
  }
  protected def loadState(in:ObjectInputStream) {
    bank = in.readInt
    baseAddress = in.readInt
    memLastByteRead = in.readInt
    ultimax = in.readBoolean
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}