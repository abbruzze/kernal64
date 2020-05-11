package ucesoft.cbm.c64

import ucesoft.cbm.peripheral.vic.VICMemory
import ucesoft.cbm.Log
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.{CPU65xx, Memory}
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.util.Properties

class C64VICMemory(mem: Memory,charROM:Memory,cpu:CPU65xx) extends VICMemory {
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
  
  def init  : Unit = {
    Log.info("Initialaizing banked memory ...")
  }
  def reset  : Unit = {
    bank = 0
    baseAddress = 0
    memLastByteRead = 0
    ultimax = false
  }
  val isActive = true

  override def getProperties: Properties = {
    properties.setProperty("VIC base address: ",baseAddress.toHexString)
    super.getProperties
  }

  final def setVideoBank(bank: Int) : Unit = {
    this.bank = ~bank & 3
    baseAddress = this.bank << 14
    //Log.debug(s"Set VIC bank to ${bank}. Internal bank is ${this.bank}")
  }
  
  final def lastByteRead = memLastByteRead
  
  def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) : Unit = {
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

  override def readPCOpcode = mem.read(cpu.getPC)
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(bank)
    out.writeInt(baseAddress)
    out.writeInt(memLastByteRead)
    out.writeBoolean(ultimax)
    
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    bank = in.readInt
    baseAddress = in.readInt
    memLastByteRead = in.readInt
    ultimax = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}