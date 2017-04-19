package ucesoft.cbm.c128

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Log
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion.LastByteReadMemory
import ucesoft.cbm.peripheral.vic.VICMemory

private[c128] class C128RAM(mmu:Memory) extends RAMComponent with VICMemory {
  val name = "C128 RAM Blocks"
  val componentID = "C128_RAM"
  val isRom = false
  val startAddress = 0
  val length = 0x10000
  val componentType = CBMComponentType.MEMORY
  val isActive = true  
  
  // RAM BANKS 0,1,2,3 ------------------------------------
  private[this] final val mem = Array.ofDim[Array[Int]](4)
  private[this] var processorBank,VICbank = 0
  private[this] var expanded = false
  // C64 stuff --------------------------------------------
  private[this] var c64mode = false
  private[this] var ULTIMAX = false
  // VIC stuff --------------------------------------------
  private[this] var videoBank = 0
  private[this] var vicBaseAddress = 0
  private[this] var memLastByteRead = 0
  // common ram ------------------------------------
  private[this] final val NO_COMMON_RAM = 0
  private[this] final val BOTTOM_COMMON_RAM = 1
  private[this] final val TOP_COMMON_RAM = 2
  private[this] final val TOP_BOTTOM_COMMON_RAM = 3
  private[this] final val COMMON_RAM_SIZE_FROM_BOTTOM = Array(0,0x400,0x1000,0x2000,0x4000) // 1K,4K,8K,16K
  private[this] final val COMMON_RAM_SIZE_FROM_TOP = COMMON_RAM_SIZE_FROM_BOTTOM map { 0xFFFF - _ }
  private[this] var commonAreaSize = 0
  private[this] var commonArea = NO_COMMON_RAM
  private[this] var commonAreaBottomLimit = COMMON_RAM_SIZE_FROM_BOTTOM(commonAreaSize + 1)
  private[this] var commonAreaTopLimit = COMMON_RAM_SIZE_FROM_TOP(commonAreaSize + 1)
  // page 0 & 1 ------------------------------------
  private[this] final val page01 = Array.ofDim[Int](2,3) // (page,bank,bank << 16 | page << 8)
  private[this] var page0Diverted, page1Diverted = false
  // -----------------------------------------------
  private[this] var charROM : Memory = _
  
  def setCharROM(charROM:Memory) = this.charROM = charROM
  
  final def init {
    if (charROM == null) throw new IllegalStateException("Cannot initialize 128 memory: missing charROM")
    Log.info("Initialaizing C128 RAM memory ...")
    // only the first two banks are initialized
    mem(0) = Array.ofDim[Int](0x10000)
    mem(1) = Array.ofDim[Int](0x10000)
  }
  
  final def reset {
    ULTIMAX = false
    processorBank = 0
    VICbank = 0
    commonAreaSize = 0
    commonArea = NO_COMMON_RAM
    commonAreaBottomLimit = COMMON_RAM_SIZE_FROM_BOTTOM(commonAreaSize + 1)
    commonAreaTopLimit = COMMON_RAM_SIZE_FROM_TOP(commonAreaSize + 1)
    page0Diverted = false
    page1Diverted = false
    c64mode = false
    videoBank = 0
    vicBaseAddress = 0
    memLastByteRead = 0
    for(m <- 0 until 4;if mem(m) != null) {
      var i = 0
      while (i < mem(m).length) {
        for(j <- 1 to 64) {
          mem(m)(i) = 0
          i += 1
        }
        for(j <- 1 to 64) {
          mem(m)(i) = 0xFF
          i += 1
        }
      }
    }
  }
  
  final def getBanksNumber : Int = if (expanded) 4 else 2
  /**
   * Set the C64 mode
   */
  def setC64Mode(c64mode:Boolean) {
    this.c64mode = c64mode
    Log.debug(s"Set c64 mode $c64mode")
  }
  /**
   * Set the ULTIMAX configuration in C64 mode
   */
  def setULTIMAXMode(ultimax:Boolean) {
    ULTIMAX = ultimax
  }
  /**
   * Set the bank that the processor sees
   * $D500/$FF00 (bit 7-6)
   */
  final def setProcessorBank(bank:Int) {
    if (expanded) processorBank = bank & 0x3 
    else processorBank = bank & 0x1
    Log.debug(s"Set processor bank to $processorBank")
  }  
  /**
   * Set the bank that the VIC sees
   * $D506 (bit 7-6)
   */
  final def setVICBank(bank:Int) {
    if (expanded) VICbank = bank & 0x3 
    else VICbank = bank & 0x1 
    Log.debug(s"Set VIC bank to $VICbank")
  }  
  /**
   *  Set the redirecting page 0/1 page
   *  $D507/$D508
   *  $D509/$D50A
   */
  final def setDivertedPage(page:Int,divertedPage:Int,divertedPageBank:Int) {
    page01(page)(0) = divertedPage & 0xFF
    page01(page)(1) = divertedPageBank & 0xFF
    updatePage01(page)
  }
  @inline private[this] def updatePage01(page:Int) {
    page01(page)(2) = page01(page)(1) << 16 | page01(page)(0) << 8
    if (page == 0) {
      page0Diverted = page01(0)(0) != 0 || page01(0)(1) != 0
      if (page0Diverted) Log.debug(s"Page 0 diverted to ${Integer.toHexString(page01(0)(2))}")
    }
    else {
      page1Diverted = page01(1)(0) != 0 || page01(1)(1) != 0
      if (page1Diverted) Log.debug(s"Page 1 diverted to ${Integer.toHexString(page01(1)(2))}")
    }
  } 
  
  final def setExpansionBanks(expanded:Boolean) {
    this.expanded = expanded
    if (expanded) {
      Log.info("C128 expanded to 256K")
      mem(2) = Array.ofDim[Int](0x10000)
      mem(3) = Array.ofDim[Int](0x10000)
    }
    else {
      Log.info("C128 back to 128K")
      mem(2) = null
      mem(3) = null
    }
  }
  /**
   * Set common ram area none, bottom,top top & bottom and size 1k,4k,8k,16k
   * $D506 (bit 3-2) (bit 1-0)
   */
  final def setCommonAreaAndSize(commonArea:Int,commonAreaSize:Int) {
    this.commonArea = commonArea & 3
    this.commonAreaSize = commonAreaSize & 3
    commonAreaBottomLimit = COMMON_RAM_SIZE_FROM_BOTTOM(commonAreaSize + 1)
    commonAreaTopLimit = COMMON_RAM_SIZE_FROM_TOP(commonAreaSize + 1)
    Log.debug(s"Common area set to $commonArea. Common area size set to $commonAreaSize. Common bottom limit ${Integer.toHexString(commonAreaBottomLimit)}. Common top limit ${Integer.toHexString(commonAreaTopLimit)}")
  }
  
  @inline private def selectBank(address:Int) : Int = {
    if (processorBank == 0 || commonArea == NO_COMMON_RAM) processorBank
    else
    if (commonArea == BOTTOM_COMMON_RAM && address < commonAreaBottomLimit) 0
    else
    if (commonArea == TOP_COMMON_RAM && address > commonAreaTopLimit) 0
    else
    if (commonArea == TOP_BOTTOM_COMMON_RAM && 
        (address < commonAreaBottomLimit || address > commonAreaTopLimit)) 0
    else processorBank
  }
  /**
   *  page 0,1 handling
   *  the returned address will be of the form 0xbxxxx where b is the bank
   */
  @inline private def selectAddress(address:Int) : Int = {
    if (!page0Diverted && !page1Diverted) return address
    
    val page = (address >> 8) & 0xFF
    if (page0Diverted) {
      if (page == 0 && processorBank == 0) {
        var targetAddress = page01(0)(2) | address & 0xFF
        if ((commonArea & BOTTOM_COMMON_RAM) != 0) targetAddress &= 0xFFFF // to be confirmed
        return targetAddress
      }
      else
      if (page == page01(0)(0) && processorBank == page01(0)(1)) return address & 0xFF // back to page 0
    }
    if (page1Diverted) {
      if (page == 1  && processorBank == 0) {
        var targetAddress = page01(1)(2) | address & 0xFF
        if ((commonArea & BOTTOM_COMMON_RAM) != 0) targetAddress &= 0xFFFF // to be confirmed
        return targetAddress
      }
      else
      if (page == page01(1)(0) && processorBank == page01(1)(1)) return 0x100 | address & 0xFF // back to page 1
    }
    address
  }
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    if (chipID == ChipID.VIC) return vicRead(address)
    if (c64mode) return readC64mode(address,chipID)
    
    val selectedAddress = selectAddress(address)
    if (selectedAddress != address) { // page0,1 relocation
      println(s"RAM reading from diverted address ${Integer.toHexString(selectedAddress)}")
      val bank = selectedAddress & 0x10000
      if (bank == 0x10000) mem(1)(selectedAddress & 0xFFFF)
      else mmu.read(selectedAddress & 0xFFFF,chipID)
    }
    else {
      val bank = selectBank(address)
      println(s"RAM reading from address ${Integer.toHexString(address)} of bank $bank")
      mem(bank)(address & 0xFFFF)
    }    
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    if (c64mode) {
      writeC64mode(address,value,chipID)
      return
    }
    
    val selectedAddress = selectAddress(address)
    if (selectedAddress != address) { // page0,1 relocation
       println(s"RAM writing $value to diverted address ${Integer.toHexString(selectedAddress)}")
      val bank = selectedAddress & 0x10000
      if (bank == 0x10000) mem(1)(selectedAddress & 0xFFFF) = value & 0xFF
      else mmu.write(selectedAddress & 0xFFFF,value,chipID)
    }
    else {
      val bank = selectBank(address)
      println(s"RAM writing $value to address ${Integer.toHexString(address)} of bank $bank")
      mem(bank)(address & 0xFFFF) = value & 0xFF
    }
  }
  // VIC memory ------------------------------------------
  final def getBank = videoBank
  final def setVideoBank(bank: Int) {
    videoBank = ~bank & 3
    vicBaseAddress = videoBank << 14
  }
  final def lastByteRead = memLastByteRead
  final def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
    ULTIMAX = !game && exrom
  }
  @inline private def vicReadPhi1(address: Int) : Int = {
    val realAddress = vicBaseAddress | address
    if (c64mode) {      
      if ((realAddress & 0x7000) == 0x1000 && !ULTIMAX) charROM.read(0xD000 | (address & 0x0FFF),ChipID.VIC)
      else mem(processorBank)(realAddress)
    }
    else { // 128 mode
      if ((address & 0x1000) == 0x1000 && charROM.isActive) charROM.read(0xD000 | (address & 0x0FFF),ChipID.VIC)
      else mem(VICbank)(realAddress)
    }    
  }
  @inline private[this] def vicRead(address:Int) : Int = {
    memLastByteRead = vicReadPhi1(address)    
    memLastByteRead
  }
  final def readPhi2(address:Int) : Int = vicReadPhi1(address)
  // C64 -------------------------------------------------
  @inline private[this] def readC64mode(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (ULTIMAX && chipID == ChipID.CPU) {
      if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return memLastByteRead
    }
    mem(processorBank)(address & 0xFFFF)
  }
  @inline private[this] def writeC64mode(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (ULTIMAX && chipID == ChipID.CPU) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return
      }
      mem(processorBank)(address & 0xFFFF) = value & 0xff
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

object C128RAM extends App {
  Log.setDebug
  val mmu = new Memory {
    val name = "MMU test"
    val isRom = false
    val startAddress = 0
    val length = 0x10000
    val isActive = true  
    def init {}
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
      println(s"Reading MMU ${Integer.toHexString(address)}")
      0
    }
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      println(s"Writing $value to MMU ${Integer.toHexString(address)}")
    }
  }
  val charROM = new Memory {
    val name = "Char ROM"
    val isRom = false
    val startAddress = 0
    val length = 0x0
    val isActive = true  
    def init {}
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
      println(s"Reading CHAR ROM ${Integer.toHexString(address)}")
      0
    }
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      println(s"Writing $value to CHAR ROM ${Integer.toHexString(address)}")
    }
  }
  val ram = new C128RAM(mmu)
  ram.setCharROM(charROM)
  ram.init
  println(ram.read(0x1000))
  ram.setProcessorBank(1)
  ram.write(0x1000,1)
  ram.setProcessorBank(0)
  println(ram.read(0x1000))
  ram.setProcessorBank(1)
  println(ram.read(0x1000))
  println(ram.read(0x1001,ChipID.VIC))
  ram.setProcessorBank(0)
  ram.write(0xffff,0xf0)
  ram.setCommonAreaAndSize(3,0)
  ram.setProcessorBank(1)
  println(ram.read(0xffff))
  ram.setCommonAreaAndSize(0,0)
  ram.setProcessorBank(1)
  ram.write(0xF00A,16)
  ram.setProcessorBank(0)
  ram.setDivertedPage(0,0xF0,1)
  ram.write(0xb,11)
  println(ram.read(0xa))
  ram.setProcessorBank(1)
  println(ram.read(0xF00B))
  
}