package ucesoft.cbm.c128

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponentType, ChipID, Log}
import ucesoft.cbm.cpu.{Memory, RAMComponent}
import ucesoft.cbm.misc.MemoryInitPattern

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

private[c128] class C128RAM extends RAMComponent {
  val name = "C128 RAM Blocks"
  val componentID = "C128_RAM"
  val isRom = false
  val startAddress = 0
  val length = 0x10000
  val componentType: Type = CBMComponentType.MEMORY
  val isActive = true

  // RAM BANKS 0,1,2,3 ------------------------------------
  private[this] final val mem = Array.ofDim[Array[Int]](4)
  private[this] var processorBank,VICbank = 0
  private[this] var expanded = false
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
  private[this] var page_0,page_0_bank,page_1_bank = 0
  private[this] var page_1 = 1
  // DMA
  private[this] var dma = false
  // C64 mode
  private var c64mode = false
  // -----------------------------------------------
  private object Bank0 extends Memory {
    val isRom = false
    val length = 0x10000
    val startAddress = 0
    val name = "RAM_bank0"

    def init  : Unit = {}
    def isActive = true
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(0)(address)
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = mem(0)(address) = value
  }

  def getBank0 : Memory = Bank0

  def getProcessorBank(): Int = processorBank
  def getVICBank(): Int = VICbank

  final def setDMA(dma:Boolean) : Unit = this.dma = dma

  final def setC64Mode(c64mode:Boolean): Unit = this.c64mode = c64mode

  override def getProperties: Properties = {
    super.getProperties
    properties.setProperty("Processor memory bank",processorBank.toString)
    properties.setProperty("VIC memory bank",VICbank.toString)
    val ca = commonArea match {
      case NO_COMMON_RAM => "None"
      case BOTTOM_COMMON_RAM => "Bottom"
      case TOP_COMMON_RAM => "Top"
      case TOP_BOTTOM_COMMON_RAM => "Bottom+Top"
    }
    properties.setProperty("Common area:",ca)
    properties.setProperty("Page 0 address:",Integer.toHexString(page_0 << 8 | page_0_bank << 16))
    properties.setProperty("Page 1 address:",Integer.toHexString(page_1 << 8 | page_1_bank << 16))
    properties
  }

  final def init  : Unit = {
    Log.info("Initializing C128 RAM memory ...")
    // only the first two banks are initialized
    mem(0) = Array.ofDim[Int](0x10000)
    mem(1) = Array.ofDim[Int](0x10000)
    MemoryInitPattern.initRAM(mem(0))
    MemoryInitPattern.initRAM(mem(1))
  }

  final def reset  : Unit = {
    processorBank = 0
    VICbank = 0
    commonAreaSize = 0
    commonArea = NO_COMMON_RAM
    commonAreaBottomLimit = COMMON_RAM_SIZE_FROM_BOTTOM(commonAreaSize + 1)
    commonAreaTopLimit = COMMON_RAM_SIZE_FROM_TOP(commonAreaSize + 1)
    page_0 = 0
    page_0_bank = 0
    page_1 = 1
    page_1_bank = 0
  }

  override def hardReset : Unit = {
    init
    reset
  }

  final def getBanksNumber : Int = if (expanded) 4 else 2
  /**
   * Set the bank that the processor sees
   * $D500/$FF00 (bit 7-6)
   */
  final def setProcessorBank(bank:Int) : Unit = {
    if (expanded) processorBank = bank & 0x3
    else processorBank = bank & 0x1
    Log.debug(s"Set processor bank to $processorBank")
    //println(s"Set processor bank to $processorBank")
  }
  /**
   * Set the bank that the VIC sees
   * $D506 (bit 7-6)
   */
  final def setVICBank(bank:Int) : Unit = {
    if (expanded) VICbank = bank & 0x3
    else VICbank = bank & 0x1
    Log.debug(s"Set VIC bank to $VICbank")
    //println(s"Set VIC bank to $VICbank")
  }
  /**
   *  Set the redirecting page 0/1 page
   *  $D507/$D508
   *  $D509/$D50A
   */
  final def setDivertedPage(page:Int,divertedPage:Int,divertedPageBank:Int) : Unit = {
    if (page == 0) {
      page_0 = divertedPage & 0xFF
      page_0_bank = divertedPageBank & (if (expanded) 0x3 else 0x1)
    }
    else {
      page_1 = divertedPage & 0xFF
      page_1_bank = divertedPageBank & (if (expanded) 0x3 else 0x1)
    }
    Log.debug(s"Page $page diverted to ${Integer.toHexString(divertedPage << 8 | (divertedPageBank & 1) << 16)}")
    //println(s"Set diverted page $page $divertedPage $divertedPageBank")
  }

  final def setExpansionBanks(expanded:Boolean) : Unit = {
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
  final def setCommonAreaAndSize(commonArea:Int,commonAreaSize:Int) : Unit = {
    this.commonArea = commonArea & 3
    this.commonAreaSize = commonAreaSize & 3
    commonAreaBottomLimit = COMMON_RAM_SIZE_FROM_BOTTOM(commonAreaSize + 1)
    commonAreaTopLimit = COMMON_RAM_SIZE_FROM_TOP(commonAreaSize + 1)
    Log.debug(s"Common area set to $commonArea. Common area size set to $commonAreaSize. Common bottom limit ${Integer.toHexString(commonAreaBottomLimit)}. Common top limit ${Integer.toHexString(commonAreaTopLimit)}")
    //println(s"Common area set to $commonArea. Common area size set to $commonAreaSize. Common bottom limit ${Integer.toHexString(commonAreaBottomLimit)}. Common top limit ${Integer.toHexString(commonAreaTopLimit)}")
  }

  @inline private def getBankAndAddressP0_P1(_address: Int): Int = {
    var address = _address | processorBank << 16
    val page = (address >> 8) & 0xFF
    val offset = address & 0xFF

    if (c64mode) {
      if (page == 0 && page_1 != 0) {}
      else if (page == 1 && page_0 != 1) {}
      else {
        if (page == page_0 && processorBank == page_0_bank)
          address = offset | processorBank << 16
        if (page == page_1 && processorBank == page_1_bank)
          address = offset | 0x100 | processorBank << 16
      }
    }
    else {
      if (page == 0 && page_0 == 0 && page_0_bank == 0) address = offset | page_0 << 8
      else if (page == 0) address = offset | page_0 << 8 | page_0_bank << 16
      else if (page == 1 && page_1 == 1 && page_1_bank == 0) address = offset | page_1 << 8
      else if (page == 1) address = offset | page_1 << 8 | page_1_bank << 16
      else {
        if (page == page_0 && processorBank == page_0_bank)
          address = offset | processorBank << 16
        if (page == page_1 && processorBank == page_1_bank)
          address = offset | 0x100 | processorBank << 16
      }
    }

    address
  }

  final def read(address:Int,bank:Int) : Int = mem(bank)(address)
  final def write(address:Int,bank:Int,value:Int): Unit = mem(bank)(address) = value

  final def read(originAddress: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    if (chipID == ChipID.VIC) return mem(VICbank)(originAddress)

    if (dma) return mem(VICbank)(originAddress) // to be investigated if it's correct

    val pagedAddress = getBankAndAddressP0_P1(originAddress)
    val address = pagedAddress & 0xFFFF
    val bank = pagedAddress >> 16
    if (originAddress < 0x200) return mem(if (commonArea == BOTTOM_COMMON_RAM || commonArea == TOP_BOTTOM_COMMON_RAM) 0 else bank)(address)

    if (bank == 0 && processorBank == 0) mem(0)(address)
    else
      commonArea match {
        case BOTTOM_COMMON_RAM =>
          if (originAddress < commonAreaBottomLimit) mem(0)(originAddress)
          else mem(bank)(address)
        case TOP_COMMON_RAM =>
          if (originAddress > commonAreaTopLimit) mem(0)(originAddress)
          else mem(bank)(address)
        case TOP_BOTTOM_COMMON_RAM =>
          if (originAddress < commonAreaBottomLimit || originAddress > commonAreaTopLimit) mem(0)(originAddress)
          else mem(bank)(address)
        case NO_COMMON_RAM =>
          mem(bank)(address)
      }
  }

  final def write(originAddress: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (dma) { // to be investigated if it's correct
      mem(VICbank)(originAddress) = value
      return
    }
    val pagedAddress = getBankAndAddressP0_P1(originAddress)
    val address = pagedAddress & 0xFFFF
    val bank = pagedAddress >> 16
    if (originAddress < 0x200) mem(if (commonArea == BOTTOM_COMMON_RAM || commonArea == TOP_BOTTOM_COMMON_RAM) 0 else bank)(address) = value
    else {
      if (bank == 0 && processorBank == 0) mem(0)(address) = value
      else
        commonArea match {
          case BOTTOM_COMMON_RAM =>
            if (originAddress < commonAreaBottomLimit) mem(0)(originAddress) = value
            else mem(bank)(address) = value
          case TOP_COMMON_RAM =>
            if (originAddress > commonAreaTopLimit) mem(0)(originAddress) = value
            else mem(bank)(address) = value
          case TOP_BOTTOM_COMMON_RAM =>
            if (originAddress < commonAreaBottomLimit || originAddress > commonAreaTopLimit) mem(0)(originAddress) = value
            else mem(bank)(address) = value
          case NO_COMMON_RAM =>
            mem(bank)(address) = value
        }
    }
  }
  // state -----------------------------------------------

  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(expanded)
    out.writeObject(mem(0))
    out.writeObject(mem(1))
    if (expanded) {
      out.writeObject(mem(2))
      out.writeObject(mem(3))
    }
    out.writeInt(processorBank)
    out.writeInt(VICbank)
    out.writeInt(commonAreaSize)
    out.writeInt(commonArea)
    out.writeInt(commonAreaBottomLimit)
    out.writeInt(commonAreaTopLimit)
    out.writeInt(page_0)
    out.writeInt(page_1)
    out.writeInt(page_0_bank)
    out.writeInt(page_1_bank)
    out.writeBoolean(dma)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    expanded = in.readBoolean
    loadMemory[Int](mem(0),in)
    loadMemory[Int](mem(1),in)
    if (expanded) {
      loadMemory[Int](mem(2),in)
      loadMemory[Int](mem(3),in)
    }
    processorBank = in.readInt
    VICbank = in.readInt
    commonAreaSize = in.readInt
    commonArea = in.readInt
    commonAreaBottomLimit = in.readInt
    commonAreaTopLimit = in.readInt
    page_0 = in.readInt
    page_1 = in.readInt
    page_0_bank = in.readInt
    page_1_bank = in.readInt
    dma = in.readBoolean
  }
  protected def allowsStateRestoring = true
}
