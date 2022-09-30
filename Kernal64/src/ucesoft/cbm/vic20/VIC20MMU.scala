package ucesoft.cbm.vic20

import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.{RAMComponent, ROM}
import ucesoft.cbm.expansion.VIC20ExpansionPort
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.misc.TestCart
import ucesoft.cbm.peripheral.drive.VIA
import ucesoft.cbm.peripheral.vic.{VICType, VIC_I}
import ucesoft.cbm.{CBMComponentType, ChipID}

import java.io.{ObjectInputStream, ObjectOutputStream}

object VIC20MMU {
  val KERNAL_PAL_ROM = new ROM(null, "KernalPal", 0, 8192, ROM.VIC20_KERNAL_PAL_ROM_PROP)
  val KERNAL_NTSC_ROM = new ROM(null, "KernalNtsc", 0, 8192, ROM.VIC20_KERNAL_NTSC_ROM_PROP)
  val BASIC_ROM = new ROM(null, "Basic", 0, 8192, ROM.VIC20_BASIC_ROM_PROP)
  val CHAR_ROM = new ROM(null, "Char", 0, 4096, ROM.VIC20_CHAR_ROM_PROP)

  final val NO_EXP   = 0
  final val EXP_BLK0 = 1
  final val EXP_BLK1 = 2
  final val EXP_BLK2 = 4
  final val EXP_BLK3 = 8
  final val EXP_BLK5 = 16

  def parseConfig(config:String,updateMemoryConfig: Int => Unit): Unit = {
    if (!config.isEmpty) {
      val blocks = config.split(",").map(_.toUpperCase() match {
        case "400" => VIC20MMU.EXP_BLK0
        case "2000" => VIC20MMU.EXP_BLK1
        case "4000" => VIC20MMU.EXP_BLK2
        case "6000" => VIC20MMU.EXP_BLK3
        case "A000" => VIC20MMU.EXP_BLK5
        case _ => VIC20MMU.NO_EXP
      }).reduce(_ | _)

      updateMemoryConfig(blocks)
    }
  }

  def getStringConfig(config:Int,blocks:Boolean = false,sep:String = ","): String = {
    val labels = if (blocks) Array("B0","B1","B2","B3","B5") else Array("400","2000","4000","6000","A000")
    Array(EXP_BLK0,EXP_BLK1,EXP_BLK2,EXP_BLK3,EXP_BLK5).zip(labels).flatMap { e =>
      if ((config & e._1) > 0) Some(e._2) else None
    }.mkString(sep)
  }

  def getLabelConfig(config:Int): String = {
    if (config == NO_EXP) ""
    else if (config == EXP_BLK0) "3K"
    else if (config == EXP_BLK1) "8K"
    else if (config == (EXP_BLK1 | EXP_BLK2)) "16K"
    else if (config == (EXP_BLK1 | EXP_BLK2 | EXP_BLK3)) "24K"
    else getStringConfig(config,true,"|")
  }
}

class VIC20MMU extends RAMComponent {
  import VIC20MMU._

  override val isRom = false
  override val length: Int = 0x10000
  override val startAddress = 0
  override val name = "VIC20MMU"
  override def isActive = true
  override val componentID: String = "VIC20MMU"
  override val componentType = CBMComponentType.MEMORY

  private trait RW {
    def read(address:Int,chipID: ID): Int
    def write(address: Int, value: Int): Unit
  }

  private val memRW = Array.ofDim[RW](0x10000)
  private val ram = Array.ofDim[Int](0x10000)
  private var basicROM : Array[Int] = _
  private var kernelROM_PAL,kernelROM_NTSC,kernelROM : Array[Int] = _
  private var charROM : Array[Int] = _
  /*
    Index   Memory
    0       0400 - 0FFF
    1       2000 - 3FFF
    2       4000 - 5FFF
    3       6000 - 7FFF
    4       A000 - BFFF
   */
  private val expansionBlocks = Array(
    new EXPRAM_BLOCK_RW(0x400),
    new EXPRAM_BLOCK_RW(0x2000),
    new EXPRAM_BLOCK_RW(0x4000),
    new EXPRAM_BLOCK_RW(0x6000),
    new EXPRAM_BLOCK_RW(0xA000)
  )
  private val ioBlocks = Array(
    new IOBLOCK_RW(),
    new IOBLOCK_RW()
  )
  private val expansionBlockMAP = expansionBlocks map { e => (e.address,e) } toMap
  private var lastByteOnBUS = 0
  private var dontUpdateLastByteOnBUS = false

  private var via1,via2 : VIA = _
  private var vic : VIC_I = _

  private var carts : List[Cartridge] = Nil
  private var specialCart: VIC20ExpansionPort = _

  private var vicType: VICType.Value = VICType.PAL

  // Constructor
  setExpansion(NO_EXP)

  def setVICType(vicModel:VICType.Value): Unit = {
    this.vicType = vicModel
    kernelROM = vicType match {
      case VICType.PAL => kernelROM_PAL
      case VICType.NTSC => kernelROM_NTSC
    }
  }

  def setBasicROM(rom:Array[Int]): Unit = basicROM = rom
  def setKernelPALROM(rom:Array[Int]): Unit = kernelROM_PAL = rom
  def setKernelNTSCROM(rom: Array[Int]): Unit = kernelROM_NTSC = rom
  def setCharROM(rom:Array[Int]): Unit = charROM = rom

  def setIO2RAM(enabled:Boolean): Unit = ioBlocks(0).enabled = enabled
  def setIO3RAM(enabled:Boolean): Unit = ioBlocks(1).enabled = enabled
  def isIO2RAMEnabled(): Boolean = ioBlocks(0).enabled
  def isIO3RAMEnabled(): Boolean = ioBlocks(1).enabled

  def setExpansion(exp:Int): Unit = {
    var b = 0
    while (b < 5) {
      val enabled = (exp & (1 << b)) > 0
      expansionBlocks(b).enabled = enabled
      if (enabled && expansionBlocks(b).hasROM()) expansionBlocks(b).removeROM()
      b += 1
    }
  }

  def getExpansionSettings(): Int = {
    var e = 0
    var i = 0
    while (i < 5) {
      if (expansionBlocks(i).enabled) e |= 1 << i
      i += 1
    }
    e
  }

  def attachCart(crt:Cartridge): Boolean = {
    for (chip <- crt.chips) {
      expansionBlockMAP get chip.startingLoadAddress match {
        case Some(exp) =>
          if (exp.hasROM()) return false
          exp.setROM(chip.romData)
        case None =>
          return false
      }
    }
    carts ::= crt
    true
  }

  def attachSpecialCart(cart:VIC20ExpansionPort): Unit = {
    if (specialCart != null) specialCart.eject()
    specialCart = cart
  }
  def detachSpecialCart(): Unit = {
    if (specialCart != null) specialCart.eject()
    specialCart = null
  }

  def detachAllCarts(): Unit = {
    for(e <- expansionBlocks) e.removeROM()
    carts = Nil
  }

  def getAttachedCarts(): List[Cartridge] = carts
  def getAttachedSpecialCart(): Option[VIC20ExpansionPort] = Option(specialCart)

  def setIO(via1:VIA,via2:VIA,vic:VIC_I): Unit = {
    this.via1 = via1
    this.via2 = via2
    this.vic = vic
  }

  private object BASICROM_RW extends RW {
    override def read(address: Int, chipID: ID): Int = basicROM(address & 0x1FFF)
    override def write(address: Int, value: Int): Unit = {}
  }
  private object KERNELROM_RW extends RW {
    override def read(address: Int, chipID: ID): Int = kernelROM(address & 0x1FFF)
    override def write(address: Int, value: Int): Unit = {}
  }
  private object CHARROM_RW extends RW {
    override def read(address: Int, chipID: ID): Int = charROM(address & 0xFFF)
    override def write(address: Int, value: Int): Unit = {}
  }

  private object RAM_RW extends RW {
    override def read(address: Int, chipID: ID): Int = ram(address & 0xFFFF)
    override def write(address: Int, value: Int): Unit = ram(address & 0xFFFF) = value
  }
  private object COLOR_RW extends RW {
    override def read(address: Int, chipID: ID): Int = {
      val color = ram(address & 0xFFFF) & 0xF
      if (chipID == ChipID.VIC) {
        dontUpdateLastByteOnBUS = true
        color
      }
      else lastByteOnBUS & 0xF0 | color
    }
    override def write(address: Int, value: Int): Unit = ram(address & 0xFFFF) = value & 0xF
  }
  private class EXPRAM_BLOCK_RW(val address:Int) extends RW {
    private var rom : Array[Int] = _
    var enabled = false

    def setROM(rom:Array[Int]): Unit = this.rom = rom
    def removeROM(): Unit = rom = null
    def hasROM(): Boolean = rom != null

    override def read(address: Int, chipID: ID): Int = {
      if (chipID == ChipID.VIC)
        lastByteOnBUS
      else {
        if (rom == null) {
          if (enabled) ram(address)
          else lastByteOnBUS
        }
        else rom(address % rom.length)
      }
    }
    override def write(address: Int, value: Int): Unit = {
      if (rom == null) {
        if (enabled) ram(address & 0xFFFF) = value
      }
    }
  }
  private class IOBLOCK_RW extends RW {
    var enabled = false
    override def read(address: Int, chipID: ID): Int = {
      if (enabled) ram(address) else lastByteOnBUS
    }
    override def write(address: Int, value: Int): Unit = {
      if (enabled) ram(address) = value
    }
  }
  private object VIC_RW extends RW {
    override def read(address: Int, chipID: ID): Int = vic.read(address)
    override def write(address: Int, value: Int): Unit = vic.write(address,value)
  }
  private object VIA1_VIA2_RW extends RW {
    override def read(address: Int, chipID: ID): Int = {
      if ((address & 0x30) == 0) lastByteOnBUS
      else {
        var tmp = 0xFF
        if ((address & 0x10) > 0) tmp = via1.read(address)
        if ((address & 0x20) > 0) tmp = via2.read(address)
        tmp
      }
    }
    override def write(address: Int, value: Int): Unit = {
      if ((address & 0x10) > 0) via1.write(address,value)
      if ((address & 0x20) > 0) via2.write(address,value)
    }
  }

  override def init(): Unit = {
    for(r <- 0 until 0x10000) {
      memRW(r) =
        if (r < 0x400) RAM_RW
        else if (r < 0x1000) expansionBlocks(0)
        else if (r < 0x2000) RAM_RW
        else if (r < 0x4000) expansionBlocks(1)
        else if (r < 0x6000) expansionBlocks(2)
        else if (r < 0x8000) expansionBlocks(3)
        else if (r < 0x9000) CHARROM_RW
        else if (r < 0x90FF) VIC_RW
        else if (r < 0x93FF) VIA1_VIA2_RW
        else if (r < 0x9800) COLOR_RW
        else if (r < 0x9C00) ioBlocks(0)
        else if (r < 0xA000) ioBlocks(1)
        else if (r < 0xC000) expansionBlocks(4)
        else if (r < 0xE000) BASICROM_RW
        else KERNELROM_RW
    }
  }
  override def reset(): Unit = {
    if (specialCart != null) specialCart.reset()
  }

  override def hardReset(): Unit = {
    java.util.Arrays.fill(ram,0)
    for(e <- expansionBlocks) {
      e.removeROM()
    }
    if (specialCart != null) specialCart.hardReset()
  }

  final override def read(address: Int, chipID: ID): Int = {
    if (specialCart != null) {
      specialCart.read(address) match {
        case None =>
        case Some(v) =>
          if (!dontUpdateLastByteOnBUS) lastByteOnBUS = v
          return v
      }
    }

    val read = memRW(address).read(address, chipID)
    if (!dontUpdateLastByteOnBUS) lastByteOnBUS = read
    dontUpdateLastByteOnBUS = false
    read
  }
  final override def write(address: Int, value: Int, chipID: ID): Unit = {
    TestCart.write(address, value)
    lastByteOnBUS = value

    if (specialCart != null) {
      if (specialCart.write(address,value)) return
    }
    memRW(address).write(address, value)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeObject(ram)
    out.writeInt(getExpansionSettings())
  }
  override protected def loadState(in: ObjectInputStream): Unit = {
    loadMemory(ram,in)
    setExpansion(in.readInt())
  }
  override protected def allowsStateRestoring: Boolean = true
}
