package ucesoft.cbm.cbm2

import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.{CPU6510_CE, RAMComponent}
import ucesoft.cbm.misc.TestCart
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.crtc.CRTC6845
import ucesoft.cbm.peripheral.mos6525.MOS6525
import ucesoft.cbm.peripheral.mos6551.ACIA6551
import ucesoft.cbm.peripheral.sid.SID

import java.io.{ObjectInputStream, ObjectOutputStream}

class CBM2MMU extends RAMComponent {
  override val componentID: String = "CBM2_MMU"
  override val componentType: Type = CBMComponentType.MEMORY
  override val isRom: Boolean = false
  override val length: Int = 0
  override val startAddress: Int = 0
  override val name: String = "CBM2_MMU"
  override val isActive = true

  private final val YIND_STA = 0x91
  private final val YIND_LDA = 0xB1

  private var model : CBM2Model  = _610PAL
  private val ram,screenRam = Array.ofDim[Int](2048)
  private val romSlot = Array.ofDim[Array[Int]](4)
  private val romSlotRAMAccess = Array.ofDim[Boolean](4)
  private final val banks = Array.ofDim[Array[Int]](16)
  private var kernalROM,basicROM : Array[Int] = _
  private var codeBank,dataBank = 15
  private var cpu : CPU6510_CE = _

  // I/O
  private var _6845 : CRTC6845 = _
  private var ciaieee,ciaip : CIA = _
  private var tpi_kb,tpi_ieee488 : MOS6525 = _
  private var sid : SID = _
  private var acia : ACIA6551 = _

  override def reset: Unit = {
    // TODO
    codeBank = 15
    dataBank = 15
  }

  override def init: Unit = {
    for(i <- 0 until banks.length) banks(i) = null

    val bs = model.memoryK / 64
    for(b <- 1 to bs) banks(b) = Array.ofDim[Int](0x10000)
    /*
    model match {
      case CBM2Model._600|CBM2Model._700 =>
        for(b <- 1 to 2) banks(b) = Array.ofDim[Int](0x10000)
      case CBM2Model._620|CBM2Model._720 =>
        for(b <- 1 to 4) banks(b) = Array.ofDim[Int](0x10000)
    }
     */
  }

  def setROM1000(rom:Array[Int]): Unit = romSlot(0) = rom
  def setROM2000(rom:Array[Int]): Unit = romSlot(1) = rom
  def setROM4000(rom:Array[Int]): Unit = romSlot(2) = rom
  def setROM6000(rom:Array[Int]): Unit = romSlot(3) = rom

  def setRAM1000(): Unit = {
    romSlotRAMAccess(0) = true
    romSlot(0) = Array.ofDim[Int](0x2000)
  }
  def setRAM2000(): Unit = {
    romSlotRAMAccess(1) = true
    romSlot(1) = Array.ofDim[Int](0x2000)
  }
  def setRAM4000(): Unit = {
    romSlotRAMAccess(2) = true
    romSlot(2) = Array.ofDim[Int](0x2000)
  }
  def setRAM6000(): Unit = {
    romSlotRAMAccess(3) = true
    romSlot(3) = Array.ofDim[Int](0x2000)
  }

  def getCRTCRam : Array[Int] = screenRam

  def setCPU(cpu:CPU6510_CE): Unit = this.cpu = cpu

  def setKernalROM(rom:Array[Int]): Unit = kernalROM = rom
  def setBasicROM(rom:Array[Int]): Unit = basicROM = rom

  def loadPRG(prg:Array[Int],bank:Int = 1): Unit = {
    val startAddress = prg(0) | prg(1) << 8
    if (bank == 15) {
      for(i <- 2 until prg.length) writeBank15(startAddress + i - 2,prg(i))
    }
    else
      System.arraycopy(prg,2,banks(bank),startAddress,prg.length - 2)
  }
  def writeBank(bank:Int,address:Int,value:Int): Unit = {
    if (bank == 15) writeBank15(address,value)
    else {
      val ram = banks(bank)
      if (ram != null) ram(address) = value
    }
  }

  def setIO(_6845:CRTC6845,ciaieee:CIA,ciaip:CIA,tpi_kb:MOS6525,tpi_ieee488:MOS6525,sid:SID,acia:ACIA6551): Unit = {
    this._6845 = _6845
    this.ciaieee = ciaieee
    this.ciaip = ciaip
    this.tpi_kb = tpi_kb
    this.tpi_ieee488 = tpi_ieee488
    this.sid = sid
    this.acia = acia
  }

  def setModel(model:CBM2Model): Unit = {
    this.model = model
    init
  }

  override final def read(address: Int, chipID: ID): Int = {
    val bank = if (cpu.isExecuting(YIND_LDA)) dataBank else codeBank
    if (bank == 15) {
      if (address == 0) codeBank
      else if (address == 1) dataBank
      else readBank15(address)
    }
    else {
      val ram = banks(bank)
      if (bank == 0)
        println(s"READING FROM BANK 0 ${address.toHexString}")
      if (ram != null) {
        //println(s"READING FROM BANK $bank")
        if (address == 0) codeBank
        else if (address == 1) dataBank
        else ram(address)
      }
      else 0xFF
    }
  }

  override final def write(address: Int, value: Int, chipID: ID): Unit = {
    val bank = if (cpu.isExecuting(YIND_STA)) dataBank else codeBank
    if (bank == 15) {
      if (address == 0) codeBank = value & 0xF
      else if (address == 1) dataBank = value & 0xF
      else writeBank15(address,value)
    }
    else {
      val ram = banks(bank)
      if (bank == 0)
        println(s"WRITING TO BANK 0 ${address.toHexString} $value")
      if (ram != null) {
        if (address == 0) codeBank = value & 0xF
        else if (address == 1) dataBank = value & 0xF
        else ram(address) = value
      }
    }
  }

  private def readBank15(address: Int): Int = {
    if (address < 0x800) ram(address) // 2K RAM
    else if (address < 0x1000) 0xFF // 2K EXT BUFFER RAM
    else if (address < 0x2000) { // 4K DISK ROM
      if (romSlot(0) != null) romSlot(0)(address & 0x1FFF) else 0xFF
    }
    else if (address < 0x4000) { // CARTRIDGE ROM/RAM bank 1
      if (romSlot(1) != null) romSlot(1)(address & 0x1FFF) else 0xFF
    }
    else if (address < 0x6000) { // CARTRIDGE ROM/RAM bank 2
      if (romSlot(2) != null) romSlot(2)(address & 0x1FFF) else 0xFF
    }
    else if (address < 0x8000) { // CARTRIDGE ROM/RAM bank 3
      if (romSlot(3) != null) romSlot(3)(address & 0x1FFF) else 0xFF
    }
    else if (address < 0xC000) basicROM(address & 0x3FFF) // 16K BASIC ROM
    // I/O
    else if (address < 0xD000) 0xFF // NOT USED
    else if (address < 0xD800) screenRam(address & 0x7FF)
    else if (address < 0xD900) _6845.read(address)
    else if (address < 0xDA00) 0xFF // DISK UNITS ??
    else if (address < 0xDB00) sid.read(address) // SID
    else if (address < 0xDC00) ciaip.read(address) // IPCIA
    else if (address < 0xDD00) ciaieee.read(address)
    // IEEE 488
    else if (address < 0xDE00) acia.read(address) // 6551 RS-232
    else if (address < 0xDF00) tpi_ieee488.read(address) // 6525 IEEE 488
    else if (address < 0xE000) tpi_kb.read(address) // 6525 KEYBOARD
    else kernalROM(address & 0x1FFF)
  }

  private def writeBank15(address: Int,value: Int): Unit = {
    if (address < 0x800) ram(address) = value
    // 2K RAM
    else if (address < 0x1000) {} // 2K EXT BUFFER RAM
    else if (address < 0x2000 && romSlotRAMAccess(0)) romSlot(0)(address & 0x1FFF) = value // 4K DISK ROM
    else if (address < 0x4000 && romSlotRAMAccess(1)) romSlot(1)(address & 0x1FFF) = value // CARTRIDGE ROM/RAM bank 1
    else if (address < 0x6000 && romSlotRAMAccess(2)) romSlot(2)(address & 0x1FFF) = value // CARTRIDGE ROM/RAM bank 2
    else if (address < 0x8000 && romSlotRAMAccess(3)) romSlot(3)(address & 0x1FFF) = value // CARTRIDGE ROM/RAM bank 3
    else if (address < 0xC000) {} // 16K BASIC ROM
    // I/O
    else if (address < 0xD000) {} // NOT USED
    else if (address < 0xD800) screenRam(address & 0x7FF) = value
    else if (address < 0xD900) _6845.write(address,value)
    else if (address < 0xDA00) {} // DISK UNITS ??
    else if (address < 0xDB00) sid.write(address,value) // SID
    else if (address < 0xDC00) ciaip.write(address,value) // IPCIA
    else if (address < 0xDD00) ciaieee.write(address,value) // IEEE 488
    else if (address < 0xDE00) acia.write(address,value) // 6551 RS-232
    else if (address < 0xDF00) tpi_ieee488.write(address,value) // 6525 IEEE 488
    else if (address < 0xE000) tpi_kb.write(address,value) // 6525 KEYBOARD

    TestCart.write(0xF0000 | address,value)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = true
}
