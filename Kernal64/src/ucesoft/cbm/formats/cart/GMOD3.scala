package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.{Cartridge, CartridgeBuilder}
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort
import ucesoft.cbm.misc.EN25QH128A

class GMOD3(crt: Cartridge,ram:Memory,forwardRam:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var bitBangMode = false
  private[this] var bankConfig = 0
  private[this] var irqVector = false
  private[this] val IRQS = Array(0x08,0x00,
                                 0x08,0x00, // NMI Vector, $0008
                                 0x0C,0x80, // RESET Vector, $800c
                                 0x0C,0x00) // IRQ Vector, $000c

  // check size
  if (crt.kbSize != 2048 && crt.kbSize != 4096 && crt.kbSize != 8192 && crt.kbSize != 16384) throw new IllegalArgumentException(s"GMOD3: bad size: ${crt.kbSize}KB")

  private[this] val spiFlash = new EN25QH128A(crt.kbSize * 1024,FlashROM)

  private object FlashROM extends Memory {
    override val isRom = false
    override val length = 0
    override val startAddress = 0
    override val name = "FlashROM"
    override def init: Unit = {}
    override def isActive = true

    override def read(address: Int, chipID: ID): Int = {
      //println(s"Reading from ${address.toHexString}")
      val bank = address / 8192
      val offset = address % 8192
      romlBankIndex = bank
      romhBankIndex = bank
      ROML.read(offset)
    }

    override def write(address: Int, value: Int, chipID: ID): Unit = {
      //println(s"Writing to ${address.toHexString} = ${value.toHexString}")
      val bank = address / 8192
      val offset = address % 8192
      romlBankIndex = bank
      romhBankIndex = bank
      ROML.asInstanceOf[ROM].data(offset) = value
    }
  }

  private object IRQHackVectorRAM extends Memory {
    override val isRom = false
    override val length = 0
    override val startAddress = 0
    override val name = "IRQHackVectorRAM"
    override def init: Unit = {}
    override def isActive = true

    override def read(address: Int, chipID: ID): Int = if (irqVector && address >= 0xFFF8 && address <= 0xFFFF) IRQS(address & 7) else -1
    override def write(address: Int, value: Int, chipID: ID): Unit = {}
  }

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    if (address >= 0xDF00) return super.read(address,chipID)
    if (bitBangMode) return spiFlash.out << 7
    if (address < 0xDE08) return bankConfig & 0xFF
    if (address >= 0xDE08 && address < 0xDE10) return bankConfig >> 8

    super.read(address,chipID)
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address < 0xDE08) {
      if (bitBangMode) {
        val cs = (value & 0x40) == 0 // CS active low
        val clk = (value & 0x20) >> 5
        val din = (value & 0x10) >> 4
        //println(s"SPI: $cs $clk $din")
        spiFlash.clock(cs,clk,din)
      }
      else {
        bankConfig = ((address & 0x07) << 8) | value
        //println(s"Bank = ${bankConfig.toHexString}")
        romlBankIndex = bankConfig
      }
    }
    else
    if (address == 0xDE08) { // control register
      bitBangMode = (value & 0x80) > 0
      exrom = (value & 0x40) > 0
      irqVector = (value & 0x20) > 0
      //println(s"EXROM=$exrom irqVector=$irqVector bitBangMode=$bitBangMode")

      notifyMemoryConfigurationChange
    }
  }
  override def reset  : Unit = {
    romlBankIndex = 0
    romhBankIndex = 0
    bankConfig = 0
    game = true
    exrom = false
    irqVector = false
    bitBangMode = false
    notifyMemoryConfigurationChange
  }

  override def eject : Unit = {
    ram.setForwardReadTo(None)
  }

  override def init : Unit = {
    forwardRam.setForwardReadTo(Some(IRQHackVectorRAM))
  }

  def createCRT : Unit = {
    val builder = new CartridgeBuilder(crt.file,"KERNAL64 GMOD3",62,false,true)
    for((b,rom:ROM) <- romlBanks) builder.addChip(0x8000,2,b,rom.data)

    builder.finish
  }
}
