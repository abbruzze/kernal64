package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

import java.io.{ObjectInputStream, ObjectOutputStream}

class PageFox(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] val cart_ram = Array.ofDim[Int](32768)
  private[this] var enabled = false
  private[this] var bankSelect,chipselect = 0

  private class ROMCart(override val startAddress:Int,isLow:Boolean) extends ROMMemory {
    val name: String = if (isLow) "ROML" else "ROMH"
    val length = 8192
    val isRom = true
    def isActive = true
    def init  : Unit = {}

    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (chipselect == 2) cart_ram((address & 0x3FFF) + (bankSelect << 14))
      else
        if (isLow) romlBanks(romlBankIndex).read(address) else romhBanks(romhBankIndex).read(address)
    }
    override def writeROM(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
      if (chipselect == 2) cart_ram((address & 0x3FFF) + (bankSelect << 14)) = value
    }
  }

  private[this] val roml = new ROMCart(0x8000,true)
  private[this] val romh = new ROMCart(0xA000,false)

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = 0

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    if (address >= 0xDE80 && address < 0xDFFF) {
      bankSelect = (value >> 1) & 0x1
      chipselect = (value >> 2) & 0x3
      enabled = (value & 0x10) == 0x0
      val bank = (chipselect << 1 | bankSelect) & 0x3
      romlBankIndex = bank
      romhBankIndex = bank
      game = !enabled
      exrom = !enabled
      notifyMemoryConfigurationChange
      //println(s"bankSelect=$bankSelect bank=$bank chipSelect=$chipselect enabled=$enabled")
    }
  }
  override def reset: Unit = {
    exrom = false
    game = false
    bankSelect = 0
    chipselect = 0
    romlBankIndex = 0
    romhBankIndex = 0
    notifyMemoryConfigurationChange
  }

  override def ROML: Memory = roml
  override def ROMH: Memory = romh

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(enabled)
    out.writeInt(bankSelect)
    out.writeInt(chipselect)
    out.writeObject(cart_ram)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    enabled = in.readBoolean
    bankSelect = in.readInt
    chipselect = in.readInt
    loadMemory[Int](cart_ram,in)
  }
}