package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Comal80(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    val bank = value & 0x3
    romlBankIndex = bank
    romhBankIndex = bank
    exrom = (value & 0x80) == 0
    game = (value & 0x40) == 0x40
    notifyMemoryConfigurationChange()
  }

  override def reset(): Unit = {
    game = false
    exrom = false
    notifyMemoryConfigurationChange()
  }
}
