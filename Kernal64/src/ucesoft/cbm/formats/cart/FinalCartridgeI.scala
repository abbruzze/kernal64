package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class FinalCartridgeI(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if (address >= 0xDF00) { // IO2
      game = false
      exrom = false
    } else { // IO1
      game = true
      exrom = true
    }
    notifyMemoryConfigurationChange
    romlBanks(0).read((address & 0x1fff) + 0x8000)
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address >= 0xDF00) { // IO2
      game = false
      exrom = false
    } else { // IO1
      game = true
      exrom = true
    }
    notifyMemoryConfigurationChange
  }
}
