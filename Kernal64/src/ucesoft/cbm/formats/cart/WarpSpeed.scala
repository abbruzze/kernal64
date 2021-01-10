package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class WarpSpeed(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    val offs = address - startAddress
    romlBanks(0).read(0x9E00 + offs)
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address >= 0xDF00) {
      game = true
      exrom = true
    }
    else {
      game = false
      exrom = false
    }
    notifyMemoryConfigurationChange
  }

  override def reset = {
    game = false
    exrom = false
    notifyMemoryConfigurationChange
  }
}
