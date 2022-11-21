package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class EpyxFastload(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (address == 0xDF18) {
      game = true
      exrom = false
      notifyMemoryConfigurationChange
    } else if (address == 0xdf38) {
      game = true
      exrom = true
      notifyMemoryConfigurationChange
    }
    romlBanks(0).read((address & 0x1fff) + 0x8000)
  }
  override def reset  : Unit = {
    game = true
    exrom = false
    notifyMemoryConfigurationChange
  }
}
