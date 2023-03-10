package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Mach5(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (address < 0xDF00) ROML.read(0x9E00 | address & 0xFF)
    else ROML.read(0x9F00 | address & 0xFF)
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    if (address < 0xDF00) {
      if (!(!exrom && game)) {
        exrom = false
        game = true
        notifyMemoryConfigurationChange()
      }
    }
    else {
      if (!(exrom && game)) {
        exrom = true
        game = true
        notifyMemoryConfigurationChange()
      }
    }
  }
  override def reset(): Unit = {
    exrom = false
    game = true
  }
}
