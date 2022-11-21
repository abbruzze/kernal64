package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class SimonsBasic(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    game = true
    notifyMemoryConfigurationChange
    0
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    val target = address - startAddress
    if (target == 0) {
      game = false
      notifyMemoryConfigurationChange
    }
  }
}
