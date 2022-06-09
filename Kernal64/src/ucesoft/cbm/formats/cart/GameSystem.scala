package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class GameSystem(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (address < 0xDF00) {
      romlBankIndex = 0
    }
    0
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address < 0xDF00) {
      romlBankIndex = (address - 0xDE00) & 0x3F
    }
  }
}
