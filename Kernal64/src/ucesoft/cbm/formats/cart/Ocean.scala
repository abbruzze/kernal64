package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Ocean(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address == 0xDE00) {
      val bank = value & 0x3F
      romlBankIndex = bank
      romhBankIndex = bank
    }
  }

  override def ROMH: Memory = ROML
}
