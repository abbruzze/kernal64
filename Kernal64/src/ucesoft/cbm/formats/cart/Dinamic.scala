package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Dinamic(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val bank = address - startAddress
    romlBankIndex = bank
    0
  }
}
