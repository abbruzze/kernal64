package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class FunPlay(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  override protected def convertBankNumber(bank: Int): Int = ((bank >> 3) & 7) | ((bank & 1) << 3)

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address == startAddress) {
      romlBankIndex = ((value >> 3) & 7) | ((value & 1) << 3)
      if ((value & 0xc6) == 0x00) {
        exrom = false
        game = true
        notifyMemoryConfigurationChange
      } else if ((value & 0xc6) == 0x86) {
        exrom = true
        game = true
        notifyMemoryConfigurationChange
      }
    }
  }
}
