package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class KCS(crt: Cartridge,ram:Memory,nmiAction: (Boolean) => Unit) extends CartridgeExpansionPort(crt,ram) {
  private[this] val ram128 = Array.ofDim[Int](128)

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (address < 0xDF00) { // IO1
      exrom = (address & 0x02) == 2
      game = true
      notifyMemoryConfigurationChange()

      ROML.asInstanceOf[ROM].data(0x1E00 + (address & 0xFF))
    }
    else { // IO2
      if (address < 0xDF80) ram128(address & 0x7F) else (if (exrom) 0x80 else 0) | (if (!game) 0x40 else 0)
    }
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    if (address < 0xDF00) { // IO1
      game = false
      exrom = (address & 0x02) == 2
      notifyMemoryConfigurationChange()
    }
    else { // IO2
      if (address < 0xDF80) ram128(address & 0x7F) = value
    }
  }
  override def reset(): Unit = {
    exrom = false
    game = false
  }

  override def isFreezeButtonSupported = true

  override def freezeButton(): Unit = {
    exrom = true
    game = false
    notifyMemoryConfigurationChange()
    nmiAction(true)
    nmiAction(false)
  }
}
