package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

import java.io.{ObjectInputStream, ObjectOutputStream}

class MagicDesk(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var reg = 0

  exrom = false
  game = true

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address < 0xDEFF) {
      reg = value
      romlBankIndex = value & 0x3F
      exrom = (value & 0x80) > 0
      notifyMemoryConfigurationChange()
    }
  }
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = reg

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeInt(reg)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    reg = in.readInt
  }
}
