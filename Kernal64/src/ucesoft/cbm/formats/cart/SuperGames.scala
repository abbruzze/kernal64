package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

import java.io.{ObjectInputStream, ObjectOutputStream}

class SuperGames(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var latch = false
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (!latch && address == 0xDF00) {
      val bank = value & 0x03
      romlBankIndex = bank
      romhBankIndex = bank
      val invGame = (value & 4) > 0
      latch = (value & 8) > 0
      game = invGame
      exrom = invGame
      notifyMemoryConfigurationChange
    }
  }

  override def reset: Unit = {
    latch = false
    romlBankIndex = 0
    romhBankIndex = 0
    game = false
    exrom = false
    notifyMemoryConfigurationChange
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(latch)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    latch = in.readBoolean
  }
}
