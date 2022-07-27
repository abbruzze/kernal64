package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.c128.C128MMU
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortType}

import java.io.{ObjectInputStream, ObjectOutputStream}

class MagicDesk128(mmu:C128MMU) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.EMPTY
  override val name: String = "MagicDesk128"
  override val componentID = "MagicDesk128"

  val EXROM = true
  val GAME = true
  val ROML: Memory = null
  val ROMH: Memory = null

  private var bank = 0

  override def reset: Unit = {
    bank = 0
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address == 0xDE00) {
      mmu.setInternalFunctionROMBank(value)
    }
  }
  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = 0

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeInt(bank)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    bank = in.readInt
  }
}
