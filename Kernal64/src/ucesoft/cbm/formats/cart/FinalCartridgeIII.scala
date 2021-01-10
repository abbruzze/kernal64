package ucesoft.cbm.formats.cart

import ucesoft.cbm.{ChipID, Clock, ClockEvent}
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

import java.io.{ObjectInputStream, ObjectOutputStream}

class FinalCartridgeIII(crt: Cartridge, nmiAction: (Boolean) => Unit,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var controlRegister = true

  game = false
  exrom = false

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    //romlBanks(romlBankIndex).read((address & 0x1fff) + 0x8000)
    //romlBanks(romlBankIndex).read(0x8000 + 0x1E00 + (address & 0x1FF))
    romlBanks(romlBankIndex).asInstanceOf[ROM].data(0x1E00 + (address & 0x1FF))
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (controlRegister && address == 0xdfff) {
      romlBankIndex = value & 3
      romhBankIndex = value & 3
      game = (value & 0x20) != 0
      exrom = (value & 0x10) != 0
      notifyMemoryConfigurationChange
      nmiAction((value & 0x40) == 0)
      if ((value & 0x80) != 0) controlRegister = false
    }
  }

  override def isFreezeButtonSupported = true

  override def freezeButton  : Unit = {
    val clk = Clock.systemClock
    clk.pause
    clk.schedule(new ClockEvent("Freeze", clk.currentCycles + 3, cycles => {
      //exrom = true
      game = false
      notifyMemoryConfigurationChange
      nmiAction(true)
      controlRegister = true
    }))
    clk.play
  }

  override def reset  : Unit = {
    controlRegister = true
    romlBankIndex = 0
    romhBankIndex = 0
    game = false//crt.GAME
    exrom = false//crt.EXROM
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(controlRegister)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    controlRegister = in.readBoolean
  }
}
