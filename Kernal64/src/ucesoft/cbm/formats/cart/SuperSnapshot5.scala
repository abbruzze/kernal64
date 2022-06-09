package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

import java.io.{ObjectInputStream, ObjectOutputStream}

class SuperSnapshot5(crt: Cartridge, nmiAction: (Boolean) => Unit,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var enabled = true
  private[this] var ramEnabled = false
  private[this] val internalRam = Array.ofDim[Int](8192)

  private object CRTRAM extends ROMMemory {
    val name = "SuperSnapShot v5 RAM"
    val startAddress = 0x0000
    val length = 0x8000
    val isActive = true
    val isRom = false
    def init  : Unit = {}
    def reset()  : Unit = {}
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (ramEnabled) internalRam(address & 0x1FFF)
      else romlBanks(romlBankIndex).read(address)
    }
    override def writeROM(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
      if (ramEnabled && enabled) internalRam(address & 0x1FFF) = value
      else romlBanks(romlBankIndex).write(address,value)
    }
  }

  game = false
  exrom = false

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    romlBanks(romlBankIndex).asInstanceOf[ROM].data((address & 0x1FFF))
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if ((address == 0xDE00 || address == 0xDE01) && enabled) {
      enabled = (value & 0x8) == 0
      romlBankIndex = ((value >> 2) & 0x1 | (value >> 3) & 0x2)
      romhBankIndex = romlBankIndex
      exrom = (value & 0x2) == 0
      ramEnabled = exrom
      game = (value & 0x1) == 1
      if (game) nmiAction(false)
      notifyMemoryConfigurationChange
    }
  }

  override def ROML: Memory = CRTRAM : Memory

  override def isFreezeButtonSupported = true

  override def freezeButton  : Unit = {
    exrom = true
    game = false
    notifyMemoryConfigurationChange
    nmiAction(true)
  }

  override def reset  : Unit = {
    ramEnabled = false
    enabled = true
    romlBankIndex = 0
    romhBankIndex = 0
    game = false//crt.GAME
    exrom = false//crt.EXROM
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(enabled)
    out.writeBoolean(ramEnabled)
    out.writeObject(internalRam)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    enabled = in.readBoolean
    ramEnabled = in.readBoolean
    loadMemory[Int](internalRam,in)
  }
}
