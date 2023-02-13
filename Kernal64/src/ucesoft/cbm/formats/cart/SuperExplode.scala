package ucesoft.cbm.formats.cart

import ucesoft.cbm.{ChipID, Clock, ClockEvent}
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class SuperExplode(crt: Cartridge,ram:Memory,mainReset: () => Unit) extends CartridgeExpansionPort(crt,ram) {
  private final val CAPACITOR_TIMEOUT = 295 // +/- 300us
  private var lastStateChangeClock = 0L
  private var clocked = false
  private final val clk = Clock.systemClock

  private class ROMLAccess extends Memory {
    override val isRom: Boolean = true
    override val length: Int = 0
    override val startAddress: Int = 0
    override val name: String = ""

    override def init(): Unit = {}
    override def isActive: Boolean = true
    override def read(address: Int, chipID: ID): Int = {
      enableROM()
      romLBank.read(address)
    }

    override def write(address: Int, value: Int, chipID: ID): Unit = {
      romLBank.write(address,value)
    }
  }

  private val romlBridge = new ROMLAccess

  override def ROML: Memory = romlBridge

  private def schedule(): Unit = {
    clocked = true
    clk.schedule(new ClockEvent("SuperExplode",clk.nextCycles,tick _))
  }

  private def enableROM(): Unit = {
    if (exrom) {
      exrom = false
      notifyMemoryConfigurationChange()
    }
    lastStateChangeClock = clk.currentCycles
    if (!clocked) schedule()
  }

  private def tick(clocks:Long): Unit = {
    if (clocks - lastStateChangeClock > CAPACITOR_TIMEOUT && !exrom) {
      exrom = true
      notifyMemoryConfigurationChange()
      clocked = false
    }
    else schedule()
  }

  override def reset(): Unit = {
    game = true
    exrom = true
    romlBankIndex = 0
    enableROM()
    if (!clocked) schedule()
  }

  override def freezeButton(): Unit = mainReset()
  override def isFreezeButtonSupported: Boolean = true

  override def eject(): Unit = {
    clk.cancel("SuperExplode")
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if ((address & 0xFF00) == 0xDE00) enableROM()
    else if (address == 0xDF00) romlBankIndex = value >> 7
  }

  override def read(address: Int, chipID: ID): Int = {
    if ((address & 0xFF00) == 0xDE00) enableROM()
    if ((address & 0xFF00) == 0xDF00) romLBank.read(0x9F00 | address & 0xFF)
    else 0x80
  }
}
