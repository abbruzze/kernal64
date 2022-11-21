package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Zaxxon(crt: Cartridge, ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  object ROMLMirrored extends ROMMemory {
    val name = "ROML"
    val startAddress = 0x8000
    val length = 8192
    val isRom = true
    def isActive = true
    def init  : Unit = {}
    val roml: ROM = Zaxxon.super.ROML.asInstanceOf[ROM]
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      romhBankIndex = if ((address & 0x9000) != 0x8000) 1 else 0
      roml.data(address & 0x0FFF)
    }
  }

  override def ROML: Memory  = ROMLMirrored
}
