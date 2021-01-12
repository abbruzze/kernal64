package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Zaxxon(crt: Cartridge, ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  object ROMLMirrored extends Memory {
    val name = "ROML"
    val startAddress = 0x8000
    val length = 8192
    val isRom = true
    def isActive = true
    def init  : Unit = {}
    val roml = Zaxxon.super.ROML.asInstanceOf[ROM]
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      romhBankIndex = if ((address & 0x9000) != 0x8000) 1 else 0
      roml.data(address & 0x0FFF)
    }
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  }

  override def ROML: Memory  = ROMLMirrored
}