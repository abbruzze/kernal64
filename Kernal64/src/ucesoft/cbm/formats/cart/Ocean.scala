package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort

class Ocean(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  object ROMLMirrored extends ROMMemory {
    val name = "ROMH"
    val startAddress = 0xA000
    val length = 8192
    val isRom = true
    def isActive = true
    def init  : Unit = {}
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = romlBanks(romlBankIndex).read(0x8000 + address - startAddress)
  }
  private[this] val VALID_CRT = crt.kbSize == 128 || crt.kbSize == 256 || crt.kbSize == 512
  private[this] val CRT_16K = crt.kbSize == 128 || crt.kbSize == 256
  private[this] lazy val T2 = crt.name == "T2"

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address == 0xDE00) {
      val bank = value & 0x3F
      if (CRT_16K) {
        if (bank < 16) romlBankIndex = bank else romhBankIndex = bank - 16
      }
      else { // 8K
        romlBankIndex = bank
      }
    }
  }

  override def convertBankNumber(bank: Int): Int = if (!T2 && bank > 15) bank - 16 else bank

  override def ROMH = if (VALID_CRT) super.ROMH else ROMLMirrored
}
