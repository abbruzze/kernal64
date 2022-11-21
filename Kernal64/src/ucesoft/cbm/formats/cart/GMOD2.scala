package ucesoft.cbm.formats.cart

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort
import ucesoft.cbm.misc.M93C86
import ucesoft.cbm.{ChipID, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

class GMOD2(crt: Cartridge,ram:Memory,config:Properties) extends CartridgeExpansionPort(crt,ram) {
  private[this] val CONFIGURATION_GMOD2_FILE = "gmod2.file"
  private[this] var reg = 0
  private[this] val m93c86 = new M93C86(x16 = true)

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (address == 0xDE00) m93c86.output << 7 else 0
  }
  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address == 0xDE00) {
      reg = value
      if ((value & 0x40) == 0) {
        m93c86.chipSelect(false)
        val bank = value & 0x3F
        romlBankIndex = bank
        romhBankIndex = bank
      }
      else {
        m93c86.chipSelect(true)
        m93c86.clock((value & 0x20) > 0)
        m93c86.input((value >> 4) & 1)
      }
    }
  }
  override def reset  : Unit = {
    romlBankIndex = 0
    romhBankIndex = 0
  }
  override def eject: Unit = saveEeprom
  override def shutdown: Unit = saveEeprom
  override def init  : Unit = {
    Option(config.getProperty(CONFIGURATION_GMOD2_FILE)) match {
      case None =>
      case Some(eeprom) =>
        val file = new java.io.File(eeprom)
        if (file.exists) {
          m93c86.load(file)
          Log.info(s"EEPROM loaded from $file")
        }
    }
  }
  private def saveEeprom()  : Unit = {
    Option(config.getProperty(CONFIGURATION_GMOD2_FILE)) match {
      case None =>
      case Some(eeprom) =>
        m93c86.save(new java.io.File(eeprom))
        Log.info(s"EEPROM saved to $eeprom")
    }
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeInt(reg)
    m93c86.save(out)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    reg = in.readInt
    m93c86.load(in)
  }
}
