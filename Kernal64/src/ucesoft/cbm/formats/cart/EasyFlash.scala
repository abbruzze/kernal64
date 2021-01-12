package ucesoft.cbm.formats.cart

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.{Cartridge, CartridgeBuilder}
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort
import ucesoft.cbm.misc.{AMF29F040, FlashListener}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util

object EasyFlash {
  var jumper = false
}

class EasyFlash(crt: Cartridge, ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] val io2mem = Array.ofDim[Int](256)

  private class FlashROM(low:Boolean) extends Memory with FlashListener {
    val name = "FLASH"
    val startAddress = if (low) 0x8000 else 0xE000
    val length = 8192
    val isRom = true
    def isActive = true
    def init  : Unit = {}

    private val amf29f040 = new AMF29F040(startAddress,low,this)
    val bankErasedMap = Array.ofDim[Boolean](64)

    updateROMBank
    checkErasedBanks

    private def checkErasedBanks : Unit = {
      val map = if (low) romlBanks else romhBanks
      for(b <- 0 to 63) {
        bankErasedMap(b) = !map.contains(b)
      }
    }

    override def eraseSector: Unit = {
      val bank = (if (low) romlBankIndex else romhBankIndex) & 0xF8
      for(b <- bank until bank + 8) {
        /*
        val bankPresent = if (low) romlBanks.contains(b) else romhBanks.contains(b)
        if (bankPresent) {
          val rom = (if (low) romlBanks(b) else romhBanks(b)).asInstanceOf[ROM]
          util.Arrays.fill(rom.data, 0xFF)
          bankErasedMap(b) = true
        }
         */
        val rom = getOrCreateBank(Some(b)).asInstanceOf[ROM]
        util.Arrays.fill(rom.data, 0xFF)
        bankErasedMap(b) = true
      }
    }

    private def getOrCreateBank(bankIndex:Option[Int]) : Memory = {
      val bankPresent = if (low) romlBanks.contains(bankIndex.getOrElse(romlBankIndex)) else romhBanks.contains(bankIndex.getOrElse(romhBankIndex))
      if (!bankPresent) {
        val bank = if (low) bankIndex.getOrElse(romlBankIndex) else bankIndex.getOrElse(romhBankIndex)
        val newBank = new ROM(s"$name-${if (low) "roml" else "romh"}-$bank",if (low) 0x8000 else 0xA000,0x2000,Array.fill[Int](0x2000)(0xFF))
        if (low) {
          romlBanks += bank -> newBank
          romlBankIndex = bank // force the new bank to be selected
        }
        else {
          romhBanks += bank -> newBank
          romhBankIndex = bank // force the new bank to be selected
        }
        updateROMBank
        newBank
      } else if (low) EasyFlash.super.ROML else EasyFlash.super.ROMH
    }

    override def flash(address: Int, value: Int, low: Boolean): Unit = {
      val rom = getOrCreateBank(None)
      rom.asInstanceOf[ROM].data(address) = value // flash value
      val bank = if (low) romlBankIndex else romhBankIndex
      bankErasedMap(bank) = false
      //println("FLASHING[%5b] bank %2d address %4X value %2X bank present=%5b".format(low,if (low) romlBankIndex else romhBankIndex,address,value,if (low) romlBanks.contains(romlBankIndex) else romhBanks.contains(romhBankIndex)))
    }

    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = amf29f040.read(address)

    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
      //println("Flash write to %4X = %2X".format(address,value))
      amf29f040.write(address,value)
    }

    def updateROMBank : Unit = amf29f040.setROMBank(if (low) EasyFlash.super.ROML else EasyFlash.super.ROMH)
  }

  private val flashL = new FlashROM(true)
  private val flashH = new FlashROM(false)

  override def ROML : Memory = flashL
  override def ROMH : Memory = flashH

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if (address >= 0xDF00) io2mem(address & 0xFF) else 0
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address >= 0xDF00) io2mem(address & 0xFF) = value
    else {
      if ((address & 2) == 0) {
        val bank = value & 0x3F
        //println(s"Selecting bank $bank")
        romlBankIndex = bank
        romhBankIndex = bank
        flashL.updateROMBank
        flashH.updateROMBank
      }
      else {
        //println(s"EasyFlash Control = $value (${address.toHexString})")
        val gameControlledViaBit0 = (value & 4) == 4
        exrom = (value & 2) == 0
        game = if (gameControlledViaBit0) (value & 1) == 0 else EasyFlash.jumper
        notifyMemoryConfigurationChange
      }
    }
  }

  override def reset  : Unit = {
    game = EasyFlash.jumper
    exrom = true
    romlBankIndex = 0
    romhBankIndex = 0
    notifyMemoryConfigurationChange
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeObject(io2mem)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    loadMemory[Int](io2mem,in)
  }

  def createCRT : Unit = {
    val builder = new CartridgeBuilder(crt.file,"KERNAL64 EASYFLASH",32,true,false)
    for(hl <- 0 to 1) {
      val banks = if (hl == 0) romlBanks else romhBanks
      val erasedMap = if (hl == 0) flashL.bankErasedMap else flashH.bankErasedMap
      for(b <- 0 to 63) {
        banks get b match {
          case Some(bank:ROM) =>
            if (!erasedMap(b)) builder.addChip(bank.startAddress,2,b,bank.data)
          case None =>
        }
      }
    }
    builder.finish
  }
}
