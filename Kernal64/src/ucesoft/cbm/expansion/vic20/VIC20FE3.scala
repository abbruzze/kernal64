package ucesoft.cbm.expansion.vic20

import ucesoft.cbm.expansion.vic20.VIC20ExpansionPort.{Signals, VIC20ExpansionPortStateHandler}
import ucesoft.cbm.misc.AMF29F040

import java.io.{File, ObjectInputStream, ObjectOutputStream}

object VIC20FE3 extends VIC20ExpansionPortStateHandler {
  def make(romPath: String,
           signals: Signals): Either[Throwable, VIC20FE3] = {
    try {
      val f = new File(romPath)
      if (f.length() > 512 * 1024) throw new IllegalArgumentException("FE3 ROM length maximum size is 512K")
      val tmpRom = java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
      val rom = Array.ofDim[Int](0x80000)
      System.arraycopy(tmpRom,0,rom,0x5000,tmpRom.length)
      Right(new VIC20FE3(rom, signals))
    }
    catch {
      case io: Throwable =>
        Left(io)
    }
  }
  override def load(in: ObjectInputStream, signals: Signals): VIC20ExpansionPort = ???
  override def save(cart: VIC20ExpansionPort, out: ObjectOutputStream): Unit = ???
}
/*
class AMF29F040(address:Int,val rom:Array[Int]) {
  private final val READMODE_NORMAL = 0
  private final val READMODE_AUTO = 1
  private final val READMODE_POLLING = 2
  private val magic = Array(Array(0x555,0xFFF),Array(0x2AA,0xFFF))
  private var readMode = READMODE_NORMAL
  private var step = 0
  private val clk = Clock.systemClock
  private var status = 0

  @inline private def isMagic(index:Int,address:Int): Boolean = (address & magic(index)(1)) == magic(index)(0)

  def write(address:Int, value:Int) : Unit = {
    this.step match {
      case 0 =>
        if (value == 0xF0) {
          println("RESET")
          readMode = READMODE_NORMAL
          return
        }
        if (isMagic(0,address) && (value == 0xAA)) {
          step += 1
          return
        }
        readMode = READMODE_NORMAL
        return
      case 1|4 =>
        if (isMagic(1,address) & (value == 0x55)) {
          step += 1
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 2 =>
        if (!isMagic(0,address)) return
        value match {
          case 0xF0 =>
            step = 0
            readMode = READMODE_NORMAL
            return
          case 0x90 =>
            readMode = READMODE_AUTO
            step = 0
            return
          case 0xA0 =>
            step = 6
            return
          case 0x80 =>
            step += 1
            return
          case _ =>
            step = 0
            readMode = READMODE_NORMAL
            return
        }
      case 3 =>
        if (isMagic(0,address) && (value == 0xAA)) {
          step += 1
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 5 =>
        if (isMagic(0,address) && (value == 0x10)) {
          println(s"Erase chip: ${address.toHexString}")
          // chip erase ignored
          return
        }
        if (value == 0x30) {
          //flash.eraseSector
          println(s"Erase sector: ${address.toHexString} => sector erasing ${(address & 0x70000).toHexString} - ${((address & 0x70000) + 0xFFFF).toHexString}")
          java.util.Arrays.fill(rom,address & 0x70000,(address & 0x70000) + 0xFFFF,0xFF)
          status = (value & 0x80) ^ 0x80
          readMode = READMODE_POLLING
          val cycles = clk.getClockHz.toInt // 1 second (see datasheet)
          clk.schedule(new ClockEvent("AMF290F040_ERASE",clk.currentCycles + cycles, _ => readMode = READMODE_NORMAL ))
          step = 0
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 6 =>
        //flash.flash(address & 0x1FFF,value,low)
        println(s"Writing ${address.toHexString} = $value")
        rom(address) = value
        status = (value & 0x80) ^ 0x80
        readMode = READMODE_POLLING
        val cycles = 7 // 7 microseconds (ignore differences between PAL/NTSC) (see datasheet)
        clk.schedule(new ClockEvent("AMF290F040_WRITE",clk.currentCycles + cycles, _ => readMode = READMODE_NORMAL ))
    }

    step = 0
  }

  def read(address:Int) : Int = {
    readMode match {
      case READMODE_AUTO =>
        address & 3 match { // autoselect codes for AMF29F040
          case 0 => 1
          case 1 => 0xA4
          case 2 => 0
        }
      case READMODE_POLLING =>
        status ^= 0x40
        status
      case _ =>
        if (rom != null) rom(address) else 0xF1
    }
  }
}*/

class VIC20FE3(val rom:Array[Int],
               override val signals:Signals) extends VIC20ExpansionPort(signals) {

  override val portType = VIC20ExpansionPort.VICExpansionPortType.FE3
  override val componentID: String = "VIC20FE3"

  private val ram = Array.ofDim[Int](0x80000)
  private var lockbit = true
  private val regs = Array.ofDim[Int](2)
  private val flash = new AMF29F040(AMF29F040.AMF29F040TypeB)

  final private val BASE_RAM123   = 4
  final private val BASE_BLK1     = 0
  final private val BASE_BLK2     = 1
  final private val BASE_BLK3     = 2
  final private val BASE_BLK5     = 3

  final private val MODE_START    = 0
  final private val MODE_SUPER_ROM= 2
  final private val MODE_RAM1     = 4
  final private val MODE_RAM2     = 6
  final private val MODE_SUPER_RAM= 5
  final private val MODE_RAM_ROM  = 3
  final private val MODE_FLASH    = 1

  flash.setROMBank(rom)

  @inline private def calcAddress(bank:Int,base:Int,offset:Int): Int = (bank & 0xF) << 15 | ((base & 0x3) ^ ((regs(1) >> 5) & 3)) << 13 | (offset & 0x1FFF)
  @inline private def mode(): Int = (regs(0) >> 5) & 7
  @inline private def isRegsAccessible(): Boolean = {
    if ((regs(1) & 0x80) > 0) return false
    if (mode() == MODE_START) !lockbit else true
  }
  @inline private def isRAM123Enabled(): Boolean = (regs(1) & 1) == 0
  @inline private def isBLK1Enabled(): Boolean = (regs(1) & 2) == 0
  @inline private def isBLK2Enabled(): Boolean = (regs(1) & 4) == 0
  @inline private def isBLK3Enabled(): Boolean = (regs(1) & 8) == 0
  @inline private def isBLK5Enabled(): Boolean = (regs(1) & 0x10) == 0
  @inline private def blockBit(block:Int): Int = {
    block match {
      case BASE_RAM123 => 1
      case BASE_BLK1 => 2
      case BASE_BLK2 => 4
      case BASE_BLK3 => 8
      case BASE_BLK5 => 16
    }
  }
  @inline private def isRAM123WP(): Boolean = (regs(0) & 1) > 0

  override def eject(): Unit = {
    // TODO
  }

  final override def reset: Unit = {
    lockbit = true
    regs(0) = 0
    regs(1) = 0
  }

  protected def readMode(address:Int,block:Int): Option[Int] = {
    mode() match {
      case MODE_START =>
        read_START_MODE(address,block)
      case MODE_SUPER_ROM =>
        read_SUPER_ROM_MODE(address,block)
      case MODE_RAM1 =>
        read_RAM1_MODE(address,block)
      case MODE_RAM2 =>
        read_RAM2_MODE(address, block)
      case MODE_SUPER_RAM =>
        read_SUPER_RAM_MODE(address, block)
      case MODE_RAM_ROM =>
        read_RAM_ROM_MODE(address, block)
      case MODE_FLASH =>
        read_FLASH_MODE(address, block)
    }
  }

  protected def writeMode(address:Int,value:Int,block:Int): Boolean = {
    mode() match {
      case MODE_START =>
        write_START_MODE(address,value,block)
      case MODE_SUPER_ROM =>
        write_SUPER_ROM_MODE(address,value,block)
      case MODE_RAM1 =>
        write_RAM1_MODE(address,value,block)
      case MODE_RAM2 =>
        write_RAM2_MODE(address, value, block)
      case MODE_SUPER_RAM =>
        write_SUPER_RAM_MODE(address, value, block)
      case MODE_RAM_ROM =>
        write_RAM_ROM_MODE(address, value, block)
      case MODE_FLASH =>
        write_FLASH_MODE(address, value, block)
    }
  }

  // ============================= START MODE =========================================
  protected def read_START_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 | BASE_BLK1 | BASE_BLK2 | BASE_BLK3 =>
        None
      case BASE_BLK5 =>
        lockbit = true
        Some(read_eeprom(calcAddress(0, block, address)))
    }
  }
  protected def write_START_MODE(address:Int,value:Int,block:Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        false
      case _ =>
        lockbit = false
        ram(calcAddress(1,block,address)) = value
        true
    }
  }
  // ============================= SUPER ROM MODE =====================================
  protected def read_SUPER_ROM_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        Some(ram(calcAddress(0, block, address)))
      case _ =>
        val bank = regs(0) & 0xF
        Some(read_eeprom(calcAddress(bank, block, address)))
    }
  }
  protected def write_SUPER_ROM_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        ram(calcAddress(0, block, address)) = value
        true
      case _ =>
        ram(calcAddress(1, block, address)) = value
        true
    }
  }
  // ============================= RAM1 MODE ==========================================
  protected def read_RAM1_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        Some(ram(calcAddress(0, block, address)))
      case _ =>
        Some(ram(calcAddress(1, block, address)))
    }
  }
  protected def write_RAM1_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        if (!isRAM123WP())
          ram(calcAddress(0, block, address)) = value
        true
      case _ =>
        val bank = if ((regs(0) & blockBit(block)) == 0) 1 else 2
        ram(calcAddress(bank, block, address)) = value
        true
    }
  }
  // ============================= RAM2 MODE ==========================================
  protected def read_RAM2_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        Some(ram(calcAddress(0, block, address)))
      case _ =>
        val bank = if ((regs(0) & blockBit(block)) == 0) 1 else 2
        Some(ram(calcAddress(bank, block, address)))
    }
  }
  protected def write_RAM2_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        if (!isRAM123WP())
          ram(calcAddress(0, block, address)) = value
        true
      case _ =>
        ram(calcAddress(1, block, address)) = value
        true
    }
  }
  // ============================= SUPER RAM MODE =====================================
  protected def read_SUPER_RAM_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        Some(ram(calcAddress(0, block, address)))
      case _ =>
        val bank = regs(0) & 0xF
        Some(ram(calcAddress(bank, block, address)))
    }
  }
  protected def write_SUPER_RAM_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        ram(calcAddress(0, block, address)) = value
        true
      case _ =>
        val bank = regs(0) & 0xF
        ram(calcAddress(bank, block, address)) = value
        true
    }
  }
  // ============================= RAM/ROM MODE =======================================
  protected def read_RAM_ROM_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        Some(ram(calcAddress(0, block, address)))
      case _ =>
        if ((regs(0) & blockBit(block)) == 0) Some(ram(calcAddress(1, block, address)))
        else Some(read_eeprom(calcAddress(0, block, address)))
    }
  }
  protected def write_RAM_ROM_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        if (!isRAM123WP())
          ram(calcAddress(0, block, address)) = value
        true
      case _ =>
        val bank = if ((regs(0) & blockBit(block)) == 0) 1 else 2
        ram(calcAddress(bank, block, address)) = value
        true
    }
  }
  // ============================= FLASH MODE =========================================
  protected def read_FLASH_MODE(address: Int, block: Int): Option[Int] = {
    block match {
      case BASE_RAM123 =>
        None
      case _ =>
        val bank = regs(0) & 0xF
        Some(read_eeprom(calcAddress(bank, block, address)))
    }
  }
  protected def write_FLASH_MODE(address: Int, value: Int, block: Int): Boolean = {
    block match {
      case BASE_RAM123 =>
        false
      case _ =>
        val bank = regs(0) & 0xF
        flash_eeprom(calcAddress(bank, block, address),value)
        true
    }
  }
  // ==================================================================================
  protected def read_eeprom(address:Int): Int = {
    flash.read(address)
  }

  protected def flash_eeprom(address:Int,value:Int): Unit = {
    flash.write(address,value)
  }

  override def read(address: Int): Option[Int] = {
    address match {
      case 0x9C02 =>
        if (isRegsAccessible()) Some(regs(0))
        else None
      case 0x9C03 =>
        if (isRegsAccessible()) Some(regs(1))
        else None
      case _ =>
        if (address >= 0x400 && address < 0x1000) { // RAM123
          if (!isRAM123Enabled()) return None
          readMode(address, BASE_RAM123)
        }
        else if (address >= 0x2000 && address < 0x4000) { // BLK1
          if (!isBLK1Enabled()) return None
          readMode(address,BASE_BLK1)
        }
        else if (address >= 0x4000 && address < 0x6000) { // BLK2
          if (!isBLK2Enabled()) return None
          readMode(address, BASE_BLK2)
        }
        else if (address >= 0x6000 && address < 0x8000) { // BLK3
          if (!isBLK3Enabled()) return None
          readMode(address, BASE_BLK3)
        }
        else if (address >= 0xA000 && address < 0xC000) { // BLK5
          if (!isBLK5Enabled()) return None
          readMode(address, BASE_BLK5)
        }
        else
          None
    }
  }

  override def write(address: Int, value: Int): Boolean = {
    address match {
      case 0x9C02 =>
        if (isRegsAccessible()) {
          regs(0) = value
          true
        }
        else false
      case 0x9C03 =>
        if (isRegsAccessible()) {
          regs(1) = value
          true
        }
        else false
      case _ =>
        if (address >= 0x400 && address < 0x1000) { // RAM123
          if (!isRAM123Enabled()) return false
          writeMode(address,value,BASE_RAM123)
        }
        else if (address >= 0x2000 && address < 0x4000) { // BLK1
          if (!isBLK1Enabled()) return false
          writeMode(address,value,BASE_BLK1)
        }
        else if (address >= 0x4000 && address < 0x6000) { // BLK2
          if (!isBLK2Enabled()) return false
          writeMode(address, value, BASE_BLK2)
        }
        else if (address >= 0x6000 && address < 0x8000) { // BLK3
          if (!isBLK3Enabled()) return false
          writeMode(address, value, BASE_BLK3)
        }
        else if (address >= 0xA000 && address < 0xC000) { // BLK5
          if (!isBLK5Enabled()) return false
          writeMode(address, value, BASE_BLK5)
        }
        else
          false
    }
  }

}
