package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory

trait FlashListener {
  def flash(address:Int,value:Int,low:Boolean)
  def eraseSector : Unit
}

class AMF29F040(address:Int,low:Boolean,flash : FlashListener) {
  private final val READMODE_NORMAL = 0
  private final val READMODE_AUTO = 1
  private val mask5555 = address | 0x555
  private val mask2aaa = address | 0x2AA
  private var readMode = READMODE_NORMAL
  private var step = 0
  private var rom : Memory = _

  def setROMBank(bank:Memory) : Unit = rom = bank

  def write(address:Int, value:Int) : Unit = {
    this.step match {
      case 0 =>
        if (value == 0xF0) {
          readMode = READMODE_NORMAL
          return
        }
        if ((address == mask5555) && (value == 0xAA)) {
          step += 1
          return
        }
        readMode = READMODE_NORMAL
        return
      case 1|4 =>
        if ((address == mask2aaa) & (value == 0x55)) {
          step += 1
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 2 =>
        if (address != mask5555) return
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
        if ((address == mask5555) && (value == 0xAA)) {
          step += 1
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 5 =>
        if ((address == mask5555) && (value == 0x10)) {
          // chip erase ignored
          return
        }
        if (value == 0x30) {
          flash.eraseSector
          step = 0
          return
        }
        step = 0
        readMode = READMODE_NORMAL
        return
      case 6 =>
        flash.flash(address & 0x1FFF,value,low)
    }

    step = 0
  }

  def read(address:Int) : Int = {
    if (readMode == READMODE_AUTO && (address & 0x9FFF) < 0x8003) {
      address & 3 match { // autoselect codes for AMF29F040
        case 0 => 1
        case 1 => 0xA4
        case 2 => 0
      }
    }
    else rom.read(address)
  }
}

