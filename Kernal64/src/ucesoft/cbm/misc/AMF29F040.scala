package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.{Clock, ClockEvent}

trait FlashListener {
  def flash(address:Int,value:Int,low:Boolean) : Unit
  def eraseSector() : Unit
}

class AMF29F040(address:Int,low:Boolean,flash : FlashListener) {
  private final val READMODE_NORMAL = 0
  private final val READMODE_AUTO = 1
  private final val READMODE_POLLING = 2
  private val mask5555 = address | 0x555
  private val mask2aaa = address | 0x2AA
  private var readMode = READMODE_NORMAL
  private var step = 0
  private var rom : Memory = _
  private val clk = Clock.systemClock
  private var status = 0

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
        flash.flash(address & 0x1FFF,value,low)
        status = (value & 0x80) ^ 0x80
        readMode = READMODE_POLLING
        val cycles = 7 // 7 microseconds (ignore differences between PAL/NTSC) (see datasheet)
        clk.schedule(new ClockEvent("AMF290F040_WRITE",clk.currentCycles + cycles, _ => readMode = READMODE_NORMAL ))
    }

    step = 0
  }

  def read(address:Int) : Int = {
    readMode match {
      case READMODE_AUTO if (address & 0x9FFF) < 0x8003 =>
        address & 3 match { // autoselect codes for AMF29F040
          case 0 => 1
          case 1 => 0xA4
          case 2 => 0
        }
      case READMODE_POLLING =>
        status ^= 0x40
        status
      case _ =>
        if (rom != null) rom.read(address) else 0xF1
    }
  }
}

