package ucesoft.cbm.misc

import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.{Clock, ClockEvent}

trait FlashListener {
  def flash(address:Int,value:Int,low:Boolean) : Unit
  def eraseSector(address:Int) : Unit
  def chipErase(): Unit = {}
}

object AMF29F040 {
  sealed trait AMF29F040Type {
    val manufacturerID : Int
    val deviceID : Int
    val deviceIDAddress : Int
    val magics : Array[Int]
    val magicMask : Int
    val sectorSize : Int
    val sectorMask : Int
  }
  case object AMF29F040TypeB extends AMF29F040Type {
    override val manufacturerID = 1
    override val deviceID = 0xA4
    override val deviceIDAddress = 1
    override val magics = Array(0x555,0x2AA)
    override val magicMask = 0x7FF
    override val sectorSize = 0x10000
    override val sectorMask = 0x70000
  }

  case object AMF29F040Type064 extends AMF29F040Type {
    override val manufacturerID = 1
    override val deviceID = 0x7E
    override val deviceIDAddress = 2
    override val magics = Array(0xAAA, 0x555)
    override val magicMask = 0xFFF
    override val sectorSize = 0x10000
    override val sectorMask = 0x7F0000
  }

}

class AMF29F040(flashType:AMF29F040.AMF29F040Type,low:Boolean = false,flash : FlashListener = null) {
  private final val READMODE_NORMAL = 0
  private final val READMODE_AUTO = 1
  private final val READMODE_POLLING = 2
  private var readMode = READMODE_NORMAL
  private var step = 0
  private var rom : Memory = _
  private val clk = Clock.systemClock
  private var status = 0
  private var romModified = false

  def hasBeenWritten(): Boolean = romModified

  def setROMBank(bank:Memory) : Unit = rom = bank
  def setROMBank(bank:Array[Int]): Unit = {
    rom = new Memory {
      override val isRom = false
      override val length = bank.length
      override val startAddress = 0
      override val name = ""

      override def init(): Unit = {}
      override def isActive: Boolean = false

      override def read(address: Int, chipID: ID): Int = bank(address)
      override def write(address: Int, value: Int, chipID: ID): Unit = bank(address) = value
    }
  }

  @inline private def isMagic(index:Int,address:Int): Boolean = (address & flashType.magicMask) == flashType.magics(index)

  def write(address:Int, value:Int) : Unit = {
    this.step match {
      case 0 =>
        if (value == 0xF0) {
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
          romModified = true
          if (flash != null) flash.chipErase()
          else
            for(address <- 0 until rom.length) rom.write(address,0xFF)
          return
        }
        if (value == 0x30) {
          //println(s"Erase sector: ${address.toHexString} => sector erasing ${(address & flashType.sectorMask).toHexString} - ${((address & flashType.sectorMask) + flashType.sectorSize - 1).toHexString}")
          romModified = true
          if (flash != null) flash.eraseSector(address)
          else {
            val eraseFrom = address & flashType.sectorMask
            val eraseTo = (eraseFrom + flashType.sectorSize - 1) % rom.length
            for(address <- eraseFrom to eraseTo) rom.write(address,0xFF)
          }
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
        romModified = true
        if (flash != null) flash.flash(address,value,low)
        else rom.write(address,value)

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
        val adr = address & 3
        if (adr == 0) flashType.manufacturerID
        else if (adr == flashType.deviceIDAddress) flashType.deviceID
        else 0
      case READMODE_POLLING =>
        status ^= 0x40
        status
      case _ =>
        if (rom != null) rom.read(address) else 0xF1
    }
  }
}

