package ucesoft.cbm.misc

import ucesoft.cbm.{Clock, ClockEvent}
import ucesoft.cbm.cpu.Memory

import scala.collection.mutable.ListBuffer

class EN25QH128A(size:Int,mem:Memory) {
  private val STATUS_WIP = 0x01
  private val STATUS_WEL = 0x02
  private val STATUS_BLOCK_PROTECT_MASK = 0x04 | 0x08 | 0x10 | 0x20

  private val SPI_2MB_FLASH_SIZE = 2 * 1024 * 1024
  private val SPI_4MB_FLASH_SIZE = 4 * 1024 * 1024
  private val SPI_8MB_FLASH_SIZE = 8 * 1024 * 1024
  private val SPI_16MB_FLASH_SIZE = 16 * 1024 * 1024

  private val MANUFACTURER_ID = 0x1C
  private val DEVICE_ID = 0x17
  private val RDID = MANUFACTURER_ID << 16 | 0x70 << 8 | (size match {
    case SPI_2MB_FLASH_SIZE => 0x03
    case SPI_4MB_FLASH_SIZE => 0x06
    case SPI_8MB_FLASH_SIZE => 0x0C
    case SPI_16MB_FLASH_SIZE => 0x18
  })
  private val UNIQUE_ID_NUMBER = 0x19191919 // 96 bits = 3 x 32 bits

  private val STATE_READY = 0
  private val STATE_RST_1 = 1
  private val STATE_RST_2 = 2
  private val STATE_WRDI = 3
  private val STATE_WRDEN = 4
  private val STATE_RDSR = 5
  private val STATE_WRSR_1 = 6
  private val STATE_WRSR_2 = 7
  private val STATE_READ_1 = 8
  private val STATE_READ_2 = 9
  private val STATE_WRITE_1 = 10
  private val STATE_WRITE_2 = 11
  private val STATE_BLOCK_ERASE_1 = 12
  private val STATE_BLOCK_ERASE_2 = 13
  private val STATE_CHIP_ERASE_1 = 14
  private val STATE_READ_MAN_DEV_ID_1 = 15
  private val STATE_READ_MAN_DEV_ID_2 = 16
  private val STATE_READ_ID_1 = 17
  private val STATE_READ_ID_2 = 18
  private val STATE_READ_UNIQUE_ID_1 = 19
  private val STATE_READ_UNIQUE_ID_2 = 20
  private val STATE_READ_UNIQUE_ID_3 = 21
  private val STATE_READ_UNIQUE_ID_4 = 22

  private var status = 0
  private var state = STATE_READY
  private var flashDout = 0
  private var flashCs = false
  private var flashClk = 1
  private var bitCounter = 0
  private var data = 0
  private var rsten = false
  private var address = 0
  private val writeBuffer = new ListBuffer[Int]
  private var blockEraseUnit = 0
  private val clock = Clock.systemClock

  def reset : Unit = {
    status = 0
    state = STATE_READY
    flashDout = 0
    flashCs = false
    flashClk = 1
    bitCounter = 0
    data = 0
    rsten = false
    address = 0
    writeBuffer.clear()
    blockEraseUnit = 0
  }

  final def out : Int = flashDout

  final def clock(cs:Boolean,clk:Int,flashDin:Int) : Unit = {
    state match {
      case STATE_READY =>
        if (shiftBitIn(cs,clk,flashDin)) { // command read
          // check command
          data match {
            case 0x66 => // RSTEN
              state = STATE_RST_1
            case 0x99 if rsten => // RST
              rsten = false
              state = STATE_RST_2
            case 0x04 => // WRDI
              state = STATE_WRDI
            case 0x06 => // WRDEN
              state = STATE_WRDEN
            case 0x05 => // RDSR
              data = status
              state = STATE_RDSR
            case 0x01 if (status & STATUS_WEL) > 0 => // WRSR
              state = STATE_WRSR_1
            case 0x03 => // READ
              state = STATE_READ_1
            case 0x02 if (status & STATUS_WEL) > 0 => // PP (Page Program)
              writeBuffer.clear()
              state = STATE_WRITE_1
            case 0x20 if (status & STATUS_WEL) > 0 => // SECTOR ERASE
              state = STATE_BLOCK_ERASE_1
              blockEraseUnit = 4096
            case 0x52 if (status & STATUS_WEL) > 0 => // 32K (Half Block) ERASE
              state = STATE_BLOCK_ERASE_1
              blockEraseUnit = 32768
            case 0xD8 if (status & STATUS_WEL) > 0 => // 64K Block ERASE
              state = STATE_BLOCK_ERASE_1
              blockEraseUnit = 65536
            case 0xC7 | 0x60 if (status & STATUS_WEL) > 0 => // CHIP ERASE
              state = STATE_CHIP_ERASE_1
            case 0x90 => // READ manufacturer ID and  device ID
              state = STATE_READ_MAN_DEV_ID_1
            case 0x9F if (status & STATUS_WEL) == 0 => // READID
              state = STATE_READ_ID_1
            case 0x5A => // READ UNIQUE ID
              state = STATE_READ_UNIQUE_ID_1
            case _ =>
              println(s"Command ${data.toHexString} not supported")
              ready
              rsten = false
          }
        }
      case STATE_RST_1 => // wait CS HIGH
        if (!cs) rsten = true
        ready
      case STATE_RST_2 => // wait CS HIGH
        if (!cs) rst
        ready
      case STATE_WRDI => // wait CS HIGH
        if (!cs) wrdi
        ready
      case STATE_WRDEN => // wait CS HIGH
        if (!cs) wrden
        ready
      case STATE_RDSR =>
        if (!cs) ready
        else if (shiftBitOut(clk)) data = status
      case STATE_WRSR_1 => // waiting status byte
        if (shiftBitIn(cs,clk,flashDin)) state = STATE_WRSR_2
      case STATE_WRSR_2 => // wait CS HIGH
        if (!cs) wrsr
        ready
      case STATE_READ_1 => // wait address
        if (shiftBitIn(cs,clk,flashDin,24)) {
          address = data
          state = STATE_READ_2
          data = mem.read(address)
          //println(s"READ $address")
        }
      case STATE_READ_2 =>
        if (!cs) ready
        else if (shiftBitOut(clk)) {
          address = (address + 1) & 0xFFFFFF
          data = mem.read(address)
        }
      case STATE_WRITE_1 => // wait address
        if (shiftBitIn(cs,clk,flashDin,24)) {
          address = data
          state = STATE_WRITE_2
        }
      case STATE_WRITE_2 => // wait data to write
        if (!cs) writeData
        else if (shiftBitIn(cs,clk,flashDin)) {
          if (writeBuffer.size == 256) writeBuffer.drop(1)
          writeBuffer += data
        }
      case STATE_BLOCK_ERASE_1 => // wait address
        if (shiftBitIn(cs,clk,flashDin,24)) {
          address = data
          state = STATE_BLOCK_ERASE_2
        }
      case STATE_BLOCK_ERASE_2 =>
        if (!cs) blockErase
        else ready
      case STATE_CHIP_ERASE_1 =>
        if (!cs) chipErase
        else ready
      case STATE_READ_MAN_DEV_ID_1 =>
        if (shiftBitIn(cs,clk,flashDin,24)) {
          address = data
          state = STATE_READ_MAN_DEV_ID_2
          if (address == 0) data = MANUFACTURER_ID << 8 | DEVICE_ID else data = DEVICE_ID << 8 | MANUFACTURER_ID
        }
      case STATE_READ_MAN_DEV_ID_2 =>
        if (!cs) ready
        else if (shiftBitOut(clk,16)) ready
      case STATE_READ_ID_1 => // read 3 dummy bytes ???
        if (!cs) ready
        if (shiftBitIn(cs,clk,flashDin,24)) {
          state = STATE_READ_ID_2
          data = RDID
        }
      case STATE_READ_ID_2 =>
        if (!cs) ready
        shiftBitOut(clk,24)
      case STATE_READ_UNIQUE_ID_1 => // read 3 bytes of address and a dummy byte
        if (shiftBitIn(cs,clk,flashDin,32)) {
          if ((address >> 8) == 0x80) {
            state = STATE_READ_UNIQUE_ID_2
            data = UNIQUE_ID_NUMBER
          }
          else ready
        }
      case STATE_READ_UNIQUE_ID_2 =>
        if (shiftBitOut(clk,32)) {
          state = STATE_READ_UNIQUE_ID_3
          data = UNIQUE_ID_NUMBER
        }
      case STATE_READ_UNIQUE_ID_3 =>
        if (shiftBitOut(clk,32)) {
          state = STATE_READ_UNIQUE_ID_4
          data = UNIQUE_ID_NUMBER
        }
      case STATE_READ_UNIQUE_ID_4 =>
        if (shiftBitOut(clk,32)) ready
    }

    flashCs = cs
    flashClk = clk
  }

  private def ready : Unit = {
    state = STATE_READY
    bitCounter = 0
  }

  private def chipErase : Unit = {
    status |= STATUS_WIP
    //println(s"Chip erase from 0 to ${(size - 1).toHexString}")
    for(a <- 0 to size - 1) mem.write(a,0xFF)
    waitForAndDisableWIP(60000000) // 60s
    ready
  }

  private def blockErase : Unit = {
    status |= STATUS_WIP
    val sector = address / blockEraseUnit
    val fromAddress = sector * blockEraseUnit
    for(a <- fromAddress to fromAddress + blockEraseUnit - 1) mem.write(a,0xFF)
    //println(s"Sector erase ${address.toHexString}: sector=${sector.toHexString} from: ${fromAddress.toHexString} to: ${(fromAddress + blockEraseUnit - 1).toHexString}")
    val waitFor = blockEraseUnit match {
      case 4096 => 40000    // typical 0.04s
      case 32768 => 200000  // typical 0.2s
      case 65536 => 300000  // typical 0.3s
    }
    waitForAndDisableWIP(waitFor)
    ready
  }

  private def writeData : Unit = {
    status |= STATUS_WIP
    //println(s"Writing [${writeBuffer.size}] to ${address.toHexString}")
    val msb = address & 0xFFFF00
    var a = address & 0xFF
    val it = writeBuffer.iterator
    while (it.hasNext) {
      mem.write(msb | a,it.next)
      a = (a + 1) & 0xFF
    }
    waitForAndDisableWIP(500) // typical 0.5ms
    ready
  }

  private def rst : Unit = {
    //println("RST")
    ready
    status = 0
  }

  private def wrden : Unit = status |= STATUS_WEL

  private def wrdi : Unit = status &= ~STATUS_WEL

  private def wrsr : Unit = {
    status |= STATUS_WIP
    waitForAndDisableWIP(10000) // typical 10ms
    ready
    // TODO
    //println(s"WRSR: $data")
  }

  private def waitForAndDisableWIP(cycles:Int) : Unit = clock.schedule(new ClockEvent("EN25QH128A_wait",clock.currentCycles + cycles,_ => status &= ~STATUS_WIP))

  private def shiftBitOut(clk:Int,bits:Int = 8) : Boolean = {
    if (flashClk == 1 && clk == 0) { // falling edge
      val msb = 1 << (bits - 1)
      flashDout = if ((data & msb) == msb) 1 else 0
      data <<= 1
      bitCounter += 1
    }
    if (bitCounter == bits) {
      bitCounter = 0
      true
    }
    else false
  }

  private def shiftBitIn(cs:Boolean,clk:Int,flashDin:Int,bits : Int = 8) : Boolean = {
    if (cs && flashClk == 0 && clk == 1) { // rising edge
      data = (data << 1) | (flashDin & 1)
      bitCounter += 1
    }
    if (bitCounter == bits) {
      bitCounter = 0
      data &= (1 << bits) - 1
      true
    }
    else false
  }
}
