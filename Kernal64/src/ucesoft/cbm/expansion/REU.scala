package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import java.io.{BufferedInputStream, File, FileInputStream, ObjectInputStream, ObjectOutputStream}

import ucesoft.cbm.Log

object REU {
  val REU_1700 = 128
  val REU_1750 = 512
  val REU_1764 = 256
  val REU_1M = 1024
  val REU_2M = 2048
  val REU_4M = 4096
  val REU_8M = 8192
  val REU_16M = 16384

  var attached16MFileName : String = null

  def getREU(size:Int,mem:Memory,setDMA: (Boolean) => Unit,setIRQ: (Boolean) => Unit,file:Option[File]) : ExpansionPort = {
    val reu = new REUImpl(size,mem,setDMA,setIRQ)
    file match {
      case Some(f) =>
        reu.loadFrom(f)
      case None =>
    }
    reu
  }

  private class REUImpl(size:Int,
                        mem:Memory,
                        setDMA: (Boolean) => Unit,
                        setIRQ: (Boolean) => Unit) extends ExpansionPort {
    val TYPE : ExpansionPortType.Value = ExpansionPortType.REU
    override val name = "REU_" + size
    override val componentID = "REU"

    val EXROM = true
    val GAME = true
    val ROML = null
    val ROMH = null

    final private[this] val IS_256 = size == REU.REU_1764
    private[this] val J1 = if (size != REU.REU_1700) 1 << 4 else 0

    private[this] val clk = Clock.systemClock
    private[this] val reuMem = Array.ofDim[Int](size << 10)
    final private[this] val REU_WRAP_ADDRESS = (size << 10) - 1
    // status & command registers
    private[this] var statusRegister = 0
    private[this] var commandRegister = 0x10
    // addressing registers
    private[this] var c64Address,reuAddress,transferRegister = 0
    private[this] var shadowC64Address = 0
    private[this] var shadowReuAddress = 0xF80000
    private[this] var shadowTransferRegister = 0xFF
    // control registers
    private[this] var interruptMaskRegister = 0x1F
    private[this] var addressControlRegister = 0x3F

    private[this] var currentOperation = IDLE_OP
    private[this] var exchangeFirstPhase = true
    private[this] var exchangeTmp1,exchangeTmp2 = 0
    private[this] var ff00 = false

    final private[this] val IDLE_OP = -1
    final private[this] val C64_TO_REU_OP = 0
    final private[this] val REU_TO_C64_OP = 1
    final private[this] val EXCHANGE_OP = 2
    final private[this] val VERIFY_OP = 3

    final private[this] val STATUS_END_OF_BLOCK = 0x40
    final private[this] val STATUS_VERIFY_ERROR = 0x20
    final private[this] val CMD_EXECUTE = 0x80
    final private[this] val CMD_AUTOLOAD = 0x20
    final private[this] val CMD_FF00_TRIGGER = 0x10
    final private[this] val CMD_TRANSFER_TYPE = 0x3
    final private[this] val CTRL_IRQ_MASK = 0x80
    final private[this] val CTRL_IRQ_END_OF_BLOCK_MASK = 0x40
    final private[this] val CTRL_IRQ_VERIFY_ERROR_MASK = 0x20

    final private[this] val FLOATING_BUS_VALUE_DECAY_CYCLES = 260
    private[this] var floatingBusValue = 0xFF
    private[this] var floatingBusValueDecayCycle = Long.MaxValue
    private[this] var reuBank = 0


    override def eject  : Unit = {
      mem.setForwardWriteTo(None)
    }

    def loadFrom(file:File) : Unit = {
      if (file.length > reuMem.length) throw new IllegalArgumentException("REU file size is greater than the REU size")
      Log.info("Loading REU from " + file)
      val in = new BufferedInputStream(new FileInputStream(file))
      val buffer = Array.ofDim[Byte](1024)
      var index = 0
      var read = 0
      while (read != -1) {
        read = in.read(buffer)
        if (read != -1) {
          for(i <- 0 until read) reuMem(index + i) = buffer(i).toInt & 0xFF
          index += read
        }
      }
      in.close
    }

    override def init : Unit = if (size != REU_16M) ramInitPattern

    final override def reset  : Unit = {
      statusRegister = 0
      commandRegister = 0x10
      c64Address = 0
      reuAddress = 0
      transferRegister = 0
      interruptMaskRegister = 0x1F
      addressControlRegister = 0x3F
      currentOperation = IDLE_OP
      shadowC64Address = 0
      shadowReuAddress = 0xF80000
      shadowTransferRegister = 0xFF
      floatingBusValue = 0xFF
      floatingBusValueDecayCycle = Long.MaxValue
      reuBank = 0
    }

    private def ramInitPattern : Unit = {
      var s = 0xFF
      var m = 0
      var i = 0
      while (i < reuMem.length) {
        for(_ <- 0 until 64) {
          reuMem(i) = s ; i += 1
          reuMem(i) = m ; i += 1
          reuMem(i) = m ; i += 1
          reuMem(i) = s ; i += 1
        }
        s ^= 0xFF
        m ^= 0xFF
      }
    }

    override def hardReset : Unit = {
      reset
      if (size != REU_16M) ramInitPattern
    }

    final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (currentOperation == IDLE_OP && address >= 0xDF00 && address < 0xE000) readREU(address & 0x1F)
      else super.read(address,chipID)
    }

    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
      if (address == 0xFF00 && ff00 && (commandRegister & CMD_EXECUTE) > 0) {
        ff00 = false
        //println("Starting deferred operation")
        startOperation
      }
      else
        if (currentOperation == IDLE_OP && address >= 0xDF00 && address < 0xE000) {
          //println(s"Writing REU at ${Integer.toHexString(address)}")
          writeREU(address & 0x1F,value)
        }
    }

    private def readREU(offset:Int) = {
      offset match {
        case 0 =>	// status register, bit 7-5 cleared after reading
          val oldReg = statusRegister
          statusRegister &= 0x1F
          setIRQ(false)
          (oldReg | J1) & 0xF8 // 0-3 bits are 0
        case 1 => commandRegister
        case 2 => c64Address & 0xFF
        case 3 => c64Address >> 8
        case 4 => reuAddress & 0xFF
        case 5 => reuAddress >> 8
        case 6 => ((reuAddress >> 16) & 0xFF) | 0xF8
        case 7 => transferRegister & 0xFF
        case 8 => transferRegister >> 8
        case 9 => interruptMaskRegister | 0x1F
        case 10 => addressControlRegister | 0x3F
        case _ => 0xFF
      }
    }

    private def writeREU(offset:Int,value:Int) : Unit = {
      offset match {
        case 1 =>
          commandRegister = value
          checkOperation
        case 2 =>
          shadowC64Address &= 0xff00
          shadowC64Address |= (value & 0xff)
          c64Address = shadowC64Address
        case 3 =>
          shadowC64Address &= 0x00ff
          shadowC64Address |= (value & 0xff) << 8
          c64Address = shadowC64Address
        case 4 =>
          shadowReuAddress &= 0xffff00
          shadowReuAddress |= (value & 0xff)
          /* copy bits, keep Bank */
          reuAddress &= REU_WRAP_ADDRESS & 0xff0000
          reuAddress |= shadowReuAddress & 0xffff
          reuAddress &= REU_WRAP_ADDRESS
        case 5 =>
          shadowReuAddress &= 0xff00ff
          shadowReuAddress |= (value & 0xff) << 8
          /* copy bits, keep Bank */
          reuAddress &= REU_WRAP_ADDRESS & 0xff0000
          reuAddress |= shadowReuAddress & 0xffff
          reuAddress &= REU_WRAP_ADDRESS
        case 6 =>
          /*
    		   * Modify bank and shadow copy of bank, kept on the high bits of
    		   * ramAddr, which is a deviation from hardware's behavior.
    		   */
          reuAddress &= 0xffff
          reuAddress |= (value & 0xff) << 16
          reuAddress &= REU_WRAP_ADDRESS
          shadowReuAddress &= 0xffff
          shadowReuAddress |= (value & 0xff) << 16
          reuBank = value & 0x07
        case 7 =>
          shadowTransferRegister &= 0xff00
          shadowTransferRegister |= (value & 0xff)
          transferRegister = shadowTransferRegister
        case 8 =>
          shadowTransferRegister &= 0x00ff
          shadowTransferRegister |= (value & 0xff) << 8
          transferRegister = shadowTransferRegister
        case 9 =>
          interruptMaskRegister = value
          checkInterrupt
        case 10 => addressControlRegister = value
        case _ =>
      }
    }

    private def checkOperation  : Unit = {
      ff00 = (commandRegister & 0x90) == 0x80
      if ((commandRegister & 0x90) == 0x90) clk.schedule(new ClockEvent("REUStartOperation",clk.nextCycles,cycles => startOperation))
      if (ff00) {
        //println(s"Start of operation ${currentOperation} deferred clk=${clk.currentCycles}")
        mem.setForwardWriteTo(Some(this))
      }
      else mem.setForwardWriteTo(None)
    }

    private def startOperation  : Unit = {
      currentOperation = commandRegister & CMD_TRANSFER_TYPE
      //println(s"Start of operation ${currentOperation} clk=${clk.currentCycles} c64Addr=${Integer.toHexString(c64Address)} reuAddr=${Integer.toHexString(reuAddress)} length=${Integer.toHexString(transferRegister)}")
      setDMA(true)	// DMA request

      currentOperation match {
        case C64_TO_REU_OP => transferOperation(true)
        case REU_TO_C64_OP => transferOperation(false)
        case VERIFY_OP => verifyOperation
        case EXCHANGE_OP =>
          exchangeFirstPhase = true
          exchangeOperation
      }
    }

    @inline private def readFromREU : Int = {
      val r = if (IS_256 && reuBank > 3) {
        //if (clk.currentCycles > floatingBusValueDecayCycle) floatingBusValue = 0xFF
        //floatingBusValueDecayCycle = clk.currentCycles + FLOATING_BUS_VALUE_DECAY_CYCLES
        floatingBusValue
      }
      else reuMem(reuAddress)
      //floatingBusValueDecayCycle = clk.currentCycles + FLOATING_BUS_VALUE_DECAY_CYCLES
      r
    }

    @inline private def writeToREU(value:Int) : Unit = {
      if (IS_256) {
        if (reuBank < 4) reuMem(reuAddress) = value
      }
      else reuMem(reuAddress) = value

      floatingBusValue = value
      //floatingBusValueDecayCycle = clk.currentCycles + FLOATING_BUS_VALUE_DECAY_CYCLES
    }

    private def exchangeOperation  : Unit = {
      if (!baLow) { // exchange
        if (exchangeFirstPhase) {
          exchangeTmp1 = mem.read(c64Address)
          exchangeTmp2 = readFromREU
          clk.schedule(new ClockEvent("REUExchange",clk.nextCycles,cycles => exchangeOperation))
        }
        else {
          mem.write(c64Address,exchangeTmp2)
          writeToREU(exchangeTmp1)
          incrementAddresses
          if (transferRegister == 0x01) {
            floatingBusValue = readFromREU
            statusRegister |= STATUS_END_OF_BLOCK
            clk.schedule(new ClockEvent("REUEndOperation",clk.currentCycles + 1,cycles => endOperation))
          }
          else {
            transferRegister = (transferRegister - 1) & 0xFFFF
            clk.schedule(new ClockEvent("REUExchange",clk.nextCycles,cycles => exchangeOperation))
          }
        }
        exchangeFirstPhase = !exchangeFirstPhase
      }
      else clk.schedule(new ClockEvent("REUExchange",clk.nextCycles,cycles => exchangeOperation))
    }

    private def verifyOperation  : Unit = {
      if (!baLow) { // verify
        if (mem.read(c64Address) != readFromREU) statusRegister |= STATUS_VERIFY_ERROR
        incrementAddresses
        if (transferRegister == 0x01) {
          statusRegister |= STATUS_END_OF_BLOCK
          clk.schedule(new ClockEvent("REUEndOperation",clk.currentCycles + 1,cycles => endOperation))
        }
        else {
          transferRegister = (transferRegister - 1) & 0xFFFF
          if ((statusRegister & STATUS_VERIFY_ERROR) > 0) {
            if (transferRegister == 0x01 && mem.read(c64Address) == readFromREU) statusRegister |= STATUS_END_OF_BLOCK
            clk.schedule(new ClockEvent("REUEndOperation",clk.currentCycles + 2,_ => endOperation))
          }
          else
            clk.schedule(new ClockEvent("REUVerify",clk.nextCycles,cycles => verifyOperation))
        }
      }
      else
        clk.schedule(new ClockEvent("REUVerify",clk.nextCycles,cycles => verifyOperation))
    }

    private def transferOperation(isC64Source:Boolean) : Unit = {
      //println(s"Transfer balow=$baLow op=${currentOperation} clk=${clk.currentCycles} c64Addr=${Integer.toHexString(c64Address)} reuAddr=${Integer.toHexString(reuAddress)} length=${Integer.toHexString(transferRegister)}")
      if (!baLow) { // transfer
        if (isC64Source) writeToREU(mem.read(c64Address))
        else mem.write(c64Address,readFromREU)
        incrementAddresses
        if (transferRegister == 0x01) {
          statusRegister |= STATUS_END_OF_BLOCK
          clk.schedule(new ClockEvent("REUEndOperation",clk.currentCycles + 1,cycles => endOperation))
          if (!isC64Source) {
            floatingBusValue = readFromREU
          }
        }
        else {
          transferRegister = (transferRegister - 1) & 0xFFFF
          clk.schedule(new ClockEvent("REUTransfer",clk.nextCycles,cycles => transferOperation(isC64Source)))
        }
      }
      else
        clk.schedule(new ClockEvent("REUTransfer",clk.nextCycles,cycles => transferOperation(isC64Source)))
    }

    private def checkInterrupt  : Unit = {
      if ((interruptMaskRegister & CTRL_IRQ_MASK) > 0) {
        val irq = ((statusRegister & STATUS_END_OF_BLOCK) > 0 && (interruptMaskRegister & CTRL_IRQ_END_OF_BLOCK_MASK) > 0) ||
          ((statusRegister & STATUS_VERIFY_ERROR) > 0 && (interruptMaskRegister & CTRL_IRQ_VERIFY_ERROR_MASK) > 0)
        if (irq) {
          setIRQ(true)
          statusRegister |= 0x80
        }
      }
    }

    private def endOperation  : Unit = {
      // clear execute bit
      commandRegister &= ~CMD_EXECUTE
      // set FF00 bit
      commandRegister |= CMD_FF00_TRIGGER
      mem.setForwardWriteTo(None)
      // released DMA
      setDMA(false)
      // check IRQ
      checkInterrupt
      // check autoload
      if ((commandRegister & CMD_AUTOLOAD) > 0) {
        //println(s"Autoload ${currentOperation} clk=${clk.currentCycles} c64Addr=${Integer.toHexString(c64Address)} reuAddr=${Integer.toHexString(reuAddress)} length=${Integer.toHexString(transferRegister)}")
        c64Address = shadowC64Address
        reuAddress = shadowReuAddress & REU_WRAP_ADDRESS
        transferRegister = shadowTransferRegister
      }
      //println(s"End of operation ${currentOperation} clk=${clk.currentCycles} c64Addr=${Integer.toHexString(c64Address)} reuAddr=${Integer.toHexString(reuAddress)} length=${Integer.toHexString(transferRegister)}")
      currentOperation = IDLE_OP
    }

    @inline private def incrementAddresses  : Unit = {
      if ((addressControlRegister & 0x80) == 0) c64Address = (c64Address + 1) & 0xFFFF
      if ((addressControlRegister & 0x40) == 0) reuAddress = (reuAddress + 1) & REU_WRAP_ADDRESS
    }

    override def saveState(out: ObjectOutputStream): Unit = {
      out.writeInt(size)
      super.saveState(out)
      out.writeInt(statusRegister)
      out.writeInt(commandRegister)
      out.writeInt(c64Address)
      out.writeInt(reuAddress)
      out.writeInt(transferRegister)
      out.writeInt(interruptMaskRegister)
      out.writeInt(addressControlRegister)
      out.writeInt(currentOperation)
      out.writeInt(shadowC64Address)
      out.writeInt(shadowReuAddress)
      out.writeInt(shadowTransferRegister)
      out.writeObject(reuMem)
      out.writeInt(reuBank)
    }

    override def loadState(in: ObjectInputStream): Unit = {
      // size is handled by export port handler
      super.loadState(in)
      statusRegister = in.readInt
      commandRegister = in.readInt
      c64Address = in.readInt
      reuAddress = in.readInt
      transferRegister = in.readInt
      interruptMaskRegister = in.readInt
      addressControlRegister = in.readInt
      currentOperation = in.readInt
      shadowC64Address = in.readInt
      shadowReuAddress = in.readInt
      shadowTransferRegister = in.readInt
      loadMemory[Int](reuMem,in)
      reuBank = in.readInt
      checkOperation
    }
  }
}
