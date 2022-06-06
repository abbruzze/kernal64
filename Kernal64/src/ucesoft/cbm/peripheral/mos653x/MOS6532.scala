package ucesoft.cbm.peripheral.mos653x

object MOS653X {
  final val PRA   = 0
  final val DDRA  = 1
  final val PRB   = 2
  final val DDRB  = 3
  final val TIMER = 4
  final val TIMER2 = 4 | 2
  final val INTERRUPT_FLAG = 5
  final val INTERRUPT_FLAG2 = 5 | 2

  trait Port {
    def read(): Int
    def write(value:Int): Unit
  }
}

class MOS6532(val portA:MOS653X.Port,val portB:MOS653X.Port,val irqLow: Boolean => Unit) {
  import MOS653X._

  protected val ram = Array.ofDim[Int](128)
  protected val regs  = Array.ofDim[Int](4)
  protected var timer = 0
  protected var lastPb7Value = true
  protected var pb7NegativeEdgeMode = true
  protected var pb7IRQ,pb7IRQEnabled = false
  protected var timerIRQ,timerIRQEnabled = false
  protected var timerMultiplier = 1
  protected var timerMultiplierCounter = 1

  def reset(): Unit = {
    java.util.Arrays.fill(ram,0)
    java.util.Arrays.fill(regs,0)
    timer = 0
    lastPb7Value = true
    pb7NegativeEdgeMode = true
    pb7IRQ = false
    pb7IRQEnabled = false
    timerIRQ = false
    timerIRQEnabled = false
    timerMultiplier = 1
    timerMultiplierCounter = 1
  }

  def writeRAM(address:Int,value:Int): Unit = ram(address & (ram.length - 1)) = value
  def readRAM(address:Int): Unit = ram(address & (ram.length - 1))

  def write(address:Int,value:Int): Unit = {
    address & 7 match {
      case adr@(PRA|DDRA) =>
        regs(adr) = value
        val byte = (regs(PRA) | ~regs(DDRA)) & 0xFF
        portA.write(byte)
      case adr@(PRB|DDRB) =>
        regs(adr) = value
        val byte = (regs(PRB) | ~regs(DDRB)) & 0xFF
        portB.write(byte)
      case _ =>
        writeTimerOrEdgeDetectControl(address,value)
    }
  }

  protected def writeTimerOrEdgeDetectControl(address:Int,value:Int): Unit = {
    if ((address & 0x10) > 0)
      writeTimer(address,value)
    else
      writeEdgeDetectControl(address)
  }

  protected def writeTimer(address:Int,value:Int): Unit = {
    timerIRQEnabled = (address & 0x08) > 0
    timerMultiplier = address & 3 match {
      case 0 => 1
      case 1 => 8
      case 2 => 64
      case 3 => 1024
    }
    timer = value
    timerMultiplierCounter = timerMultiplier
    timerIRQ = false
    checkIRQ()
  }

  protected def writeEdgeDetectControl(address:Int): Unit = {
    pb7IRQEnabled = (address & 2) > 0
    pb7NegativeEdgeMode = (address & 1) == 0
  }

  def pb7(value:Boolean): Unit = {
    if (pb7NegativeEdgeMode) {
      pb7IRQ = lastPb7Value && !value
    }
    else {
      pb7IRQ = !lastPb7Value && value
    }
    checkIRQ()
    lastPb7Value = value
  }

  def clock(): Unit = {
    if (timer > -255) {
      timerMultiplierCounter -= 1
      if (timerMultiplierCounter == 0) {
        timerMultiplierCounter = timerMultiplier
        timer -= 1
        if (timer == -1) timerIRQOccurred()
      }
    }
  }

  protected def timerIRQOccurred(): Unit = {
    timerIRQ = true
    checkIRQ()
  }

  def read(address:Int): Int = {
    address & 7 match {
      case PRA =>
        val byte = portA.read()
        (byte & ~regs(DDRA)) | (regs(PRA) & regs(DDRA))
      case DDRA =>
        regs(DDRA)
      case PRB =>
        val byte = portB.read()
        (byte & ~regs(DDRB)) | (regs(PRB) & regs(DDRB))
      case DDRB =>
        regs(DDRB)
      case TIMER|TIMER2 =>
        timerIRQEnabled = (address & 0x08) > 0
        timerIRQ = false
        checkIRQ()
        timer & 0xFF
      case INTERRUPT_FLAG|INTERRUPT_FLAG2 =>
        readInterruptFlag()
    }
  }

  protected def checkIRQ(): Unit = {
    irqLow((pb7IRQEnabled & pb7IRQ) | (timerIRQEnabled & timerIRQ))
  }

  protected def readInterruptFlag(): Int = {
    val st = (if (timerIRQ) 0x80 else 0) | (if (pb7IRQ) 0x40 else 0)
    pb7IRQ = false // clear PB7 IRQ
    checkIRQ()
    st
  }
}
