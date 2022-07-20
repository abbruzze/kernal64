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

class MOS6532(val name:String,val portA:MOS653X.Port,val portB:MOS653X.Port,val irqLow: Boolean => Unit) {
  import MOS653X._

  protected val ram: Array[Int] = Array.ofDim[Int](128)
  protected val regs: Array[Int] = Array.ofDim[Int](4)
  protected var timer = 0
  protected var lastPA7Value = true
  protected var pa7NegativeEdgeMode = true
  protected var pa7IRQ,pa7IRQEnabled = false
  protected var timerIRQ,timerIRQEnabled = false
  protected var timerMultiplier = 1
  protected var timerMultiplierCounter = 1

  def reset(): Unit = {
    java.util.Arrays.fill(ram,0)
    java.util.Arrays.fill(regs,0)
    timer = 0
    lastPA7Value = true
    pa7NegativeEdgeMode = true
    pa7IRQ = false
    pa7IRQEnabled = false
    timerIRQ = false
    timerIRQEnabled = false
    timerMultiplier = 1
    timerMultiplierCounter = 1
  }

  def writeRAM(address:Int,value:Int): Unit = ram(address & (ram.length - 1)) = value
  def readRAM(address:Int): Int = ram(address & (ram.length - 1))

  def write(address:Int,value:Int): Unit = {
    address & 7 match {
      case adr@(PRA|DDRA) =>
        println(s"[6532 $name] write ${if (adr == PRA) "PRA" else "DDRA"} = ${value.toHexString}")
        regs(adr) = value
        val byte = (regs(PRA) | ~regs(DDRA)) & 0xFF
        portA.write(byte)
      case adr@(PRB|DDRB) =>
        println(s"[6532 $name] write ${if (adr == PRB) "PRB" else "DDRB"} = ${value.toHexString}")
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
    println(s"[6532 $name] setting timer = ${value.toHexString} multiplier = $timerMultiplier")
    checkIRQ()
  }

  protected def writeEdgeDetectControl(address:Int): Unit = {
    pa7IRQEnabled = (address & 2) > 0
    pa7NegativeEdgeMode = (address & 1) == 0
    println(s"[6532 $name] write PA7 mode: pa7IRQEnabled=$pa7IRQEnabled pa7NegativeEdgeMode=$pa7NegativeEdgeMode")
  }

  def pa7(value:Boolean): Unit = {
    if (pa7NegativeEdgeMode) {
      pa7IRQ = lastPA7Value && !value
    }
    else {
      pa7IRQ = !lastPA7Value && value
    }
    checkIRQ()
    lastPA7Value = value
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
    irqLow((pa7IRQEnabled & pa7IRQ) | (timerIRQEnabled & timerIRQ))
  }

  protected def readInterruptFlag(): Int = {
    val st = (if (timerIRQ) 0x80 else 0) | (if (pa7IRQ) 0x40 else 0)
    pa7IRQ = false // clear PA7 IRQ
    checkIRQ()
    st
  }
}
