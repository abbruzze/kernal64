package ucesoft.cbm.peripheral.mos6525

object MOS6525 {
  trait PortAB {
    def setMOS6525(MOS6525: MOS6525): Unit = {}
    def read(): Int
    def write(value:Int): Unit
  }
  trait PortC extends PortAB {
    def setCA(bit:Int): Unit
    def setCB(bit:Int): Unit
  }

  final val PRA   = 0
  final val PRB   = 1
  final val PRC   = 2
  final val DDRA  = 3
  final val DDRB  = 4
  final val DDRC  = 5
  final val CR    = 6
  final val AIR   = 7

  sealed trait InterruptPin {
    val pin : Int
  }
  case object INT_I0 extends InterruptPin { override val pin = 0 }
  case object INT_I1 extends InterruptPin { override val pin = 1 }
  case object INT_I2 extends InterruptPin { override val pin = 2 }
  case object INT_I3 extends InterruptPin { override val pin = 3 }
  case object INT_I4 extends InterruptPin { override val pin = 4 }
}

class MOS6525(name:String,pa:MOS6525.PortAB,pb:MOS6525.PortAB,pc:MOS6525.PortC,irqLow: Boolean => Unit) {
  import MOS6525._

  val regs: Array[Int] = Array.ofDim[Int](8)
  private var ca,cb = 0
  private var irqStack = 0
  private var irqPrevious = 0xFF

  @inline private def isMode1(): Boolean = (regs(CR) & 1) == 1
  @inline private def isInterruptPriority(): Boolean = (regs(CR) & 2) == 2
  @inline private def CAControl(): Int = (regs(CR) >> 4) & 3
  @inline private def CBControl(): Int = (regs(CR) >> 6) & 3

  pa.setMOS6525(this)
  pb.setMOS6525(this)
  pc.setMOS6525(this)

  private def dump(): Unit = {
    println(s"PRA=${regs(PRA)}/${regs(DDRA)} PRB=${regs(PRB)}/${regs(DDRB)} PRC=${regs(PRC)}/${regs(DDRC)} CR=${regs(CR)} AIR=${regs(AIR)} stack=$irqStack")
  }

  private def setLatchBit(bit:Int):Unit = {
    //println(s"6525 $name Latched bit $bit priority=${isInterruptPriority()} stack=$irqStack")
    regs(PRC) |= bit // IRQ latched
    if ((regs(DDRC) & bit) > 0) { // ok, not masked
      if (isInterruptPriority()) {
        // check priority
        if (bit > (regs(PRC) & ~bit)) {
          regs(AIR) = bit
          irqLow(true)
        }
      }
      else {
        if (regs(AIR) == 0) {
          regs(AIR) |= bit
          irqLow(true)
        }
      }
      irqStack |= bit
    }
  }

  private def pushIRQState(): Int = { // READ AIR
    //println(s"PUSH: AIR = ${regs(AIR)}")
    val oldActive = regs(AIR)
    regs(PRC) &= ~oldActive
    irqStack &= ~oldActive
    regs(AIR) = 0
    if (!isInterruptPriority()) {
      regs(AIR) = irqStack
      irqStack = 0
    }
    irqLow(regs(AIR) > 0)

    oldActive
  }

  private def popIRQState(): Unit = { // WRITE AIR
    //println("POP")
    if (isInterruptPriority()) {
      if (irqStack > 0) {
        var i = 4
        while (i >= 0) {
          val bit = 1 << i
          if ((irqStack & bit) > 0) {
            regs(AIR) = bit
            irqStack &= ~bit
            i = 0
          }
          i -= 1
        }
      }
    }
    irqLow(regs(AIR) > 0)
  }

  final def write(address:Int,value:Int): Unit = {
    //println(s"6525 $name reg ${address & 7} = $value")
    val adr = address & 7
    adr match {
      case PRA|DDRA =>
        regs(adr) = value
        val byte = (regs(PRA) | ~regs(DDRA)) & 0xFF
        pa.write(byte)
      case PRB|DDRB =>
        regs(adr) = value
        val byte = (regs(PRB) | ~regs(DDRB)) & 0xFF
        pb.write(byte)
        if (isMode1()) {
          CBControl() match {
            case 0 => // LOW
              cb = 0
              pc.setCB(0)
            case 1 => // PULSES LOW
              pc.setCB(0)
              cb = 1
              pc.setCB(1)
            case _ =>
          }
        }
      case PRC|DDRC =>
        if (isMode1()) {
          if (adr == PRC) regs(PRC) &= value
          else {
            regs(DDRC) = value // irq mask
            var i = 4
            while (i >= 0) {
              val bit = 1 << i
              if ((bit & value & regs(PRC)) > 0) setLatchBit(bit)
              i -= 1
            }
          }
        }
        else {
          regs(adr) = value
          val byte = (regs(PRC) | ~regs(DDRC)) & 0xFF
          pc.write(byte)
        }
      case CR =>
        regs(adr) = value
        if (isMode1()) {
          val cac = CAControl()
          if ((cac & 2) > 0) {
            ca = cac & 1
            pc.setCA(ca)
          }
          else {
            if ((cac & 1) > 0) {
              ca = 1
              pc.setCA(1)
            }
          }
          val cbc = CBControl()
          if ((cbc & 2) > 0) {
            cb = cbc & 1
            pc.setCB(cb)
          }
          else {
            if ((cbc & 1) > 0) {
              cb = 1
              pc.setCB(1)
            }
          }
        }
      case AIR =>
        popIRQState()
    }
  }

  final def read(address:Int): Int = {
    val adr = address & 7
    adr match {
      case PRA =>
        if (isMode1()) {
          CAControl() match {
            case 0 => // LOW
              ca = 0
              pc.setCA(0)
            case 1 => // PULSES LOW
              pc.setCA(0)
              ca = 1
              pc.setCA(1)
            case _ =>
          }
        }
        pa.read()
      case PRB =>
        pb.read()
      case PRC =>
        if (isMode1())
          0xC0 | (if (regs(AIR) > 0) 0x20 else 0) | regs(PRC) & 0x1F // CA & CB are output pins => 0xC0
        else pc.read()
      case AIR =>
        pushIRQState()
      case _ =>
        regs(adr)
    }
  }

  final def setInterruptPin(_pin:InterruptPin,value:Int): Unit = {
    if (!isMode1()) return

    val pin = _pin.pin

    val bit = 1 << pin
    if (pin < 3) { // I0,I1,I2
      if ((irqPrevious & bit) > 0 && value == 0) setLatchBit(bit) // check high -> low transition on I0,I1,I2
    }
    else { // I3,I4
      pin match {
        case 3 =>
          if (CAControl() == 0 && (irqPrevious & bit) != (value << pin)) {
            ca = 1
            pc.setCA(1)
          }
          (regs(CR) >> 2) & 1 match { // I3 Edge Sel
            case 0 => // high -> low transition
              if ((irqPrevious & bit) > 0 && value == 0) setLatchBit(bit)
            case 1 => // low -> high transition
              if ((irqPrevious & bit) == 0 && value > 0) setLatchBit(bit)
          }
        case 4 =>
          if (CBControl() == 0 && (irqPrevious & bit) != (value << pin)) {
            cb = 1
            pc.setCB(1)
          }
          (regs(CR) >> 3) & 1 match { // I4 Edge Sel
            case 0 => // high -> low transition
              if ((irqPrevious & bit) > 0 && value == 0) setLatchBit(bit)
            case 1 => // low -> high transition
              if ((irqPrevious & bit) == 0 && value > 0) setLatchBit(bit)
          }
      }
    }
    if (value == 0) irqPrevious &= ~bit else irqPrevious |= bit
  }
}
