package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.{Chip, ChipID, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

abstract class VIA(val name:String,
				   val startAddress:Int,
				   irqAction:Boolean => Unit) extends Chip with RAMComponent {
  val isRom = false
  val length = 0x400
  val isActive = true
  val id: ID = ChipID.VIA
    
  protected[this] final val regs = Array.ofDim[Int](length)
  private[this] var paLatch = 0
  private[this] var pbLatch = 0
  private[this] var t2ll 	= 0		// T2 low order latch
  private[this] var PB7 	= 0		// 7th bit of PB set by Timer 1
  private[this] var pending_t1,pending_t2,reload_t1,reload_t2 = false
  private[this] var oneshotB,oneshotBNew,acrNew = false
  protected[this] final val PB 	= 0x00	// Port B
  protected[this] final val PA 	= 0x01	// Port A
  protected[this] final val DDRB 	= 0x02  // Data Direction Register B
  protected[this] final val DDRA 	= 0x03  // Data Direction Register A
  protected[this] final val T1LC 	= 0x04  // Timer 1 Low Counter
  protected[this] final val T1HC 	= 0x05	// Timer 1 High Counter
  protected[this] final val T1LL 	= 0x06	// Timer 1 Low Latch
  protected[this] final val T1HL 	= 0x07	// Timer 1 High Latch
  protected[this] final val T2LC 	= 0x08	// Timer 2 Low Counter
  protected[this] final val T2HC 	= 0x09	// Timer 2 High Counter
  protected[this] final val SR	= 0x0A	// Shift Register
  protected[this] final val ACR 	= 0x0B	// Auxiliary Control Register
  protected[this] final val PCR 	= 0x0C	// Peripheral Control Register
  protected[this] final val IFR 	= 0x0D	// Interrupt Flag Register
  protected[this] final val IER 	= 0x0E	// Interrupt Enable Register
  protected[this] final val PA2	= 0x0F
  
  protected[this] final val IRQ_CA2 	= 0x01
  protected[this] final val IRQ_CA1 	= 0x02
  protected[this] final val IRQ_SR 	= 0x04
  protected[this] final val IRQ_CB2 	= 0x08
  protected[this] final val IRQ_CB1 	= 0x10
  private[this] final val IRQ_TIMER_2 = 0x020
  private[this] final val IRQ_TIMER_1 = 0x040
  
  private[this] final val PA_LATCH_ENABLED = 0x01
  private[this] final val PB_LATCH_ENABLED = 0x02

  // handshaking lines
  protected var lastCA1,lastCB1,lastCA2,lastCB2 = false
  // SR
  protected var srStarted,srLoaded = false
  protected var srCounter,srStartDelay = 0
  
  protected var active = true

  def init() : Unit = {
    active = true
    initCA1()
    initCA2()
    initCB1()
    initCB2()
  }

  def reset() : Unit = {
    java.util.Arrays.fill(regs,0)
    active = true
    paLatch = 0
    pbLatch = 0
    t2ll = 0
    PB7 = 0
    pending_t1 = false
    pending_t2 = false
    reload_t1 = false
    reload_t2 = false
    oneshotB = false
    oneshotBNew = true // because ACR5 = 0 => oneshot
    acrNew = true
    srStarted = false
    srLoaded = false
    srCounter = 0
    srStartDelay = 0

    initCA1()
    initCA2()
    initCB1()
    initCB2()
  }
    
  def setActive(active:Boolean) : Unit = {
    if (!this.active && active) {
      init()
    }
    this.active = active
  }
  
  final def irq_set(irq:Int) : Unit = {
    regs(IFR) |= irq //| 0x80	// enable irq bit + 7th bit
    checkIRQ()
  }
  final def irq_clr(irq:Int) : Unit = {
    regs(IFR) &= ~irq & 0x7F
    //if ((regs(IFR) & 0x7F) == 0) regs(IFR) = 0 // if no more irq are set clear 7th bit
    checkIRQ()
  }
  @inline private def checkIRQ() : Unit ={
    val irq = (regs(IFR) & regs(IER)) > 0
    if (irq) regs(IFR) |= 0x80 else regs(IFR) &= 0x7F
    irqAction(irq)
  }
  
  @inline private def is_set(reg:Int,bits:Int) = (regs(reg) & bits) > 0

  @inline private def srMode(): Int = (regs(ACR) >> 2) & 7

  protected def addPB7(value:Int):Int = if (is_set(ACR,0x80)) (value & 0x7F) | PB7 else value

  /*
   * Ignores DDRA & DDRB. Subclasses are in charge for this check
   */
  def read(address: Int, chipID: ChipID.ID): Int = address & 0x0F match {
    case PA =>
      irq_clr(IRQ_CA1)
      val PCR_CA2_CTRL = (regs(PCR) >> 1) & 7
      if (PCR_CA2_CTRL != 1 && PCR_CA2_CTRL != 3) irq_clr(IRQ_CA2) // check for independent interrupt mode
      Log.debug(s"Cleared IRQ_CA1: IFR=${Integer.toBinaryString(regs(IFR))} IER=${Integer.toBinaryString(regs(IER))}")
      if (is_set(ACR,PA_LATCH_ENABLED)) paLatch else regs(PA) // & ~regs(DDRA)
    case PA2 =>
      if (is_set(ACR,PA_LATCH_ENABLED)) paLatch else regs(PA) // & ~regs(DDRA)
    case PB =>
      irq_clr(IRQ_CB1)
      val PCR_CB2_CTRL = (regs(PCR) >> 5) & 7
      if (PCR_CB2_CTRL != 1 && PCR_CB2_CTRL != 3) irq_clr(IRQ_CB2) // check for independent interrupt mode
      val pb7 = if (is_set(ACR,0x80)) PB7 else 0
      (if (is_set(ACR,PB_LATCH_ENABLED)) paLatch else regs(PB)) | pb7 //& (~regs(DDRB) | (if (is_set(ACR,0x80)) 0x80 else 0))
    case SR =>
      irq_clr(IRQ_SR)
      checkSR()
      regs(SR)
    case T1LC =>
      irq_clr(IRQ_TIMER_1)
      regs(T1LC)
	  case T2LC =>
  	  irq_clr(IRQ_TIMER_2)
  	  regs(T2LC)
    case IER =>
      regs(IER) | 0x80
    case ofs =>
      regs(ofs)
  }
  
  /*
   * Ignores DDRA & DDRB. Subclasses are in charge for this check
   */
  def write(address: Int, value: Int, chipID: ChipID.ID): Unit = address & 0x0F match {
    case DDRA =>
      regs(DDRA) = value
      write(startAddress + PA,(regs(PA) | ~value) & 0xFF,chipID)
    case DDRB =>
      regs(DDRB) = value
      write(startAddress + PB,(regs(PB) | ~value) & 0xFF,chipID)
    case PA =>
      irq_clr(IRQ_CA1)
      val PCR_CA2_CTRL = (regs(PCR) >> 1) & 7
      if (PCR_CA2_CTRL != 1 && PCR_CA2_CTRL != 3) irq_clr(IRQ_CA2) // check for independent interrupt mode
      regs(PA) = value
    case PB =>
      irq_clr(IRQ_CB1)
      val PCR_CB2_CTRL = (regs(PCR) >> 5) & 7
      if (PCR_CB2_CTRL != 1 && PCR_CB2_CTRL != 3) irq_clr(IRQ_CB2) // check for independent interrupt mode
      regs(PB) = value
    case T1LC =>
      regs(T1LL) = value
      Log.debug(s"$name write to T1LC => T1LL=${Integer.toHexString(regs(T1LL))}")
    case T1HC =>
      regs(T1HL) = value
      regs(T1HC) = value
      regs(T1LC) = regs(T1LL)
      pending_t1 = true
      reload_t1 = true
      irq_clr(IRQ_TIMER_1)
      if (is_set(ACR,0x80)) PB7 = 0x00
      Log.debug(s"$name write T1HC => T1HL=${Integer.toHexString(regs(T1HL))} T1HC=${Integer.toHexString(regs(T1HC))} T1LC=${Integer.toHexString(regs(T1LC))} PB7=$PB7")
    case T1LL =>
      regs(T1LL) = value
      Log.debug(s"$name write T1LL => T1LL=${Integer.toHexString(regs(T1LL))}")
    case T1HL =>
      regs(T1HL) = value
      irq_clr(IRQ_TIMER_1)
      Log.debug(s"$name write T1HL => T1HL=${Integer.toHexString(regs(T1HL))}")
    case T2LC =>
      t2ll = value
      Log.debug(s"$name writing T2LC => t2ll=${Integer.toHexString(t2ll)}")
    case T2HC =>
      regs(T2HC) = value
      regs(T2LC) = t2ll
      irq_clr(IRQ_TIMER_2)
      pending_t2 = true
      reload_t2 = true
      Log.debug(s"$name writing T2HC => T2LC=${Integer.toHexString(regs(T2LC))} T2HC=${Integer.toHexString(regs(T2HC))}")
    case SR =>
      regs(SR) = value
      irq_clr(IRQ_SR)
      checkSR()
      Log.debug(s"$name writing SR => SR=${Integer.toHexString(regs(SR))}")
    case IFR =>
      regs(IFR) &= ~value
      Log.debug(s"$name writing IFR => IFR=${Integer.toBinaryString(regs(IFR))}")
      checkIRQ()
    case IER =>
      if ((value & 0x80) > 0) regs(IER) |= value & 0x7F else regs(IER) &= ~value
      Log.debug(s"$name writing IER => IER=${Integer.toBinaryString(regs(IER))}")
      checkIRQ()
    case ACR =>
      acrNew = true
      regs(ACR) = value
      oneshotBNew = !is_set(ACR,0x20)
      //if (!is_set(ACR,0x80)) PB7 = 0x00
      PB7 = if (is_set(ACR,0x80)) 0x80 else 0x00

      checkSR()
    case PCR =>
      regs(PCR) = value
      checkPCR()
    case ofs => 
      regs(ofs) = value
      Log.debug(s"$name writing reg $ofs => ${Integer.toBinaryString(value)}")
  }

  protected def checkSR(): Unit = {
    if (!srStarted && srMode() != 0) {
      srStarted = true
      srCounter = 0
      srStartDelay = 0
      srMode() match {
        case 1 =>
          incT2(2)
        case 2 =>
          srStartDelay = 5
        case 4 =>
          incT2(2)
        case 5 =>
          incT2(2)
          srLoaded = true
        case 6 =>
          srStartDelay = 5
        case _ =>
      }
    }
  }

  @inline private def incT2(value:Int): Unit = {
    regs(T2LC) += value
    if (regs(T2LC) > 0xFF) {
      regs(T2LC) &= 0xFF
      regs(T2HC) = (regs(T2HC) + 1) & 0xFF
    }
  }

  // ============== Handshaking lines ==============================================

  protected def checkPCR(): Unit = {
    //println(s"VIA($name) PCR set to ${regs(PCR)}")
    // CA2
    (regs(PCR) >> 1) & 7 match {
      case 6 /*110 Manual output mode: CA2 = low */ => CA2Out(false)
      case 7 /*111 Manual output mode: CA2 = high */ => CA2Out(true)
      case _ =>
    }
    // CB2
    (regs(PCR) >> 5) & 7 match {
      case 6 /*110 Manual output mode: CA2 = low */ => CB2Out(false)
      case 7 /*111 Manual output mode: CA2 = high */ => CB2Out(true)
      case _ =>
    }
  }

  protected def initCA1(): Unit = lastCA1 = false
  protected def initCA2(): Unit = lastCA2 = false
  protected def initCB1(): Unit = lastCB1 = false
  protected def initCB2(): Unit = lastCB2 = false

  protected def CA2Out(state:Boolean): Unit = {}
  protected def CB2Out(state:Boolean): Unit = {}
  def CA1In(state:Boolean): Unit = {
    val ca1HighToLow = (regs(PCR) & 1) == 0
    if (ca1HighToLow) {
      if (lastCA1 && !state) irq_set(IRQ_CA1)
    }
    else {
      if (!lastCA1 && state) irq_set(IRQ_CA1)
    }
    lastCA1 = state
  }
  def CB1In(state: Boolean): Unit = {
    val cb1HighToLow = (regs(PCR) & 0x10) == 0
    if (cb1HighToLow) {
      if (lastCB1 && !state)
        irq_set(IRQ_CB1)
    }
    else {
      if (!lastCB1 && state)
        irq_set(IRQ_CB1)
    }
    lastCB1 = state
  }
  def CA2In(state:Boolean): Unit = {
    (regs(PCR) >> 1) & 7 match {
      case 0 | 1 =>
        if (lastCA2 && !state)
          irq_set(IRQ_CA2)
      case 2 | 3 =>
        if (!lastCA2 && state) {
          irq_set(IRQ_CA2)
        }
      case _ =>
    }
    lastCA2 = state
  }

  def CB2In(state: Boolean): Unit = {
    (regs(PCR) >> 5) & 7 match {
      case 0 | 1 =>
        if (lastCB2 && !state) irq_set(IRQ_CB2)
      case 2 | 3 =>
        if (!lastCB2 && state) irq_set(IRQ_CB2)
      case _ =>
    }
    lastCB2 = state
  }
  // ===============================================================================
  
  def clock(cycles:Long) : Unit = {
    if (active) {
      updateT1()
      updateT2()

      if (acrNew) {
        acrNew = false
        oneshotB = oneshotBNew
      }

      if (srStarted) {
        srMode() match {
          case 2 =>
            if (srStartDelay == 0) shiftSR(in = true)
            else {
              srStartDelay -= 1
              if (srStartDelay == 0) {
                CA2In(true)
                CA2In(false)
              }
            }
          case 6 =>
            if (srStartDelay == 0) shiftSR(in = false)
            else srStartDelay -= 1
          case _ =>
        }
      }

    }
  }

  @inline private def updateT1(): Unit = {
    var counter = 0
    if (reload_t1) {
      counter = regs(T1LL) | regs(T1HL) << 8
      reload_t1 = false
    }
    else {
      counter = regs(T1LC) | regs(T1HC) << 8
      counter = (counter - 1) & 0xFFFF
      reload_t1 = counter == 0xFFFF
      val timeout_t1 = pending_t1 && reload_t1
      if (timeout_t1 && is_set(ACR,0x80)) {
        if (is_set(ACR,0x40)) PB7 ^= 0x80
        else PB7 = 0x80
      }
      if (timeout_t1) {
        irq_set(IRQ_TIMER_1)
        pending_t1 = is_set(ACR,0x40)
      }
    }
    regs(T1LC) = counter & 0xFF
    regs(T1HC) = (counter >> 8) & 0xFF
  }

  @inline private def updateT2(): Unit = {
    var counter = 0
    if (reload_t2) {
      counter = t2ll | regs(T2HC) << 8

      reload_t2 = false
      regs(T2LC) = counter & 0xFF
      regs(T2HC) = (counter >> 8) & 0xFF
    }
    else if (oneshotB) { // DO NOT count pulses on PB6
      counter = regs(T2LC) | regs(T2HC) << 8
      counter = (counter - 1) & 0xFFFF
      val timeout_t2 = (pending_t2 || srMode() == 2) && counter == 0xFFFF
      if (timeout_t2) {
        irq_set(IRQ_TIMER_2)
        pending_t2 = false
      }
      if (srStarted && (counter & 0xFF) == 0xFF) {
        srMode() match {
          case 1 =>
            shiftSR(in = true)
            reload_t2 = true
          case 4|5 =>
            shiftSR(in = false)
            reload_t2 = true
          case _ =>
        }
      }

      regs(T2LC) = counter & 0xFF
      regs(T2HC) = (counter >> 8) & 0xFF
    }
  }

  protected def shiftSR(in:Boolean): Unit = {
    if (in) {
      srCounter += 1
      if ((srCounter & 1) == 0) {
        regs(SR) = (regs(SR) << 1 | 1) & 0xFF
        CB1In(true)
        CB2In(true)
        CB2In(false)
      }
      else CB1In(false)

      if (srCounter == 16) {
        srCounter = 0
        srStarted = false
        irq_set(IRQ_SR)
      }
    }
    else {
      srMode() match {
        case 4 =>
          srCounter = (srCounter + 1) & 1
          if (srCounter == 1) {
            regs(SR) <<= 1
            if ((regs(SR) & 0x100) == 0x100) regs(SR) |= 1
            regs(SR) &= 0xFF
            CA2In((regs(SR) & 0x80) > 0)
          }
        case 5|6 =>
          srCounter += 1
          if ((srCounter & 1) == 1) {
            regs(SR) <<= 1
            if ((regs(SR) & 0x100) == 0x100) regs(SR) |= 1
            regs(SR) &= 0xFF
            CB1In(true)
          }
          else {
            CA2In((regs(SR) & 0x80) > 0)
            CB1In(false)
          }

          if (srCounter == 16) {
            srCounter = 0
            srStarted = srLoaded
            srLoaded = false
            irq_set(IRQ_SR)
          }
        case _ =>
      }
    }
  }

  override def getProperties: Properties = {
    properties.setProperty("T1 counter",Integer.toHexString(regs(T1LC) | regs(T1HC) << 8))
    properties.setProperty("T2 counter",Integer.toHexString(regs(T2LC) | regs(T2HC) << 8))
    properties.setProperty("T1 free running mode",is_set(ACR,0x40).toString)
    properties.setProperty("IFR / IER",s"${regs(IFR).toHexString} / ${regs(IER).toHexString}")
    properties.setProperty("T1 counter reload",reload_t1.toString)
    properties.setProperty("T2 counter reload",reload_t2.toString)
    properties.setProperty("T1 counter pending",pending_t1.toString)
    properties.setProperty("T2 counter pending",pending_t2.toString)
    super.getProperties
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(paLatch)
    out.writeInt(pbLatch)
    out.writeInt(t2ll)
    out.writeInt(PB7)
    out.writeBoolean(active)
    out.writeObject(regs)
    out.writeBoolean(reload_t1)
    out.writeBoolean(reload_t2)
    out.writeBoolean(pending_t1)
    out.writeBoolean(pending_t1)
    out.writeBoolean(oneshotB)
    out.writeBoolean(oneshotBNew)
    out.writeBoolean(acrNew)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    paLatch = in.readInt
    pbLatch = in.readInt
    t2ll = in.readInt
    PB7 = in.readInt
    active = in.readBoolean
    loadMemory[Int](regs,in)
    reload_t1 = in.readBoolean
    reload_t2 = in.readBoolean
    pending_t1 = in.readBoolean
    pending_t2 = in.readBoolean
    oneshotB = in.readBoolean
    oneshotBNew = in.readBoolean
    acrNew = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}