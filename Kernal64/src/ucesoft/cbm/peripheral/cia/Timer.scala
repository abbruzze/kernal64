package ucesoft.cbm.peripheral.cia

import java.io.{ObjectInputStream, ObjectOutputStream}

import ucesoft.cbm.{CBMComponent, CBMComponentType, Log}

abstract class Timer(ciaName: String,
            id: Int,
            irqAction: (Int) => Unit,
            idleAction : (Boolean) => Unit) extends CBMComponent {

  val componentID = ciaName + "_TB"
  val componentType = CBMComponentType.CHIP

  protected final val CIAT_CR_START = 0x01
  private[this] final val CIAT_STEP = 0x04
  private[this] final val CIAT_CR_ONESHOT = 0x08
  private[this] final val CIAT_CR_FLOAD = 0x10
  private[this] final val CIAT_PHI2IN = 0x20
  private[this] final val CIAT_CR_MASK = CIAT_CR_START | CIAT_CR_ONESHOT | CIAT_CR_FLOAD | CIAT_PHI2IN

  private[this] final val CIAT_COUNT2 = 0x100
  private[this] final val CIAT_COUNT3 = 0x200

  private[this] final val CIAT_ONESHOT0 = 0x08 << 8
  private[this] final val CIAT_ONESHOT = 0x08 << 16
  private[this] final val CIAT_LOAD1 = 0x10 << 8
  private[this] final val CIAT_LOAD = 0x10 << 16

  private[this] val CIAT_OUT = 0x80000000

  private[this] var state = 0
  private[this] var lastControlValue = 0
  private[this] var timer,latch = 0xFFFF
  private[this] var pbToggle = false
  private[this] var serialActionCallback : () => Unit = null
  private[this] var stopped = false

  override def getProperties = {
    properties.setProperty("Control register", lastControlValue.toString)
    properties.setProperty("State", state.toHexString)
    properties.setProperty("Timer", timer.toString)
    properties.setProperty("Latch", latch.toString)
    properties.setProperty("Idle", stopped.toString)
    properties.setProperty("PbToggle", pbToggle.toString)
    properties.setProperty("PbToggleValue", isStateOut.toString)
    properties
  }

  def init : Unit = {}

  def reset: Unit = {
    state = 0
    lastControlValue = 0
    timer = 0xFFFF
    latch = 0xFFFF
    pbToggle = false
    stopped = false
  }

  def setSerialCallBack(serialActionCallback : Option[() => Unit]) = this.serialActionCallback = serialActionCallback.getOrElse(null)

  final def writeLo(low: Int): Unit = {
    latch = latch & 0xff00 | low & 0xff
    if ((state & CIAT_LOAD) != 0) timer = timer & 0xff00 | low & 0xff
    Log.debug(s"${componentID} set counter lo to ${low} latch=${latch}")
  }
  final def writeHi(high: Int): Unit = {
    latch = latch & 0xff | (high & 0xff) << 8
    if ((state & CIAT_LOAD) != 0 || (state & CIAT_CR_START) == 0) timer = latch
    Log.debug(s"${componentID} set counter hi to ${high} latch=${latch} started=${(lastControlValue & 1) != 0}")
  }
  final def readLo: Int = timer & 0xFF
  final def readHi: Int = timer >> 8
  final def getPbToggle : Boolean = pbToggle
  final def readCR : Int = lastControlValue
  final def setStep : Unit = state |= CIAT_STEP

  def writeCR(cr: Int): Unit = {
    state &= ~(CIAT_CR_MASK)
    state |= cr & CIAT_CR_MASK ^ CIAT_PHI2IN
    if ((cr & 1) != 0 && (lastControlValue & 1) == 0) pbToggle = true
    Log.debug(s"${componentID} started=${(cr & 1) != 0}")
    lastControlValue = cr
    stopped = false
    idleAction(false)
  }

  final def getState : Int = state
  final def isStateOut : Boolean = (state & CIAT_OUT) != 0

  final def clock : Unit = {
    if ((timer != 0) && ((state & CIAT_COUNT3) != 0)) timer -= 1

    var adj = state & (CIAT_CR_START | CIAT_CR_ONESHOT | CIAT_PHI2IN)
    if ((state & (CIAT_CR_START | CIAT_PHI2IN)) == (CIAT_CR_START | CIAT_PHI2IN)) adj |= CIAT_COUNT2
    if (((state & CIAT_COUNT2) != 0) || ((state & (CIAT_STEP | CIAT_CR_START)) == (CIAT_STEP | CIAT_CR_START))) adj |= CIAT_COUNT3
    /*
     * CR_FLOAD -> LOAD1, CR_ONESHOT -> ONESHOT0, LOAD1 -> LOAD,
     * ONESHOT0 -> ONESHOT
     */
    adj |= (state & (CIAT_CR_FLOAD | CIAT_CR_ONESHOT | CIAT_LOAD1 | CIAT_ONESHOT0)) << 8
    state = adj

    if ((timer == 0) && ((state & CIAT_COUNT3) != 0)) {
      state |= CIAT_LOAD | CIAT_OUT
      if ((state & (CIAT_ONESHOT | CIAT_ONESHOT0)) != 0) state &= ~(CIAT_CR_START | CIAT_COUNT2)
      // By setting bits 2&3 of the control register,
      // PB6/PB7 will be toggled between high and low at each
      // underflow.
      pbToggle ^= true
      // Implementation of the serial port
      if (/*!oneShot &&*/ serialActionCallback != null && (lastControlValue & 0x40) == 0x40) serialActionCallback()
      // Timer A signals underflow handling: IRQ/B-count
      underflow
    }

    if ((state & CIAT_LOAD) != 0) {
      timer = latch
      state &= ~CIAT_COUNT3
    }

    checkIdle
  }

  @inline private def checkIdle : Unit = {
    val unwanted = CIAT_OUT | CIAT_CR_FLOAD | CIAT_LOAD1 | CIAT_LOAD | CIAT_COUNT3
    if ((state & unwanted) != 0) return

    val unwanted1 = CIAT_CR_START | CIAT_PHI2IN
    val unwanted2 = CIAT_CR_START | CIAT_STEP
    if ((state & unwanted1) == unwanted1 || (state & unwanted2) == unwanted2) return

    // stop
    stopped = true
    idleAction((state & CIAT_CR_START) == 0)
  }

  protected def underflow : Unit

  protected def allowsStateRestoring : Boolean = true
  protected def loadState(in:ObjectInputStream): Unit = {
    state = in.readInt
    lastControlValue = in.readInt
    timer = in.readInt
    latch = in.readInt
    pbToggle = in.readBoolean
  }
  protected def saveState(out:ObjectOutputStream): Unit = {
    out.writeInt(state)
    out.writeInt(lastControlValue)
    out.writeInt(timer)
    out.writeInt(latch)
    out.writeBoolean(pbToggle)
  }
}
