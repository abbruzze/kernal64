package ucesoft.cbm.peripheral.cia

import java.io.{ObjectInputStream, ObjectOutputStream}

import ucesoft.cbm.{CBMComponent, CBMComponentType, Log}

class TimerB(ciaName: String,
             id: Int,
             irqAction: (Int) => Unit,
             idleAction : (Boolean) => Unit) extends CBMComponent {

  val componentID = ciaName + "_TB"
  val componentType = CBMComponentType.CHIP

  final protected val COUNT_CLOCK = 0
  final protected val COUNT_CNT = 1
  final protected val COUNT_A = 2

  final private val S_STOP = 0
  final private val S_WAIT_THEN_COUNT = 1
  final private val S_LOAD_THEN_STOP = 2
  final private val S_LOAD_THEN_COUNT = 3
  final private val S_LOAD_THEN_WAIT_THEN_COUNT = 4
  final private val S_COUNT = 5
  final private val S_COUNT_THEN_STOP = 6
  final private val S_WAIT_THEN_COUNT_CASCADE = 7
  final private val S_COUNT_CASCADE = 8
  final private val S_LOAD_PB7_IRQ_THEN_COUNT_CASCADE = 9
  final private val S_LOAD_PB7_IRQ_THEN_STOP = 10

  private[this] var latch = 0xFFFF
  private[this] var counter = 0xFFFF
  private[this] var serialActionCallback : () => Unit = null
  private[this] var cr,newCR = 0
  private[this] var hasNewCR = false
  private[this] var countMode = COUNT_CLOCK
  protected var newCountMode = COUNT_CLOCK
  protected var hasNewCountMode = false
  private[this] var newCountModeCounter = 0
  private[this] var state = S_STOP

  var timerUnderflowOnPortB = false
  var toggleMode = true
  var flipFlop = true
  var toggleValue = false
  private[this] var resetToggleCounter = 0

  // ------------------- helpers -------------------------------------
  @inline private def isStarted(cr:Int) = (cr & 0x01) == 0x01
  @inline private def isOneshot(cr:Int) = (cr & 0x08) == 0x08
  @inline private def isTimerUnderflowOnPortB(cr:Int) = (cr & 0x02) == 0x02
  @inline private def isToggleMode(cr:Int) = (cr & 0x04) == 0x04
  @inline private def isReload(cr:Int) = (cr & 0x10) == 0x10
  // -----------------------------------------------------------------

  private def stateToString = state match {
    case S_STOP => "STOP"
    case S_WAIT_THEN_COUNT => "WAIT_THEN_COUNT"
    case S_LOAD_THEN_COUNT => "LOAD_THEN_COUNT"
    case S_COUNT => "COUNT"
    case S_LOAD_THEN_STOP => "LOAD_THEN_STOP"
    case S_COUNT_THEN_STOP => "COUNT_THEN_STOP"
    case S_LOAD_THEN_WAIT_THEN_COUNT => "LOAD_THEN_WAIT_THEN_COUNT"
    case S_WAIT_THEN_COUNT_CASCADE => "WAIT_THEN_COUNT_CASCADE"
    case S_LOAD_PB7_IRQ_THEN_COUNT_CASCADE => "LOAD_PB7_IRQ_THEN_COUNT_CASCADE"
    case S_COUNT_CASCADE => "COUNT_CASCADE"
    case S_LOAD_PB7_IRQ_THEN_STOP => "LOAD_PB7_IRQ_THEN_STOP"
  }

  private def countModeToString = countMode match {
    case COUNT_CNT => "CNT"
    case COUNT_CLOCK => "SYSTEM CLOCK"
    case COUNT_A => "COUNT TIMER A"
  }

  override def getProperties = {
    properties.setProperty("Control register",cr.toString)
    properties.setProperty("Started",isStarted(cr).toString)
    properties.setProperty("Counter",counter.toString)
    properties.setProperty("Latch",latch.toString)
    properties.setProperty("One shot",isOneshot(cr).toString)
    properties.setProperty("State",stateToString)
    properties.setProperty("Count mode",countModeToString)
    properties.setProperty("Idle",isIdle.toString)
    properties.setProperty("TimerUnderflowOnPortB",isTimerUnderflowOnPortB(cr).toString)
    properties.setProperty("ToggleMode",isToggleMode(cr).toString)
    properties.setProperty("FlipFlop",flipFlop.toString)
    properties.setProperty("Toggle value",toggleValue.toString)
    properties
  }

  def init {}

  def reset: Unit = {
    latch = 0xFFFF
    counter = 0xFFFF
    cr = 0
    timerUnderflowOnPortB = false
    toggleMode = true
    flipFlop = true
    toggleValue = false
    countMode = COUNT_CLOCK
    hasNewCR = false
    hasNewCountMode = false
    state = S_STOP
    resetToggleCounter = 0
    newCountModeCounter = 0
  }

  def setSerialCallBack(serialActionCallback : Option[() => Unit]) = this.serialActionCallback = serialActionCallback.getOrElse(null)

  final def writeLo(lo: Int) {
    latch = (latch & 0xFF00) | (lo & 0xFF)
    Log.debug(s"${componentID} set counter lo to ${lo} latch=${latch}")
  }

  final def writeHi(hi: Int) {
    latch = ((hi & 0xFF) << 8) | (latch & 0x00FF)
    if (!isStarted(cr)) counter = latch
    Log.debug(s"${componentID} set counter hi to ${hi} latch=${latch} started=${isStarted(cr)}")
  }

  final def readLo = counter & 0xFF
  final def readHi = counter >> 8

  final def readCR = cr

  def getCountMode : Int = countMode

  protected def setCountMode(cr:Int) : Unit = {
    val bit56 = (cr >> 5) & 0x3
    newCountMode = bit56 match {
      case 0 => COUNT_CLOCK
      case 1 => COUNT_CNT
      case 2 => COUNT_A
      case 3 => COUNT_A // TODO
    }
  }

  final def writeCR(value: Int): Unit = {
    hasNewCR = true
    newCR = value
    setCountMode(value)

    if ((cr & 1) == 0 && (newCR & 1) == 1) flipFlop = latch != 1 // don't know WHY ?? without this ciavarious/c10-13 don't work for M & O

    if (countMode != newCountMode) {
    //if ((countMode == COUNT_CLOCK && newCountMode == COUNT_CNT) || (newCountMode == COUNT_CLOCK && countMode == COUNT_CNT)) {
      hasNewCountMode = true
      newCountModeCounter = 2
    }
    else countMode = newCountMode

    idleAction(false)
  }

  final def clock: Unit = {
    val oldIdle = isIdle
    state match {
      case S_STOP =>
      case S_WAIT_THEN_COUNT =>
        state = S_COUNT
      case S_LOAD_THEN_STOP =>
        state = S_STOP
        reloadFromLatch
      case S_LOAD_THEN_COUNT =>
        state = S_COUNT
        reloadFromLatch
      case S_LOAD_THEN_WAIT_THEN_COUNT =>
        state = S_WAIT_THEN_COUNT
        if (counter == 1) underflow
        else reloadFromLatch
      case S_COUNT =>
        count
      case S_COUNT_THEN_STOP =>
        state = S_STOP
        count
      case S_WAIT_THEN_COUNT_CASCADE =>
        state = S_COUNT_CASCADE
      case S_COUNT_CASCADE =>
        state = S_COUNT
        countCascade
      case S_LOAD_PB7_IRQ_THEN_COUNT_CASCADE =>
        reloadFromLatch
        pb67
        irqAction(id)
        state = S_COUNT
      case S_LOAD_PB7_IRQ_THEN_STOP =>
        reloadFromLatch
        pb67
        irqAction(id)
        state = S_STOP
    }
    check_new_cr
    val newIdle = isIdle
    if (oldIdle != newIdle) idleAction(newIdle)
  }

  @inline private def isIdle : Boolean = state == S_STOP && !hasNewCountMode && !hasNewCR && resetToggleCounter == 0

  @inline private def decrement: Unit = {
    if (counter == 0 || counter - 1 == 0) {
      counter = 0

      if (state != S_STOP) underflow
    }
    else counter = (counter - 1) & 0xFFFF
  }

  final def externalUnderflow = {
    /*
    if (state != S_STOP && state != S_COUNT_THEN_STOP && state != S_LOAD_THEN_STOP) {
      if (counter == 0) {
        if (isOneshot(cr)) {
          cr &= 0xFE // stop timer
          newCR &= 0xFE
          state = S_LOAD_PB7_IRQ_THEN_STOP
        }
        else state = S_LOAD_PB7_IRQ_THEN_COUNT_CASCADE
      }
      else state = S_WAIT_THEN_COUNT_CASCADE
    }

     */
    if (state != S_STOP && state != S_COUNT_THEN_STOP && state != S_LOAD_THEN_STOP) {
      if (counter == 0) underflow
      else countCascade
    }
  }

  @inline private def count: Unit = {
    if (countMode == COUNT_CLOCK) decrement
  }

  @inline private def countCascade: Unit = {
    counter = (counter - 1) & 0xFFFF
  }

  protected def underflow: Unit = {
    reloadFromLatch
    irqAction(id)

    if (isOneshot(cr)) {
      cr &= 0xFE // stop timer
      newCR &= 0xFE
      state = S_LOAD_THEN_STOP
    }
    else state = S_WAIT_THEN_COUNT

    pb67

    // check serial callback
    if (/*!oneShot &&*/ serialActionCallback != null && (cr & 0x40) == 0x40) serialActionCallback()
  }

  @inline private def pb67: Unit = {
    //if (isTimerUnderflowOnPortB(cr)) {
      flipFlop ^= true
      if (!toggleMode) {
        toggleValue = true
        resetToggleCounter = 2
      }
    //}
  }

  @inline private def reloadFromLatch: Unit = counter = latch

  @inline private def check_new_cr: Unit = {
    if (hasNewCR) {
      state match {
        case S_STOP | S_LOAD_THEN_STOP =>
          state = if (isStarted(newCR)) { // Timer stopped, request to start
            if (isReload(newCR)) S_LOAD_THEN_WAIT_THEN_COUNT else S_WAIT_THEN_COUNT
          }
          else // Timer stopped, request to stop
          if (isReload(newCR)) S_LOAD_THEN_STOP else state
        case S_COUNT =>
          state = if (isStarted(newCR)) { // Timer started, request to start
            if (isReload(newCR)) S_LOAD_THEN_WAIT_THEN_COUNT else state
          }
          else // Timer started, request to stop
          if (isReload(newCR)) S_LOAD_THEN_STOP else S_COUNT_THEN_STOP
        case S_LOAD_THEN_COUNT | S_WAIT_THEN_COUNT =>
          if (isStarted(newCR)) { // request to start
            if (isOneshot(newCR)) {
              newCR &= 0xFE // stop timer
              state = S_STOP
            }
            else
            if (isReload(newCR)) state = S_LOAD_THEN_WAIT_THEN_COUNT
          }
          else state = S_STOP // request to stop
        case S_COUNT_CASCADE =>
          if (!isStarted(newCR)) {
            state = if (isReload(newCR)) S_LOAD_THEN_STOP else S_COUNT_THEN_STOP
          }
          else {
            if (isReload(newCR))
              state = S_LOAD_THEN_COUNT
          }
      }

      cr = newCR & 0xEF // mask reload
      hasNewCR = false
    }

    if (hasNewCountMode) {
      newCountModeCounter -= 1
      if (newCountModeCounter == 0) {
        countMode = newCountMode
        hasNewCountMode = false
      }
    }

    if (resetToggleCounter > 0) {
      resetToggleCounter -= 1
      if (resetToggleCounter == 0) toggleValue = false
    }

    timerUnderflowOnPortB = isTimerUnderflowOnPortB(cr)
    toggleMode = isToggleMode(cr)
  }

  protected def allowsStateRestoring : Boolean = true
  protected def loadState(in:ObjectInputStream): Unit = {
    latch = in.readInt
    counter = in.readInt
    cr = in.readInt
    newCR = in.readInt
    countMode = in.readInt
    state = in.readInt
    resetToggleCounter = in.readInt
    newCountModeCounter = in.readInt
    timerUnderflowOnPortB = in.readBoolean
    toggleMode = in.readBoolean
    flipFlop = in.readBoolean
    toggleValue = in.readBoolean
    hasNewCR = in.readBoolean
    hasNewCountMode = in.readBoolean
  }
  protected def saveState(out:ObjectOutputStream): Unit = {
    out.writeInt(latch)
    out.writeInt(counter)
    out.writeInt(cr)
    out.writeInt(newCR)
    out.writeInt(countMode)
    out.writeInt(state)
    out.writeInt(resetToggleCounter)
    out.writeInt(newCountModeCounter)
    out.writeBoolean(timerUnderflowOnPortB)
    out.writeBoolean(toggleMode)
    out.writeBoolean(flipFlop)
    out.writeBoolean(toggleValue)
    out.writeBoolean(hasNewCR)
    out.writeBoolean(hasNewCountMode)
  }
}
