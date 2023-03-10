package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

/**
 * Fast emulation for CIA: but lot of incompatibilities...
 * @deprecated since 1.6
 */
class CIATimerA2(ciaName: String,
                 id: Int,
                 irqAction: (Int) => Unit,
                 autoClock: Boolean = true,
                 timerToNotify: Option[CIATimerA2] = None) extends CBMComponent {
  val componentID: String = ciaName + "_TA"
  val componentType: Type = CBMComponentType.CHIP
  
  final private[this] val EVENT_ID = componentID
  final private[this] val START_DELAY = 2
  final private[this] val START_CONT_DELAY = 1
  //state
  final private[this] val UNDERFLOW_SUBID = 1
  final private[this] val TOGGLE_TO_FALSE_SUBID = 2

  private[this] var latch = 0xFFFF
  private[this] var counter = 0xFFFF
  protected var cr = 0
  private[this] var oneShot = true
  var timerUnderflowOnPortB = false
  var toggleMode = true
  var flipFlop = true
  var toggleValue = false
  private[this] var started = false
  private[this] var countExternal = false
  private[this] var serialActionCallback : Option[() => Unit] = None
  protected[this] var countSystemClock = true
  private[this] var startDelayCount = 0
  
  private[this] var startCycle = 0L
  
  def setSerialCallBack(serialActionCallback : Option[() => Unit]): Unit = this.serialActionCallback = serialActionCallback
  def getLatch: Int = latch
  def isStartedAndInContinousMode: Boolean = started && !oneShot
  def isStarted: Boolean = started
  
  def init(): Unit = {}
  
  import Clock.systemClock

  def reset(): Unit = {
    latch = 0xFFFF
    counter = 0xFFFF
    cr = 0
    oneShot = true
    timerUnderflowOnPortB = false
    toggleMode = true
    flipFlop = true
    toggleValue = false
    started = false
    countExternal = false
    if (autoClock) systemClock.cancel(EVENT_ID)
  }
  
  override def getProperties: Properties = {
    properties.setProperty("Control register",Integer.toHexString(readCR))
    properties.setProperty("Started",started.toString)
    properties.setProperty("Counter",Integer.toHexString(getCounter))
    properties.setProperty("Latch",Integer.toHexString(latch))
    properties.setProperty("One shot",oneShot.toString)
    properties.setProperty("Count external",countExternal.toString)
    properties
  }  

  final def writeLo(lo: Int) : Unit = {
    latch = (latch & 0xFF00) | (lo & 0xFF)
    //if ((cr & 0x10) > 0) counter = (counter & 0xFF00) | lo & 0xFF
    Log.debug(s"$componentID set counter lo to $lo latch=$latch")
    //println(s"${ciaName}-${id} set counter lo to ${lo} latch=${latch} prev=${prev}")
  }

  final def writeHi(hi: Int) : Unit = {
    latch = ((hi & 0xFF) << 8) | (latch & 0x00FF)
    if (!started) counter = latch
    Log.debug(s"$componentID set counter hi to $hi latch=$latch started=$started")
    //println(s"${componentID} set counter hi to ${hi} latch=${latch} prev=${prev}")
  }

  final def readLo: Int = (if (autoClock) getCounter else counter) & 0xFF
  final def readHi: Int = (if (autoClock) getCounter else counter) >> 8
  
  @inline private def getCounter : Int = {
    val cycles = systemClock.currentCycles
    if (!started || countExternal || cycles < startCycle || !countSystemClock) counter
    else {
      val elapsed = cycles - startCycle
      (counter - elapsed).asInstanceOf[Int] & 0xFFFF
      //if (actualCounter > 0) actualCounter else 0
    }
  }

  final def readCR: Int = (cr & 0xEE) | (if (started) 1 else 0) // we mask the reload bit
  final def writeCR(value: Int) : Unit = {
    cr = value
    val startTimer = (cr & 1) == 1
    oneShot = (cr & 8) == 8
    timerUnderflowOnPortB = (cr & 2) == 2
    toggleMode = (cr & 4) == 0
    val reload = (cr & 0x10) == 0x10
    val newCountSystemClock = (value & 0x20) == 0
    var startedWithCountSystemClock = false
    if (!newCountSystemClock && countSystemClock) {
      if (autoClock && started) {
        systemClock.cancel(EVENT_ID)
        counter = getCounter - 2
      }
    }
    else
    if (newCountSystemClock && !countSystemClock) {
      if (autoClock && started && startTimer) startedWithCountSystemClock = true
    }

    countSystemClock = newCountSystemClock
    // bit 1,2 and 5 ignored
    val currentCountExternal = countExternal
    handleCR567()
    enableTimer(startTimer,reload,currentCountExternal,startedWithCountSystemClock)
/*
    if (reload) {
      counter = latch // reload immediately
    }
*/
    Log.debug(s"$componentID control register set to $cr: started=$started latch=$latch countSystemClock=$countSystemClock reload=$reload onshot=$oneShot countExt=$countExternal")
  }

  protected def handleCR567() : Unit = {}
  
  private[this] val underflowCallback = underflow _
  private[this] val toggleToFalse = (cycles:Long) => { toggleValue = false }
  
  @inline private def reschedule(delay:Int,count:Int) : Unit = {
    if (countSystemClock) {
      val cycles = systemClock.currentCycles
      startCycle = cycles + delay
      val zeroCycle = startCycle + count
      flipFlop = true
      systemClock.schedule(new ClockEvent(EVENT_ID, zeroCycle, underflowCallback, UNDERFLOW_SUBID))
    }
  }

  private def enableTimer(enabled: Boolean,reload:Boolean,oldCountExternal:Boolean,startedWithCountSystemClock:Boolean) : Unit = {
    // STOPPED AND REQUEST TO START || CNT -> CLK
    if ((!started && enabled) || startedWithCountSystemClock) {
      val toCount = if (reload) latch else counter
      val startDelay = /*if (toCount == 1) 1 else*/ START_DELAY + (if (reload) 1 else 0)

      if (!countExternal && autoClock) reschedule(startDelay,toCount)
      startDelayCount = startDelay
    } 
    else
    // STARTED AND REQUEST TO START
    if (started && enabled) {
      if (reload && autoClock) {
        val startDelay = 1//if (latch == 1) 1 else 2
        systemClock.cancel(EVENT_ID)
        if (!countExternal) {
          counter = getCounter
          reschedule(startDelay,latch)
        }
      }
      else
      if (reload) counter = latch
    }
    else
    // STARTED AND REQUEST TO STOP
    if (started && !enabled) {
      if (autoClock) {
        systemClock.cancel(EVENT_ID)
        if (!oldCountExternal && countSystemClock) {
          if (reload) counter = latch else counter = getCounter + 1
        }
      }
      else {
        if (reload) counter = latch
      }
    }
    // STOPPED AND REQUEST TO STOP
    else {
      if (reload) counter = latch
    }

    started = enabled
  }

  private def externalUnderflow(): Unit = if (countExternal && started) {
    if (counter == 0) counter = latch
    else counter = (counter - 1) & 0xFFFF
    if (counter == 0) underflow(systemClock.currentCycles)
    //else counter -= 1
  }
  protected def setCountExternal(enabled: Boolean) : Unit = {
    countExternal = enabled
    Log.debug(s"$componentID countExternal=$enabled")
  }
  
  /**
   * Manual clock
   */
  final def clock() : Unit = {
    if (started) {
      if (startDelayCount > 0) startDelayCount -= 1
      else {
        if (counter == 0) underflow(0)
        else if (countSystemClock && !countExternal) counter -= 1
      }
    }
  }

  private def underflow(cycles: Long) : Unit = {
    if (!countSystemClock) return // don't manage CNT counting
    
    // check serial callback
    if (/*!oneShot &&*/ serialActionCallback.isDefined) serialActionCallback.get()
    // reset counter with latch value
    if (!countExternal) counter = latch// else counter = (latch + 1) & 0xFFFF
    //Log.debug(s"${componentID} counter is zero")
    if (timerUnderflowOnPortB) {
      if (toggleMode) flipFlop = !flipFlop
      else {
        if (autoClock) {
          toggleValue = true
          systemClock.schedule(new ClockEvent(EVENT_ID,cycles + 2, toggleToFalse,TOGGLE_TO_FALSE_SUBID))
        }
      }
    }
    timerToNotify match {
      case None =>
      case Some(tm) =>
        tm.externalUnderflow()
    }

    irqAction(id)
    if (oneShot) started = false
    else if (!countExternal && autoClock) reschedule(START_CONT_DELAY,counter)
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(latch)
    out.writeInt(counter)
    out.writeInt(cr)
    out.writeBoolean(oneShot)
    out.writeBoolean(timerUnderflowOnPortB)
    out.writeBoolean(toggleMode)
    out.writeBoolean(flipFlop)
    out.writeBoolean(toggleValue)
    out.writeBoolean(started)
    out.writeBoolean(countExternal)
    out.writeBoolean(countSystemClock)
    out.writeLong(startCycle) 
    out.writeInt(startDelayCount)
    saveClockEvents(out)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    latch = in.readInt
    counter = in.readInt
    cr = in.readInt
    oneShot = in.readBoolean
    timerUnderflowOnPortB = in.readBoolean
    toggleMode = in.readBoolean
    flipFlop = in.readBoolean
    toggleValue = in.readBoolean
    started = in.readBoolean
    countExternal = in.readBoolean
    countSystemClock = in.readBoolean
    startCycle = in.readLong
    startDelayCount = in.readInt
    loadClockEvents(in) {
      case (UNDERFLOW_SUBID,w) =>
        new ClockEvent(EVENT_ID,w,underflowCallback,UNDERFLOW_SUBID)
      case (TOGGLE_TO_FALSE_SUBID,w) =>
        new ClockEvent(EVENT_ID,w, toggleToFalse,TOGGLE_TO_FALSE_SUBID)
    }
  }
  protected def allowsStateRestoring : Boolean = true
}

class CIATimerB2(ciaName: String, id: Int, irqAction: (Int) => Unit,autoClock:Boolean = true) extends CIATimerA2(ciaName, id, irqAction,autoClock) {
  override val componentID: String = ciaName + "_TB"
  override protected def handleCR567(): Unit = {
    val bit56 = (cr >> 5) & 0x3
    setCountExternal(bit56 == 2)
    countSystemClock = bit56 == 2 || // Timer counts underflow of timer A, TODO can be removed
                       bit56 == 0 || // Timer counts System cycle
                       bit56 == 3    // Timer counts underflow of timer A if the CNT-pin is high
  }
}