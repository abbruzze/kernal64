package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.Log
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class CIATimerA2(ciaName: String,
                 id: String,
                 irqAction: (String) => Unit,
                 autoClock: Boolean = true,
                 timerToNotify: Option[CIATimerA2] = None) extends CBMComponent {
  val componentID = ciaName + id
  val componentType = CBMComponentType.CHIP 
  
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
  
  def setSerialCallBack(serialActionCallback : Option[() => Unit]) = this.serialActionCallback = serialActionCallback
  def getLatch = latch
  def isStartedAndInContinousMode = started && !oneShot
  def isStarted = started
  
  def init {}
  
  import Clock.systemClock

  def reset {
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
    systemClock.cancel(EVENT_ID)
  }
  
  override def getProperties = {
    properties.setProperty("Control register",Integer.toHexString(readCR))
    properties.setProperty("Started",started.toString)
    properties.setProperty("Counter",Integer.toHexString(readLo | readHi << 8))
    properties.setProperty("Latch",Integer.toHexString(latch))
    properties.setProperty("One shot",oneShot.toString)
    properties.setProperty("Count external",countExternal.toString)
    properties
  }  

  final def writeLo(lo: Int) {
    latch = (latch & 0xFF00) | (lo & 0xFF)
    //if ((cr & 0x10) > 0) counter = (counter & 0xFF00) | lo & 0xFF
    Log.debug(s"${ciaName}-${id} set counter lo to ${lo} latch=${latch}")
    //println(s"${ciaName}-${id} set counter lo to ${lo} latch=${latch} prev=${prev}")
  }

  final def writeHi(hi: Int) {
    latch = ((hi & 0xFF) << 8) | (latch & 0x00FF)
    if (!started) counter = latch
    Log.debug(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch}")
    //println(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch} prev=${prev}")
  }

  final def readLo = (if (autoClock) getCounter else counter) & 0xFF  
  final def readHi = (if (autoClock) getCounter else counter) >> 8
  
  @inline private def getCounter : Int = {
    val cycles = systemClock.currentCycles
    if (!started || countExternal || cycles < startCycle) counter
    else {
      val elapsed = cycles - startCycle
      val actualCounter = ((counter - elapsed).asInstanceOf[Int] & 0xFFFF)
      if (actualCounter > 0) actualCounter else 0
    }
  }

  final def readCR = (cr & 0xEE) | (if (started) 1 else 0) // we mask the reload bit
  final def writeCR(value: Int) {
    cr = value
    val startTimer = (cr & 1) == 1
    oneShot = (cr & 8) == 8
    timerUnderflowOnPortB = (cr & 2) == 2
    toggleMode = (cr & 4) == 0
    val reload = (cr & 0x10) == 0x10
    countSystemClock = (value & 0x20) == 0 
    // bit 1,2 and 5 ignored
    val currentCountExternal = countExternal
    handleCR567    
    if (reload) {
      counter = latch // reload immediately
    }
    enableTimer(startTimer,reload,currentCountExternal)
    Log.debug(s"${ciaName}-${id} control register set to ${cr} latch=${latch}")
  }

  protected def handleCR567 {}
  
  private[this] val underflowCallback = underflow _
  private[this] val toggleToFalse = (cycles:Long) => { toggleValue = false }
  
  @inline private def reschedule(delay:Int,count:Int) {
    val cycles = systemClock.currentCycles
    startCycle = cycles + delay
    val zeroCycle = startCycle + count
    
    systemClock.schedule(new ClockEvent(EVENT_ID,zeroCycle,underflowCallback,UNDERFLOW_SUBID))
  }

  private def enableTimer(enabled: Boolean,reload:Boolean,oldCountExternal:Boolean) {
    if (!started && enabled) { // start from stopped
      if (!countExternal && autoClock) reschedule(START_DELAY,counter)
      startDelayCount = START_DELAY
    } 
    else 
    if (started && enabled) { // start from started
      if (reload && autoClock) {
        systemClock.cancel(EVENT_ID)
        if (!countExternal) reschedule(START_DELAY,counter)        
      }
    }
    else
    if (started && !enabled) { // stop timer from started
      if (autoClock) {
        systemClock.cancel(EVENT_ID)
        if (!oldCountExternal && countSystemClock) counter = getCounter
      }            
    }

    started = enabled
  }

  private def externalUnderflow = if (countExternal && started) {
    counter = (counter - 1) & 0xFFFF
    if (counter == 0) underflow(systemClock.currentCycles)
    //else counter -= 1
  }
  protected def setCountExternal(enabled: Boolean) {
    countExternal = enabled
    Log.debug(s"${ciaName}-${id} countExternal=${enabled}")
  }
  
  /**
   * Manual clock
   */
  final def clock {
    if (started) {
      if (startDelayCount > 0) startDelayCount -= 1
      else {
        if (counter == 0) underflow(0)
        else counter -= 1
      }
    }
  }

  private def underflow(cycles: Long) {
    if (!countSystemClock) return // don't manage CNT counting
    
    // check serial callback
    if (/*!oneShot &&*/ serialActionCallback.isDefined) serialActionCallback.get()
    // reset counter with latch value
    counter = latch
    //Log.debug(s"${ciaName}-${id} counter is zero")
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
      case Some(tm) => tm.externalUnderflow
    }

    irqAction(id)
    if (oneShot) started = false
    else if (!countExternal && autoClock) reschedule(START_CONT_DELAY,counter)
  }
  // state
  protected def saveState(out:ObjectOutputStream) {
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
  protected def loadState(in:ObjectInputStream) {
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
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}

class CIATimerB2(ciaName: String, id: String, irqAction: (String) => Unit,autoClock:Boolean = true) extends CIATimerA2(ciaName, id, irqAction,autoClock) {
  override protected def handleCR567 {
    val bit56 = (cr >> 5) & 0x3
    setCountExternal(bit56 == 2)
    countSystemClock = bit56 == 2 || bit56 == 0
  }
}