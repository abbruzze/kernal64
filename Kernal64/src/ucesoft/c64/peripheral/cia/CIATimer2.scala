package ucesoft.c64.peripheral.cia

import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent
import ucesoft.c64.Log
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
/*
object CIATimer extends App {
  var prev = 0L
  def finishB(id:String) = {
    val delta = (System.currentTimeMillis - prev) / 1000
    println(delta)
    prev = System.currentTimeMillis
  }
  
  def finishA(id:String) {
  }
  
  Clock.setSystemClock(1) { c => }
  val tb = new CIATimerB("CIA","B",finishB)
  val ta = new CIATimerA("CIA","A",finishA,Some(tb))
  
  val event = new ClockEvent("",1,(cycles) => {
  ta.writeLo(255)
  ta.writeHi(255)
  tb.writeLo(100)
  tb.writeHi(0)
  tb.writeCR(0xC1 | 8) 
  ta.writeCR(1)
  })
  Clock.systemClock.schedule(event)
  
  Clock.systemClock.play
}
*/
class CIATimerA2(ciaName: String, id: String, irqAction: (String) => Unit, timerToNotify: Option[CIATimerA2] = None) extends C64Component {
  val componentID = ciaName + " " + id
  val componentType = C64ComponentType.CHIP 
  
  final private[this] val EVENT_ID = ciaName + id
  final private[this] val START_DELAY = 2
  final private[this] val START_CONT_DELAY = 1

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
  
  private[this] var startCycle = 0L
  
  def setSerialCallBack(serialActionCallback : Option[() => Unit]) = this.serialActionCallback = serialActionCallback
  def getLatch = latch
  def isStartedAndInContinousMode = started && !oneShot
  
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
    properties.setProperty("Counter",Integer.toHexString(counter))
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
    if (!started || (cr & 0x10) > 0) counter = latch
    Log.debug(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch}")
    //println(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch} prev=${prev}")
  }

  final def readLo = {
   val cycles = systemClock.currentCycles
   val adjCycles = (if (!countExternal && started && cycles > startCycle) cycles - startCycle else 0).toInt
   require(counter - adjCycles >= 0,"BAD COUNTER: " + EVENT_ID + " " + (counter - adjCycles) + " " + counter)
   (counter - adjCycles) & 0xFF 
  }
  final def readHi = {
    val cycles = systemClock.currentCycles
    val adjCycles = (if (!countExternal && started && cycles > startCycle) cycles - startCycle else 0).toInt
    require(counter - adjCycles >= 0,"BAD COUNTER: " + (counter - adjCycles))
    ((counter - adjCycles) >> 8) & 0xFF
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
    handleCR567
    enableTimer(startTimer)
    if (reload) {
      counter = latch // reload immediately
    }
    Log.debug(s"${ciaName}-${id} control register set to ${cr} latch=${latch}")
  }

  protected def handleCR567 {}
  
  @inline private def reschedule(delay:Int) {
    val cycles = systemClock.currentCycles
    startCycle = cycles + delay
    val zeroCycle = startCycle + latch
    
    systemClock.schedule(new ClockEvent(EVENT_ID,zeroCycle, underflow _))
  }

  private def enableTimer(enabled: Boolean) {
    if (!started && enabled) {
      if (!countExternal) reschedule(START_DELAY)
    } 
    else 
    if (started && enabled) {
      systemClock.cancel(EVENT_ID)
      if (!countExternal) reschedule(START_DELAY)
    }
    else
    if (!enabled) {
      systemClock.cancel(EVENT_ID)
      if (started && !countExternal && countSystemClock) counter = (counter - (systemClock.currentCycles - startCycle).toInt) & 0xFFFF      
    }

    started = enabled
  }

  private def externalNotify = if (countExternal && started) {
    if (counter <= 0) underflow(systemClock.currentCycles)
    else counter -= 1
  }
  protected def setCountExternal(enabled: Boolean) {
    countExternal = enabled
    Log.debug(s"${ciaName}-${id} countExternal=${enabled}")
  }

  private def underflow(cycles: Long) {
    if (!countSystemClock) return // don't manage CNT counting
    
    // check serial callback
    if (!oneShot && serialActionCallback.isDefined) serialActionCallback.get()
    // reset counter with latch value
    counter = latch
    //Log.debug(s"${ciaName}-${id} counter is zero")
    if (timerUnderflowOnPortB) {
      if (toggleMode) flipFlop = !flipFlop
      else {
        toggleValue = true
        systemClock.schedule(new ClockEvent(EVENT_ID,cycles + 2, (c) => { toggleValue = false }))
      }
    }
    timerToNotify match {
      case None =>
      case Some(tm) => tm.externalNotify
    }

    irqAction(id)
    if (oneShot) started = false
    else if (!countExternal) reschedule(START_CONT_DELAY)
  }

}

class CIATimerB2(ciaName: String, id: String, irqAction: (String) => Unit) extends CIATimerA2(ciaName, id, irqAction) {
  override protected def handleCR567 {
    val bit56 = (cr >> 5) & 0x3
    setCountExternal(bit56 == 2)
    countSystemClock = bit56 == 2 || bit56 == 0
  }
}