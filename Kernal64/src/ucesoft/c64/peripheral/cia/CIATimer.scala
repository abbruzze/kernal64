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
class CIATimerA(ciaName: String, id: String, irqAction: (String) => Unit, timerToNotify: Option[CIATimerA] = None) extends C64Component {
  val componentID = ciaName + " " + id
  val componentType = C64ComponentType.CHIP 
  
  private[this] val EVENT_ID = ciaName + id

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
  protected var countSystemClock = true
  
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
    properties.setProperty("One shot",oneShot.toString)
    properties.setProperty("Count external",countExternal.toString)
    properties
  }  

  final def writeLo(lo: Int) {
    latch = (latch & 0xFF00) | (lo & 0xFF)
    if ((cr & 0x10) > 0) counter = (counter & 0xFF00) | lo & 0xFF
    Log.debug(s"${ciaName}-${id} set counter lo to ${lo} latch=${latch}")
    //println(s"${ciaName}-${id} set counter lo to ${lo} latch=${latch} prev=${prev}")
  }

  final def writeHi(hi: Int) {
    latch = ((hi & 0xFF) << 8) | (latch & 0x00FF)
    if (!started || (cr & 0x10) > 0) counter = latch
    Log.debug(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch}")
    //println(s"${ciaName}-${id} set counter hi to ${hi} latch=${latch} prev=${prev}")
  }

  final def readLo = counter & 0xFF
  final def readHi = (counter >> 8) & 0xFF

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
    //println(s"${ciaName}-${id} control register set to ${cr} latch=${latch} start=${startTimer} oneShot=${oneShot} under=${timerUnderflowOnPortB} toggle=${toggleMode} reload=${reload}")
  }

  protected def handleCR567 {}

  private def enableTimer(enabled: Boolean) {
    if (!started && enabled) {
      if (!countExternal) {
        systemClock.schedule(new ClockEvent(EVENT_ID,systemClock.currentCycles + 2/*systemClock.nextCycles*/, executeCount _))
        //println(s"${ciaName}-${id} started counter=${counter} latch=${latch}")
      }
    } 
    else 
    if (started && enabled) {
      systemClock.cancel(EVENT_ID)
      systemClock.schedule(new ClockEvent(EVENT_ID,systemClock.currentCycles + 2, executeCount _))
    }
    else
    if (!enabled) systemClock.cancel(EVENT_ID)

    started = enabled
  }

  private def externalNotify = if (countExternal && started) executeCount(systemClock.currentCycles)//systemClock.schedule(new ClockEvent(EVENT_ID, systemClock.nextCycles, executeCount _))
  protected def setCountExternal(enabled: Boolean) {
    countExternal = enabled
    Log.debug(s"${ciaName}-${id} countExternal=${enabled}")
    //println(s"${ciaName}-${id} countExternal=${enabled}")
  }

  private def executeCount(cycles: Long) {
    if (!countSystemClock) return // don't manage CNT counting
    
//    if (reload) {
//      reload = false
//      counter = latch
//      Log.debug(s"${ciaName}-${id} reload to ${latch}")
//    }
    if (counter <= 0) {
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
      else if (!countExternal) systemClock.schedule(new ClockEvent(EVENT_ID,cycles + 1, executeCount _))
    } else {
      counter -= 1
      if (!countExternal) systemClock.schedule(new ClockEvent(EVENT_ID,cycles + 1, executeCount _))
    }
  }

}

class CIATimerB(ciaName: String, id: String, irqAction: (String) => Unit) extends CIATimerA(ciaName, id, irqAction) {
  override protected def handleCR567 {
    val bit56 = (cr >> 5) & 0x3
    setCountExternal(bit56 == 2)
    countSystemClock = bit56 == 2 || bit56 == 0
  }
}