package ucesoft.cbm

import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class ClockEvent (val id : String,val when : Long,val execute: (Long) => Unit,val subid : Int = 0) {
  var canceled = false
  override def toString = s"${id}(${when} canceled=$canceled)"
}

object Clock extends App {
  private var clock : Clock = null
  def systemClock = clock
  def isAvailable = clock != null
  
  def setSystemClock(errorHandler:Option[(Throwable) => Unit] = None)(mainLoop: (Long) => Unit) = {
    if (clock == null) {
      clock = new Clock(errorHandler)(mainLoop)
      clock.start      
    }
    clock
  }
  
  def makeClock(clockName:String,errorHandler:Option[(Throwable) => Unit] = None)(mainLoop: (Long) => Unit) = {
    val clock = new Clock(errorHandler,clockName)(mainLoop)
    clock.start
    clock
  }
}

class Clock private (errorHandler:Option[(Throwable) => Unit],name:String = "Clock")(mainLoop: (Long) => Unit) extends Thread(name) with CBMComponent {
  val componentID = "System Clock"
  val componentType = CBMComponentType.CHIP 
  
  private class EventList(val e:ClockEvent,var next:EventList = null) {
    override def toString = {
      val sb = new StringBuilder
      var ptr = this
      while (ptr != null) {
        sb.append(ptr.e)
        ptr = ptr.next
        if (ptr != null) sb.append(",")
      }
      s"EventList[${sb}]"
    }
  }
  
  Log.info(s"${name} clock started")
  
  private[this] var events : EventList = null
  @volatile private[this] var running = false
  @volatile private[this] var suspended = true
  @volatile private[this] var suspendedConfim = false
  private[this] val suspendedLock = new Object
  // external threads
  @volatile private[this] var externalEvents : EventList = null
  private[this] val externalEventsLock = new Object
  
  // ------ PERFORMANCE MANAGEMENT -------------
  final private[this] val DEFAULT_CLOCK_HZ = 985248.0
  private[this] var C64_CLOCK_HZ = DEFAULT_CLOCK_HZ
  private[this] var C64_CLOCK_HZ_DIV_1000 = DEFAULT_CLOCK_HZ / 1000
  private[this] var C64_CLOCK_HZ_INV_BY_1000 = 1000 / DEFAULT_CLOCK_HZ
  final private[this] val PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS = 1 * 1000
  
  private[this] var _maximumSpeed = false
  private[this] var lastCorrectionTime = 0L
  private[this] var lastCorrectionCycles = 0L
  private[this] var nextPerformanceMeasurementTime = 0L
  private[this] var lastPerformance = 0
  private[this] var throttleStartedAt = 0L
  // -------------------------------------------
  private[this] var limitCycles = -1L
  
  def limitCyclesTo(cycles:Long) = limitCycles = cycles
  
  def setDefaultClockHz = setClockHz(DEFAULT_CLOCK_HZ)
  
  def setClockHzSpeedFactor(f:Double) = setClockHz(DEFAULT_CLOCK_HZ * f)
  
  def getClockHz = C64_CLOCK_HZ
  
  def setClockHz(hz:Double) {
    C64_CLOCK_HZ = hz
    C64_CLOCK_HZ_DIV_1000 = hz / 1000
    C64_CLOCK_HZ_INV_BY_1000 = 1000 / hz
  }
  
  private[this] var cycles = 0L
  
  def currentCycles = cycles
  def nextCycles = cycles + 1
  
  def maximumSpeed = _maximumSpeed
  def maximumSpeed_=(maximumSpeed:Boolean) {
    _maximumSpeed = maximumSpeed
    lastCorrectionTime = 0
  }
  
  override def getProperties = {
    properties.setProperty("cycles","%10d".format(cycles))
    properties
  }
  
  def init {}
  
  def reset {
    events = null
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
  }
      
  final override def run {
    running = true      
    while (running) {
      try {
      	if (suspended) {
      	  while (suspended) suspendedLock.synchronized {
      	    suspendedConfim = true
      	    suspendedLock.wait 
      	  } 
      	}
      	
      	mainLoop(cycles)
      	while (events != null && cycles >= events.e.when) {
      	  if (!events.e.canceled) events.e.execute(cycles)
      	  val next = events.next
      	  events.next = null 	// cut from list
      	  events = next
      	}
      	if (externalEvents != null) externalEventsLock.synchronized {
      	  while (externalEvents != null) {
      	    val extEvent = externalEvents.e
      	    schedule(extEvent)
      	    val next = externalEvents.next
      	    externalEvents.next = null
      	    externalEvents = next
      	  }
      	}
      	cycles += 1
      	if (limitCycles > 0 && cycles > limitCycles) sys.exit(0xFF)
      	throttle
      }
      catch {
        case t:Throwable => errorHandler match {
          case None => t.printStackTrace
          case Some(h) => h(t)
        }
      }
    }
  }
  
  @inline private def setupNextMeasurement {
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
    throttleStartedAt = cycles
    nextPerformanceMeasurementTime = System.currentTimeMillis + PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS
  }
  
  @inline private def throttle {
    if (!_maximumSpeed) {
      val timeDiff = System.currentTimeMillis - lastCorrectionTime
      val cyclesDiff = cycles - lastCorrectionCycles
      val expectedCycles = timeDiff * C64_CLOCK_HZ_DIV_1000
      if (cyclesDiff > expectedCycles) {
        val waitTime = (C64_CLOCK_HZ_INV_BY_1000 * (cyclesDiff - expectedCycles)).asInstanceOf[Int]
        Thread.sleep(waitTime)
      }
    }
    if (System.currentTimeMillis > nextPerformanceMeasurementTime) {
      val executed = cycles - throttleStartedAt
      lastPerformance = math.round(100.0 * executed / C64_CLOCK_HZ / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
      setupNextMeasurement
    }
  }
  
  def getLastPerformancePerc = lastPerformance
  
  final def cancel(id:String) {
    if (events != null) {
      var ptr = events
      while (ptr != null) {
        if (ptr.e.id == id) ptr.e.canceled = true
        ptr = ptr.next
      }
    }
  }
  
  def scheduleExternal(e:ClockEvent) = externalEventsLock.synchronized {
    if (externalEvents == null) {
      externalEvents = new EventList(e)
    }
    else
    if (e.when <= externalEvents.e.when) {
      externalEvents = new EventList(e,externalEvents)
    }
    else {
      var ptr = externalEvents
      var ptrNext = externalEvents.next
      val when = e.when
      while (ptrNext != null && when > ptrNext.e.when) {
        ptr = ptrNext
        ptrNext = ptrNext.next
      }
      ptr.next = new EventList(e,ptrNext)
    }
  }
  
  final def schedule(e:ClockEvent) {
    //require(e.when > cycles,"Can't schedule an event in the past " + e.when + "(" + cycles + ")")
    if (events == null) {
      events = new EventList(e)
    }
    else
    if (e.when <= events.e.when) {
      events = new EventList(e,events)
    }
    else {
      var ptr = events
      var ptrNext = events.next
      val when = e.when
      while (ptrNext != null && when > ptrNext.e.when) {
        ptr = ptrNext
        ptrNext = ptrNext.next
      }
      ptr.next = new EventList(e,ptrNext)
    }
  }
  
  def isPaused = suspendedConfim
  
  def pause {
    if (Thread.currentThread == this) return
    
    suspendedLock.synchronized { suspended = true }
    while (!suspendedConfim) { Thread.sleep(10) }
  }
  
  def play = suspendedLock.synchronized { 
    suspended = false
    suspendedConfim = false
    suspendedLock.notify 
  }
  def halt = running = false
  def printEvents { println(if (events != null) events else "No events") }
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeLong(cycles)
  }
  protected def loadState(in:ObjectInputStream) {
    cycles = in.readLong
    events = null
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  
  def getSubIdListFor(id:String) : List[(Int,Long)] = {
    var ids : List[(Int,Long)] = Nil
    if (events != null) {
      var ptr = events
      while (ptr != null) {
        if (ptr.e.id == id && !ptr.e.canceled) ids = (ptr.e.subid,ptr.e.when) :: ids
        ptr = ptr.next
      }
    }
    ids
  }
}