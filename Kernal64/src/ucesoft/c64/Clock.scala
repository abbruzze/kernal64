package ucesoft.c64

import scala.collection.mutable.ListBuffer

class ClockEvent (val id : String,val when : Long,val execute: (Long) => Unit) {
  var canceled = false
  override def toString = s"${id}(${when})"
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

class Clock private (errorHandler:Option[(Throwable) => Unit],name:String = "Clock")(mainLoop: (Long) => Unit) extends Thread(name) with C64Component {
  val componentID = "System Clock"
  val componentType = C64ComponentType.CHIP 
  
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
  
  // ------ PERFORMANCE MANAGEMENT -------------
  private[this] val C64_CLOCK_HZ = 985248.0
  private[this] val PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS = 1 * 1000
  
  private[this] var _maximumSpeed = false
  private[this] var lastCorrectionTime = 0L
  private[this] var lastCorrectionCycles = 0L
  private[this] var nextPerformanceMeasurementTime = 0L
  private[this] var lastPerformance = 0
  private[this] var throttleStartedAt = 0L
  // -------------------------------------------
  
  private[this] var cycles = 0L
  
  @inline def currentCycles = cycles
  @inline def nextCycles = cycles + 1
  
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
    resetThrottleMeasurement
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
    	while (events != null && cycles >= events.e.when) {
    	  if (!events.e.canceled) events.e.execute(cycles)
    	  val next = events.next
    	  events.next = null 	// cut from list
    	  events = next
    	}
    	mainLoop(cycles)
    	cycles += 1  
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
  
  def resetThrottleMeasurement {
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
  }
  
  private def setupNextMeasurement {
    resetThrottleMeasurement
    throttleStartedAt = cycles
    nextPerformanceMeasurementTime = System.currentTimeMillis + PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS
  }
  
  private def throttle {
    if (lastCorrectionTime == 0) setupNextMeasurement
    else {
      if (!_maximumSpeed) {
        val timeDiff = System.currentTimeMillis - lastCorrectionTime
        val cyclesDiff = cycles - lastCorrectionCycles
        val expectedCycles = timeDiff * C64_CLOCK_HZ / 1000
        if (cyclesDiff > expectedCycles) {
          val waitTime = (1000 * (cyclesDiff - expectedCycles) / C64_CLOCK_HZ).asInstanceOf[Int]
          Thread.sleep(waitTime)
        }
      }
      if (System.currentTimeMillis > nextPerformanceMeasurementTime) {
        val executed = cycles - throttleStartedAt
        lastPerformance = math.round(100.0 * executed / C64_CLOCK_HZ / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
        setupNextMeasurement
      }
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
  
  final def schedule(e:ClockEvent) {
    require(e.when > cycles,"Can't schedule an event in the past " + e.when + "(" + cycles + ")")
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
  
  def pause = {
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
}