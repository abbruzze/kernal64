package ucesoft.cbm

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.misc.TestCart

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

class ClockEvent (val id : String,val when : Long,val execute: (Long) => Unit,val subid : Int = 0) {
  var canceled = false
  override def toString = s"$id($when canceled=$canceled)"
}

object Clock {
  private var clock : Clock = null
  def systemClock: Clock = clock
  def isAvailable: Boolean = clock != null

  def setSystemClock(errorHandler:Option[(Throwable) => Unit] = None)(mainLoop: (Long) => Unit): Clock = {
    if (clock == null) {
      clock = new Clock(errorHandler)(mainLoop)
      clock.start()
    }
    clock
  }

  def makeClock(clockName:String,errorHandler:Option[(Throwable) => Unit] = None)(mainLoop: (Long) => Unit): Clock = {
    val clock = new Clock(errorHandler,clockName)(mainLoop)
    clock.start()
    clock
  }
}

class Clock private (errorHandler:Option[(Throwable) => Unit],name:String = "Clock")(mainLoop: (Long) => Unit) extends Thread(name) with CBMComponent with Tickable {
  val componentID = "System Clock"
  val componentType: Type = CBMComponentType.CHIP

  private class EventList(val e:ClockEvent,var next:EventList = null) {
    override def toString: String = {
      val sb = new StringBuilder
      var ptr = this
      while (ptr != null) {
        sb.append(ptr.e)
        ptr = ptr.next
        if (ptr != null) sb.append(",")
      }
      s"EventList[$sb]"
    }
  }

  Log.info(s"$name clock started")

  private[this] var events : EventList = null
  @volatile private[this] var running = false
  @volatile private[this] var suspended = true
  @volatile private[this] var suspendedConfim = false
  private[this] val suspendedLock = new Object
  private[this] var changeFrequencyListenerList : List[Double => Unit] = Nil

  final val PAL_CLOCK_HZ = 985248.0d
  final val NTSC_CLOCK_HZ = 1022730.0d

  // ------ PERFORMANCE MANAGEMENT -------------
  final private[this] val DEFAULT_CLOCK_HZ = PAL_CLOCK_HZ
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
  private[this] var skipThrottle = false
  // -------------------------------------------
  private[this] var limitCycles = -1L

  def limitCyclesTo(cycles:Long) : Unit = limitCycles = cycles

  def setDefaultClockHz() : Unit = setClockHz(DEFAULT_CLOCK_HZ)

  def setClockHzSpeedFactor(f:Double) : Unit = setClockHz(DEFAULT_CLOCK_HZ * f)

  def getClockHz : Double = C64_CLOCK_HZ

  def setClockHz(hz:Double) : Unit = {
    C64_CLOCK_HZ = hz
    C64_CLOCK_HZ_DIV_1000 = hz / 1000
    C64_CLOCK_HZ_INV_BY_1000 = 1000 / hz

    // notifies listeners
    changeFrequencyListenerList foreach { _(hz) }
  }

  def addChangeFrequencyListener(l: Double => Unit) : Unit = {
    changeFrequencyListenerList ::= l
  }

  private[this] var cycles = 0L

  override final def currentCycles: Long = cycles
  final def nextCycles: Long = cycles + 1

  def maximumSpeed: Boolean = _maximumSpeed
  def maximumSpeed_=(maximumSpeed:Boolean) : Unit = {
    if (!maximumSpeed) {
      skipThrottle = true
      setupNextMeasurement
    }
    _maximumSpeed = maximumSpeed
  }

  override def getProperties: Properties = {
    properties.setProperty("cycles","%10d".format(cycles))
    properties
  }

  def init  : Unit = {}

  def reset  : Unit = {
    events = null
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
  }

  final override def run()  : Unit = {
    running = true
    while (running) {
      try {
        if (suspended) {
          while (suspended) suspendedLock.synchronized {
            suspendedConfim = true
            suspendedLock.wait()
          }
        }

        mainLoop(cycles)
        while (events != null && cycles >= events.e.when) {
          if (!events.e.canceled) events.e.execute(cycles)
          val next = events.next
          events.next = null 	// cut from list
          events = next
        }

        cycles += 1
        if (limitCycles > 0 && cycles > limitCycles) TestCart.exit(0x01)
        throttle
      }
      catch {
        case t:Throwable => errorHandler match {
          case None => t.printStackTrace()
          case Some(h) => h(t)
        }
      }
    }
  }

  @inline private def setupNextMeasurement()  : Unit = {
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
    throttleStartedAt = cycles
    nextPerformanceMeasurementTime = System.currentTimeMillis + PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS
  }

  @inline private def throttle()  : Unit = {
    if (!_maximumSpeed && !skipThrottle) {
      val timeDiff = System.currentTimeMillis - lastCorrectionTime
      val cyclesDiff = cycles - lastCorrectionCycles
      val expectedCycles = timeDiff * C64_CLOCK_HZ_DIV_1000
      if (cyclesDiff > expectedCycles) {
        val waitTime = (C64_CLOCK_HZ_INV_BY_1000 * (cyclesDiff - expectedCycles)).asInstanceOf[Int]
        Thread.sleep(if (waitTime == 0) 1 else waitTime)
      }
    }
    if (skipThrottle || System.currentTimeMillis > nextPerformanceMeasurementTime) {
      skipThrottle = false
      val executed = cycles - throttleStartedAt
      lastPerformance = math.round(100.0 * executed / C64_CLOCK_HZ / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
      setupNextMeasurement
    }
  }

  def getLastPerformancePerc: Int = lastPerformance

  final def cancel(id:String) : Unit = {
    if (events != null) {
      var ptr = events
      while (ptr != null) {
        if (ptr.e.id == id) ptr.e.canceled = true
        ptr = ptr.next
      }
    }
  }

  final def schedule(e:ClockEvent) : Unit = {
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

  def isPaused: Boolean = suspendedConfim

  def pause()  : Unit = {
    if (Thread.currentThread == this) return

    suspendedLock.synchronized { suspended = true }
    while (!suspendedConfim) { Thread.sleep(10) }
  }

  def play(): Unit = suspendedLock.synchronized {
    suspended = false
    suspendedConfim = false
    suspendedLock.notify()
  }
  def halt(): Unit = running = false
  def printEvents()  : Unit = { println(if (events != null) events else "No events") }

  def waitFor(millis:Long): Unit = {
    if (Thread.currentThread == this) {
      cycles += (C64_CLOCK_HZ * millis / 1000).toLong - 1
      Thread.sleep(millis)
    }
    else {
      println
    }
  }

  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeLong(cycles)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    cycles = in.readLong
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = cycles
    throttleStartedAt = cycles
    events = null
  }
  protected def allowsStateRestoring : Boolean = true

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