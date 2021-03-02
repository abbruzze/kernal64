package ucesoft.cbm.peripheral.c2n

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.formats.TAP
import java.io.{File, ObjectInputStream, ObjectOutputStream}

class Datassette(ciaFlagLow : () => Unit) extends CBMComponent {
  val componentID = "Datassette 1530"
  val componentType = CBMComponentType.TAPE

  private[this] final val MOTOR_DELAY = 32000
  private[this] var motorOn = false
  private[this] var playPressed = false
  private[this] var recordPressed = false
  private[this] val clk = Clock.systemClock
  private[this] var tap : Option[TAP] = None
  private[this] var motorEvent = false
  private[this] var lastWriteLineChangeClock = 0L
  private[this] var lastWriteLine = false
  private[this] var tapeListener : Option[DatassetteListener] = None

  private[this] var counter,counterOffset = 0
  private[this] var counterMap : TAP.TapCounterMap = _

  def setTapeListener(tapeListener:DatassetteListener) = this.tapeListener = Some(tapeListener)

  def init  : Unit = {}
  def reset  : Unit = {
    motorOn = false
    playPressed = false
    recordPressed = false
    lastWriteLineChangeClock = 0
    lastWriteLine = false
    if (tap.isDefined) pressStop
  }
  def setTAP(tap:Option[TAP],pos:Option[Int]) : Unit = {
    if (this.tap.isDefined) this.tap.get.close
    notifyStateChangedTo(DatassetteState.STOPPED)
    this.tap = tap
    counterMap = TAP.anaylize(new File(tap.get.getFilename)).counterMap
    resetToStart
    pos match {
      case Some(p) =>
        tap.get.goTo(p)
        counter = counterMap.findCounter(p)
        update
      case None =>
    }
  }
  def isPlayPressed : Boolean = playPressed
  def pressPlay  : Unit = {
    if (!playPressed) {
      playPressed = true
      notifyStateChangedTo(DatassetteState.PLAYING)
      if (motorOn) { // in case of end of tape
        clk.pause
        clk.schedule(new ClockEvent(componentID, clk.currentCycles + 1, clockPlay _))
        clk.play
      }
    }
  }
  def pressForward : Unit = {
    pressStop
    notifyStateChangedTo(DatassetteState.FORWARD)
    clk.schedule(new ClockEvent(componentID, clk.nextCycles, clockForward(true) _))
  }

  def pressStop  : Unit = {
    if (playPressed) {
      playPressed = false
    }
    notifyStateChangedTo(DatassetteState.STOPPED)
    recordPressed = false
    clk.cancel(componentID)
  }
  def pressRecordAndPlay  : Unit = {
    playPressed = true
    notifyStateChangedTo(DatassetteState.RECORDING)
    recordPressed = true
  }
  def resetToStart   : Unit = {
    playPressed = false
    recordPressed = false
    tap match {
      case None =>
      case Some(tape) => tape.rewind
    }
    counterOffset = 0
    counter = 0
    notifyStateChangedTo(DatassetteState.STOPPED)
    update
  }

  def pressRewind : Unit = {
    pressStop
    notifyStateChangedTo(DatassetteState.REWIND)
    clk.schedule(new ClockEvent(componentID, clk.nextCycles, clockForward(false) _))
  }

  def resetCounter : Unit = {
    counterOffset = counter
    tapeListener match {
      case Some(tl) =>
        tl.datassetteUpdateCounter(0)
      case None =>
    }
  }

  def setWriteLine(on:Boolean) : Unit = {
    if (recordPressed && (lastWriteLine != on)) {
      if (!lastWriteLine && on) { // raising edge
        if (lastWriteLineChangeClock != 0) {
          val cycles = clk.currentCycles - lastWriteLineChangeClock
          //println("Writing " + Integer.toHexString(cycles.toInt) + " " + Integer.toHexString(cycles.toInt >> 3))
          tap match {
            case Some(t) =>
              t.write(cycles.toInt >> 3)
              counter = counterMap.findCounter(tap.get.getOffset.toInt) % 1000
              update
            case None =>
          }
        }
        lastWriteLineChangeClock = clk.currentCycles
      }
      lastWriteLine = on
    }
  }

  def setMotor(on: Boolean) = {
    if (!motorOn && on) {
      if (!recordPressed) {
        clk.schedule(new ClockEvent(componentID + "_Motor",clk.currentCycles + MOTOR_DELAY,clockPlay _))
        if (playPressed) notifyStateChangedTo(DatassetteState.PLAYING)
      }
      else notifyStateChangedTo(DatassetteState.RECORDING)
      motorEvent = true
    }
    else
      if (motorOn && !on) {
        clk.cancel(componentID)
        lastWriteLineChangeClock = 0
        lastWriteLine = false
      }
    motorOn = on
  }

  private def notifyStateChangedTo(state:DatassetteState.Value) : Unit = {
    tapeListener match {
      case Some(tl) => tl.datassetteStateChanged(state)
      case None =>
    }
  }

  override def getProperties = {
    super.getProperties
    properties.setProperty("TAP file",if (tap.isDefined) tap.get.getFilename else "-")
    properties.setProperty("TAP version",if (tap.isDefined) tap.get.version.toString else "-")
    properties.setProperty("TAP length",if (tap.isDefined) tap.get.tapeLength.toString else "-")
    properties.setProperty("Motor on",motorOn.toString)
    properties.setProperty("Play pressed",playPressed.toString)
    properties.setProperty("Record pressed",recordPressed.toString)
    properties.setProperty("Offset",if (tap.isDefined) tap.get.getOffset.toString else "-")
    properties
  }

  private def clockPlay(cycles: Long) : Unit = {
    if (motorOn && playPressed) {
      tap match {
        case None =>
        case Some(tape) =>
          if (motorEvent) motorEvent = false
          else ciaFlagLow()

          if (tape.hasNext) {
            val gap = tape.next
            counter = counterMap.findCounter(tap.get.getOffset.toInt) % 1000
            update
            clk.schedule(new ClockEvent(componentID,cycles + gap,clockPlay _))
          }
          else pressStop
      }
    }
  }

  private def clockForward(forward:Boolean)(cycles:Long) : Unit = {
    import TAP.FAST_FORWARD_PERIOD

    tap match {
      case Some(tape) =>
        counter += (if (forward) 1 else -1)
        val pos = counterMap.map.getOrElse(counter,tape.getOffset.toInt)
        if (counter <= counterMap.maxCounter && tape.goTo(pos)) {
          update
          if (counter > 0) clk.schedule(new ClockEvent(componentID, cycles + FAST_FORWARD_PERIOD, clockForward(forward) _))
          else pressStop
        }
        else pressStop
      case None =>
    }
  }

  private def update : Unit = {
    val progressPerc = ((tap.get.getOffset / tap.get.tapeLength.toDouble) * 100).toInt
    tapeListener match {
      case Some(tl) =>
        val c = counter - counterOffset
        tl.datassetteUpdatePosition(progressPerc, if (c >= 0) c % 1000 else c + 1000)
      case None =>
    }
  }

  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = {
    if (motorOn) showError("State error","Can't load/save state if datassette is playing or recording")
    !motorOn
  }
}