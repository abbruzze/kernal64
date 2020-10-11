package ucesoft.cbm.peripheral.c2n

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.formats.TAP
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.JOptionPane

class Datassette(ciaFlagLow : () => Unit) extends CBMComponent {
  val componentID = "Datassette 1530"
  val componentType = CBMComponentType.TAPE

  private[this] final val COUNTER_PERIOD = 3.15d * 985248
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
  private[this] var firstCyclePlay = 0L

  def setTapeListener(tapeListener:DatassetteListener) = this.tapeListener = Some(tapeListener)

  def init  : Unit = {}
  def reset  : Unit = {
    motorOn = false
    playPressed = false
    recordPressed = false
    lastWriteLineChangeClock = 0
    lastWriteLine = false
    tap match {
      case None =>
      case Some(tape) => tape.rewind
    }
  }
  def setTAP(tap:Option[TAP]) = {
    if (this.tap.isDefined) this.tap.get.close
    playPressed = false
    notifyStateChangedTo(DatassetteState.STOPPED)
    (tapeListener,tap) match {
      case (Some(tl),Some(tap)) =>
        tl.datassetteUpdatePosition(((tap.getOffset / tap.tapeLength.toDouble) * 100).toInt,counter)
      case _ =>
    }
    this.tap = tap
  }
  def isPlayPressed = playPressed
  def pressPlay  : Unit = {
    if (!playPressed) {
      firstCyclePlay = clk.currentCycles
      playPressed = true
      notifyStateChangedTo(DatassetteState.PLAYING)
      if (motorOn) { // in case of end of tape
        clk.pause
        clk.schedule(new ClockEvent(componentID, clk.currentCycles + 1, clock _))
        clk.play
      }
    }
  }
  def pressStop  : Unit = {
    if (playPressed) {
      playPressed = false
      notifyStateChangedTo(DatassetteState.STOPPED)
      counterOffset = counter
    }
    recordPressed = false
  }
  def pressRecordAndPlay  : Unit = {
    playPressed = true
    firstCyclePlay = clk.currentCycles
    notifyStateChangedTo(DatassetteState.RECORDING)
    recordPressed = true
  }
  def pressRewind   : Unit = {
    playPressed = false
    recordPressed = false
    tap match {
      case None =>
      case Some(tape) => tape.rewind
    }
    resetCounter
  }

  def resetCounter : Unit = {
    counterOffset = 0
    if (playPressed) firstCyclePlay = clk.currentCycles
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
            case Some(t) => t.write(cycles.toInt >> 3)
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
        clk.schedule(new ClockEvent(componentID + "_Motor",clk.currentCycles + MOTOR_DELAY,clock _))
        if (playPressed) notifyStateChangedTo(DatassetteState.PLAYING)
      }
      else notifyStateChangedTo(DatassetteState.RECORDING)
      motorEvent = true
      firstCyclePlay = clk.currentCycles
    }
    else
      if (motorOn && !on) {
        clk.cancel(componentID)
        lastWriteLineChangeClock = 0
        lastWriteLine = false
        counterOffset = counter
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

  private def clock(cycles: Long) : Unit = {
    if (motorOn && playPressed) {
      tap match {
        case None =>
        case Some(tape) =>
          if (motorEvent) motorEvent = false
          else ciaFlagLow()

          if (tape.hasNext) {
            val gap = tape.next
            val progressPerc = ((tap.get.getOffset / tap.get.tapeLength.toDouble) * 100).toInt
            tapeListener match {
              case Some(tl) =>
                val delta = cycles - firstCyclePlay
                counter = (counterOffset + math.round(delta / COUNTER_PERIOD).toInt) % 1000

                tl.datassetteUpdatePosition(progressPerc,counter)
              case None =>
            }
            clk.schedule(new ClockEvent(componentID,cycles + gap,clock _))
          }
          else pressStop
      }
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