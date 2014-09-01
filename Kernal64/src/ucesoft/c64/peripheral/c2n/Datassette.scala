package ucesoft.c64.peripheral.c2n

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent
import ucesoft.c64.formats.TAP

class Datassette(ciaFlagLow : () => Unit) extends C64Component {
  val componentID = "Datassette 1530"
  val componentType = C64ComponentType.TAPE

  private[this] val MOTOR_DELAY = 1//32000
  private[this] var motorOn = false
  private[this] var playPressed = false
  private[this] val clk = Clock.systemClock
  private[this] var tap : Option[TAP] = None
  private[this] var motorEvent = false
  
  def init {}
  def reset {
    motorOn = false
    playPressed = false
    tap match {
      case None =>
      case Some(tape) => tape.setOffset(0)
    }
  }
  def setTAP(tap:Option[TAP]) = {
    if (this.tap.isDefined) this.tap.get.close
    this.tap = tap
  }
  def isPlayPressed = playPressed
  def pressPlay = playPressed = true
  def pressStop = playPressed = false

  def setMotor(on: Boolean) = {    
    if (!motorOn && on) {
      clk.schedule(new ClockEvent(componentID + "_Motor",clk.currentCycles + MOTOR_DELAY,clock _))
      motorEvent = true
    }
    else
    if (motorOn && !on) clk.cancel(componentID)
    motorOn = on
  }

  override def getProperties = {
    super.getProperties
    properties.setProperty("TAP file",if (tap.isDefined) tap.get.getFilename else "-")
    properties.setProperty("TAP version",if (tap.isDefined) tap.get.version.toString else "-")
    properties.setProperty("TAP length",if (tap.isDefined) tap.get.tapeLength.toString else "-")
    properties.setProperty("Motor on",motorOn.toString)
    properties.setProperty("Play pressed",playPressed.toString)
    properties.setProperty("Offset",if (tap.isDefined) tap.get.getOffset.toString else "-")
    properties
  }
  
  private def clock(cycles: Long) {    
    if (motorOn && playPressed) {
      tap match {
        case None =>
        case Some(tape) =>
          if (motorEvent) motorEvent = false
	      else ciaFlagLow()
	      
          if (tape.hasNext) {	        
	        val gap = tape.next
	        println(Integer.toHexString(gap) + " " + ((tap.get.getOffset / tap.get.tapeLength.toDouble) * 100).toInt + "%")
	        clk.schedule(new ClockEvent(componentID,cycles + gap,clock _))
          }
          else playPressed = false
      }
    }
  }
}