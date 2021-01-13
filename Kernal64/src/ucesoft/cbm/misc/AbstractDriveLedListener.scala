package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.DriveLedListener

abstract class AbstractDriveLedListener(led:DriveLed) extends DriveLedListener {
  override def writeMode(enabled:Boolean) = {
    led.driveWriteMode = enabled
    led.repaint()
  }

  override def isOn = led.driveLedOn

  def setPowerLedMode(on:Boolean) = led.setPowerLedMode(on)

  def turnPower(on:Boolean) : Unit = {
    if (on != led.powerOn) {
      led.powerOn = on
      led.repaint()
    }
  }

  override def turnOn  : Unit = {
    if (!led.driveLedOn) {
      led.driveLedOn = true
      led.repaint()
    }
  }
  override def turnOff  : Unit = {
    if (led.driveLedOn) {
      led.driveLedOn = false
      led.repaint()
    }
  }

  override def moveTo(track:Int,sector:Option[Int],halfTrack:Boolean) : Unit = {
    val info = new StringBuilder
    info.append("%s%02d".format(if (halfTrack) "." else "",track))
    if (sector.isDefined) info.append(".%02d".format(sector.get))
    led.showLedInfo(info.toString)
  }
}