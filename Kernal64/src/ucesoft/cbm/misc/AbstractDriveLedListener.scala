package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.DriveLedListener

abstract class AbstractDriveLedListener(led:DriveLed,progress:DriveLoadProgressPanel) extends DriveLedListener {
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
    
    override def beginLoadingOf(fileName:String,indeterminate:Boolean=false) : Unit = {
      progress.setIndeterminate(indeterminate)
      progress.beginLoading(fileName)
    }
    override def updateLoading(perc:Int) : Unit = {
      progress.updateValue(perc)
    }
    override def endLoading  : Unit = {
      progress.endLoading
    }
    override def beginSavingOf(fileName:String) : Unit = {
      progress.beginLoading(fileName)
      progress.setIndeterminate(true)
    }
    override def endSaving  : Unit = {
      progress.endLoading
      progress.setIndeterminate(false)
    }
  }