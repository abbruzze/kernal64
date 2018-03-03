package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.DriveLedListener

abstract class AbstractDriveLedListener(led:DriveLed,progress:DriveLoadProgressPanel) extends DriveLedListener {
    override def writeMode(enabled:Boolean) = {
      led.driveWriteMode = enabled
      led.repaint()
    }
    
    override def isOn = led.driveLedOn
    
    def setPowerLedMode(on:Boolean) = led.setPowerLedMode(on)
    
    def turnPower(on:Boolean) {
      if (on != led.powerOn) {
        led.powerOn = on
        led.repaint()
      }
    }
    
    override def turnOn {
      if (!led.driveLedOn) {        
        led.driveLedOn = true
        led.repaint()
      }
    }
    override def turnOff {
      if (led.driveLedOn) {
        led.driveLedOn = false
        led.repaint()
      }    
    }
    
    override def beginLoadingOf(fileName:String,indeterminate:Boolean=false) {
      progress.setIndeterminate(indeterminate)
      progress.beginLoading(fileName)
    }
    override def updateLoading(perc:Int) {
      progress.updateValue(perc)
    }
    override def endLoading {
      progress.endLoading
    }
    override def beginSavingOf(fileName:String) {
      progress.beginLoading(fileName)
      progress.setIndeterminate(true)
    }
    override def endSaving {
      progress.endLoading
      progress.setIndeterminate(false)
    }
  }