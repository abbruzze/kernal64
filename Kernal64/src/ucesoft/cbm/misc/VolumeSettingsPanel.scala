package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.sid.AudioDriverDevice

import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}
import scala.collection.mutable

object VolumeSettingsPanel {
  trait VolumeSetting {
    def addDriver(driver:AudioDriverDevice): Unit
    def removeDriver(driver:AudioDriverDevice): Unit
    def setVolume(vol:Int): Unit
  }
  type VolumeDialog = JDialog with VolumeSetting

  def getDialog(frame:JFrame,driver:AudioDriverDevice): VolumeDialog = {
    val vs = new VolumeSettingsPanel(driver)
    val dialog = new JDialog(frame,"Volume settings") with VolumeSetting {
      override def addDriver(driver: AudioDriverDevice): Unit = vs.addDriver(driver)
      override def removeDriver(driver: AudioDriverDevice): Unit = vs.removeDriver(driver)
      override def setVolume(vol: Int): Unit = vs.setVolume(vol)
    }
    dialog.getContentPane.add("Center",vs)
    dialog.pack()
    dialog.setResizable(false)
    dialog
  }

  class VolumeSettingsPanel(driver: AudioDriverDevice) extends JPanel with ChangeListener with VolumeSetting {
    private val drivers = new mutable.HashSet[AudioDriverDevice]()
    private[this] val slider = new JSlider
    private[this] val mute = new JCheckBox

    drivers.add(driver)
    mute.addChangeListener(this)
    slider.setValue(driver.getMasterVolume)
    slider.addChangeListener(this)
    slider.setPaintLabels(true)
    slider.setPaintTicks(true)
    slider.setPaintTrack(true)
    slider.setMajorTickSpacing(20)
    slider.setMinorTickSpacing(2)

    add(new JLabel("Mute:"))
    add(mute)
    add(slider)

    def stateChanged(e: ChangeEvent): Unit = if (!slider.getValueIsAdjusting) {
      if (e.getSource == slider) for(driver <- drivers) driver.setMasterVolume(slider.getValue)
      if (e.getSource == mute) for(driver <- drivers) driver.setMuted(mute.isSelected)
    }

    override def addDriver(driver: AudioDriverDevice): Unit = {
      drivers += driver
      driver.setMasterVolume(slider.getValue)
      driver.setMuted(mute.isSelected)
    }
    override def removeDriver(driver: AudioDriverDevice): Unit = drivers -= driver
    override def setVolume(vol: Int): Unit = slider.setValue(vol)
  }
}