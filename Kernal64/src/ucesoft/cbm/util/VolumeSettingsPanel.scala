package ucesoft.cbm.util

import javax.swing._
import ucesoft.cbm.peripheral.sid.AudioDriverDevice
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.BorderLayout

object VolumeSettingsPanel {
  def getDialog(frame:JFrame,driver:AudioDriverDevice) = {
    val dialog = new JDialog(frame,"Volume settings")
    dialog.getContentPane.add("Center",new VolumeSettingsPanel(driver))
    dialog.pack
    dialog.setResizable(false)
    dialog
  }
}

class VolumeSettingsPanel(driver:AudioDriverDevice) extends JPanel with ChangeListener {
  private[this] val slider = new JSlider
  private[this] val mute = new JCheckBox
  
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
  
  def stateChanged(e:ChangeEvent) = if (!slider.getValueIsAdjusting) {
    if (e.getSource == slider) driver.setMasterVolume(slider.getValue)
    if (e.getSource == mute) driver.setSoundOn(!mute.isSelected)
  }
}