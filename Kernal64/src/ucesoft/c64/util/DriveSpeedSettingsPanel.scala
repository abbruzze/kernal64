package ucesoft.c64.util

import javax.swing._
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import java.awt.BorderLayout
import ucesoft.c64.peripheral.drive.Drive

object DriveSpeedSettingsPanel {
  def getDialog(frame:JFrame,drives:Array[Drive]) = {
    val dialog = new JDialog(frame,"Drive Speed settings")
    dialog.getContentPane.add("Center",new DriveSpeedSettingsPanel(drives))
    dialog.pack
    dialog.setResizable(false)
    dialog
  }
}

class DriveSpeedSettingsPanel(drives:Array[Drive]) extends JPanel with ChangeListener {
  private[this] val slider = new JSlider
  private[this] val speed = new JLabel(drives(0).getSpeedHz.toString + " Hz",SwingConstants.CENTER)
  
  slider.setMinimum(drives(0).MIN_SPEED_HZ)
  slider.setMaximum(drives(0).MAX_SPEED_HZ)
  
  slider.setValue(drives(0).getSpeedHz)
  slider.addChangeListener(this)
  //slider.setPaintLabels(true)
  slider.setPaintTicks(true)
  slider.setPaintTrack(true)
  slider.setMajorTickSpacing(1000)
  //slider.setMinorTickSpacing(2)
  
  setLayout(new BorderLayout)
  
  add("Center",slider)
  add("North",speed)
  
  def stateChanged(e:ChangeEvent) = if (!slider.getValueIsAdjusting) {
    drives foreach { _.setSpeedHz(slider.getValue) }
    speed.setText(drives(0).getSpeedHz.toString + " Hz")
  }
}