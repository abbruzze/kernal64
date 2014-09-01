package ucesoft.c64.peripheral.controlport

import javax.swing._
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.util.Properties
import java.awt.GridLayout
import Joysticks._

class JoystickSettingDialog(parent: JFrame, configuration: Properties) extends JDialog(parent, "Joystick settings", true) with ActionListener {
  private def setCombo(c:JComboBox[String],port:String) {
    configuration.getProperty(port) match {
      case CONFIGURATION_KEYPAD_VALUE => c.setSelectedIndex(0)
      case CONFIGURATION_JOYSTICK_VALUE => c.setSelectedIndex(1)
      case _ => c.setSelectedIndex(2) 
    }
  }
  
  val port1Panel = new JPanel
  port1Panel.add(new JLabel("Port #1:", SwingConstants.RIGHT))
  val port2Panel = new JPanel
  port2Panel.add(new JLabel("Port #2:", SwingConstants.RIGHT))
  val port1JoyCombo = new JComboBox(Array("Keyboard", "Gamepad", "None"))
  port1Panel.add(port1JoyCombo)
  val port2JoyCombo = new JComboBox(Array("Keyboard", "Gamepad", "None"))
  port2Panel.add(port2JoyCombo)
  
  setCombo(port1JoyCombo,CONFIGURATION_JOY_PORT_1)
  setCombo(port2JoyCombo,CONFIGURATION_JOY_PORT_2)

  val centerPanel = new JPanel(new GridLayout(2, 1))
  centerPanel.add(port1Panel)
  centerPanel.add(port2Panel)

  val okButton = new JButton("Ok")
  val cancelButton = new JButton("Cancel")
  val buttonPanel = new JPanel
  buttonPanel.add(okButton)
  buttonPanel.add(cancelButton)
  okButton.addActionListener(this)
  okButton.setActionCommand("OK")
  cancelButton.addActionListener(this)
  cancelButton.setActionCommand("CANCEL")

  getContentPane.add("Center", centerPanel)
  getContentPane.add("South", buttonPanel)
  val parentLoc = parent.getLocation
  val parentSize = parent.getSize
  pack
  setLocation(parentLoc.x + (parentSize.width - getSize.width) / 2,parentLoc.y + (parentSize.height - getSize.height) / 2)  
  
  private def updateConfigFor(c:JComboBox[String],port:String) = {
    c.getSelectedIndex match {
      case 0 => configuration.setProperty(port,CONFIGURATION_KEYPAD_VALUE)
      case 1 => configuration.setProperty(port,CONFIGURATION_JOYSTICK_VALUE)
      case 2 => configuration.remove(port)
    }
  }

  def actionPerformed(e: ActionEvent) {
    e.getActionCommand match {
      case "OK" =>
        updateConfigFor(port1JoyCombo,CONFIGURATION_JOY_PORT_1)
        updateConfigFor(port2JoyCombo,CONFIGURATION_JOY_PORT_2)
      case _ =>
    }
    dispose
  }
}

object JoystickSettingDialog extends App {
  new JoystickSettingDialog(null, new Properties).show
}