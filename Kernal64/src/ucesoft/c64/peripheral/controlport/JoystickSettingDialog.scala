package ucesoft.c64.peripheral.controlport

import javax.swing._
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.util.Properties
import java.awt.GridLayout
import net.java.games.input.Controller
import net.java.games.input.ControllerEnvironment
import net.java.games.input.Component
import Joysticks._

class JoystickSettingDialog(parent: JFrame, configuration: Properties) extends JDialog(parent, "Joystick settings", true) with ActionListener {
  private[this] var joyButtonSelected = ""
  private[this] var joystickDialog : JDialog = null
  private[this] val fireButtonLabel = new JLabel("...")
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
  val gamepadButton = new JButton("GamePad configuration ...")
  val buttonPanel = new JPanel
  buttonPanel.add(gamepadButton)
  buttonPanel.add(okButton)
  buttonPanel.add(cancelButton)
  okButton.addActionListener(this)
  okButton.setActionCommand("OK")
  cancelButton.addActionListener(this)
  cancelButton.setActionCommand("CANCEL")
  gamepadButton.addActionListener(this)
  gamepadButton.setActionCommand("GAMEPAD")

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
        dispose
      case "CANCEL" =>
        dispose
      case "GAMEPAD" =>
        gamePadConfig
      case "FIRE_CANCEL" =>
        joyButtonSelected = ""
        joystickDialog.dispose
      case "FIRE_OK" =>
        joystickDialog.dispose
    }
    
  }
  
  private def gamePadConfig {
    try {
      val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers map { _.asInstanceOf[Object] }
      val controller = JOptionPane.showInputDialog(this,"Select Joystick","GamePad configuration",JOptionPane.QUESTION_MESSAGE,null,controllers,controllers(0)).asInstanceOf[Controller]      
      if (controller != null) {
        val buttons = controller.getComponents filter { _.getName.toUpperCase.indexOf("BUTTON") >= 0 }
        joystickDialog = new JDialog(parent,"Fire button selection",true)
        val okButton = new JButton("Ok")
        okButton.setEnabled(false)
        okButton.addActionListener(this)
        okButton.setActionCommand("FIRE_OK")
        val cancelButton = new JButton("Cancel")
        cancelButton.addActionListener(this)
        cancelButton.setActionCommand("FIRE_CANCEL")
        joystickDialog.getContentPane.add("North",new JLabel("Press fire button..."))
        joystickDialog.getContentPane.add("Center",fireButtonLabel)
        val buttonPanel = new JPanel
        buttonPanel.add(okButton)
        buttonPanel.add(cancelButton)
        joystickDialog.getContentPane.add("South",buttonPanel)
        val pollingThread = new Thread {
          override def run {
            while (!isInterrupted) {
              controller.poll
              for(b <- buttons) if (b.getPollData != 0.0f) {
                joyButtonSelected = b.getIdentifier.getName
                SwingUtilities.invokeLater(new Runnable { 
                  def run = {
                    fireButtonLabel.setText(b.getName)
                    okButton.setEnabled(true)
                  }
                })
              }
            }
          }
        }
        pollingThread.start
        joystickDialog.pack
        joystickDialog.setLocation(parentLoc.x + (parentSize.width - joystickDialog.getSize.width) / 2,parentLoc.y + (parentSize.height - joystickDialog.getSize.height) / 2)
        joystickDialog.setVisible(true)
        pollingThread.interrupt
        if (joyButtonSelected != "") {
          configuration.setProperty(CONFIG_CONTROLLER_NAME,controller.getName)
          configuration.setProperty(CONFIF_CONTROLLER_FIRE_BUTTON,joyButtonSelected)
        }
      }
    }
    catch {
      case t:Throwable => 
        JOptionPane.showMessageDialog(this,"Error while polling joystick: " + t,"Joystick error",JOptionPane.ERROR_MESSAGE)
    }
  }
}

object JoystickSettingDialog extends App {
  new JoystickSettingDialog(new javax.swing.JFrame, new Properties).show
}