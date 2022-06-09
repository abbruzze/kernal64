package ucesoft.cbm.peripheral.controlport

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.cbm.peripheral.controlport.Joysticks._

import java.awt.event._
import java.awt.{Dimension, GridLayout, Point}
import java.util.Properties
import javax.swing._

class JoystickSettingDialog(parent: JFrame, configuration: Properties,gamepad:GamePadControlPort) extends JDialog(parent, "Joystick settings", true) with ActionListener {
  private[this] var joyButtonSelected = ""
  private[this] var joystickDialog : JDialog = null
  private[this] var keyboardDialog : JDialog = null
  private[this] val fireButtonLabel = new JLabel("...")
  private def setCombo(c:JComboBox[String],port:String) : Unit = {
    configuration.getProperty(port) match {
      case CONFIGURATION_KEYPAD_VALUE => c.setSelectedIndex(0)
      case CONFIGURATION_JOYSTICK_VALUE => c.setSelectedIndex(1)
      case CONFIGURATION_KEYBOARD_VALUE => c.setSelectedIndex(2)
      case _ => c.setSelectedIndex(3)
    }
  }
  
  private val keybButtons = Array(new JButton("Up"),
                              new JButton("Down"),
                              new JButton("Left"),
                              new JButton("Right"),
                              new JButton("Up-Right"),
                              new JButton("Up-Left"),
                              new JButton("Down-Right"),
                              new JButton("Down-Left"),
                              new JButton("Fire"))
                              
  private val keybLabels = {
    import ControlPort._
    val labels = Array.fill[JLabel](9)(new JLabel)
  
    for(b <- keybButtons.zipWithIndex) {
      val ac = b._2 match {
        case 0 => CONFIGURATION_UD_JOYSTICK_UP 
        case 1 => CONFIGURATION_UD_JOYSTICK_DOWN
        case 2 => CONFIGURATION_UD_JOYSTICK_LEFT
        case 3 => CONFIGURATION_UD_JOYSTICK_RIGHT
        case 4 => CONFIGURATION_UD_JOYSTICK_UP_RIGHT
        case 5 => CONFIGURATION_UD_JOYSTICK_UP_LEFT
        case 6 => CONFIGURATION_UD_JOYSTICK_DOWN_RIGHT
        case 7 => CONFIGURATION_UD_JOYSTICK_DOWN_LEFT
        case 8 => CONFIGURATION_UD_JOYSTICK_FIRE
      }
      b._1.setActionCommand(ac)
      b._1.addActionListener(this)
      val labelText = Option(configuration.getProperty(ac)) match {
        case Some(l) => KeyEvent.getKeyText(l.toInt)
        case None => "Empty"
      }
      labels(b._2).setText(labelText)
    }
    labels
  }
  
  private val keybBarLabel = new JLabel("Press the buttons and type the key")
  private var keyIndex = 0
  private var keyCmd = ""
  
  private val keybListener : KeyListener = new KeyAdapter{
    override def keyPressed(e:KeyEvent) : Unit = {
      if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
        JOptionPane.showMessageDialog(keyboardDialog,"Invalid key location", "Bad key location",JOptionPane.ERROR_MESSAGE)
      }
      else {
        removeKeyListener(keybListener)
        keybLabels(keyIndex).setText(KeyEvent.getKeyText(e.getExtendedKeyCode))
        for(i <- 0 until 9) keybButtons(i).setEnabled(true)
        configuration.setProperty(keyCmd,e.getExtendedKeyCode.toString)
      }
    }
  }
  
  val port1Panel = new JPanel
  port1Panel.add(new JLabel("Port #1:", SwingConstants.RIGHT))
  val port2Panel = new JPanel
  port2Panel.add(new JLabel("Port #2:", SwingConstants.RIGHT))
  val port1JoyCombo = new JComboBox(Array("Keypad", "Gamepad", "Keyboard", "None"))
  port1Panel.add(port1JoyCombo)
  val port2JoyCombo = new JComboBox(Array("Keypad", "Gamepad", "Keyboard" , "None"))
  port2Panel.add(port2JoyCombo)
  
  setCombo(port1JoyCombo,CONFIGURATION_JOY_PORT_1)
  setCombo(port2JoyCombo,CONFIGURATION_JOY_PORT_2)

  val centerPanel = new JPanel(new GridLayout(2, 1))
  centerPanel.add(port1Panel)
  centerPanel.add(port2Panel)

  val okButton = new JButton("Ok")
  val cancelButton = new JButton("Cancel")
  val gamepadButton = new JButton("GamePad configuration ...")
  val keybButton = new JButton("Keyboard configuration ...")
  val buttonPanel = new JPanel
  buttonPanel.add(keybButton)
  buttonPanel.add(gamepadButton)
  buttonPanel.add(okButton)
  buttonPanel.add(cancelButton)
  okButton.addActionListener(this)
  okButton.setActionCommand("OK")
  cancelButton.addActionListener(this)
  cancelButton.setActionCommand("CANCEL")
  gamepadButton.addActionListener(this)
  gamepadButton.setActionCommand("GAMEPAD")
  keybButton.addActionListener(this)
  keybButton.setActionCommand("KEYB")

  getContentPane.add("Center", centerPanel)
  getContentPane.add("South", buttonPanel)
  val parentLoc: Point = parent.getLocation
  val parentSize: Dimension = parent.getSize
  pack()
  setLocation(parentLoc.x + (parentSize.width - getSize.width) / 2,parentLoc.y + (parentSize.height - getSize.height) / 2)  
  
  private def updateConfigFor(c:JComboBox[String],port:String) = {
    c.getSelectedIndex match {
      case 0 => configuration.setProperty(port,CONFIGURATION_KEYPAD_VALUE)
      case 1 => configuration.setProperty(port,CONFIGURATION_JOYSTICK_VALUE)
      case 2 => configuration.setProperty(port,CONFIGURATION_KEYBOARD_VALUE)
      case 3 => configuration.remove(port)
    }
  }

  def actionPerformed(e: ActionEvent) : Unit = {
    import ControlPort._
    e.getActionCommand match {
      case "OK" =>
        updateConfigFor(port1JoyCombo,CONFIGURATION_JOY_PORT_1)
        updateConfigFor(port2JoyCombo,CONFIGURATION_JOY_PORT_2)
        dispose()
      case "CANCEL" =>
        dispose()
      case "GAMEPAD" =>
        gamePadConfig
      case "FIRE_CANCEL" =>
        joyButtonSelected = ""
        joystickDialog.dispose()
      case "FIRE_OK" =>
        joystickDialog.dispose()
      case "KEYB" =>
        keyboardConfig
      // key buttons
      case CONFIGURATION_UD_JOYSTICK_UP => listenKey(e.getActionCommand,0)
      case CONFIGURATION_UD_JOYSTICK_DOWN => listenKey(e.getActionCommand,1)
      case CONFIGURATION_UD_JOYSTICK_LEFT => listenKey(e.getActionCommand,2)
      case CONFIGURATION_UD_JOYSTICK_RIGHT => listenKey(e.getActionCommand,3)
      case CONFIGURATION_UD_JOYSTICK_UP_RIGHT => listenKey(e.getActionCommand,4)
      case CONFIGURATION_UD_JOYSTICK_UP_LEFT => listenKey(e.getActionCommand,5)
      case CONFIGURATION_UD_JOYSTICK_DOWN_RIGHT => listenKey(e.getActionCommand,6)
      case CONFIGURATION_UD_JOYSTICK_DOWN_LEFT => listenKey(e.getActionCommand,7)
      case CONFIGURATION_UD_JOYSTICK_FIRE => listenKey(e.getActionCommand,8)
    }
    
  }
  
  private def listenKey(cmd:String,index:Int) : Unit = {
    keyCmd = cmd
    keyIndex = index
    keyboardDialog.requestFocus()
    keyboardDialog.addKeyListener(keybListener) 
    for(i <- 0 until 9) keybButtons(i).setEnabled(false)
  }  
  
  private def keyboardConfig() : Unit = {
    keyboardDialog = new JDialog(parent,"Keyboard keys selection",true)
    val buttonPanel = new JPanel(new GridLayout(9,2,5,5))
    for(i <- 0 until 9) {
      buttonPanel.add(keybButtons(i))
      buttonPanel.add(keybLabels(i))
    }
    keyboardDialog.getContentPane.add("Center",buttonPanel)
    keyboardDialog.getContentPane.add("South",keybBarLabel)
    keyboardDialog.pack()
    keyboardDialog.setResizable(false)
    keyboardDialog.setLocation(parentLoc.x + (parentSize.width - keyboardDialog.getSize.width) / 2,parentLoc.y + (parentSize.height - keyboardDialog.getSize.height) / 2)
    keyboardDialog.setVisible(true)
  }
  
  private def gamePadConfig() : Unit = {
    try {
      val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers map { _.asInstanceOf[Object] }
      val controller = JOptionPane.showInputDialog(this,"Select Joystick","GamePad configuration",JOptionPane.QUESTION_MESSAGE,null,controllers,controllers(0)).asInstanceOf[Controller]      
      if (controller != null) {
        val buttons = controller.getComponents filter { _.getIdentifier.isInstanceOf[Component.Identifier.Button] }
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
          override def run() : Unit = {
            while (!isInterrupted) {
              controller.poll
              for(b <- buttons) if (b.getPollData != 0.0f) {
                joyButtonSelected = b.getIdentifier.getName
                SwingUtilities.invokeLater(new Runnable { 
                  def run(): Unit = {
                    fireButtonLabel.setText(b.getName)
                    okButton.setEnabled(true)
                  }
                })
              }
            }
          }
        }
        pollingThread.start()
        joystickDialog.pack()
        joystickDialog.setResizable(false)
        joystickDialog.setLocation(parentLoc.x + (parentSize.width - joystickDialog.getSize.width) / 2,parentLoc.y + (parentSize.height - joystickDialog.getSize.height) / 2)
        joystickDialog.setVisible(true)
        pollingThread.interrupt()
        if (joyButtonSelected != "") {
          configuration.setProperty(CONFIG_CONTROLLER_NAME,controller.getName)
          configuration.setProperty(CONFIG_CONTROLLER_FIRE_BUTTON,joyButtonSelected)
          gamepad.findController
        }
      }
    }
    catch {
      case t:Throwable => 
        JOptionPane.showMessageDialog(this,"Error while polling joystick: " + t,"Joystick error",JOptionPane.ERROR_MESSAGE)
    }
  }
}