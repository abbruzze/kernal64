package ucesoft.cbm.peripheral.controlport

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener}
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties
import javax.swing.SwingUtilities

abstract class ControlPort extends CBMComponent {
  val componentType: Type = CBMComponentType.INPUT_DEVICE
  val componentID = "ControlPort"
  val isConnected = true
    
  private[this] var emulatedBit = 0
  private[this] var lightPenEmulationEnabled = false
  private[this] var mouse1351EmulationEnabled = false
  
  def init() : Unit = {}
  def reset() : Unit = {
    emulatedBit = 0
  }
  
  override def getProperties: Properties = {
    properties.setProperty("~Value",Integer.toHexString(~readPort & 0xFF))
    properties
  }
  
  def readPort: Int = read & (~emulatedBit & 0xFF)

  def consumeKey(e:KeyEvent): Boolean = false
  
  protected def read : Int	// 4 bits

  def updateConfiguration() : Unit = {}
  
  def emulateFire(): Unit = emulatedBit = 16
  def emulateUp(): Unit = emulatedBit = 1
  def emulateDown(): Unit = emulatedBit = 2
  def emulateLeft(): Unit = emulatedBit = 4
  def emulateRight(): Unit = emulatedBit = 8
  def releaseEmulated(): Unit = emulatedBit = 0
  
  def setLightPenEmulation(enabled:Boolean): Unit = lightPenEmulationEnabled = enabled
  def isLightPenEmulationEnabled: Boolean = lightPenEmulationEnabled
  def setMouse1351Emulation(enabled:Boolean): Unit = mouse1351EmulationEnabled = enabled
  def isMouse1351EmulationEnabled: Boolean = mouse1351EmulationEnabled
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
}

object ControlPort {
  val CONFIGURATION_UD_JOYSTICK_UP = "joy.ud.key.up"
  val CONFIGURATION_UD_JOYSTICK_DOWN = "joy.ud.key.down"
  val CONFIGURATION_UD_JOYSTICK_LEFT = "joy.ud.key.left"
  val CONFIGURATION_UD_JOYSTICK_RIGHT = "joy.ud.key.right"
  val CONFIGURATION_UD_JOYSTICK_UP_RIGHT = "joy.ud.key.up_right"
  val CONFIGURATION_UD_JOYSTICK_UP_LEFT = "joy.ud.key.up_left"
  val CONFIGURATION_UD_JOYSTICK_DOWN_RIGHT = "joy.ud.key.down_right"
  val CONFIGURATION_UD_JOYSTICK_DOWN_LEFT = "joy.ud.key.down_left"
  val CONFIGURATION_UD_JOYSTICK_FIRE = "joy.ud.key.fire"
  
  private abstract class AbstractControlPort extends ControlPort with MouseListener with KeyListener {
    protected var mask = 0

    protected def getKeyMask(e:KeyEvent) : Int
    def mouseClicked(e:MouseEvent) : Unit = {}
    def mouseEntered(e:MouseEvent) : Unit = {}
    def mouseExited(e:MouseEvent) : Unit = {}
    def mousePressed(e:MouseEvent) : Unit = {
      if (isMouse1351EmulationEnabled) {
        if (SwingUtilities.isRightMouseButton(e)) mask |= 1 // 1351 right button emulates joy-UP
        else mask |= 16 // 1351 right button emulates joy-FIRE
      }
      else
      if (!isLightPenEmulationEnabled) mask |= 16
    }
    def mouseReleased(e:MouseEvent) : Unit = {
      if (isMouse1351EmulationEnabled) {
        if (SwingUtilities.isRightMouseButton(e)) mask &= ~1 // 1351 right button emulates joy-UP
        else mask &= ~16 // 1351 right button emulates joy-FIRE
      }
      else
      if (!isLightPenEmulationEnabled) mask &= ~16
    }
    
    def keyPressed(e: KeyEvent): Unit = mask |= getKeyMask(e)
    
    def keyReleased(e: KeyEvent): Unit = mask &= ~getKeyMask(e)
    def keyTyped(e: KeyEvent) : Unit = {}
   
    protected def read: Int = ~mask & 0xFF
  }
  
  val emptyControlPort : ControlPort with MouseListener = new AbstractControlPort {
    override val isConnected = false
    protected def getKeyMask(e:KeyEvent) : Int = 0
    override def mousePressed(e:MouseEvent): Unit = {
      if (isMouse1351EmulationEnabled) super.mousePressed(e)
    }
    override def mouseReleased(e: MouseEvent): Unit = {
      if (isMouse1351EmulationEnabled) super.mouseReleased(e)
    }
  }
  /**
   * Joystick emulation via KeyPad
   */
  def keypadControlPort : ControlPort with MouseListener with KeyListener = new AbstractControlPort {
    override def consumeKey(e:KeyEvent): Boolean = {
      import KeyEvent._
      if (e.getKeyLocation == KEY_LOCATION_NUMPAD) {
        e.getKeyCode match {
          case VK_NUMPAD8 | VK_UP | VK_NUMPAD2 | VK_DOWN | VK_NUMPAD4 | VK_LEFT | VK_NUMPAD6 | VK_RIGHT | VK_NUMPAD9 | VK_NUMPAD3 | VK_NUMPAD7 | VK_NUMPAD1 | VK_NUMPAD0 | VK_INSERT => true
          case _ => false
        }
      }
      else false
    }
    protected def getKeyMask(e:KeyEvent): Int = {
      import KeyEvent._
      if (e.getKeyLocation == KEY_LOCATION_NUMPAD) {
        e.getKeyCode match {
          case VK_NUMPAD8|VK_UP if (mask & 2) == 0 => 1 // up
          case VK_NUMPAD2|VK_DOWN if (mask & 1) == 0 => 2 // down
          case VK_NUMPAD4|VK_LEFT if (mask & 8) == 0 => 4 // left
          case VK_NUMPAD6|VK_RIGHT if (mask & 4) == 0 => 8 // right
          case VK_NUMPAD9 => 9 // up+right
          case VK_NUMPAD3 => 10// down+right
          case VK_NUMPAD7 => 5 // up+left
          case VK_NUMPAD1 => 6 // down+left
          case VK_NUMPAD0|VK_INSERT => 16	// fire
          case _ => 0
        }
      }
      else 0
    }    
  }  
  /**
   * Joystick emulation via user defined keys
   */
  def userDefinedKeyControlPort(conf:Properties) : ControlPort with MouseListener with KeyListener = new AbstractControlPort {
    private[this] var upKey = 0
    private[this] var downKey = 0
    private[this] var leftKey = 0
    private[this] var rightKey = 0
    private[this] var upRightKey = 0
    private[this] var upLeftKey = 0
    private[this] var downRightKey = 0
    private[this] var downLeftKey = 0
    private[this] var fireKey = 0

    updateConfiguration()

    override def consumeKey(e:KeyEvent): Boolean = {
      import KeyEvent._
      if (e.getKeyLocation != KEY_LOCATION_NUMPAD) {
        val k = e.getKeyCode
        k == upKey || k == downKey || k == leftKey || k == rightKey || k == upRightKey || k == upLeftKey || k == downLeftKey || k == downRightKey || k == fireKey
      }
      else false
    }

    protected def getKeyMask(e:KeyEvent): Int = {
      import KeyEvent._
      if (e.getKeyLocation != KEY_LOCATION_NUMPAD) {
        val keyCode = e.getKeyCode
        if (keyCode == upKey && (mask & 2) == 0)             1 // up
        else if (keyCode == downKey && (mask & 1) == 0)      2 // down
        else if (keyCode == leftKey && (mask & 8) == 0)      4 // left
        else if (keyCode == rightKey && (mask & 4) == 0)     8 // right
        else if (keyCode == upRightKey)                      9 // up+right
        else if (keyCode == downRightKey)                    10// down+right
        else if (keyCode == upLeftKey)                       5 // up+left
        else if (keyCode == downLeftKey)                     6 // down+left
        else if (keyCode == fireKey)                         16// fire
        else 0
      }
      else 0
    }

    override def updateConfiguration() : Unit = {
      upKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP,"-1").toInt
      downKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN,"-1").toInt
      leftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_LEFT,"-1").toInt
      rightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_RIGHT,"-1").toInt
      upRightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP_RIGHT,"-1").toInt
      upLeftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP_LEFT,"-1").toInt
      downRightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN_RIGHT,"-1").toInt
      downLeftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN_LEFT,"-1").toInt
      fireKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_FIRE,"-1").toInt
    }
  }
}
  