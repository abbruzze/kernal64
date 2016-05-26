package ucesoft.c64.peripheral.controlport

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import java.util.Properties

abstract class ControlPort extends C64Component {
  val componentType = C64ComponentType.INPUT_DEVICE
  val componentID = "ControlPort"
    
  private[this] var emulatedBit = 0
  private[this] var lightPenEmulationEnabled = false
  
  def init {}
  def reset {
    emulatedBit = 0
  }
  
  override def getProperties = {
    properties.setProperty("~Value",Integer.toHexString(~readPort & 0xFF))
    properties
  }
  
  def readPort = read & (~emulatedBit & 0xFF)
  
  protected def read : Int	// 4 bits
  
  def emulateFire = emulatedBit = 16
  def emulateUp = emulatedBit = 1
  def emulateDown = emulatedBit = 2
  def emulateLeft = emulatedBit = 4
  def emulateRight = emulatedBit = 8
  def releaseEmulated = emulatedBit = 0
  
  def setLightPenEmulation(enabled:Boolean) = lightPenEmulationEnabled = enabled
  def isLightPenEmulationEnabled = lightPenEmulationEnabled
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
    private[this] var mask = 0
    protected def getKeyMask(e:KeyEvent) : Int
    def mouseClicked(e:MouseEvent) {}
    def mouseEntered(e:MouseEvent) {}
    def mouseExited(e:MouseEvent) {}
    def mousePressed(e:MouseEvent) = if (!isLightPenEmulationEnabled) mask |= 16
    def mouseReleased(e:MouseEvent) = if (!isLightPenEmulationEnabled) mask &= ~16
    
    def keyPressed(e: KeyEvent) = mask |= getKeyMask(e)
    
    def keyReleased(e: KeyEvent) = mask &= ~getKeyMask(e)
    def keyTyped(e: KeyEvent) {}
   
    protected def read = ~mask & 0xFF
  }
  
  def emptyControlPort = new ControlPort {
    protected def read = 0xFF
  }
  /**
   * Joystick emulation via KeyPad
   */
  def keypadControlPort : ControlPort with MouseListener with KeyListener = new AbstractControlPort {
    
    protected def getKeyMask(e:KeyEvent) = {
      import KeyEvent._
      if (e.getKeyLocation == KEY_LOCATION_NUMPAD) {
        e.getKeyCode match {
          case VK_NUMPAD8 => 1 // up
          case VK_NUMPAD2 => 2 // down
          case VK_NUMPAD4 => 4 // left
          case VK_NUMPAD6 => 8 // right
          case VK_NUMPAD9 => 9 // up+right
          case VK_NUMPAD3 => 10// down+right
          case VK_NUMPAD7 => 5 // up+left
          case VK_NUMPAD1 => 6 // down+left
          case VK_NUMPAD0 => 16	// fire
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
    private[this] val upKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP,"-1").toInt
    private[this] val downKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN,"-1").toInt
    private[this] val leftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_LEFT,"-1").toInt
    private[this] val rightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_RIGHT,"-1").toInt
    private[this] val upRightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP_RIGHT,"-1").toInt
    private[this] val upLeftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_UP_LEFT,"-1").toInt
    private[this] val downRightKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN_RIGHT,"-1").toInt
    private[this] val downLeftKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_DOWN_LEFT,"-1").toInt
    private[this] val fireKey = conf.getProperty(CONFIGURATION_UD_JOYSTICK_FIRE,"-1").toInt
    
    protected def getKeyMask(e:KeyEvent) = {
      import KeyEvent._
      if (e.getKeyLocation != KEY_LOCATION_NUMPAD) {
        e.getKeyCode match {
          case `upKey` => 1         // up
          case `downKey` => 2       // down
          case `leftKey` => 4       // left
          case `rightKey` => 8      // right
          case `upRightKey` => 9    // up+right
          case `downRightKey` => 10 // down+right
          case `upLeftKey` => 5     // up+left
          case `downLeftKey` => 6   // down+left
          case `fireKey` => 16	    // fire
          case _ => 0
        }
      }
      else 0
    }    
  }
}
  