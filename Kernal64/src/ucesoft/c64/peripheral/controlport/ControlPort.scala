package ucesoft.c64.peripheral.controlport

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

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
  def emptyControlPort = new ControlPort {
    protected def read = 0xFF
  }
  
  def keypadControlPort = new ControlPort with MouseListener with KeyListener {
    private[this] var mask = 0
    private def getKeyMask(e:KeyEvent) = {
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
}
  