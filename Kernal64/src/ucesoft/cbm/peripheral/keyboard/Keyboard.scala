package ucesoft.cbm.peripheral.keyboard

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import CKey._
import ucesoft.cbm.Log
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class Keyboard(keyMapper: KeyboardMapper, nmiAction: (Boolean) => Unit = x => {},c128 : Boolean = false) extends KeyListener with CBMComponent {
  val componentID = "Keyboard"
  val componentType = CBMComponentType.INPUT_DEVICE 
  
  private[this] val keysPressed = collection.mutable.Set.empty[CKey.Key]
  private[this] val keyMap = keyMapper.map
  private[this] val keyPadMap = keyMapper.keypad_map
  private[this] val keyMapChar = keyMapper.cmap
  private[this] val rowSelector,c128ExtendedRowSelector = Array.fill(8)(false)
  private[this] val colSelector = Array.fill(8)(false)
  private[this] var enabled,keypadEnabled = true
  // 128 CAPS-LOCK & 40/80 keys handling
  private[this] var c128_CapsLockPressed = false
  private[this] var c128_40_80_Pressed = false
  
  def isCapsLockPressed = c128_CapsLockPressed
  def is4080Pressed = c128_40_80_Pressed
  def set4080Pressed(pressed:Boolean) = c128_40_80_Pressed = pressed
  
  def init {}
  def reset {
    keysPressed.clear
    for(i <- 0 until rowSelector.length) rowSelector(i) = false
    for(i <- 0 until rowSelector.length) c128ExtendedRowSelector(i) = false
    for(i <- 0 until rowSelector.length) colSelector(i) = false
  }
    
  override def getProperties = {
    properties.setProperty("Number of key pressed",keysPressed.size.toString)
    properties
  }

  final def setEnabled(enabled: Boolean) = {
    this.enabled = enabled
  }
  
  final def enableKeypad(enabled:Boolean) {
    keypadEnabled = enabled
  }

  final def keyPressed(e: KeyEvent) : Unit = synchronized {
    if (c128 && e.getKeyCode == KeyEvent.VK_CAPS_LOCK) {
      c128_CapsLockPressed = !c128_CapsLockPressed      
      return
    }
    if (c128 && e.getKeyCode == KeyEvent.VK_F9) {
      c128_40_80_Pressed = !c128_40_80_Pressed
      return
    }
    if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
      if (keypadEnabled)
      keyPadMap get e.getKeyCode match {
        case Some(key) =>
          keysPressed += key
        case None =>
      }
    }
    else {
      if ((e.getModifiers & java.awt.Event.ALT_MASK) == 0) {
        keyMap get e.getKeyCode match {
          case None =>
            keyMapChar get e.getKeyChar match {
              case None => //Log.debug("Unknown char: " + e)
              case Some((key,shift)) =>
                //Log.debug("Char pressed: " + KeyEvent.getKeyText(e.getKeyCode) + " loc:" + e.getKeyLocation)
                if (key == RESTORE) {
                  nmiAction(true)
                  nmiAction(false) // clears immediately NMI
                }
                else { 
                  keysPressed += key
                  if (shift) keysPressed += CKey.L_SHIFT 
                }
            }           
          case Some(key) =>
            //Log.debug("Pressed: " + KeyEvent.getKeyText(e.getKeyCode) + " loc:" + e.getKeyLocation)
            if (key == RESTORE) {
              nmiAction(true)
              nmiAction(false) // clears immediately NMI
            }
            else keysPressed += key
        }      
      }
    }
  }
  final def keyReleased(e: KeyEvent) = synchronized {
    if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
      if (keypadEnabled)
      keyPadMap get e.getKeyCode match {
        case Some(key) =>
          keysPressed -= key
        case None =>
      }
    }
    else {
      keyMap get e.getKeyCode match {
        case None =>
          keyMapChar get e.getKeyChar match {
              case None =>
              case Some((key,shift)) =>
                if (key != RESTORE) {
                  keysPressed -= key 
                  if (shift) keysPressed -= CKey.L_SHIFT
                }
                //else nmiAction(false)
          }
        case Some(key) =>
          if (key != RESTORE) keysPressed -= key else nmiAction(false)
      }
    }
  }
  final def keyTyped(e: KeyEvent) {}

  private final def select(value: Int,selector:Array[Boolean]) {
    var mask = 1
    var row = 0
    while (mask != 0x100) {
      selector(row) = (!((value & mask) == mask))
      row += 1
      mask <<= 1
    }
  }
  
  final def selectRow(value:Int) = select(value,rowSelector)
  final def selectC128ExtendedRow(value:Int) = select(value,c128ExtendedRowSelector)
  final def selectCol(value:Int) = select(value,colSelector)
  final def readCol = read(rowSelector) & read(c128ExtendedRowSelector)  
  final def readRow = read(colSelector)

  private final def read(selector:Array[Boolean]) = synchronized {
    if (enabled) {
      val isRowSel = (selector == rowSelector) || (selector == c128ExtendedRowSelector)
      val isExtendedSelector = selector == c128ExtendedRowSelector
      var res = 0
      val keys = keysPressed.iterator
      while (keys.hasNext) {
        val k = keys.next
        val (r, c) = CKey.getRowCol(k)
        val row = if (isRowSel) r else c
        val col = if (!isRowSel) r else c
        if (CKey.is128Key(k) == isExtendedSelector && selector(row)) res |= 1 << col 
      }
      0xFF - res
    } 
    else 0xFF
  }  
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeObject(c128ExtendedRowSelector)
    out.writeObject(rowSelector)
    out.writeObject(colSelector)
    out.writeBoolean(enabled)
    out.writeBoolean(keypadEnabled)
  }
  protected def loadState(in:ObjectInputStream) {
    loadMemory[Boolean](c128ExtendedRowSelector,in)
    loadMemory[Boolean](rowSelector,in)
    loadMemory[Boolean](colSelector,in)
    enabled = in.readBoolean
    keypadEnabled = in.readBoolean
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}