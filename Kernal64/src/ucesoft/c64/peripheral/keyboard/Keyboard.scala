package ucesoft.c64.peripheral.keyboard

import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import CKey._
import ucesoft.c64.Log
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class Keyboard(keyMapper: KeyboardMapper = DefaultKeyboardMapper, nmiAction: (Boolean) => Unit = x => {}) extends KeyListener with C64Component {
  val componentID = "Keyboard"
  val componentType = C64ComponentType.INPUT_DEVICE 
  
  private[this] val keysPressed = collection.mutable.Set.empty[CKey.Key]
  private[this] val keyMap = keyMapper.map
  private[this] val keyMapChar = keyMapper.cmap
  private[this] val rowSelector = Array.fill(8)(false)
  private[this] val colSelector = Array.fill(8)(false)
  private[this] var enabled = true
  
  def init {}
  def reset {
    keysPressed.clear
    for(i <- 0 until rowSelector.length) rowSelector(i) = false
    for(i <- 0 until rowSelector.length) colSelector(i) = false
  }
  
  override def getProperties = {
    properties.setProperty("Number of key pressed",keysPressed.size.toString)
    properties
  }

  final def setEnabled(enabled: Boolean) = {
    this.enabled = enabled
  }

  final def keyPressed(e: KeyEvent) = synchronized {
    if (e.getKeyLocation != KeyEvent.KEY_LOCATION_NUMPAD) {
      keyMap get e.getKeyCode match {
        case None =>
          keyMapChar get e.getKeyChar match {
            case None => //Log.debug("Unknown char: " + e)
            case Some((key,shift)) =>
              Log.debug("Char pressed: " + KeyEvent.getKeyText(e.getKeyCode) + " loc:" + e.getKeyLocation)
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
          Log.debug("Pressed: " + KeyEvent.getKeyText(e.getKeyCode) + " loc:" + e.getKeyLocation)
          if (key == RESTORE) {
            nmiAction(true)
            nmiAction(false) // clears immediately NMI
          }
          else keysPressed += key
      }
    }
  }
  final def keyReleased(e: KeyEvent) = synchronized {
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
  final def selectCol(value:Int) = select(value,colSelector)
  final def readCol = read(rowSelector)
  final def readRow = read(colSelector)

  private final def read(selector:Array[Boolean]) = synchronized {
    if (enabled) {
      val isRowSel = selector == rowSelector
      var res = 0
      val keys = keysPressed.iterator
      while (keys.hasNext) {
        val k = keys.next
        val (r, c) = CKey.getRowCol(k)
        val row = if (isRowSel) r else c
        val col = if (!isRowSel) r else c
        if (selector(row)) res |= 1 << col 
      }
      0xFF - res
    } 
    else 0xFF
  }  
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeObject(rowSelector)
    out.writeObject(colSelector)
    out.writeBoolean(enabled)
  }
  protected def loadState(in:ObjectInputStream) {
    loadMemory[Boolean](rowSelector,in)
    loadMemory[Boolean](colSelector,in)
    enabled = in.readBoolean
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}