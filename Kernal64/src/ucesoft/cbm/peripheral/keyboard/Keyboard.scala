package ucesoft.cbm.peripheral.keyboard

import java.awt.event.KeyListener
import java.awt.event.KeyEvent

import CKey._
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import javax.swing.JFrame
import ucesoft.cbm.cpu.Memory

object Keyboard {
  def insertSmallTextIntoKeyboardBuffer(txt:String,mem:Memory,c64Mode:Boolean) : Unit = {
    val bufferAddr = if (c64Mode) 631 else 842
    val lenAddr = if (c64Mode) 198 else 208
    for(i <- 0 until txt.length) {
      mem.write(bufferAddr + i,txt.charAt(i))
    }
    mem.write(lenAddr,txt.length)
  }

  def insertTextIntoKeyboardBuffer(txt:String,mem:Memory,c64Mode:Boolean): Unit = {
    new Thread {
      val maxLenAddr = if (c64Mode) 649 else 2592
      val bufferAddr = if (c64Mode) 631 else 842
      val lenAddr = if (c64Mode) 198 else 208
      override def run : Unit = {
        val len = mem.read(maxLenAddr)
        var strpos = 0
        while (strpos < txt.length) {
          val size = if (len < txt.length - strpos) len else txt.length - strpos
          for(i <- 0 until size) {
            val c = txt.charAt(strpos).toUpper
            mem.write(bufferAddr + i,if (c != '\n') c else 0x0D)
            strpos += 1
          }
          mem.write(lenAddr,size)
          val ts = System.currentTimeMillis
          // wait max 10 secs...
          while (mem.read(lenAddr) > 0 && System.currentTimeMillis - ts < 10000) Thread.sleep(1)
        }
      }
    }.start
  }
}

class Keyboard(private var keyMapper: KeyboardMapper, nmiAction: (Boolean) => Unit = x => {},c128 : Boolean = false) extends KeyListener with CBMComponent {
  val componentID = "Keyboard"
  val componentType = CBMComponentType.INPUT_DEVICE 
  
  private[this] val keysPressed = collection.mutable.Set.empty[CKey.Key]
  private[this] var keyMap = keyMapper.map
  private[this] var keyPadMap = keyMapper.keypad_map
  private[this] val rowSelector,c128ExtendedRowSelector = Array.fill(8)(false)
  private[this] val colSelector = Array.fill(8)(false)
  private[this] var enabled,keypadEnabled = true
  // 128 CAPS-LOCK & 40/80 keys handling
  private[this] var c128_CapsLockPressed = false
  private[this] var c128_40_80_Pressed = false
  private[this] var _40_80_KEY : Int = keyMapper.map.map(kv => (kv._2,kv._1)).getOrElse(_40_80,KeyEvent.VK_F9)
  private[this] var CAPS_LOCK_KEY : Int = keyMapper.map.map(kv => (kv._2,kv._1)).getOrElse(CAPS_LOCK,KeyEvent.VK_CAPS_LOCK)

  def setKeyboardMapper(km:KeyboardMapper): Unit = {
    keyMapper = km
    keyMap = keyMapper.map
    keyPadMap = keyMapper.keypad_map
    _40_80_KEY = keyMapper.map.map(kv => (kv._2,kv._1)).getOrElse(_40_80,KeyEvent.VK_F9)
    CAPS_LOCK_KEY = keyMapper.map.map(kv => (kv._2,kv._1)).getOrElse(CAPS_LOCK,KeyEvent.VK_CAPS_LOCK)
  }

  def getKeyboardMapper : KeyboardMapper = keyMapper
  
  def isCapsLockPressed = c128_CapsLockPressed
  def is4080Pressed = c128_40_80_Pressed
  def set4080Pressed(pressed:Boolean) = c128_40_80_Pressed = pressed
  
  def init : Unit = {}
  def reset : Unit = {
    keysPressed.clear
    for(i <- 0 until rowSelector.length) rowSelector(i) = false
    for(i <- 0 until rowSelector.length) c128ExtendedRowSelector(i) = false
    for(i <- 0 until rowSelector.length) colSelector(i) = false
  }
    
  override def getProperties = {
    properties.setProperty("Number of key pressed",keysPressed.size.toString)
    properties
  }

  final def setEnabled(enabled: Boolean) : Unit ={
    this.enabled = enabled
  }
  
  final def enableKeypad(enabled:Boolean) : Unit = {
    keypadEnabled = enabled
  }

  final def keyPressed(e: KeyEvent) : Unit = synchronized {
    if (c128 && e.getKeyCode == CAPS_LOCK_KEY) {
      c128_CapsLockPressed = !c128_CapsLockPressed      
      return
    }
    if (c128 && e.getKeyCode == _40_80_KEY) {
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
      if (!e.isAltDown) {
        val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode
        keyMap get key match {
          case None =>
          case Some(key) =>
            //Log.debug("Pressed: " + KeyEvent.getKeyText(e.getKeyCode) + " loc:" + e.getKeyLocation)
            if (key == RESTORE) {
              nmiAction(true)
              nmiAction(false) // clears immediately NMI
            }
            else
            if (key == L_SHIFT && e.getKeyLocation == KeyEvent.KEY_LOCATION_RIGHT) keysPressed += R_SHIFT
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
      val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode
      keyMap get key match {
        case None =>
        case Some(key) =>
          if (key == L_SHIFT && e.getKeyLocation == KeyEvent.KEY_LOCATION_RIGHT) keysPressed -= R_SHIFT
          else
          if (key != RESTORE) keysPressed -= key// else nmiAction(false)
      }
    }
  }
  final def keyTyped(e: KeyEvent) : Unit = {}

  private final def select(value: Int,selector:Array[Boolean]) : Unit = {
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
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeObject(c128ExtendedRowSelector)
    out.writeObject(rowSelector)
    out.writeObject(colSelector)
    out.writeBoolean(enabled)
    out.writeBoolean(keypadEnabled)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    loadMemory[Boolean](c128ExtendedRowSelector,in)
    loadMemory[Boolean](rowSelector,in)
    loadMemory[Boolean](colSelector,in)
    enabled = in.readBoolean
    keypadEnabled = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}