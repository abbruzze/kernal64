package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.keyboard.CKey._
import ucesoft.cbm.{C128Model, CBMComponentType, CBMComputerModel}

import java.awt.event.KeyEvent
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object HomeKeyboard {
  private var keybThread : KeyboardThread = _
  private var keybThreadRunning = false

  private class KeyboardThread(txt:String,mem:Memory,c64Mode:Boolean) extends Thread {
    override def run() : Unit = {
      keybThreadRunning = true
      val maxLenAddr = if (c64Mode) 649 else 2592
      val bufferAddr = if (c64Mode) 631 else 842
      val lenAddr = if (c64Mode) 198 else 208

      val len = mem.read(maxLenAddr)
      var strpos = 0
      while (keybThreadRunning && strpos < txt.length) {
        val size = if (len < txt.length - strpos) len else txt.length - strpos
        for(i <- 0 until size) {
          val c = txt.charAt(strpos).toUpper
          mem.write(bufferAddr + i,if (c != '\n') c else 0x0D)
          strpos += 1
        }
        mem.write(lenAddr,size)
        while (keybThreadRunning && mem.read(lenAddr) > 0) Thread.sleep(1)
      }
      keybThreadRunning = false
    }
  }

  def insertSmallTextIntoKeyboardBuffer(txt:String,mem:Memory,c64Mode:Boolean) : Unit = {
    val bufferAddr = if (c64Mode) 631 else 842
    val lenAddr = if (c64Mode) 198 else 208
    for(i <- 0 until txt.length) {
      mem.write(bufferAddr + i,txt.charAt(i))
    }
    mem.write(lenAddr,txt.length)
  }

  def insertTextIntoKeyboardBuffer(txt:String,mem:Memory,c64Mode:Boolean): Unit = {
    if (keybThreadRunning) keybThreadRunning = false
    keybThread = new KeyboardThread(txt,mem,c64Mode)
    keybThread.start()
  }
}

class HomeKeyboard(_keyMapper: KeyboardMapper, nmiAction: Boolean => Unit = _ => {},override protected val model:CBMComputerModel) extends Keyboard(_keyMapper,model) {
  val componentID = "Keyboard"
  val componentType: Type = CBMComponentType.INPUT_DEVICE

  private[this] val rowSelector,c128ExtendedRowSelector = Array.fill(8)(false)
  private[this] val colSelector = Array.fill(8)(false)
  // 128 CAPS-LOCK & 40/80 keys handling
  private[this] var c128_CapsLockPressed = false
  private[this] var c128_40_80_Pressed = false
  private[this] var _40_80_KEY : Int = findCKey(_40_80,KeyEvent.VK_F9)
  private[this] var CAPS_LOCK_KEY : Int = findCKey(CAPS_LOCK,KeyEvent.VK_CAPS_LOCK)
  private final val c128 = model == C128Model
  private var capsLockListener : () => Unit = _

  def setCapsLockListener(listener:() => Unit): Unit = capsLockListener = listener

  override def setKeyboardMapper(km:KeyboardMapper): Unit = {
    super.setKeyboardMapper(km)
    _40_80_KEY = findCKey(_40_80,KeyEvent.VK_F9)
    CAPS_LOCK_KEY = findCKey(CAPS_LOCK,KeyEvent.VK_CAPS_LOCK)
  }

  def isCapsLockPressed: Boolean = c128_CapsLockPressed
  def is4080Pressed: Boolean = c128_40_80_Pressed
  def set4080Pressed(pressed:Boolean): Unit = c128_40_80_Pressed = pressed

  override def reset() : Unit = {
    super.reset()
    for(i <- 0 until rowSelector.length) rowSelector(i) = false
    for(i <- 0 until rowSelector.length) c128ExtendedRowSelector(i) = false
    for(i <- 0 until rowSelector.length) colSelector(i) = false
    HomeKeyboard.keybThreadRunning = false
  }
    
  override def getProperties: Properties = {
    properties.setProperty("Number of key pressed", keysPressed.size.toString)
    properties
  }

  final def keyPressed(e: KeyEvent) : Unit = synchronized {
    if (c128 && e.getKeyCode == CAPS_LOCK_KEY) {
      c128_CapsLockPressed = !c128_CapsLockPressed
      if (capsLockListener != null) capsLockListener()
      return
    }
    if (c128 && e.getKeyCode == _40_80_KEY) {
      c128_40_80_Pressed = !c128_40_80_Pressed
      return
    }
    findPressedKey(e) match {
      case Some(PressedKeys(hideShift,keys)) =>
        //val oldPressed = keysPressed.clone()
        for (key <- keys) {
          if (key == RESTORE || key == VIC20_RESTORE) {
            nmiAction(true)
            nmiAction(false) // clears immediately NMI
          }
          else keysPressed += remapShift(key,e)
        }
        if (hideShift) clearAllPressedShifts()
        //println(s"MATCH: hideShift=$hideShift | $oldPressed -> $keysPressed event=$e")
      case None =>
        //println(s"Unmatched: $e alt=${e.isAltDown} altg=${e.isAltGraphDown}")
    }
  }
  final def keyReleased(e: KeyEvent): Unit = synchronized {
    findReleasedKey(e) match {
      case Some(keys) =>
        for (key <- keys) keysPressed -= key
      case None =>
        //if (e.getKeyCode == KeyEvent.VK_SHIFT) keysPressed -= getModelShift(e)
      keysPressed.clear()
    }
    if (e.getKeyCode == KeyEvent.VK_SHIFT) clearAllPressedShifts()
  }

  private final def select(value: Int,selector:Array[Boolean]) : Unit = {
    var mask = 1
    var row = 0
    while (mask != 0x100) {
      selector(row) = !((value & mask) == mask)
      row += 1
      mask <<= 1
    }
  }
  
  final def selectRow(value:Int): Unit = select(value,rowSelector)
  final def selectC128ExtendedRow(value:Int): Unit = select(value,c128ExtendedRowSelector)
  final def selectCol(value:Int): Unit = select(value,colSelector)
  final def readCol: Int = read(rowSelector) & read(c128ExtendedRowSelector)
  final def readRow: Int = read(colSelector)

  private final def read(selector:Array[Boolean]) = synchronized {
    if (enabled) {
      val isRowSel = (selector == rowSelector) || (selector == c128ExtendedRowSelector)
      val isExtendedSelector = selector == c128ExtendedRowSelector
      var res = 0
      val keys = keysPressed.iterator
      while (keys.hasNext) {
        val k = keys.next()
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