package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.peripheral.keyboard.CKey.{CBM2_SHIFT, L_SHIFT, R_SHIFT, VIC20_L_SHIFT, VIC20_R_SHIFT}
import ucesoft.cbm.{CBMComponent, CBMComputerModel, CBMIIModel, VIC20Model}

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.event.InputEvent._

abstract class Keyboard(protected var km:KeyboardMapper,protected val model:CBMComputerModel) extends CBMComponent with KeyListener {
  protected val keysPressed = collection.mutable.Set.empty[CKey.Key]
  protected var keyMap = km.map
  protected var keyPadMap = km.keypad_map
  protected var enabled,keypadEnabled = true
  private var lastKey: KeyEvent = _
  private final val ALT_CTRL_ALTG_MASK = ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK | CTRL_DOWN_MASK
  private var isLastShiftRight = false
  private final val windowsOS = System.getProperty("os.name").toUpperCase().startsWith("WINDOWS")
  protected var hideShift = false

  final def setEnabled(enabled: Boolean): Unit = {
    this.enabled = enabled
  }

  final def enableKeypad(enabled: Boolean): Unit = {
    keypadEnabled = enabled
  }

  def setKeyboardMapper(km:KeyboardMapper): Unit = {
    this.km = km
    keyMap = km.map
    keyPadMap = km.keypad_map
  }
  def getKeyboardMapper : KeyboardMapper = km

  override def init: Unit = {}

  override def reset: Unit = {
    keysPressed.clear()
    lastKey = null
    isLastShiftRight = false
    hideShift = false
  }

  override final def keyTyped(e: KeyEvent) : Unit = {}

  protected def findCKey(key:CKey.Value,defaultKey:Int): Int = km.map.map(kv => (kv._2,kv._1)).find(lkv => lkv._1.contains(key)).map(_._2.code).getOrElse(defaultKey)

  protected def findPressedKey(e: KeyEvent): Option[List[CKey.Value]] = {
    if (e.isAltDown && !e.isAltGraphDown) return None // only ALT
    if (e.getID == KeyEvent.KEY_PRESSED && e.getKeyCode == KeyEvent.VK_SHIFT) isLastShiftRight = e.getKeyLocation == KeyEvent.KEY_LOCATION_RIGHT
    // On Windows if you press AltGr, it is generated for first a CTRL press followed by the key with ALT+CTRL+AltGr modifiers
    // the following hack removes the first "ghost" event when the second appears
    if (windowsOS) {
      if (e.getID == KeyEvent.KEY_PRESSED && (e.getModifiersEx & ALT_CTRL_ALTG_MASK) == ALT_CTRL_ALTG_MASK && lastKey != null && (lastKey.getModifiersEx & ALT_CTRL_ALTG_MASK) == CTRL_DOWN_MASK) {
        keyReleased(lastKey)
      }
    }
    lastKey = e
    val map = if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
      if (!keypadEnabled) return None else keyPadMap
    }
    else keyMap

    val hk = HostKey(e.getExtendedKeyCode, e.isShiftDown, e.isAltGraphDown)
    map.get(hk) match {
      case None =>
        None
      case list@Some(keys) =>
        hideShift = !keys.exists(k => CKey.isShift(k)) && e.isShiftDown
        list
    }
  }

  protected def findReleasedKey(e: KeyEvent): Option[List[CKey.Value]] = {
    val map = if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
      if (!keypadEnabled) return None else keyPadMap
    }
    else keyMap

    val keys = map.view.filter(kv => kv._1.code == e.getExtendedKeyCode).map(_._2).flatten.toList
    if (keys.isEmpty) None else Some(keys)
  }

  protected def remapShift(key:CKey.Value,e:KeyEvent): CKey.Value = {
    if (CKey.isShift(key)) {
      if (e.isShiftDown) return getModelShift(isLastShiftRight)
    }
    key
  }

  protected def getModelShift(e:KeyEvent): CKey.Value = getModelShift(e.getKeyLocation == KeyEvent.KEY_LOCATION_RIGHT)

  protected def getModelShift(rightLocation:Boolean): CKey.Value = {
    model match {
      case VIC20Model => if (rightLocation) VIC20_R_SHIFT else VIC20_L_SHIFT
      case CBMIIModel => CBM2_SHIFT
      case _ => if (rightLocation) R_SHIFT else L_SHIFT
    }
  }

  protected def clearAllPressedShifts(): Unit = {
    model match {
      case VIC20Model =>
        keysPressed -= VIC20_L_SHIFT
        keysPressed -= VIC20_R_SHIFT
      case CBMIIModel =>
        keysPressed -= CBM2_SHIFT
      case _ =>
        keysPressed -= L_SHIFT
        keysPressed -= R_SHIFT
    }
  }
}
