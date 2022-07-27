package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cbm2.CBM2MMU

import java.awt.event.KeyEvent
import java.io.{ObjectInputStream, ObjectOutputStream}

object BKeyboard {
  import CKey._

  import KeyEvent._

  private var keybThread : KeyboardThread = _
  private var keybThreadRunning = false

  private class KeyboardThread(txt:String,mem:CBM2MMU) extends Thread {
    override def run() : Unit = {
      keybThreadRunning = true
      val bufferAddr = 939
      val lenAddr = 209
      val len = 10
      var strpos = 0
      while (keybThreadRunning && strpos < txt.length) {
        val size = if (len < txt.length - strpos) len else txt.length - strpos
        for(i <- 0 until size) {
          val c = txt.charAt(strpos).toUpper
          mem.writeBank(bufferAddr + i,if (c != '\n') c else 0x0D,forcedBank = 15)
          strpos += 1
        }
        mem.writeBank(lenAddr,size,forcedBank = 15)
        while (keybThreadRunning && mem.readBank(lenAddr,forcedBank = 15) > 0) Thread.sleep(1)
      }
      keybThreadRunning = false
    }
  }

  def insertTextIntoKeyboardBuffer(txt:String,mem:CBM2MMU): Unit = {
    if (keybThreadRunning) keybThreadRunning = false
    keybThread = new KeyboardThread(txt,mem)
    keybThread.start()
  }

  val DEF_CBM2_KEYMAPPER: KeyboardMapper = new KeyboardMapper {
    override val map: Map[Int, Key] = Map(
      // A
      VK_F1 -> CBM2_F1, VK_F2 -> CBM2_F2, VK_F3 -> CBM2_F3, VK_F4 -> CBM2_F4, VK_F5 -> CBM2_F5, VK_F6 -> CBM2_F6, VK_F7 -> CBM2_F7, VK_F8 -> CBM2_F8, VK_F9 -> CBM2_F9, VK_F10 -> CBM2_F10,VK_DOWN -> CBM2_DOWN,VK_UP -> CBM2_UP,VK_HOME -> CBM2_HOME,VK_F11 -> CBM2_RVS,VK_F12 -> CBM2_GRAPH,VK_INSERT -> CBM2_STOP,
      // B
      VK_ESCAPE -> CBM2_ESC,VK_1 -> CBM2_1,VK_2 -> CBM2_2,VK_3 -> CBM2_3,VK_4 -> CBM2_4,VK_5 -> CBM2_5,VK_7 -> CBM2_7,VK_8 -> CBM2_8,VK_9 -> CBM2_9,VK_0 -> CBM2_0,VK_QUOTE -> CBM2_EQUAL,VK_LEFT -> CBM2_LEFT,
      // C
      VK_TAB -> CBM2_TAB,VK_Q -> CBM2_Q,VK_W -> CBM2_W,VK_E -> CBM2_E,VK_R -> CBM2_R,VK_6 -> CBM2_6,VK_U -> CBM2_U,VK_I -> CBM2_I,VK_O -> CBM2_O,VK_MINUS -> CBM2_-,VK_BACK_SLASH -> CBM2_BARROW,VK_RIGHT -> CBM2_RIGHT,
      // D
      VK_A -> CBM2_A,VK_S -> CBM2_S,VK_D -> CBM2_D,VK_T -> CBM2_T,VK_Y -> CBM2_Y,VK_J -> CBM2_J,VK_K -> CBM2_K,VK_L -> CBM2_L,VK_P -> CBM2_P,/*è*/16777448 -> CBM2_CL_SQ_BR,VK_BACK_SPACE -> CBM2_DEL,
      // E
      VK_SHIFT -> CBM2_SHIFT,VK_Z -> CBM2_Z,VK_X -> CBM2_X,VK_F -> CBM2_F,VK_G -> CBM2_G,VK_H -> CBM2_H,VK_M -> CBM2_M,VK_PERIOD -> CBM2_DOT,/*ò*/16777458 -> CBM2_SEMICOL,VK_PLUS -> CBM2_OP_SQ_BR,VK_ENTER -> CBM2_RETURN,VK_DELETE -> CBM2_COMMODORE,
      // F
      VK_CONTROL -> CBM2_CONTROL,VK_C -> CBM2_C,VK_V -> CBM2_V,VK_B -> CBM2_B,VK_N -> CBM2_N,VK_SPACE -> CBM2_SPACE,VK_COMMA -> CBM2_COMMA,/*ì*/16777452 -> CBM2_/,/*à*/16777440 -> CBM2_DB_QUOTE,/*ù*/16777465 -> CBM2_PI
    )
    override val keypad_map: Map[Int, Key] = Map(
      // A
      // B
      VK_NUM_LOCK -> CBM2_KP_?, VK_PAGE_UP -> CBM2_KP_CE, VK_MULTIPLY -> CBM2_KP_*, VK_DIVIDE -> CBM2_KP_/,
      // C
      VK_NUMPAD7 -> CBM2_KP_7, VK_NUMPAD8 -> CBM2_KP_8, VK_NUMPAD9 -> CBM2_KP_9, VK_SUBTRACT -> CBM2_KP_-,
      // D
      VK_NUMPAD4 -> CBM2_KP_4, VK_NUMPAD5 -> CBM2_KP_5, VK_NUMPAD6 -> CBM2_KP_6, VK_ADD -> CBM2_KP_+,
      // E
      VK_NUMPAD1 -> CBM2_KP_1, VK_NUMPAD2 -> CBM2_KP_2, VK_NUMPAD3 -> CBM2_KP_3, VK_ENTER -> CBM2_KP_ENTER,
      // F
      VK_NUMPAD0 -> CBM2_KP_0, VK_DECIMAL -> CBM2_KP_DOT, VK_PRINTSCREEN -> CBM2_KP_00
    )
  }
}

class BKeyboard(private var km:KeyboardMapper) extends Keyboard {
  val componentID = "Keyboard"
  val componentType: Type = CBMComponentType.INPUT_DEVICE

  private val keysPressed = collection.mutable.Set.empty[CKey.Key]
  private var keyMap = km.map
  private var keyPadMap = km.keypad_map
  private var colAddress = 0

  override def init : Unit = {}
  override def reset : Unit = {
    keysPressed.clear()
  }

  override def setKeyboardMapper(km:KeyboardMapper): Unit = {
    keyMap = km.map
    keyPadMap = km.keypad_map
    this.km = km
  }
  override def getKeyboardMapper : KeyboardMapper = km

  final def selectLowColAddress(address:Int): Unit = colAddress = (~address & 0xFF) | (colAddress & 0xFF00)
  final def selectHighColAddress(address:Int): Unit = colAddress = ((~address & 0xFF) << 8) | (colAddress & 0xFF)
  final def read(): Int = {
    var byte = 0x3F // A - F rows
    if (keysPressed.size > 0) {
      val keys = keysPressed.iterator
      while (keys.hasNext) {
        val k = keys.next
        val (r, c) = CKey.getRowCol(k)
        if ((colAddress & (1 << c)) > 0) byte &= ~(1 << r)
      }
    }

    byte
  }

  override final def keyPressed(e: KeyEvent): Unit = {
    if (!e.isAltDown) {
      val keyMap = if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) this.keyPadMap else this.keyMap
      keyMap get e.getExtendedKeyCode match {
        case Some(key) =>
          keysPressed += key
        case None =>
      }
    }
  }

  override final def keyReleased(e: KeyEvent): Unit = {
    if (!e.isAltDown) {
      val keyMap = if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) this.keyPadMap else this.keyMap
      keyMap get e.getExtendedKeyCode match {
        case Some(key) =>
          keysPressed -= key
        case None =>
      }
    }
  }

  override final def keyTyped(e: KeyEvent) : Unit = {}

  override protected def saveState(out:ObjectOutputStream) : Unit = {}
  override protected def loadState(in:ObjectInputStream) : Unit = {}
  override protected def allowsStateRestoring : Boolean = true
}
