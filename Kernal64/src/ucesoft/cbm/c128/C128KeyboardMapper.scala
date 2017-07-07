package ucesoft.cbm.c128

import java.awt.event.KeyEvent._
import ucesoft.cbm.peripheral.keyboard._
import CKey._

object C128KeyboardMapper extends KeyboardMapper {
  val cmap = ucesoft.cbm.c64.C64KeyboardMapper.cmap
  val map = ucesoft.cbm.c64.C64KeyboardMapper.map ++ Map(
      VK_F11 -> TAB,
      VK_SCROLL_LOCK -> NO_SCROLL,
      VK_F8 -> ESC,
      VK_PAGE_DOWN -> ALT,
      VK_PAUSE -> LINE_FEED,
      VK_F12 -> HELP,
      VK_F9 -> _40_80,
      VK_CAPS_LOCK -> CAPS_LOCK
  )
  val keypad_map = Map(
      VK_NUMPAD1 -> KEYPAD_N_1,
      VK_NUMPAD2 -> KEYPAD_N_2,
      VK_NUMPAD3 -> KEYPAD_N_3,
      VK_NUMPAD4 -> KEYPAD_N_4,
      VK_NUMPAD5 -> KEYPAD_N_5,
      VK_NUMPAD6 -> KEYPAD_N_6,
      VK_NUMPAD7 -> KEYPAD_N_7,
      VK_NUMPAD8 -> KEYPAD_N_8,
      VK_NUMPAD9 -> KEYPAD_N_9,
      VK_NUMPAD0 -> KEYPAD_N_0,
      VK_ADD -> KEYPAD_PLUS,
      VK_SUBTRACT -> KEYPAD_MINUS,
      VK_DECIMAL -> KEYPAD_DOT,
      VK_RIGHT -> KEYPAD_RIGHT,
      VK_LEFT -> KEYPAD_LEFT,
      VK_UP -> KEYPAD_UP,
      VK_DOWN -> KEYPAD_DOWN,
      VK_ENTER -> KEYPAD_ENTER
  )
}
