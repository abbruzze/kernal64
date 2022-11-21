package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.CBMComponent

import java.awt.event.KeyListener

trait Keyboard extends CBMComponent with KeyListener {
  def setKeyboardMapper(km:KeyboardMapper): Unit
  def getKeyboardMapper : KeyboardMapper
}
