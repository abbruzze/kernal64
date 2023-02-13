package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.{CBMComponentType, CBMComputerModel}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cbm2.CBM2MMU
import java.awt.event.KeyEvent
import java.io.{ObjectInputStream, ObjectOutputStream}

object BKeyboard {
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
}

class BKeyboard(_km:KeyboardMapper,override protected val model:CBMComputerModel) extends Keyboard(_km,model) {
  val componentID = "Keyboard"
  val componentType: Type = CBMComponentType.INPUT_DEVICE
  private var colAddress = 0

  final def selectLowColAddress(address:Int): Unit = colAddress = (~address & 0xFF) | (colAddress & 0xFF00)
  final def selectHighColAddress(address:Int): Unit = colAddress = ((~address & 0xFF) << 8) | (colAddress & 0xFF)
  final def read(): Int = {
    var byte = 0x3F // A - F rows
    if (keysPressed.size > 0) {
      val keys = keysPressed.iterator
      while (keys.hasNext) {
        val k = keys.next
        val skip = hideShift && CKey.isShift(k)
        if (!skip) {
          val (r, c) = CKey.getRowCol(k)
          if ((colAddress & (1 << c)) > 0) byte &= ~(1 << r)
        }
      }
    }

    byte
  }

  override final def keyPressed(e: KeyEvent): Unit = {
    findPressedKey(e) match {
      case Some(keys) =>
        for(key <- keys) keysPressed += key
      case None =>
    }
  }

  override final def keyReleased(e: KeyEvent): Unit = {
    findPressedKey(e) match {
        case Some(keys) =>
          for(key <- keys) keysPressed -= key
        case None =>
      }
    if (e.getKeyCode == KeyEvent.VK_SHIFT) clearAllPressedShifts()
  }

  override protected def saveState(out:ObjectOutputStream) : Unit = {}
  override protected def loadState(in:ObjectInputStream) : Unit = {}
  override protected def allowsStateRestoring : Boolean = true
}
