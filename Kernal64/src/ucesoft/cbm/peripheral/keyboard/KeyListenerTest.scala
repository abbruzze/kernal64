package ucesoft.cbm.peripheral.keyboard

import javax.swing.JFrame

import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.JPanel
import ucesoft.cbm.Log
import ucesoft.cbm.peripheral.controlport.ControlPort

object KeyListenerTest extends App {
    Log.setDebug
	val kb = new Keyboard
	val cp = ControlPort.keypadControlPort
	
	val f = new JFrame("Key Test")
	f.setSize(100,100)
	val keyPanel = new JPanel
	f.getContentPane.add("Center",keyPanel)
	f.addKeyListener(kb)
	f.addKeyListener(cp)
	f.requestFocus()
	f.setVisible(true)
	
	new Thread {
	  override def run {	    
	    while (true) {
	      Thread.sleep(10)
	      val joy = cp.readPort
	      if (joy != 0xff) println("Joy=" + joy)
	      kb.selectRow(0)
	      if (kb.readCol != 0xFF) {
	    	  var col = 0xFE
	    	  do {
	    	    kb.selectRow(col)
	    	    val read = kb.readCol
	    	    if (read != 0xFF) printf("%02X,%02X\n",col,read)
	    	    col = ((col << 1) | 1) & 0xFF
	    	  }
	    	  while (col != 0xFF)
	      }
	    }
	  }
	}.start	
}