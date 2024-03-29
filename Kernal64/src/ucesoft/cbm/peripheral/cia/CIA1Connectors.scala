package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.controlport.ControlPort
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard

object CIA1Connectors {
	class PortAConnector(kb:HomeKeyboard, ctrlPort:ControlPort) extends Connector {
	  val componentID = "CIA1 Port A Connector"
	  final def read: Int = {
	    val port = ctrlPort.readPort
	    kb.readRow & (latch | ~ddr) & port
	  }
	  final protected def performWrite(data:Int): Unit = {
	    val port = ctrlPort.readPort
	    kb.selectRow((data | ~ddr) & port & 0xFF)	  	  
	  }
	}
	
	class PortBConnector(kb:HomeKeyboard, ctrlPort:ControlPort, lightPenTriggerHandler : () => Unit) extends Connector {
	  val componentID = "CIA1 Port B Connector"
	  private[this] var lastLPOn = false
	  final def read: Int = {
	    val port = ctrlPort.readPort
	    val reg = kb.readCol & (latch | ~ddr) & port	    
	    reg
	  }
	  final protected def performWrite(data:Int) : Unit = {
	    val port = ctrlPort.readPort
	    val lpPressed = (data & ddr & 0x10) > 0
	    if (lastLPOn && !lpPressed) lightPenTriggerHandler() // FF -> 00
      lastLPOn = lpPressed
	    kb.selectCol((data | ~ddr) & port & 0xFF)
	  }
	}
}