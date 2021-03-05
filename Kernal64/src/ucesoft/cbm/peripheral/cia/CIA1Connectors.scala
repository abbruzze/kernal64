package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.controlport.ControlPort
import ucesoft.cbm.peripheral.Connector

object CIA1Connectors {
	class PortAConnector(kb:Keyboard,ctrlPort:ControlPort) extends Connector {
	  val componentID = "CIA1 Port A Connector"
	  final def read = {
	    val port = ctrlPort.readPort
	    kb.readRow & (latch | ~ddr) & port
	  }
	  final protected def performWrite(data:Int) = {
	    val port = ctrlPort.readPort
	    kb.selectRow((data | ~ddr) & port & 0xFF)	  	  
	  }
	}
	
	class PortBConnector(kb:Keyboard,ctrlPort:ControlPort,lightPenTriggerHandler : () => Unit) extends Connector {
	  val componentID = "CIA1 Port B Connector"
	  private[this] var lastLPOn = false
	  final def read = {
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