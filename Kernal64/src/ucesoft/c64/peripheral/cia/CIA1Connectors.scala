package ucesoft.c64.peripheral.cia

import ucesoft.c64.peripheral.keyboard.Keyboard
import ucesoft.c64.peripheral.controlport.ControlPort
import ucesoft.c64.peripheral.Connector

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
	  private[this] var lastLP = 0x10
	  final def read = {
	    val port = ctrlPort.readPort
	    val reg = kb.readCol & (latch | ~ddr) & port	    
	    reg
	  }
	  final protected def performWrite(data:Int) {
	    val port = ctrlPort.readPort
	    val lpCheck = (data | ~ddr) & 0x10
	    if (lpCheck != lastLP) {
	      lastLP = lpCheck
	      lightPenTriggerHandler()
	    }
	    kb.selectCol((data | ~ddr) & port & 0xFF)
	  }
	}
}