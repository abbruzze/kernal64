package ucesoft.c64.peripheral.cia

import ucesoft.c64.peripheral.vic.BankedMemory
import ucesoft.c64.peripheral.bus._
import ucesoft.c64.peripheral.Connector

object CIA2Connectors {
  class PortAConnector(mem:BankedMemory,bus:IECBus) extends Connector with IECBusListener {
    val componentID = "CIA2 Port A Connector"
    override val isController = true
    val busid = "CIA2_PortA"
    
    private[this] var bank = 0
      
    bus.registerListener(this)
      
    import IECBus._
    final def read = {
      //latch
      import bus._
      (~((clk << 6) | (data << 7)) & 0xC0) | (latch & 0x3C) | bank
    }
    final protected def performWrite(data:Int) = {
      val value = data | ~ddr // WHY ??
      bank = value & 3
      mem.setBank(bank)
      
      bus.setLine(busid,IECBusLine.ATN,if ((value & 8) > 0) GROUND else VOLTAGE)
      bus.setLine(busid,IECBusLine.CLK,if ((value & 16) > 0) GROUND else VOLTAGE)
      bus.setLine(busid,IECBusLine.DATA,if ((value & 32) > 0) GROUND else VOLTAGE)
    }    
  }
  object PortBConnector extends Connector {
    val componentID = "CIA2 Port B Connector"
    final def read = latch
    final protected def performWrite(data:Int) {}
  }
}