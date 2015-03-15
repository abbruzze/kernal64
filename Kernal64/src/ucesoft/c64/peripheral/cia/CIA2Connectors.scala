package ucesoft.c64.peripheral.cia

import ucesoft.c64.peripheral.vic.BankedMemory
import ucesoft.c64.peripheral.bus._
import ucesoft.c64.peripheral.Connector
import ucesoft.c64.peripheral.rs232.RS232

object CIA2Connectors {
  class PortAConnector(mem:BankedMemory,bus:IECBus,rs232:RS232) extends Connector with IECBusListener {
    val componentID = "CIA2 Port A Connector"
    override val isController = true
    val busid = "CIA2_PortA"
    
    private[this] var bank = 0
      
    bus.registerListener(this)
      
    import IECBus._
    final def read = {
      import bus._
      (~((clk << 6) | (data << 7)) & 0xC0) | (latch & 0x3C) | bank | rs232.getTXD << 2
    }
    final protected def performWrite(data:Int) = {
      val value = data | ~ddr // WHY ??
      bank = value & 3
      mem.setBank(bank)
      
//      bus.setLine(busid,IECBusLine.ATN,if ((value & 8) > 0) GROUND else VOLTAGE)
//      bus.setLine(busid,IECBusLine.CLK,if ((value & 16) > 0) GROUND else VOLTAGE)
//      bus.setLine(busid,IECBusLine.DATA,if ((value & 32) > 0) GROUND else VOLTAGE)
      bus.setLine(busid,if ((value & 8) > 0) GROUND else VOLTAGE,  // ATN
                        if ((value & 32) > 0) GROUND else VOLTAGE, // DATA
                        if ((value & 16) > 0) GROUND else VOLTAGE) // CLOCK
      rs232.setTXD((data >> 2) & 1)
    }    
  }
  
  class PortBConnector(rs232:RS232) extends Connector {
    val componentID = "CIA2 Port B Connector"
    final def read = {
      rs232.getOthers
    }
    final protected def performWrite(data:Int) {
      rs232.setOthers(data/* | ~ddr*/)
    }
  }
}