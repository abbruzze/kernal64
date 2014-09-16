package ucesoft.c64.peripheral.cia

import ucesoft.c64.peripheral.vic.BankedMemory
import ucesoft.c64.peripheral.bus._
import ucesoft.c64.peripheral.Connector

object CIA2Connectors {
  class PortAConnector(mem:BankedMemory,bus:IECBus) extends Connector with IECBusListener {
    val componentID = "CIA2 Port A Connector"
    override val isController = true
    val busid = "CIA2_PortA"
      
    bus.registerListener(this)
      
    import IECBus._
    final def read = {
      //latch
      import bus._
      (~((clk << 6) | (data << 7)) & 0xC0) | (latch & 0x3F)
    }
    final protected def performWrite(data:Int) = {
      var bank = 0
      if ((ddr & 1) > 0 && (data & 1) > 0) bank |= 1
      if ((ddr & 2) > 0 && (data & 2) > 0) bank |= 2
      if ((ddr & 4) > 0 && (data & 4) > 0) bank |= 4
      mem.setBank(bank)
      
      bus.setLine(busid,IECBusLine.ATN,if ((ddr & 8) == 8 && (data & 8) == 8) GROUND else VOLTAGE)
      bus.setLine(busid,IECBusLine.CLK,if ((ddr & 16) == 16 && (data & 16) == 16) GROUND else VOLTAGE)
      bus.setLine(busid,IECBusLine.DATA,if ((ddr & 32) == 32 && (data & 32) == 32) GROUND else VOLTAGE)
    }    
  }
  object PortBConnector extends Connector {
    val componentID = "CIA2 Port B Connector"
    final def read = latch
    final protected def performWrite(data:Int) {}
  }
}