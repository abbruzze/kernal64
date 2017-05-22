package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.peripheral.bus._
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.rs232.RS232
import ucesoft.cbm.peripheral.drive.ParallelCable
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import ucesoft.cbm.peripheral.vic.VICMemory

object CIA2Connectors {
  val CIA2_PORTA_BUSID = "CIA2_PortA"
  
  class PortAConnector(mem:VICMemory,bus:IECBus,rs232:RS232) extends Connector with IECBusListener {
    val componentID = "CIA2 Port A Connector"
    override val isController = true
    val busid = CIA2_PORTA_BUSID
    
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
      mem.setVideoBank(bank)
      
      bus.setLine(busid,if ((value & 8) > 0) GROUND else VOLTAGE,  // ATN
                        if ((value & 32) > 0) GROUND else VOLTAGE, // DATA
                        if ((value & 16) > 0) GROUND else VOLTAGE) // CLOCK
      if ((ddr & 4) > 0) rs232.setTXD((data >> 2) & 1)      
    }
    // state
    override protected def saveState(out:ObjectOutputStream) {
      super.saveState(out)
      out.writeInt(bank)
    }
    override protected def loadState(in:ObjectInputStream) {
      super.loadState(in)
      bank = in.readInt
    }
  }
  
  class PortBConnector(rs232:RS232) extends Connector {
    val componentID = "CIA2 Port B Connector"
    final def read = {
      if (ParallelCable.enabled) {
        ParallelCable.onPC
        ParallelCable.read
      }
      else
      rs232.getOthers
    }
    final protected def performWrite(data:Int) {
      if (ParallelCable.enabled) {
        ParallelCable.onPC
        ParallelCable.write(data)
      }
      else rs232.setOthers(data/* | ~ddr*/)
    }
  }
}