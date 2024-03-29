package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.expansion.{DigiMAX, WiC64}
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.bus._
import ucesoft.cbm.peripheral.drive.ParallelCable
import ucesoft.cbm.peripheral.rs232.RS232
import ucesoft.cbm.peripheral.vic.VIC_II_Memory

import java.io.{ObjectInputStream, ObjectOutputStream}

object CIA2Connectors {
  val CIA2_PORTA_BUSID = "CIA2_PortA"
  
  class PortAConnector(mem:VIC_II_Memory, bus:IECBus, rs232:RS232) extends Connector with IECBusListener {
    val componentID = "CIA2 Port A Connector"
    override val isController = true
    val busid: String = CIA2_PORTA_BUSID
    
    private[this] var bank = 0x03
      
    bus.registerListener(this)
      
    import IECBus._
    final def read: Int = {
      import bus._
      (~((clk << 6) | (data << 7)) & 0xC0) | ((latch | ~ddr) & 0x38) | bank | rs232.getTXD << 2
    }
    final protected def performWrite(data:Int): Unit = {
      val value = data | ~ddr // WHY ??
      bank = value & 3
      mem.setVideoBank(bank)
      
      bus.setLine(this,if ((value & 8) > 0) GROUND else VOLTAGE,  // ATN
                        if ((value & 32) > 0) GROUND else VOLTAGE, // DATA
                        if ((value & 16) > 0) GROUND else VOLTAGE) // CLOCK
      rs232.setTXD((value >> 2) & 1)
      if (DigiMAX.isEnabledOnUserPort) {
        val a0a1 = (data >> 2) & 3
        DigiMAX.selectChannel(a0a1)
      }
      if (WiC64.enabled) {
        WiC64.setMode(data & 4)
      }
    }
    // state
    override protected def saveState(out:ObjectOutputStream) : Unit = {
      super.saveState(out)
      out.writeInt(bank)
    }
    override protected def loadState(in:ObjectInputStream) : Unit = {
      super.loadState(in)
      bank = in.readInt
    }
  }
  
  class PortBConnector(rs232:RS232) extends Connector {
    val componentID = "CIA2 Port B Connector"
    final def read: Int = {
      if (WiC64.enabled) WiC64.read()
      else if (ParallelCable.enabled) {
        ParallelCable.onPC()
        ParallelCable.read
      }
      else if (rs232.isEnabled) rs232.getOthers else (latch | ~ddr) & 0xFF
    }
    final protected def performWrite(data:Int) : Unit = {
      if (WiC64.enabled) {
        WiC64.write(data)
      }
      if (ParallelCable.enabled) {
        ParallelCable.onPC()
        ParallelCable.write(data)
      }
      else rs232.setOthers(data/* | ~ddr*/)
      if (DigiMAX.isEnabledOnUserPort) {
        DigiMAX.write(data)
      }
    }
  }
}