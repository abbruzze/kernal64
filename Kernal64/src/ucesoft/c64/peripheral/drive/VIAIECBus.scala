package ucesoft.c64.peripheral.drive

import ucesoft.c64.peripheral.bus.IECBusListener
import ucesoft.c64.ChipID
import ucesoft.c64.peripheral.bus.IECBus
import ucesoft.c64.peripheral.bus.IECBusLine
import ucesoft.c64.Log
import ucesoft.c64.Clock

class VIAIECBus(driveID:Int,
				bus:IECBus,
				irqAction:(Boolean) => Unit,atnOn: () => Unit) extends VIA("VIA_IECBus",0x1800,irqAction) with IECBusListener {
  override lazy val componentID = "VIA1 (Bus)"
  val busid = name
  private[this] val IDJACK = driveID & 0x03
  private[this] var data_out = IECBus.VOLTAGE
  private[this] var driveEnabled = true
  
  bus.registerListener(this)
  
  def setEnabled(enabled:Boolean) = driveEnabled = enabled
  
  override def atnChanged(oldValue:Int,newValue:Int) {
    if (driveEnabled) {
      if (newValue == IECBus.GROUND) {
        irq_set(IRQ_CA1)
        atnOn()
      }
      autoacknwoledgeData
    }
  }
  
  override def read(address: Int, chipID: ChipID.ID) = address - startAddress match {
    case PA|PA2 =>
      super.read(address,chipID)
      0xFF
    case PB =>
      (super.read(address,chipID) & 0x1A) | bus.data | bus.clk << 2 | bus.atn << 7 | IDJACK << 5
      
    case ofs => super.read(address,chipID)
  }
  
  override def write(address: Int, value: Int, chipID: ChipID.ID) {
    super.write(address,value,chipID)
    address - startAddress match {
      case PB|DDRB =>        
        val busValue = ~regs(DDRB) | regs(PB)
        //data_out = if ((regs(DDRB) & 0x02) > 0 && (regs(PB) & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
        data_out = if ((busValue & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
        bus.setLine(busid,IECBusLine.DATA,data_out)
        //val clock_out = (regs(DDRB) & 0x08) > 0 && (regs(PB) & 0x08) > 0
        val clock_out = (busValue & 0x08) > 0
        bus.setLine(busid,IECBusLine.CLK,if (clock_out) IECBus.GROUND else IECBus.VOLTAGE)
        
        autoacknwoledgeData
      case _ =>
    }         
  }
  
  private def autoacknwoledgeData {
    //val atna = (regs(DDRB) & 0x10) > 0 && (regs(PB) & 0x10) > 0
    val atna = ((~regs(DDRB) | regs(PB)) & 0x10) > 0
    val dataOut = (bus.atn == IECBus.GROUND) ^ atna
    if (dataOut) bus.setLine(busid,IECBusLine.DATA,IECBus.GROUND) else bus.setLine(busid,IECBusLine.DATA,data_out)
  }
}