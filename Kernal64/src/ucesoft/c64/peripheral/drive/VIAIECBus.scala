package ucesoft.c64.peripheral.drive

import ucesoft.c64.peripheral.bus.IECBusListener
import ucesoft.c64.ChipID
import ucesoft.c64.peripheral.bus.IECBus
import ucesoft.c64.peripheral.bus.IECBusLine
import ucesoft.c64.Log
import ucesoft.c64.Clock
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

class VIAIECBus(driveID:Int,
				bus:IECBus,
				irqAction:(Boolean) => Unit,atnOn: () => Unit) extends VIA("VIA_IECBus" + driveID,0x1800,irqAction) with IECBusListener {
  override lazy val componentID = "VIA1 (Bus)"
  val busid = name
  private[this] val IDJACK = driveID & 0x03
  //private[this] var data_out = IECBus.VOLTAGE
  private[this] var driveEnabled = true
  
  bus.registerListener(this)
  if (driveID == 0) ParallelCable.pcCallback = onCB1 _
  
  override def reset {
    super.reset
    //data_out = IECBus.VOLTAGE
  }
  
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
  
  private def onCB1 = irq_set(IRQ_CB1)
  
  override def read(address: Int, chipID: ChipID.ID) = (address & 0x0F) match {
    case ad@(PA|PA2) =>
      super.read(address,chipID)
      if (driveID == 0 && ParallelCable.enabled) {
        val r = ParallelCable.read & ~regs(DDRA)
        if (ad == PA && (regs(PCR) & 0xE) == 0xA) ParallelCable.onCA2
        r
      }
      else 0xFF
    case PB =>
      (super.read(address,chipID) & 0x1A) | (bus.data|data_out) | (bus.clk|clock_out) << 2 | bus.atn << 7 | IDJACK << 5
      
    case ofs => super.read(address,chipID)
  }
  
  override def write(address: Int, value: Int, chipID: ChipID.ID) {
    super.write(address,value,chipID)
    (address & 0x0F) match {
      case PB|DDRB =>        
        bus.setLine(busid,IECBusLine.DATA,data_out)
        bus.setLine(busid,IECBusLine.CLK,clock_out)
        
        autoacknwoledgeData
      case ad@(PA|PA2) =>
        if (driveID == 0 && ParallelCable.enabled) {
          ParallelCable.write(value)
          if (ad == PA && (regs(PCR) & 0xE) == 0xA) ParallelCable.onCA2
        }
      case _ =>
    }         
  }
  
  @inline private def data_out = if ((regs(DDRB) & regs(PB) & 0x02) > 0) IECBus.GROUND else IECBus.VOLTAGE
  @inline private def clock_out = if ((regs(DDRB) & regs(PB) & 0x08) > 0) IECBus.GROUND else IECBus.VOLTAGE
  
  @inline private def autoacknwoledgeData {
    val atna = (regs(DDRB) & regs(PB) & 0x10) > 0
    //val atna = ((~regs(DDRB) | regs(PB)) & 0x10) > 0
    val dataOut = (bus.atn == IECBus.GROUND) ^ atna
    if (dataOut) bus.setLine(busid,IECBusLine.DATA,IECBus.GROUND) else bus.setLine(busid,IECBusLine.DATA,data_out)
  }
  // state
  override protected def saveState(out:ObjectOutputStream) {
    super.saveState(out)
    //out.writeInt(data_out)
    out.writeBoolean(driveEnabled)
  }
  override protected def loadState(in:ObjectInputStream) {
    super.loadState(in)
    driveEnabled = in.readBoolean
  }
}