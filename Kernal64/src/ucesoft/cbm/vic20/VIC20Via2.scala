package ucesoft.cbm.vic20

import ucesoft.cbm.ChipID
import ucesoft.cbm.peripheral.bus.IECBus.{GROUND, VOLTAGE}
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.controlport.ControlPort
import ucesoft.cbm.peripheral.drive.VIA
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard

/*
9120-912F   37152-37167	6522 VIA#2
===================================================
  9120      37152	      Port B output register
       keyboard column scan
       (PB3) Bit 3 =cassette write line
       (PB7) Bit 7 =Joy 3
  9121      37153	      Port A output register
       keyboard row scan

  912C      37164	      Peripheral control register
     CA1 Cassette read line (Bit 0)
     CA2 Serial clock out (Bits 1-3)
     CB1 Serial SRQ IN (Bit 4)
     CB2 Serial data out (Bits 5-7)
 */
class VIC20Via2(bus:IECBus,
                kb:HomeKeyboard,
                controlPort:ControlPort,
                datassette:Datassette,
                irqAction:Boolean => Unit,
                busListener: IECBusListener) extends VIA("VIA_2",0x9120,irqAction) {

  override def read(address: Int, chipID: ChipID.ID): Int = address & 0x0F match {
    case PA|PA2 =>
      super.read(address, chipID)
      (kb.readRow & (regs(PA) | ~regs(DDRA))) & 0xFF
    case PB =>
      super.read(address, chipID)
      val joy3 = 0x7F | (controlPort.readPort & 8) << 4
      (kb.readCol & (regs(PB) | ~regs(DDRB)) & joy3) & 0xFF
    case _ =>
      super.read(address, chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID): Unit = address & 0x0F match {
    case PA =>
      super.write(address, value, chipID)
      kb.selectRow(value)
    case PB =>
      super.write(address, value, chipID)
      kb.selectCol(value)
      datassette.setWriteLine((value & 8) > 0)
    case _ =>
      super.write(address, value, chipID)
  }

  def datassetteReadLine(): Unit = irq_set(IRQ_CA1) // simplified version

  override def CA2Out(state: Boolean): Unit = bus.setLine(busListener,IECBusLine.CLK,if (state) GROUND else VOLTAGE)
  override def CB2Out(state: Boolean): Unit = bus.setLine(busListener,IECBusLine.DATA,if (state) GROUND else VOLTAGE)
}
