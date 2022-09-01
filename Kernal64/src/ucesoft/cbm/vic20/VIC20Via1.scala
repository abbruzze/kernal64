package ucesoft.cbm.vic20

import ucesoft.cbm.ChipID
import ucesoft.cbm.peripheral.bus.IECBus.{GROUND, VOLTAGE}
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.controlport.ControlPort
import ucesoft.cbm.peripheral.drive.VIA

/*
9110-911F  37136-37151 6522 VIA#1
===================================================
   9110    37136     Port B output register
         (user port and RS-232 lines)
     PIN    6522 DESCRIPTION	         EIA   ABV
     ID     ID

     C      PB0 Received data	        (BB)  Sin
     D      PB1 Request to Send	      (CA)  RTS
     E      PB2 Data terminal ready   (CD)  DTR
     F      PB3 Ring indicator	      (CE)  RI
     H      PB4 Received line signal  (CF)  DCD
     J      PB5 Unassigned	          ( )   XXX
     K      PB6 Clear to send	        (CB)  CTS
     L      PB7 Data set ready    	  (CC)  DSR
     B      CB1 Interrupt for Sin     (BB)  Sin
     M      CB2 Transmitted data      (BA)  Sout

     A      GND Protective ground   (M)   GND
     N      GND Signal ground	  (AB)  GND

  9111     37137     Port A output register
       (PA0) Bit 0=Serial CLK IN
       (PA1) Bit 1=Serial DATA IN
       (PA2) Bit 2=Joy 0 / Up
       (PA3) Bit 3=Joy 1 / Down
       (PA4) Bit 4=Joy 2 / Left
       (PA5) Bit 5 = Lightpen/Fire button
       (PA6) Bit 6=Cassette switch sense
       (PA7) Bit 7=Serial ATN out

  911C    37148  Peripheral control register
        (CA1, CA2, CB1, CB2)
        CA1 = restore key (Bit 0)
        CA2 = cassette motor control (Bits 1-3)
        CB1 = interrupt signal for received
            RS-232 data (Bit 4)
        CB2=transmitted RS-232 data (Bits5-7)
 */
class VIC20Via1(bus:IECBus,
                controlPort:ControlPort,
                datassette:Datassette,
                nmiAction:Boolean => Unit) extends VIA("VIA_1",0x9110,nmiAction) with IECBusListener {
  override val busid = "VIA_I_buslistener"
  override val isController = true

  bus.registerListener(this)

  override def read(address: Int, chipID: ChipID.ID): Int = address & 0x0F match {
    case PA|PA2 =>
      super.read(address,chipID)
      val joy = controlPort.readPort
      val joy012Fire = (joy & 7) << 2 | (joy & 0x10) << 1
      val serial = (~(bus.clk | bus.data << 1/* | bus.atn << 7*/)) & 0x3
      serial | joy012Fire | (if (datassette.isPlayPressed) 0 else 0x40)
    case _ =>
      super.read(address,chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID): Unit = address & 0x0F match {
    case PA|PA2 =>
      super.write(address, value, chipID)
      bus.setLine(this,IECBusLine.ATN,if ((value & 0x80) > 0) GROUND else VOLTAGE)
    case _ =>
      super.write(address, value, chipID)
  }

  def restoreKeyPressed(pressed:Boolean): Unit = CA1In(pressed)

  override def CA2Out(state: Boolean): Unit = datassette.setMotor(!state)
}
