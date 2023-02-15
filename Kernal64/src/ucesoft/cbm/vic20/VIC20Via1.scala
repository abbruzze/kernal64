package ucesoft.cbm.vic20

import ucesoft.cbm.ChipID
import ucesoft.cbm.expansion.WiC64
import ucesoft.cbm.peripheral.bus.IECBus.{GROUND, VOLTAGE}
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.controlport.ControlPort
import ucesoft.cbm.peripheral.drive.VIA
import ucesoft.cbm.peripheral.rs232.RS232

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
                userPort:ControlPort,
                rs232: RS232,
                datassette:Datassette,
                nmiAction:Boolean => Unit,
                lightPenTriggerHandler: () => Unit) extends VIA("VIA_1",0x9110,nmiAction) with IECBusListener {
  override val busid = "VIA_I_buslistener"
  override lazy val componentID = "VIA1 (9110)"
  override val isController = true

  bus.registerListener(this)
  rs232.setBitReceivedListener( () => {
    CB1In(false)
    CB1In(true)
  })

  override def read(address: Int, chipID: ChipID.ID): Int = address & 0x0F match {
    case PA|PA2 =>
      super.read(address,chipID)
      val joy = controlPort.readPort
      val joy012Fire = (joy & 7) << 2 | (joy & 0x10) << 1
      val serial = (~(bus.clk | bus.data << 1)) & 0x3
      val byte = 0x80 | serial | joy012Fire | (if (datassette.isPlayPressed) 0 else 0x40) // 0x80 = atn in
      byte & (regs(PA) | ~regs(DDRA))
    case PB =>
      /*
        Second joystick on User Port
        PB1 = joy 3 (right = 8)
        PB2 = joy 0 (up = 1)
        PB3 = joy 1 (down = 2)
        PB4 = joy 2 (left = 4)
        PB5 = fire ( = 16)
       */
      super.read(address, chipID)
      if (WiC64.enabled) return WiC64.read()

      val joy = userPort.readPort
      val userJoy = (joy & 7) << 2 | (joy & 8) >> 2 | (joy & 16) << 1 | 0xC1
      val user = userJoy & (if (rs232.isEnabled) rs232.getOthers else 0xFF)
      user & (regs(PB) | ~regs(DDRB))
    case _ =>
      super.read(address,chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID): Unit = address & 0x0F match {
    case adr@(PA|PA2) =>
      val oldValue = regs(adr)
      super.write(address, value, chipID)
      bus.setLine(this,IECBusLine.ATN,if ((value & 0x80) > 0) GROUND else VOLTAGE)
      if ((regs(DDRA) & 0x20) > 0 && (oldValue & 0x20) > 0 && (value & 0x20) == 0) lightPenTriggerHandler() // raising edge
    case PB =>
      super.write(address, value, chipID)
      if (WiC64.enabled) WiC64.write(value)
    case _ =>
      super.write(address, value, chipID)
  }

  def restoreKeyPressed(pressed:Boolean): Unit = CA1In(pressed)

  override def CA2Out(state: Boolean): Unit = datassette.setMotor(!state)
  override def CB2Out(state: Boolean): Unit = {
    rs232.setTXD(if (state) 1 else 0)
    if (WiC64.enabled) WiC64.setMode(if (state) 4 else 0)
  }
}
