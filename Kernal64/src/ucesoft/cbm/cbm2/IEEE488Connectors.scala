package ucesoft.cbm.cbm2

import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusListener, IEEE488Bus}
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.mos6525.MOS6525

object IEEE488Connectors {
  // CIA
  class CIAIEEE488ConnectorA(bus:IEEE488Bus) extends Connector {
    override val componentID: String = "CIA1-PortA"
    private var directionOut = true

    def setDirection(out:Boolean): Unit = directionOut = out

    override def read: Int = {
      //println(s"READ IEEE DIO = ${bus.getDIO()}")
      bus.getDIO() ^ 0xFF
    }
    override protected def performWrite(data: Int): Unit = {
      val ieeeData = if (directionOut) data else 0xFF
      //println(s"IEEE488 DIO = $ieeeData (${ieeeData ^ 0xFF})")
      bus.setDIO(ieeeData)
    }
  }

  class CIAIEEE488ConnectorB extends Connector {
    override val componentID: String = "CIA1-PortB"

    override def read: Int = 0
    override protected def performWrite(data: Int): Unit = {
      println(s"CIA write on port B = $data")
    }
  }

  /**
   * 6525
   *
   * PA0 - DC (not used)
   * PA1 - TE (direction)
   * PA2 - REN
   * PA3 - ATN
   * PA4 - DAV
   * PA5 - EOI
   * PA6 - NDAC
   * PA7 - NRFD
   */
  class IEEE488InterfaceA(bus:IEEE488Bus,ciaConnector:CIAIEEE488ConnectorA) extends IEEE488Bus.LineListener with MOS6525.PortAB {
    import IEEE488Bus.LineType._
    import IEEE488Bus.LineValue._
    override val isController: Boolean = true
    private var mos6525 : MOS6525 = _
    // INIT
    bus.registerListener(this)

    override def setMOS6525(mos6525: MOS6525): Unit = this.mos6525 = mos6525

    override def read(): Int = {
      var byte = 0x07
      if (bus.getLine(ATN) == RELEASED) byte |= 0x08
      if (bus.getLine(DAV) == RELEASED) byte |= 0x10
      if (bus.getLine(EOI) == RELEASED) byte |= 0x20
      if (bus.getLine(NDAC) == RELEASED) byte |= 0x40
      if (bus.getLine(NRFD) == RELEASED) byte |= 0x80

      //println(s"IEEE read ATN = ${bus.getLine(ATN)} DAV = ${bus.getLine(DAV)} EOI = ${bus.getLine(EOI)} NDAC = ${bus.getLine(NDAC)} NRFD = ${bus.getLine(NRFD)}")

      import MOS6525._
      byte = (byte & ~mos6525.regs(DDRA)) | (mos6525.regs(PRA) & mos6525.regs(DDRA))

      byte
    }
    override def write(value: Int): Unit = {
      val data = value ^ 0xFF
      val directionOut = (value & 0x2) > 0
      ciaConnector.setDirection(directionOut)
      if (directionOut) {
        bus.releaseLine(this,NRFD)
        bus.releaseLine(this,NDAC)
        if ((data & 0x8) > 0) bus.pullLine(this,ATN) else bus.releaseLine(this,ATN)
        if ((data & 0x10) > 0) bus.pullLine(this,DAV) else bus.releaseLine(this,DAV)
        if ((data & 0x20) > 0) bus.pullLine(this,EOI) else bus.releaseLine(this,EOI)
        //println(s"IEEE write out ATN = ${data & 8} DAV = ${data & 0x10} EOI = ${data & 0x20}")
      }
      else {
        if ((data & 0x80) > 0) bus.pullLine(this,NRFD) else bus.releaseLine(this,NRFD)
        if ((data & 0x40) > 0) bus.pullLine(this,NDAC) else bus.releaseLine(this,NDAC)
        bus.releaseLine(this,ATN)
        bus.releaseLine(this,DAV)
        bus.releaseLine(this,EOI)
        //println(s"IEEE set input direction NRFD = ${data & 0x80} NDAC = ${data & 0x40}")
      }
    }
  }

  class IEEE488InterfaceB(bus:IEEE488Bus,datassette: Datassette) extends IEEE488Bus.LineListener with MOS6525.PortAB {
    override def read(): Int = {
      // TODO return also PB0, PB1 from bus (IFC, SRQ)
      val byte = 0x7F | (if (datassette.isPlayPressed) 0x80 else 0x00)
      //byte = (byte & ~tpiIeee.regs(MOS6525.DDRB)) | (tpiIeee.regs(MOS6525.PRB) & tpiIeee.regs(MOS6525.DDRB))
      //println(s"6525 IEEE B# read ${byte.toHexString} DDRB=${tpiIeee.regs(MOS6525.DDRB)}")
      byte
    }
    override def write(value: Int): Unit = {
      val datassetteMotor = (value & 0x40) == 0
      val datassetteWrite = (value & 0x20) == 0
      datassette.setMotor(datassetteMotor)
      datassette.setWriteLine(datassetteWrite)
    }
  }

  /**
    1 GND -----------+     1 SRQ
    2 +5V            |---  2 GND
    3 MTR -----------|---  3 ATN       =====> PB6
    4 RD ------|           4 CLK ----+ =====> PB7
    5 WR ------+---------  5 DATA    | =====> PB5
    6 SNS                  6 RESET   |
       +------------------------------
   */
  class IECInterfaceB(iec:IECBus,flagLow: () => Unit) extends IECBusListener with MOS6525.PortAB {
    import IECBus._
    override val isController = true
    override val busid: String = "IECBUS-Adapter"

    iec.registerListener(this)

    override def read(): Int = /*~*/(iec.clk << 7 | iec.data << 5 | iec.atn << 6) & 0xFF
    override def write(value: Int): Unit = {
      val atn = if ((value & 0x40) > 0) GROUND else VOLTAGE
      val data = if ((value & 0x20) == 0) GROUND else VOLTAGE
      val clock = if ((value & 0x80) == 0) GROUND else VOLTAGE
      println(s"WRITE IEC atn=$atn data=$data clock=$clock")
      iec.setLine(this,atn,data,clock)
      flagLow()
    }
  }
}
