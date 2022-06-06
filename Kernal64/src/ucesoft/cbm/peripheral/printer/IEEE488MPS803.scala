package ucesoft.cbm.peripheral.printer

import ucesoft.cbm.peripheral.bus.{IEEE488Bus, IEEE488BusCommand}

class IEEE488MPS803(override val name:String,override val deviceID:Int,bus: IEEE488Bus,driver:PrinterDriver) extends IEEE488BusCommand(name,deviceID, bus) with Printer {
  private var active = false

  override val componentID: String = name

  override def isThisDevice(device: Int): Boolean = super.isThisDevice(device) && active

  override protected def openChannel(): Unit = {
    if (secondaryAddress == 7) driver.print(17) else driver.print(145)
  }

  override protected def receiveData(data:Int): Unit = {
    lastCommand match {
      case OPEN(_) =>
       // no named channels
      case _ =>
        if (checkData(data)) {
          println(s"PRINTING $data '${data.toChar}''")
          driver.print(data)
        }
    }
  }

  override def clock(cycles: Long): Unit = {}

  override def setActive(active: Boolean): Unit = this.active = active
}
