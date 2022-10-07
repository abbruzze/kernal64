package ucesoft.cbm.peripheral.printer

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.io.{ObjectInputStream, ObjectOutputStream}

trait Printer extends CBMComponent {
  val componentID = "Commodore MPS803"
  val componentType: Type = CBMComponentType.PRINTER

  def clock(cycles: Long) : Unit
  def setActive(active:Boolean): Unit
}
