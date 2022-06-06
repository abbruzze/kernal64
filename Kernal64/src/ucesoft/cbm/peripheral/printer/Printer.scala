package ucesoft.cbm.peripheral.printer

import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.io.{ObjectInputStream, ObjectOutputStream}

trait Printer extends CBMComponent {
  val componentID = "Commodore MPS803"
  val componentType = CBMComponentType.PRINTER

  def clock(cycles: Long) : Unit
  def setActive(active:Boolean): Unit

  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
}
