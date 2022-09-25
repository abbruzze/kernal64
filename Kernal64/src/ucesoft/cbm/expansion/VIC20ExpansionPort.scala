package ucesoft.cbm.expansion

import ucesoft.cbm.{CBMComponent, CBMComponentType}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.RAMComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

abstract class VIC20ExpansionPort(val irqHandler: Boolean => Unit,
                                  val nmiHandler: Boolean => Unit,
                                  val mmu:RAMComponent) extends CBMComponent {
  override val componentID: String = "VIC20ExpansionPort"
  override val componentType: Type = CBMComponentType.MEMORY

  override def reset(): Unit = {}

  override def init(): Unit = {}

  def read(address:Int): Option[Int] = None
  def write(address:Int,value:Int): Unit = {}

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = true
}
