package ucesoft.cbm.expansion

import ucesoft.cbm.{CBMComponent, CBMComponentType}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.misc.Preferences

import java.io.{ObjectInputStream, ObjectOutputStream}

object VIC20ExpansionPort {
  object VICExpansionPortType extends Enumeration {
    val GEORAM = Value
    val ULTIMEM = Value
  }

  trait VIC20ExpansionPortStateHandler {
    def load(in:ObjectInputStream,
             pref:Preferences,
             irqHandler: Boolean => Unit,
             nmiHandler: Boolean => Unit,
             mmu:RAMComponent,
             resetHandler: () => Unit): VIC20ExpansionPort
    def save(cart:VIC20ExpansionPort,out:ObjectOutputStream): Unit
  }
}

abstract class VIC20ExpansionPort(val irqHandler: Boolean => Unit,
                                  val nmiHandler: Boolean => Unit,
                                  val mmu:RAMComponent,
                                  val resetHandler: () => Unit) extends CBMComponent {
  import VIC20ExpansionPort._

  override val componentID: String = "VIC20ExpansionPort"
  override val componentType: Type = CBMComponentType.MEMORY
  val portType : VICExpansionPortType.Value

  override def reset(): Unit = {}

  override def init(): Unit = {}

  def eject(): Unit = {}

  def read(address:Int): Option[Int] = None
  def write(address:Int,value:Int): Boolean = false

  override protected def saveState(out: ObjectOutputStream): Unit = {}

  override protected def loadState(in: ObjectInputStream): Unit = {}

  override protected def allowsStateRestoring: Boolean = true
}
