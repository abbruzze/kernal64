package ucesoft.cbm.expansion.vic20

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.misc.Preferences
import ucesoft.cbm.peripheral.bus.{IECBus, IEEE488Bus}
import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.io.{ObjectInputStream, ObjectOutputStream}

object VIC20ExpansionPort {
  object VICExpansionPortType extends Enumeration {
    val GEORAM = Value
    val ULTIMEM = Value
    val IEEE488 = Value
    val FE3 = Value
  }

  case class Signals(pref:Preferences,
                     irqHandler: Boolean => Unit,
                     nmiHandler: Boolean => Unit,
                     resetHandler: () => Unit,
                     iecBus:IECBus,
                     ieee488Bus:IEEE488Bus,
                     mmu:RAMComponent
                    )

  trait VIC20ExpansionPortStateHandler {
    def load(in:ObjectInputStream,
             signals:Signals): VIC20ExpansionPort
    def save(cart:VIC20ExpansionPort,out:ObjectOutputStream): Unit
  }
}

abstract class VIC20ExpansionPort(val signals:VIC20ExpansionPort.Signals) extends CBMComponent {
  import VIC20ExpansionPort._

  override val componentID: String = "VIC20ExpansionPort"
  override val componentType: Type = CBMComponentType.MEMORY
  val portType : VICExpansionPortType.Value
  val needsClock = false

  override def reset(): Unit = {}

  override def init(): Unit = {}

  def eject(): Unit = {}

  def clock(cycles:Long): Unit = {}

  def read(address:Int): Option[Int] = None
  def write(address:Int,value:Int): Boolean = false

  override protected def saveState(out: ObjectOutputStream): Unit = {}

  override protected def loadState(in: ObjectInputStream): Unit = {}

  override protected def allowsStateRestoring: Boolean = true
}
