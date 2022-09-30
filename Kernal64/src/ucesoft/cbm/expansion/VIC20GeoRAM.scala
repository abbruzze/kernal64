package ucesoft.cbm.expansion

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.misc.Preferences
import ucesoft.cbm.vic20.VIC20MMU

import java.io.{ObjectInputStream, ObjectOutputStream}

object VIC20GeoRAM extends VIC20ExpansionPort.VIC20ExpansionPortStateHandler {
  override def load(in: ObjectInputStream,
                    pref: Preferences,
                    irqHandler: Boolean => Unit,
                    nmiHandler: Boolean => Unit,
                    mmu: RAMComponent,
                    resetHandler: () => Unit): VIC20ExpansionPort = {
    import ucesoft.cbm.misc.Preferences._
    val size = in.readInt()
    pref.update(PREF_GEORAM,size.toString)
    mmu.asInstanceOf[VIC20MMU].getAttachedSpecialCart().get.asInstanceOf[VIC20GeoRAM]
  }

  override def save(cart: VIC20ExpansionPort, out: ObjectOutputStream): Unit = {
    out.writeInt(cart.asInstanceOf[VIC20GeoRAM].size)
    cart.save(out)
  }
}

class VIC20GeoRAM(val size:Int,
                  override val irqHandler: Boolean => Unit,
                  override val nmiHandler: Boolean => Unit,
                  override val mmu:RAMComponent,
                  override val resetHandler: () => Unit) extends VIC20ExpansionPort(irqHandler,nmiHandler, mmu,resetHandler) {

  override val portType = VIC20ExpansionPort.VICExpansionPortType.GEORAM
  override val componentID: String = "VIC20GeoRAM"
  final private val ram = Array.ofDim[Int](size / 16, 64, 256)
  final private val blockMask = size / 16 - 1
  private var rampage: Array[Int] = ram(0)(0)
  private var block, page = 0

  final override def reset: Unit = {
    block = 0
    page = 0
    rampage = ram(0)(0)
  }

  override def hardReset: Unit = {
    reset
    for (i <- 0 until ram.length; j <- 0 until ram(i).length) {
      java.util.Arrays.fill(ram(i)(j), 0)
    }
  }

  override def read(address: Int): Option[Int] = {
    if ((address & 0xFF00) == 0x9800) Some(rampage(address & 0xFF)) else None
  }

  override def write(address: Int, value: Int): Boolean = {
    if (address == 0x9CFF || address == 0x9FFF) {
      block = value & blockMask
      rampage = ram(block)(page)
      true
    }
    else if (address == 0x9CFE || address == 0x9FFE) {
      page = value & 0x3F
      rampage = ram(block)(page)
      true
    }
    else if ((address & 0xFF00) == 0x9800) {
      rampage(address & 0xFF) = value
      true
    }
    else false
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    // size is saved by the handler
    out.writeObject(ram)
    out.writeInt(block)
    out.writeInt(page)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // size is loaded by the handler
    loadMemory[Array[Array[Int]]](ram, in)
    block = in.readInt()
    page = in.readInt()
    rampage = ram(block)(page)
  }
}
