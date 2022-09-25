package ucesoft.cbm.expansion

import ucesoft.cbm.cpu.RAMComponent

import java.io.{ObjectInputStream, ObjectOutputStream}

class VIC20GeoRAM(size:Int,
                  override val irqHandler: Boolean => Unit,
                  override val nmiHandler: Boolean => Unit,
                  override val mmu:RAMComponent) extends VIC20ExpansionPort(irqHandler,nmiHandler, mmu) {

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

  override def write(address: Int, value: Int): Unit = {
    if (address == 0x9CFF) {
      block = value & blockMask
      rampage = ram(block)(page)
    }
    else if (address == 0x9CFE) {
      page = value & 0x3F
      rampage = ram(block)(page)
    }
    else if ((address & 0xFF00) == 0x9800) rampage(address & 0xFF) = value
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    out.writeInt(size)
    super.saveState(out)
    out.writeObject(ram)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // the size is read by the expansion port handler
    super.loadState(in)
    loadMemory[Array[Array[Int]]](ram, in)
  }
}
