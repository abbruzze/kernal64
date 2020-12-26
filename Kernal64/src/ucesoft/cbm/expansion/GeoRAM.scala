package ucesoft.cbm.expansion

import java.io.{ObjectInputStream, ObjectOutputStream}

import ucesoft.cbm.ChipID

class GeoRAM(size:Int) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.GEORAM
  override val name = "GeoRAM"
  override val componentID = "GeoRAM"

  val EXROM = true
  val GAME = true
  val ROML = null
  val ROMH = null

  final private[this] val ram = Array.ofDim[Int](if (size == 256) 16 else 32,64,256)
  final private[this] val dfffMask = if (size == 256) 0x0F else 0x1F
  private[this] var rampage : Array[Int] = ram(0)(0)
  private[this] var dfff,dffe = 0

  final override def init: Unit = {
    reset
  }
  final override def reset: Unit = {
    dfff = 0
    dffe = 0
    rampage = ram(0)(0)
  }
  override def hardReset : Unit = {
    reset
    for(i <- 0 until ram.length;j <- 0 until ram(i).length) {
      java.util.Arrays.fill(ram(i)(j),0)
    }
  }

  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if ((address & 0xFF00) == 0xDE00) rampage(address & 0xFF) else super.read(address,chipID)
  }

  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    if (address == 0xDFFF) {
      dfff = value & dfffMask
      rampage = ram(dfff)(dffe)
    }
    else
    if (address == 0xDFFE) {
      dffe = value & 0x3F
      rampage = ram(dfff)(dffe)
    }
    else
    if ((address & 0xFF00) == 0xDE00) rampage(address & 0xFF) = value
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    out.writeInt(size)
    super.saveState(out)
    out.writeObject(ram)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // the size is read by the expansion port handler
    super.loadState(in)
    loadMemory[Array[Array[Int]]](ram,in)
  }
}
