package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID

class GeoRAM(size:Int) extends ExpansionPort {
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
}
