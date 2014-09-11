package ucesoft.c64.peripheral.printer

import ucesoft.c64.cpu.Memory
import ucesoft.c64.ChipID

class MPS803ROM extends Memory {
  val name = "MPS803ROM"
  val isRom = true
  lazy val length = 7 * 512
  lazy val startAddress = 0

  private val rom = {
    val mem = Array.ofDim[Int](length)
    val in = getClass.getClassLoader.getResourceAsStream("roms/mps803.rom")
    val buffer = Array.ofDim[Byte](length)
    var read = in.read(buffer)
    var offset = 0
    while (read > 0) {
      offset += read
      read = in.read(buffer, offset, length - offset)
    }
    in.close
    for (i <- 0 until length) mem(i) = buffer(i) & 0xff
    mem
  }

  def init {}
  val isActive = true
  def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = rom(address - startAddress)
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
}