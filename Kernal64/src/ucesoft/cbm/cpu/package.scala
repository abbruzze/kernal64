package ucesoft.cbm

package object cpu {
  @inline def hi(data: Int) = (data >> 8) & 0xFF
  @inline def lo(data: Int) = data & 0xFF

  @inline def lohi(data: Int) = (lo(data), hi(data))

  def hex2(data: Int) = "%02X".format(data & 0xffff)
  def hex4(data: Int) = "%04X".format(data & 0xffff)

  implicit class IntToBase(val digits: String) extends AnyVal {
    def base(b: Int) = Integer.parseInt(digits, b)
    def b = base(2)
    def o = base(8)
    def x = base(16)
  }
  
  @inline def readWordFrom(address:Int,mem:Memory) = {
    val lo = mem.read(address)
    val hi = mem.read(address + 1)
    hi << 8 | lo
  }
  
  @inline def readWordFromWithBUG(address:Int,mem:Memory) = {
    val lo = mem.read(address)
    val hi = mem.read(if ((address & 0xff) == 0xff) (address & 0xff00) else address + 1)
    hi << 8 | lo
  }
  
  @inline def isBit(byte:Int,bit:Int) = {
    val toAnd = 1 << bit
    (byte & toAnd) == toAnd
  }
}