package ucesoft.cbm.cpu.wd65816

final class ProgramCounter {
  private var _A : Int = 0
  def A : Int = _A
  def A_=(a:Int) : Unit = _A = a & 0xFFFFFF
  def wrap(v:Int) : Int = v & 0xFFFFFF

  class B {
    def wrap(v:Int) : Int = v & 0xFF
    def L : Int = _A & 0xFF
    def H : Int = (_A >> 8) & 0xFF
    def PB : Int = (_A >> 16) & 0xFF
    def L_=(l:Int) : Unit = _A = (_A & 0xFFFF00) | (l & 0xFF)
    def H_=(h:Int) : Unit = _A = (_A & 0xFF00FF) | (h & 0xFF) << 8
    def PB_=(b:Int) : Unit = _A = (_A & 0x00FFFF) | (b & 0xFF) << 16
    override def toString : String = s"(${PB.toHexString},${H.toHexString},${L.toHexString})_pc"
  }

  val B = new B

  class W  {
    def wrap(v:Int) : Int = v & 0xFFFF
    def PC : Int = _A & 0xFFFF
    def PC_=(pc:Int) : Unit = _A = (_A & 0xFF0000) | (pc & 0xFFFF)
    def PC_++ : Unit = _A = (_A & 0xFF0000) | ((_A & 0xFFFF) + 1) & 0xFFFF
    def PC_-- : Unit = _A = (_A & 0xFF0000) | ((_A & 0xFFFF) - 1) & 0xFFFF
    def Z : Int = (_A >> 16) & 0xFF
    def Z_=(z:Int) : Unit = _A = (_A & 0xFFFF) | (z & 0xFF) << 16
    override def toString : String = s"(${Z.toHexString},${PC.toHexString})_pc"
  }

  val W = new W

  override def toString : String = s"${_A.toHexString}_pc"
}
