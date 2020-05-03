package ucesoft.cbm.cpu.wd65816

final class Address {
  private var _A : Int = 0
  def A : Int = _A
  def A_=(a:Int) : Unit = _A = a & 0xFFFFFF
  def wrap(v:Int) : Int = v & 0xFFFFFF

  class B {
    def wrap(v:Int) : Int = v & 0xFF
    def L : Int = _A & 0xFF
    def H : Int = (_A >> 8) & 0xFF
    def B : Int = (_A >> 16) & 0xFF
    def L_=(l:Int) : Unit = _A = (_A & 0xFFFF00) | (l & 0xFF)
    def H_=(h:Int) : Unit = _A = (_A & 0xFF00FF) | (h & 0xFF) << 8
    def B_=(b:Int) : Unit = _A = (_A & 0x00FFFF) | (b & 0xFF) << 16
    override def toString : String = s"(${B.toHexString},${H.toHexString},${L.toHexString})_a"
  }

  val B = new B

  class W {
    def wrap(v:Int) : Int = v & 0xFFFF
    def L : Int = _A & 0xFFFF
    def L_=(l:Int) : Unit = _A = (_A & 0xFF0000) | (l & 0xFFFF)
    def H : Int = (_A >> 16) & 0xFF
    def H_=(h:Int) : Unit = _A = (_A & 0xFFFF) | (h & 0xFF) << 16
    override def toString : String = s"(${H.toHexString},${L.toHexString})_a"
  }

  val W = new W

  override def toString : String = s"${_A.toHexString}_a"
}
