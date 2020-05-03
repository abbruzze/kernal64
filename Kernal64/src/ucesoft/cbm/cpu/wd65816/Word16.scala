package ucesoft.cbm.cpu.wd65816

final class Word16 {
  private var _W : Int = 0
  def W : Int = _W
  def W_=(w:Int) : Unit = _W = w & 0xFFFF
  def W_++ : Unit = _W = (_W + 1) & 0xFFFF
  def W_-- : Unit = _W = (_W - 1) & 0xFFFF
  def wrap(v:Int) : Int = v & 0xFFFF

  class B {
    def wrap(v:Int) : Int = v & 0xFF
    def L : Int = _W & 0xFF
    def H : Int = (_W >> 8) & 0xFF
    def L_=(l:Int) : Unit = _W = (_W & 0xFF00) | (l & 0xFF)
    def H_=(h:Int) : Unit = _W = (_W & 0xFF) | (h & 0xFF) << 8
    override def toString : String = s"(${H.toHexString},${L.toHexString})_w"
  }

  val B = new B

  override def toString : String = s"${_W.toHexString}_w"
}
