package ucesoft.cbm.misc

object MemoryInitPattern {
  def initRAM(mem:Array[Int],
              _bytes:(Int,Int) = (0x00,0xFF),
              invertFirstByteEvery:Int = 4,
              invertSecondByteEvery:Int = 16384,
              _firstByteOffset:Int = 2): Unit = {
    val bytes = Array(_bytes._1,_bytes._2)
    var byteIndex = 0
    val firstByteOffset = _firstByteOffset % invertFirstByteEvery
    var firstByteCount = firstByteOffset
    var invValue = 0x00
    for (i <- mem.indices) {
      if (i > 0 && (i % invertSecondByteEvery) == 0) byteIndex ^= 1
      mem(i) = bytes(byteIndex) ^ invValue
      firstByteCount -= 1
      if (firstByteCount == 0) {
        firstByteCount = invertFirstByteEvery
        invValue = ~invValue & 0xFF
      }
    }
  }
}
