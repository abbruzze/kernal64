package ucesoft.c64.peripheral.bus

trait BusDataIterator extends Iterator[Int] {
  def isLast: Boolean
  def getPerc : Int
  def goto(pos:Int)
}