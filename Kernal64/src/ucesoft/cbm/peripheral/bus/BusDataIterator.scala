package ucesoft.cbm.peripheral.bus

object BusDataIterator {
  abstract class DataIterator extends BusDataIterator {
    protected var index = 0
    protected val dataLength : Int
    override def hasNext = index < dataLength
    override def isLast = index == dataLength - 1
    override def getPerc = (100 * index.toFloat / dataLength).toInt
    override def goto(pos: Int) = index = pos
  }
  
  class StringDataIterator(data: String) extends DataIterator {
    protected val dataLength = data.length
    override def next = {
      val c = data.charAt(index).toInt
      index += 1
      c
    }
  }
  
  class ArrayDataIterator(data: Array[Byte], sizeLimit: Option[Int] = None) extends DataIterator {
    protected val dataLength = data.length
    override def next = {
      val c = data(index).toInt
      sizeLimit match {
        case None => index += 1
        case Some(limit) => index = (index + 1) % limit
      }
      c
    }
  }  
  
  class ArrayIntDataIterator(data: Array[Int]) extends DataIterator {
    protected val dataLength = data.length
    override def next = {
      val c = data(index)
      index += 1
      c
    }
  }
}

trait BusDataIterator extends Iterator[Int] {
  def isLast: Boolean
  def getPerc : Int
  def goto(pos:Int)
}