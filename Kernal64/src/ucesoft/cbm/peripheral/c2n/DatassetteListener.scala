package ucesoft.cbm.peripheral.c2n

object DatassetteState extends Enumeration {
  val PLAYING = Value
  val STOPPED = Value
  val RECORDING = Value
  val FORWARD = Value
  val REWIND = Value
}

trait DatassetteListener {
  def datassetteStateChanged(newState:DatassetteState.Value) : Unit
  def datassetteUpdatePosition(perc:Int,counter:Int) : Unit
  def datassetteUpdateCounter(counter:Int) : Unit
}