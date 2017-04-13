package ucesoft.cbm.peripheral.c2n

object DatassetteState extends Enumeration {
  val PLAYING = Value
  val STOPPED = Value
  val RECORDING = Value
}

trait DatassetteListener {
  def datassetteStateChanged(newState:DatassetteState.Value)
  def datassetteUpdatePosition(perc:Int)
}