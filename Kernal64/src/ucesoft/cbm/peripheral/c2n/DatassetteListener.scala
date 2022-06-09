package ucesoft.cbm.peripheral.c2n

object DatassetteState extends Enumeration {
  val PLAYING: DatassetteState.Value = Value
  val STOPPED: DatassetteState.Value = Value
  val RECORDING: DatassetteState.Value = Value
  val FORWARD: DatassetteState.Value = Value
  val REWIND: DatassetteState.Value = Value
}

trait DatassetteListener {
  def datassetteStateChanged(newState:DatassetteState.Value) : Unit
  def datassetteUpdatePosition(perc:Int,counter:Int) : Unit
  def datassetteUpdateCounter(counter:Int) : Unit
}