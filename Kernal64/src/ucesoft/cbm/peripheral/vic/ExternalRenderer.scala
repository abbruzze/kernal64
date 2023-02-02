package ucesoft.cbm.peripheral.vic

trait ExternalRenderer {
  def renderCycle(): Unit
  def stop(): Unit
}

