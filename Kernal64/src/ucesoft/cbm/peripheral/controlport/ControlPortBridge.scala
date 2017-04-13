package ucesoft.cbm.peripheral.controlport

class ControlPortBridge(var controlPort:ControlPort,override val componentID : String) extends ControlPort {
  override def readPort = controlPort.readPort
  protected def read = 0
  
  override def emulateFire = controlPort.emulateFire
  override def emulateUp = controlPort.emulateUp
  override def emulateDown = controlPort.emulateDown
  override def emulateLeft = controlPort.emulateLeft
  override def emulateRight = controlPort.emulateRight
  override def releaseEmulated = controlPort.releaseEmulated
}