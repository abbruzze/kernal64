package ucesoft.cbm.peripheral.controlport
import java.awt.event.KeyEvent

class ControlPortBridge(var controlPort:ControlPort,override val componentID : String) extends ControlPort {
  override def readPort: Int = controlPort.readPort
  protected def read = 0
  
  override def emulateFire: Unit = controlPort.emulateFire
  override def emulateUp: Unit = controlPort.emulateUp
  override def emulateDown: Unit = controlPort.emulateDown
  override def emulateLeft: Unit = controlPort.emulateLeft
  override def emulateRight: Unit = controlPort.emulateRight
  override def releaseEmulated: Unit = controlPort.releaseEmulated

  override def setLightPenEmulation(enabled:Boolean): Unit = controlPort.setLightPenEmulation(enabled)
  override def isLightPenEmulationEnabled: Boolean = controlPort.isLightPenEmulationEnabled
  override def setMouse1351Emulation(enabled:Boolean): Unit = controlPort.setMouse1351Emulation(enabled)
  override def isMouse1351EmulationEnabled: Boolean = controlPort.isMouse1351EmulationEnabled

  override def consumeKey(e: KeyEvent): Boolean = controlPort.consumeKey(e)
}