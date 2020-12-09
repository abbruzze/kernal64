package ucesoft.cbm.peripheral.rs232

import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.JOptionPane

object RS232 {
  final val RXD = 1 << 0
  final val RTS = 1 << 1
  final val DTR = 1 << 2
  final val RI  = 1 << 3
  final val DCD = 1 << 4
  final val CTS = 1 << 6
  final val DSR = 1 << 7
  final val TXD = 1 << 8
  
  final val NO_PARITY = 0
  final val ODD_PARITY = 1
  final val EVEN_PARITY = 2
  final val MARK_PARITY = 3
  final val SPACE_PARITY = 4
}

trait RS232 extends CBMComponent {
  val componentType = CBMComponentType.USER_PORT
  private[this] var flowControl = false

  def flowControlEnabled = flowControl
  def setFlowControlEnabled(enabled:Boolean) = flowControl = enabled
  
  def setTXD(high:Int) : Unit
  def getTXD : Int
  
  /**
   * Bit 1: RTS
   * Bit 2: DTR
   * Bit 3: RI
   * Bit 4: DCD
   * Bit 5: User port pin J
   */
  def setOthers(value:Int) : Unit
  /**
   * Bit 0: RXD
   * Bit 3: RI
   * Bit 4: DCD
   * Bit 5: User port pin J
   * Bit 6: CTS
   * Bit 7: DSR
   */
  def getOthers : Int
  
  def setConfiguration(conf:String) : Unit
  def getConfiguration : String
  def connectionInfo:String
  
  def isEnabled : Boolean
  def setEnabled(enabled:Boolean) : Unit
  
  def setCIA12(cia1:CIA,cia2:CIA) : Unit
  
  def getDescription : String
  
  def setRS232Listener(l:RS232StatusListener) : Unit
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = {
    if (isEnabled) showError("State warning","Warning: an RS-232 device is enabled.")
    true
  }
}