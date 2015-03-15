package ucesoft.c64.peripheral.rs232

import ucesoft.c64.peripheral.cia.CIA
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

trait RS232 extends C64Component {
  val componentType = C64ComponentType.USER_PORT
  
  final val RXD = 1 << 0
  final val RTS = 1 << 1
  final val DTR = 1 << 2
  final val RI  = 1 << 3
  final val DCD = 1 << 4
  final val CTS = 1 << 6
  final val DSR = 1 << 7
  
  final val NO_PARITY = 0
  final val ODD_PARITY = 1
  final val EVEN_PARITY = 2
  final val MARK_PARITY = 3
  final val SPACE_PARITY = 4
  
  def setTXD(high:Int)
  def getTXD : Int
  
  /**
   * Bit 1: RTS
   * Bit 2: DTR
   * Bit 3: RI
   * Bit 4: DCD
   * Bit 5: User port pin J
   */
  def setOthers(value:Int)
  /**
   * Bit 0: RXD
   * Bit 3: RI
   * Bit 4: DCD
   * Bit 5: User port pin J
   * Bit 6: CTS
   * Bit 7: DSR
   */
  def getOthers : Int
  
  def setConfiguration(conf:String)
  def getConfiguration : String
  
  def isEnabled : Boolean
  def setEnabled(enabled:Boolean)
  
  def setCIA(cia2:CIA)
  
  def getDescription : String
}