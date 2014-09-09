package ucesoft.c64.peripheral.printer

import ucesoft.c64.peripheral.bus.IECBusDevice
import ucesoft.c64.peripheral.bus.IECBus
import ucesoft.c64.cpu.Memory
import ucesoft.c64.util.CBMCanvas
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import scala.collection.mutable.ListBuffer

class MPS803(bus:IECBus,charRom: Memory,device:Int = 4) extends IECBusDevice(bus,device) with C64Component {
  val componentID = "Commodore MPS803"
  val componentType = C64ComponentType.PRINTER 
  
  val busid = "MPS803"
  private[this] val COLUMNS = 80
  private[this] var mode = Mode.GRAPHICS
  private[this] var charEnhanced = false
  private[this] var charRvsOn = false
  private[this] val line = new ListBuffer[Int]
  
  val sheet = new CBMCanvas(charRom)
  private[this] val SPECIAL_CHAR_ROUTINES = Map(
      14  -> enhanceOn _,
      15  -> enhanceOff _,
      18  -> rvsOn _,
      146 -> rvsOff _,
      13  -> carrigeReturn _,
      10  -> lineFeed _,
      17  -> businessMode _,
      145 -> graphicsMode _,
      34  -> quote _,
      16  -> tab _,
      26  -> repeat _,
      27  -> dotAddress _
  )
  
  private object Mode extends Enumeration {
    val GRAPHICS = Value(0)
    val BUSINESS = Value(7)
  }
  
  // register itself to bus
  bus.registerListener(this)
  
  protected def isDeviceReady = true
  protected def loadData(fileName:String) = None
  
  def init {
    sheet.clear
    sheet.bgColor(1)  // white
    sheet.black
    sheet.rep(' ',COLUMNS).newLine
  }
  
  override def reset {
    super.reset
  }
  
  // ---------------- SPECIAL CHARS HANDLING FUNCTIONS --------------
  private def enhanceOn = charEnhanced = true
  private def enhanceOff = charEnhanced = false
  private def rvsOn = charRvsOn = true
  private def rvsOff = charRvsOn = false
  private def carrigeReturn {
    for(c <- line) sheet << c
    sheet.newLine
    line.clear
    sheet.checkSize
  }
  private def lineFeed {
    // TODO
  }
  private def businessMode = mode = Mode.BUSINESS 
  private def graphicsMode = mode = Mode.GRAPHICS 
  private def quote {
    // TODO
  }
  private def tab {
    // TODO
  }
  private def repeat {
    // TODO
  }
  private def dotAddress {
    // TODO
  }
  // ----------------------------------------------------------------
  
  override def open_channel {
    mode = channel match {
      case 7 => Mode.BUSINESS 
      case _ => Mode.GRAPHICS 
    }
  }
  
  override def close {
    println("PRINTER CLOSE")
  }
  
  override protected def byteJustRead(byte:Int,isLast:Boolean) {
    println("MPS803: byte read " + Integer.toHexString(byte) + " " + byte.toChar)
    SPECIAL_CHAR_ROUTINES get byte match {
      case None =>
        line += byte
        if (line.size == COLUMNS) {          
          carrigeReturn
        }
      case Some(routine) => routine()
    }
  }
}