package ucesoft.c64.peripheral.printer

import ucesoft.c64.peripheral.bus.IECBusDevice
import ucesoft.c64.peripheral.bus.IECBus
import ucesoft.c64.cpu.Memory
import ucesoft.c64.util.CBMCanvas
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import scala.collection.mutable.ListBuffer

class MPS803(bus:IECBus,driver:PrinterDriver,device:Int = 4) extends IECBusDevice(bus,device) with C64Component {
  val componentID = "Commodore MPS803"
  val componentType = C64ComponentType.PRINTER 
  
  val busid = "MPS803"
  
  // register itself to bus
  bus.registerListener(this)
  
  protected def isDeviceReady = true
  protected def loadData(fileName:String) = None
  
  def init {
  }
  
  override def reset {
    super.reset
  }
  
  override def open_channel {
    channel match {
      case 7 => driver.print(17) 
      case _ => driver.print(145)
    }
  }
  
  override protected def byteJustRead(byte:Int,isLast:Boolean) {
    println("MPS803: byte read " + Integer.toHexString(byte) + " " + byte.toChar)   
    driver.print(byte)
  }
}