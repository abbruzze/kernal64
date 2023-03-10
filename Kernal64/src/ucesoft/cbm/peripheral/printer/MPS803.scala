package ucesoft.cbm.peripheral.printer

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.peripheral.bus.{BusDataIterator, IECBus, IECBusDevice}

import java.io.{ObjectInputStream, ObjectOutputStream}

class MPS803(bus:IECBus,driver:PrinterDriver,device:Int = 4) extends IECBusDevice(bus,device) with CBMComponent with Printer {
  val busid = "MPS803"
    
  private[this] var active = false
  
  // register itself to bus
  bus.registerListener(this)
  
  override def setActive(active:Boolean): Unit = this.active = active
  protected def isDeviceReady: Boolean = active
  protected def loadData(fileName:String): Option[BusDataIterator] = None
  
  def init() : Unit = {}
  
  def reset() : Unit = {}
  
  override def open_channel() : Unit = {
    channel match {
      case 7 => driver.print(17) 
      case _ => driver.print(145)
    }
  }
  
  override protected def byteJustRead(byte:Int,isLast:Boolean) : Unit = {
    //println("MPS803: byte read " + Integer.toHexString(byte) + " " + byte.toChar + " last=" + isLast) 
    channels(channel).clear()
    driver.print(byte)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = {}
  override protected def loadState(in: ObjectInputStream): Unit = {}
  override protected def allowsStateRestoring: Boolean = true
}