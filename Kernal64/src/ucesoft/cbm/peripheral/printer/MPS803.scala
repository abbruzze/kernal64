package ucesoft.cbm.peripheral.printer

import ucesoft.cbm.peripheral.bus.IECBusDevice
import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class MPS803(bus:IECBus,driver:PrinterDriver,device:Int = 4) extends IECBusDevice(bus,device) with CBMComponent {
  val componentID = "Commodore MPS803"
  val componentType = CBMComponentType.PRINTER 
  
  val busid = "MPS803"
    
  private[this] var active = false
  
  // register itself to bus
  bus.registerListener(this)
  
  def setActive(active:Boolean) = this.active = active
  protected def isDeviceReady = active
  protected def loadData(fileName:String) = None
  
  def init : Unit = {}
  
  def reset : Unit = {}
  
  override def open_channel : Unit = {
    channel match {
      case 7 => driver.print(17) 
      case _ => driver.print(145)
    }
  }
  
  override protected def byteJustRead(byte:Int,isLast:Boolean) : Unit = {
    //println("MPS803: byte read " + Integer.toHexString(byte) + " " + byte.toChar + " last=" + isLast) 
    channels(channel).clear
    driver.print(byte)
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(active)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    active = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}