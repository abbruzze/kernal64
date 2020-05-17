package ucesoft.cbm.peripheral

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

abstract class Connector extends CBMComponent {
  val componentType = CBMComponentType.INTERNAL
  private var dir = 0 // 0=Input (read only), 1=Output (read and write) for each bit
  protected[this] var latch = 0
  
  def read : Int
  final def write(data:Int) = {
    latch = data
    performWrite(data)
  }
  
  protected def performWrite(data:Int) : Unit
  def ddr_=(dir:Int): Unit = {
    this.dir = dir
    write(latch)    
  }
  def ddr : Int = dir
  
  def init  : Unit = {}
  def reset  : Unit = {
    dir = 0
    latch = 0
  }
  
  override def getProperties = {
    properties.setProperty("Ddr",Integer.toHexString(dir))
    properties.setProperty("Latch",Integer.toHexString(latch))
    properties
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(dir)
    out.writeInt(latch)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    dir = in.readInt
    latch = in.readInt
  }
  protected def allowsStateRestoring : Boolean = true
}