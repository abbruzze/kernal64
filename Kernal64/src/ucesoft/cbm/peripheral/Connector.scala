package ucesoft.cbm.peripheral

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

abstract class Connector extends CBMComponent {
  val componentType: Type = CBMComponentType.INTERNAL
  private var dir = 0 // 0=Input (read only), 1=Output (read and write) for each bit
  protected[this] var latch = 0
  
  def read : Int
  final def write(data:Int): Unit = {
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
  
  override def getProperties: Properties = {
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

object EmptyConnector extends Connector {
  override val componentID: String = "EmptyConnector"
  override def read : Int = 0xFF
  override protected def performWrite(data: Int): Unit = {}
}