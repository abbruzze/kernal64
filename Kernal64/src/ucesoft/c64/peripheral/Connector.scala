package ucesoft.c64.peripheral

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

abstract class Connector extends C64Component {
  val componentType = C64ComponentType.INTERNAL
  private var dir = 0 // 0=Input (read only), 1=Output (read and write) for each bit
  protected[this] var latch = 0
  
  def read : Int
  final def write(data:Int) = {
    latch = data
    performWrite(data)
  }
  
  protected def performWrite(data:Int)
  def ddr_=(dir:Int) = {
    this.dir = dir
    write(latch)    
  }
  def ddr = dir
  
  def init {}
  def reset {
    dir = 0
    latch = 0
  }
  
  override def getProperties = {
    properties.setProperty("Ddr",Integer.toHexString(dir))
    properties.setProperty("Latch",Integer.toHexString(latch))
    properties
  }
}

object Connector {
  def createEmptyConnector(readValue:Int) = new Connector {
    val componentID = "Dummy connector"
	def read = readValue
	def performWrite(data:Int) {}
  }
}