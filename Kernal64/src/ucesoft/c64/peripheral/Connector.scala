package ucesoft.c64.peripheral

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

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
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeInt(dir)
    out.writeInt(latch)
  }
  protected def loadState(in:ObjectInputStream) {
    dir = in.readInt
    latch = in.readInt
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}