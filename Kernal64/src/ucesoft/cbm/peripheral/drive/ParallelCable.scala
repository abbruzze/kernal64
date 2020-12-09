package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

/**
 * VIA#1  User port plug 
    2, PA0      C, PB0 
    3, PA1      D, PB1 
    4, PA2      E, PB2 
    5, PA3      F, PB3 
    6, PA4      H, PB4 
    7, PA5      J, PB5 
    8, PA6      K, PB6 
    9, PA7      L, PB7 
    18, CB1     8, PC2 
    39, CA2     B, FLAG2 
 * @author ealeame
 */
object ParallelCable extends CBMComponent {
  val componentID = "Parallel Cable"
  val componentType = CBMComponentType.CABLE
  
  private[this] var cableValue = 0
  
  var pcCallback : () => Unit = _
  var ca2Callback : () => Unit = _
  var enabled = false
  
  def init : Unit = {}
  def reset : Unit = { cableValue = 0 }
  
  def read = cableValue
  def write(value:Int) : Unit = cableValue = value
  
  def onPC : Unit = pcCallback()
  def onCA2 : Unit = ca2Callback()
  
  override def getProperties = {
    properties.setProperty("Enabled",enabled.toString)
    properties.setProperty("Value",cableValue.toString)
    properties
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(cableValue)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    cableValue = in.readInt()
  }
  protected def allowsStateRestoring : Boolean = true
}