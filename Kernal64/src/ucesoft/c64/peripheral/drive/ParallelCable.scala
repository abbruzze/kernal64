package ucesoft.c64.peripheral.drive

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

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
object ParallelCable extends C64Component {
  val componentID = "Parallel Cable"
  val componentType = C64ComponentType.CABLE
  
  private[this] var cableValue = 0
  
  var pcCallback : () => Unit = _
  var ca2Callback : () => Unit = _
  var enabled = false
  
  def init {}
  def reset { cableValue = 0 }
  
  def read = cableValue
  def write(value:Int) = cableValue = value
  
  def onPC = pcCallback()
  def onCA2 = ca2Callback()
  
  override def getProperties = {
    properties.setProperty("Enabled",enabled.toString)
    properties.setProperty("Value",cableValue.toString)
    properties
  }
}