package ucesoft.c64.peripheral.keyboard

import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

class NMISwitcher(nmiAction:(Boolean) => Unit) extends C64Component {
  val componentID = "NMI Switcher (CIA2)"
  val componentType = C64ComponentType.INTERNAL 
  
  private var keyboardNMILow = false
  private var cia2NMILow = false
  def keyboardNMIAction(low:Boolean) {
    keyboardNMILow = low
    nmiAction(keyboardNMILow || cia2NMILow)
  }
  def cia2NMIAction(low:Boolean) {
    cia2NMILow = low
    nmiAction(keyboardNMILow || cia2NMILow)
  }
  
  override def getProperties = {
    properties.setProperty("CIA2 NMI",cia2NMILow.toString)
    properties.setProperty("Keyboard restore NMI",keyboardNMILow.toString)
    properties
    }
  
  def init {}
  
  def reset {
    keyboardNMILow = false
    cia2NMILow = false
  }
}