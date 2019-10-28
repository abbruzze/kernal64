package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.expansion.LastByteReadMemory
import ucesoft.cbm.expansion.ExpansionPortConfigurationListener

trait VICMemory extends RAMComponent with LastByteReadMemory with ExpansionPortConfigurationListener {
  def getBank : Int
  
  final def getBankAddress = getBank << 14
  
  def setVideoBank(bank: Int)
  
  def readPhi2(address:Int) : Int

  def readPCOpcode : Int
}