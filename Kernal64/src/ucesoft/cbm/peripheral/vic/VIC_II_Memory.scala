package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.expansion.{ExpansionPortConfigurationListener, LastByteReadMemory}

trait VIC_II_Memory extends RAMComponent with LastByteReadMemory with ExpansionPortConfigurationListener {
  def getBank : Int
  
  final def getBankAddress: Int = getBank << 14
  
  def setVideoBank(bank: Int) : Unit
  
  def readPhi2(address:Int) : Int

  def readPCOpcode : Int

  def isCharROMAddress(address:Int) : Boolean
}