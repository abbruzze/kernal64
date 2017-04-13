package ucesoft.cbm.cpu

import ucesoft.cbm.CBMComponent

trait RAMComponent extends CBMComponent with Memory {
  override def getProperties = {
    properties.setProperty("Active",isActive.toString)
    properties.setProperty("Addresses",Integer.toHexString(startAddress) + "-" + Integer.toHexString(startAddress + length - 1))
    properties
  }
}