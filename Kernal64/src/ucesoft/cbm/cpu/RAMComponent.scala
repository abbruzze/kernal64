package ucesoft.cbm.cpu

import ucesoft.cbm.CBMComponent

import java.util.Properties

trait RAMComponent extends CBMComponent with Memory {
  override def getProperties: Properties = {
    properties.setProperty("Active",isActive.toString)
    properties.setProperty("Addresses",Integer.toHexString(startAddress) + "-" + Integer.toHexString(startAddress + length - 1))
    properties
  }
}