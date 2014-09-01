package ucesoft.c64.cpu

import ucesoft.c64.C64Component

trait RAMComponent extends C64Component with Memory {
  override def getProperties = {
    properties.setProperty("Active",isActive.toString)
    properties.setProperty("Addresses",Integer.toHexString(startAddress) + "-" + Integer.toHexString(startAddress + length - 1))
    properties
  }
}