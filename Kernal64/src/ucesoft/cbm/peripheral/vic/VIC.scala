package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.{CBMComponentType, ChipID}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.RAMComponent

abstract class VIC extends RAMComponent {
  override val componentType: Type = CBMComponentType.CHIP
  val isRom = false
  val isActive = true
  val id: ID = ChipID.VIC

  def setVICModel(model:VICModel): Unit

  def clock() : Unit
}
