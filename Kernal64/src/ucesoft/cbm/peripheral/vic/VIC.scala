package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.{CBMComponentType, ChipID}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.peripheral.vic.coprocessor.VICCoprocessor

import java.awt.Dimension

abstract class VIC extends RAMComponent {
  override val componentType: Type = CBMComponentType.CHIP
  val isRom = false
  val isActive = true
  val id: ID = ChipID.VIC

  type Model <: VICModel

  def setVICModel(model:Model): Unit

  def getVICModel(): Model

  def setDisplay(display:Display): Unit

  def clock() : Unit

  def SCREEN_WIDTH: Int

  def SCREEN_HEIGHT: Int

  def VISIBLE_SCREEN_WIDTH: Int

  def VISIBLE_SCREEN_HEIGHT: Int

  def SCREEN_ASPECT_RATIO: Double

  def STANDARD_DIMENSION : Dimension = new Dimension(0,0)

  def TESTBENCH_DIMENSION : Dimension = new Dimension()

  def getRasterLine : Int
  def getRasterCycle: Int

  def setShowDebug(showDebug:Boolean) : Unit

  def setCoprocessor(cop:VICCoprocessor) : Unit

  def getCoprocessor : Option[VICCoprocessor]

  def setDrawBorder(on:Boolean) : Unit

  def enableLightPen(enabled: Boolean,offsetX:Int,offsetY:Int): Unit

  def triggerLightPen(): Unit
}
