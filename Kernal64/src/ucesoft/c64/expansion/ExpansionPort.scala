package ucesoft.c64.expansion

import ucesoft.c64.cpu.Memory
import ucesoft.c64.ChipID
import ucesoft.c64.Log
import ucesoft.c64.cpu.RAMComponent
import ucesoft.c64.C64ComponentType

abstract class ExpansionPort extends RAMComponent {
  val componentID = "ExpansionPort"
  val componentType = C64ComponentType.MEMORY 
  val startAddress = 0xDE00
  val length = 512
  val isRom = false
  val isActive = true
  protected[this] var baLow = false
  
  def setBaLow(baLow:Boolean) = this.baLow = baLow

  def EXROM: Boolean
  def GAME: Boolean
  def ROML: Memory
  def ROMH: Memory

  final def isUltimax = !GAME && EXROM

  override def init {}
  override def reset {}
  def eject {}
  def isEmpty = false

  override def getProperties = {
    properties.setProperty("EXROM", EXROM.toString)
    properties.setProperty("GAME", GAME.toString)
    properties.setProperty("ROML", if (ROML != null) ROML.toString else "-")
    properties.setProperty("ROMH", if (ROMH != null) ROMH.toString else "-")

    properties
  }

  final def notifyMemoryConfigurationChange = ExpansionPort.updateListeners

  def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = 0
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
  
  def freezeButton {}
  def isFreezeButtonSupported = false
}

object ExpansionPort {
  val emptyExpansionPort : ExpansionPort = new ExpansionPort {
    private object EmptyROM extends Memory {
      val name = "EmptyROM"
      val startAddress = 0
      val length = 0
      val isRom = true
      def isActive = false
      def init {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = 0
      def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
    }
    val name = "Empty Expansion Port"
    val EXROM = true
    val GAME = true
    val ROML = EmptyROM: Memory
    val ROMH = EmptyROM: Memory
    override def isEmpty = true
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = memoryForEmptyExpansionPort.lastByteRead
  }
  private val proxyExpansionPort : ExpansionPort = new ExpansionPort {
    val name = "Proxy Expansion Port"
    def EXROM = expansionPort.EXROM
    def GAME = expansionPort.GAME
    def ROML = expansionPort.ROML
    def ROMH = expansionPort.ROMH
    final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = expansionPort.read(address, chipID)
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = expansionPort.write(address, value, chipID)
    final override def reset = expansionPort.reset
    final override def init = expansionPort.init
    final override def isEmpty = expansionPort.isEmpty
    final override def setBaLow(baLow:Boolean) = expansionPort.setBaLow(baLow)
    final override def freezeButton = expansionPort.freezeButton
    final override def isFreezeButtonSupported = expansionPort.isFreezeButtonSupported
    final override def eject = expansionPort.eject
  }

  private[this] var expansionPort = emptyExpansionPort
  private[this] var listeners: List[ExpansionPortConfigurationListener] = Nil
  private[this] var memoryForEmptyExpansionPort : LastByteReadMemory = _

  def addConfigurationListener(l: ExpansionPortConfigurationListener) {
    listeners = l :: listeners
  }

  private def updateListeners = for (l <- listeners) l.expansionPortConfigurationChanged

  def getExpansionPort = proxyExpansionPort
  def setMemoryForEmptyExpansionPort(mem:LastByteReadMemory) = memoryForEmptyExpansionPort = mem
  def setExpansionPort(expansionPort: ExpansionPort) = {
    this.expansionPort = expansionPort
    Log.debug("Setting new expansion port: " + expansionPort.name + " listeners are " + listeners)
    updateListeners
  }
}