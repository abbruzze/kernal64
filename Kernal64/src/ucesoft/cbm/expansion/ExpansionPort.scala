package ucesoft.cbm.expansion

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.ChipID
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import javax.swing.JOptionPane

abstract class ExpansionPort extends RAMComponent {
  val componentID = "ExpansionPort"
  val componentType = CBMComponentType.MEMORY 
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

  final def notifyMemoryConfigurationChange = ExpansionPort.updateListeners(GAME,EXROM)

  def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = ExpansionPort.emptyExpansionPort.read(address)
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
  
  def freezeButton {}
  def isFreezeButtonSupported = false
  
  // state
  protected def saveState(out:ObjectOutputStream) {}
  protected def loadState(in:ObjectInputStream) {}
  protected def allowsStateRestoring : Boolean = {
    showError("State error",s"Loading/storing of cartridge's state is not supported [$componentID].")
    false
  }
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
    override protected def allowsStateRestoring : Boolean = true
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
    final override def shutdown = expansionPort.shutdown
    // state
    final override protected def saveState(out:ObjectOutputStream) = expansionPort.saveState(out)
    final override protected def loadState(in:ObjectInputStream) = expansionPort.loadState(in)
    final override protected def allowsStateRestoring : Boolean = expansionPort.allowsStateRestoring
  }

  private[this] var expansionPort = emptyExpansionPort
  private[this] var listeners: List[ExpansionPortConfigurationListener] = Nil
  private[this] var memoryForEmptyExpansionPort : LastByteReadMemory = _
  var currentCartFileName = ""

  def addConfigurationListener(l: ExpansionPortConfigurationListener) {
    listeners = l :: listeners
  }

  private def updateListeners(game:Boolean,exrom:Boolean) = {
    var ptr = listeners
    while (!ptr.isEmpty) {
      ptr.head.expansionPortConfigurationChanged(game,exrom)
      ptr = ptr.tail
    }
  }

  def getExpansionPort = proxyExpansionPort
  def setMemoryForEmptyExpansionPort(mem:LastByteReadMemory) = memoryForEmptyExpansionPort = mem
  def setExpansionPort(expansionPort: ExpansionPort) = {
    this.expansionPort = expansionPort
    expansionPort.init
    Log.info("Setting new expansion port: " + expansionPort.name + " listeners are " + listeners)
    updateListeners(expansionPort.GAME,expansionPort.EXROM)
  }
}