package ucesoft.cbm.expansion

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponentType, ChipID, Log}
import ucesoft.cbm.cpu.{Memory, RAMComponent}
import ucesoft.cbm.formats.Cartridge

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object ExpansionPortType extends Enumeration {
  val EMPTY: ExpansionPortType.Value = Value
  val CRT: ExpansionPortType.Value = Value
  val REU: ExpansionPortType.Value = Value
  val DIGIMAX: ExpansionPortType.Value = Value
  val SWIFTLINK: ExpansionPortType.Value = Value
  val DUALSID: ExpansionPortType.Value = Value
  val CPM: ExpansionPortType.Value = Value
  val GEORAM: ExpansionPortType.Value = Value
}

abstract class ExpansionPort extends RAMComponent {
  def TYPE : ExpansionPortType.Value
  val componentID = "ExpansionPort"
  val componentType: Type = CBMComponentType.MEMORY
  val startAddress = 0xDE00
  val length = 512
  val isRom = false
  val isActive = true
  protected[this] var baLow = false
  
  def setBaLow(baLow:Boolean): Unit = this.baLow = baLow

  def EXROM: Boolean
  def GAME: Boolean
  def ROML: Memory
  def ROMH: Memory

  final def isUltimax: Boolean = !GAME && EXROM

  override def init()  : Unit = {}
  override def reset()  : Unit = {}
  def eject()  : Unit = {}
  def isEmpty = false

  override def getProperties: Properties = {
    properties.setProperty("EXROM", EXROM.toString)
    properties.setProperty("GAME", GAME.toString)
    properties.setProperty("ROML", if (ROML != null) ROML.toString else "-")
    properties.setProperty("ROMH", if (ROMH != null) ROMH.toString else "-")

    properties
  }

  final def notifyMemoryConfigurationChange(): Unit = ExpansionPort.updateListeners(GAME,EXROM)

  def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = ExpansionPort.emptyExpansionPort.read(address)
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  
  def freezeButton()  : Unit = {}
  def isFreezeButtonSupported = false
  def getCRT : Option[Cartridge] = None
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(baLow)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    baLow = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = {
    //showError("State error",s"Loading/storing of cartridge's state is not supported [$componentID].")
    //false
    true
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
      def init()  : Unit = {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = 0
      def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
    }
    val TYPE: ExpansionPortType.Value = ExpansionPortType.EMPTY
    val name = "Empty Expansion Port"
    val EXROM = true
    val GAME = true
    val ROML: Memory = EmptyROM: Memory
    val ROMH: Memory = EmptyROM: Memory
    override def isEmpty = true
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = memoryForEmptyExpansionPort.lastByteRead
    override protected def allowsStateRestoring : Boolean = true
  }
  private val proxyExpansionPort : ExpansionPort = new ExpansionPort {
    def TYPE : ExpansionPortType.Value = expansionPort.TYPE
    val name = "Proxy Expansion Port"
    def EXROM: Boolean = expansionPort.EXROM
    def GAME: Boolean = expansionPort.GAME
    def ROML: Memory = expansionPort.ROML
    def ROMH: Memory = expansionPort.ROMH
    final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = expansionPort.read(address, chipID)
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = expansionPort.write(address, value, chipID)
    final override def reset(): Unit = expansionPort.reset()
    final override def hardReset(): Unit = expansionPort.hardReset()
    final override def init(): Unit = expansionPort.init()
    final override def isEmpty: Boolean = expansionPort.isEmpty
    final override def setBaLow(baLow:Boolean): Unit = expansionPort.setBaLow(baLow)
    final override def freezeButton(): Unit = expansionPort.freezeButton()
    final override def isFreezeButtonSupported: Boolean = expansionPort.isFreezeButtonSupported
    final override def getCRT: Option[Cartridge] = expansionPort.getCRT
    final override def eject(): Unit = expansionPort.eject()
    final override def shutdown(): Unit = expansionPort.shutdown()
    // state
    final override protected def saveState(out:ObjectOutputStream): Unit = {
      if (expansionPort.TYPE == ExpansionPortType.EMPTY) out.writeBoolean(false)
      else {
        out.writeBoolean(true)
        out.writeObject(expansionPort.TYPE.toString)
        expansionPort.saveState(out)
      }
    }
    final override protected def loadState(in:ObjectInputStream): Unit = {
      if (in.readBoolean) {
        expansionPortStateHandler(in,ExpansionPortType.withName(in.readObject.toString))
        expansionPort.loadState(in)
      }
      else {
        expansionPort.eject()
        setExpansionPort(emptyExpansionPort)
      }
    }
    final override protected def allowsStateRestoring : Boolean = expansionPort.allowsStateRestoring
  }

  private[this] var expansionPort = emptyExpansionPort
  private[this] var listeners: List[ExpansionPortConfigurationListener] = Nil
  private[this] var memoryForEmptyExpansionPort : LastByteReadMemory = _
  private[this] var expansionPortStateHandler : (ObjectInputStream,ExpansionPortType.Value) => Unit = _
  var currentCartFileName = ""

  def addConfigurationListener(l: ExpansionPortConfigurationListener) : Unit = {
    listeners = l :: listeners
  }

  private def updateListeners(game:Boolean,exrom:Boolean): Unit = {
    var ptr = listeners
    while (!ptr.isEmpty) {
      ptr.head.expansionPortConfigurationChanged(game,exrom)
      ptr = ptr.tail
    }
  }

  def getExpansionPort: ExpansionPort = proxyExpansionPort
  def getInternalExpansionPort: ExpansionPort = expansionPort
  def setMemoryForEmptyExpansionPort(mem:LastByteReadMemory): Unit = memoryForEmptyExpansionPort = mem
  def setExpansionPort(expansionPort: ExpansionPort): Unit = {
    this.expansionPort = expansionPort
    expansionPort.init()
    Log.info("Setting new expansion port: " + expansionPort.name + " listeners are " + listeners)
    updateListeners(expansionPort.GAME,expansionPort.EXROM)
  }
  def setExpansionPortStateHandler(expansionPortStateHandler : (ObjectInputStream,ExpansionPortType.Value) => Unit) : Unit = this.expansionPortStateHandler = expansionPortStateHandler
}