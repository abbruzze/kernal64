package ucesoft.cbm.expansion

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.ChipID
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.formats.Cartridge

import java.io.ObjectOutputStream
import java.io.ObjectInputStream

object ExpansionPortType extends Enumeration {
  val EMPTY = Value
  val CRT = Value
  val REU = Value
  val DIGIMAX = Value
  val SWIFTLINK = Value
  val DUALSID = Value
  val CPM = Value
  val GEORAM = Value
}

abstract class ExpansionPort extends RAMComponent {
  def TYPE : ExpansionPortType.Value
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

  override def init  : Unit = {}
  override def reset  : Unit = {}
  def eject  : Unit = {}
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
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  
  def freezeButton  : Unit = {}
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
      def init  : Unit = {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = 0
      def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
    }
    val TYPE = ExpansionPortType.EMPTY
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
    def TYPE : ExpansionPortType.Value = expansionPort.TYPE
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
    final override def getCRT: Option[Cartridge] = expansionPort.getCRT
    final override def eject = expansionPort.eject
    final override def shutdown = expansionPort.shutdown
    // state
    final override protected def saveState(out:ObjectOutputStream) = {
      if (expansionPort.TYPE == ExpansionPortType.EMPTY) out.writeBoolean(false)
      else {
        out.writeBoolean(true)
        out.writeObject(expansionPort.TYPE.toString)
        expansionPort.saveState(out)
      }
    }
    final override protected def loadState(in:ObjectInputStream) = {
      if (in.readBoolean) {
        expansionPortStateHandler(in,ExpansionPortType.withName(in.readObject.toString))
        expansionPort.loadState(in)
      }
      else {
        expansionPort.eject
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

  private def updateListeners(game:Boolean,exrom:Boolean) = {
    var ptr = listeners
    while (!ptr.isEmpty) {
      ptr.head.expansionPortConfigurationChanged(game,exrom)
      ptr = ptr.tail
    }
  }

  def getExpansionPort = proxyExpansionPort
  def getInternalExpansionPort = expansionPort
  def setMemoryForEmptyExpansionPort(mem:LastByteReadMemory) = memoryForEmptyExpansionPort = mem
  def setExpansionPort(expansionPort: ExpansionPort) = {
    this.expansionPort = expansionPort
    expansionPort.init
    Log.info("Setting new expansion port: " + expansionPort.name + " listeners are " + listeners)
    updateListeners(expansionPort.GAME,expansionPort.EXROM)
  }
  def setExpansionPortStateHandler(expansionPortStateHandler : (ObjectInputStream,ExpansionPortType.Value) => Unit) : Unit = this.expansionPortStateHandler = expansionPortStateHandler
}