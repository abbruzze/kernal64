package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory

import java.io.{ObjectInputStream, ObjectOutputStream}

class Isepic(nmiAction:Boolean => Unit) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.ISEPIC
  override val name = "Isepic"
  override val componentID = "Isepic"

  object ROMHAccess extends Memory {
    override val isRom: Boolean = true
    override val length: Int = 0
    override val startAddress: Int = 0
    override val name: String = ""

    override def init(): Unit = {}
    override def isActive: Boolean = true
    override def read(address: Int, chipID: ID): Int = {
      address match {
        case 0xFFFA | 0xFFFB =>
          nmiStatus += 1
          if (nmiStatus == 3) {
            game = true
            exrom = true
            notifyMemoryConfigurationChange()
            nmiStatus = 0
          }
          ram(ramPage << 8 | address & 0xFF)
        case _ =>
          0
      }
    }
    override def write(address: Int, value: Int, chipID: ID): Unit = {}
  }

  override def EXROM = exrom
  override def GAME = game

  val ROML = null
  override def ROMH : Memory = if (nmiStatus == 1 || nmiStatus == 2) ROMHAccess else null

  final private[this] val ram = Array.ofDim[Int](2048)
  private var switchEnabled = true
  private var ramPage = 0
  private var game = true
  private var exrom = true
  private var nmiStatus = 0

  def setSwitch(enabled:Boolean): Unit = {
    switchEnabled = enabled
    checkNMI()
  }

  private def checkNMI(): Unit = {
    if (switchEnabled) {
      game = false
      exrom = true
      nmiAction(true)
      nmiAction(false)
      nmiStatus = 1
    }
    else {
      game = true
      exrom = true
      nmiStatus = 0
    }
    notifyMemoryConfigurationChange()
  }

  final override def init(): Unit = {
    reset()
  }

  final override def reset(): Unit = {}
  override def hardReset(): Unit = {
    reset()
    ramPage = 0
    java.util.Arrays.fill(ram,0)
  }

  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if ((address & 0xFF00) == 0xDE00) {
      if (switchEnabled) {
        ramPage = ((address & 4) >> 2) | (address & 2) | ((address & 1) << 2)
      }
      super.read(address)
    }
    else {
      if (switchEnabled) ram(ramPage << 8 | address & 0xFF) else super.read(address)
    }
  }

  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    if ((address & 0xFF00) == 0xDE00) {
      if (switchEnabled) {
        ramPage = ((address & 4) >> 2) | (address & 2) | ((address & 1) << 2)
      }
    }
    else {
      if (switchEnabled) {
        ram(ramPage << 8 | address & 0xFF) = value
      }
    }
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    out.writeBoolean(switchEnabled)
    out.writeBoolean(game)
    out.writeBoolean(exrom)
    out.writeInt(nmiStatus)
    out.writeObject(ram)
  }
  override def loadState(in: ObjectInputStream): Unit = {
    switchEnabled = in.readBoolean()
    game = in.readBoolean()
    exrom = in.readBoolean()
    nmiStatus = in.readInt()
    loadMemory(ram,in)
  }
}
