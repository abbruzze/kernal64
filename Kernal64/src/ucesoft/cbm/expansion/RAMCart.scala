package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory

import java.io.{ObjectInputStream, ObjectOutputStream}

class RAMCart(size:Int,mainRam:Memory) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.RAMCART
  override val name = "RAMCART"
  override val componentID = "RAMCART"

  object ROMLAccess extends Memory {
    override val isRom: Boolean = true
    override val length: Int = 0
    override val startAddress: Int = 0
    override val name: String = ""

    override def init(): Unit = {}
    override def isActive: Boolean = true
    override def read(address: Int, chipID: ID): Int = {
      readMem(address)
      /*if (readOnly && ((de01 & 0x80) == 0)) readMem(address)
      else mainRam.read(address)*/
    }
    override def write(address: Int, value: Int, chipID: ID): Unit = mainRam.write(address, value)
  }

  override def EXROM : Boolean = exrom
  val GAME = true
  val ROML = ROMLAccess
  val ROMH: Memory = null

  final private[this] val ram = Array.ofDim[Int](if (size == 128) 2 else 1,256,256) // 64K or 128K
  private var readOnly = false
  private var bank = 0
  private var de01 = 0
  private var exrom = true

  def setReadOnly(ro:Boolean): Unit = {
    readOnly = ro
    checkEXROM()
  }

  private def checkEXROM(): Unit = {
    exrom = !(size == 128 && readOnly && ((de01 & 0x80) == 0))
    notifyMemoryConfigurationChange()
  }

  final override def init(): Unit = {
    reset()
  }
  final override def reset(): Unit = {
    bank = 0
    de01 = 0
    checkEXROM()
  }
  override def hardReset(): Unit = {
    reset()
    for(i <- 0 until ram.length;j <- 0 until ram(i).length) {
      java.util.Arrays.fill(ram(i)(j),0)
    }
  }

  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if ((address & 0xFF00) == 0xDF00) readMem(address)
    else if ((address & 0xFF00) == 0xDE00) {
      address & 1 match {
        case 0 => bank
        case 1 => super.read(address,chipID) & 0x7E | de01
      }
    }
    else super.read(address,chipID)
  }

  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
    address match {
      case 0xDE00 =>
        bank = value
      case 0xDE01 =>
        de01 = value & 0x81
        checkEXROM()
      case _ =>
        if ((address & 0xFF00) == 0xDF00) writeMem(address,value)
    }
  }

  private def readMem(address:Int): Int = {
    if (size == 128) ram(de01 & 1)(bank)(address & 0xFF)
    else ram(0)(bank)(address & 0xFF)
  }
  private def writeMem(address:Int,value:Int): Unit = {
    if (!readOnly) {
      if (size == 128) ram(de01 & 1)(bank)(address & 0xFF) = value
      else ram(0)(bank)(address & 0xFF) = value
    }
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    out.write(size)
    super.saveState(out)
    out.writeInt(bank)
    out.writeInt(de01)
    out.writeBoolean(readOnly)
    out.writeObject(ram)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // the size is read by the expansion port handler
    super.loadState(in)
    bank = in.readInt()
    de01 = in.readInt()
    readOnly = in.readBoolean()
    checkEXROM()
    loadMemory[Array[Array[Int]]](ram,in)
  }
}
