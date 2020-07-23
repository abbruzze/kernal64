package ucesoft.cbm.expansion

import java.io.{ObjectInputStream, ObjectOutputStream}

import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.{ChipID, Clock}

class DualSID(sid:SID,sidAddress:Int) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.DUALSID
  override val name = "DualSID"
  override val componentID = "DualSID"
  private[this] final val endAddress = sidAddress + 0x20
      
  val EXROM = true
  val GAME = true
  val ROML = null
  val ROMH = null
    
  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if (address >= sidAddress && address < endAddress) sid.read(address) else super.read(address,chipID)
  }
  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (address >= sidAddress && address < endAddress) sid.write(address,value)
  }
  
  final override def eject  : Unit = {
    sid.setStereo(false)
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    out.writeObject(sidAddress.toHexString.toUpperCase)
    super.saveState(out)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // address is handled by expansion port handler
    super.loadState(in)
  }
}

object DualSID {
  final val C64_VALID_ADDRESSES = Array(0xD420,0xD500,0xD600,0xD700,0xDE00,0xDF00) map { Integer.toHexString(_).toUpperCase() }
  final val C128_VALID_ADDRESSES = Array(0xD420,0xDE00,0xDF00) map { Integer.toHexString(_).toUpperCase() }
  private var lastAddress = 0

  @inline private def isOnExp(address:Int) : Boolean = address >= 0xDE00 && address < 0xE000

  def validAddresses(c64Mode:Boolean) : Array[String] = c64Mode match {
    case true => C64_VALID_ADDRESSES
    case false => C128_VALID_ADDRESSES
  }

  def setDualSID(address:Option[Int],sid:SID): Unit = {
    Clock.systemClock.pause
    address match {
      case None =>
        if (isOnExp(lastAddress)) {
          ExpansionPort.getExpansionPort.eject
          ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
        }
        sid.setStereo(false,None)
        lastAddress = 0
      case Some(adr) =>
        val sid2 = new SID(adr,2,Some(sid.getDriver))
        sid2.init
        sid.setStereo(true,Some(sid2))

        if (isOnExp(adr)) {
          ExpansionPort.getExpansionPort.eject
          ExpansionPort.setExpansionPort(new DualSID(sid,adr))
        }
        lastAddress = adr
    }
    Clock.systemClock.play
  }
}