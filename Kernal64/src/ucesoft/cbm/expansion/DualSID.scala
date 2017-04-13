package ucesoft.cbm.expansion

import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.ChipID

class DualSID(sid:SID,sidAddress:Int) extends ExpansionPort {
  override val name = "DualSID"
  override val componentID = "DualSID"
      
  val EXROM = true
  val GAME = true
  val ROML = null
  val ROMH = null
    
  private[this] val sid2 = new SID(sidAddress,2,Some(sid.getDriver))
  
  reset
  
  final override def reset {
    sid2.reset
  }
  
  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if ((address & 0xFF00) == sidAddress) {
      sid2.read(address)
    }
    else 0
  }
  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    if ((address & 0xFF00) == sidAddress) {
      sid2.write(address,value)
    }
  }
  
  final override def eject {
    sid2.stop
  }
}