package ucesoft.cbm
import ucesoft.cbm.CBMComponentType.Type

abstract class Chip extends CBMComponent {
  val id: ChipID.ID   
  val componentType: Type = CBMComponentType.CHIP
  lazy val componentID: String = componentType.toString
}

object ChipID extends Enumeration {
  type ID = Value
  val CPU: ChipID.Value = Value("CPU")
  val CIA: ChipID.Value = Value("CIA")
  val VIC: ChipID.Value = Value("VIC")
  val SID: ChipID.Value = Value("SID")
  val VIA: ChipID.Value = Value("VIA")
  val CPU_1541: ChipID.Value = Value("CPU_1541")
  val CPU_1571: ChipID.Value = Value("CPU_1571")
  val CPU_1581: ChipID.Value = Value("CPU_1581")
  val VIC_COP: ChipID.Value = Value("VIC_COP")
}