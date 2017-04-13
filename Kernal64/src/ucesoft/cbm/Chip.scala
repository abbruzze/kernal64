package ucesoft.cbm

abstract class Chip extends CBMComponent {
  val id: ChipID.ID   
  val componentType = CBMComponentType.CHIP
  lazy val componentID = componentType.toString
}

object ChipID extends Enumeration {
  type ID = Value
  val CPU = Value("CPU")
  val CIA = Value("CIA")
  val VIC = Value("VIC")
  val SID = Value("SID")
  val VIA = Value("VIA")
  val CPU_1541 = Value("CPU_1541")
}