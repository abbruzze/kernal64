package ucesoft.c64

abstract class Chip extends C64Component {
  val id: ChipID.ID   
  val componentType = C64ComponentType.CHIP
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