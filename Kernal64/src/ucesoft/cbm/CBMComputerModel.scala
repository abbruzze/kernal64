package ucesoft.cbm

sealed trait CBMComputerModel {
  val modelName : String
}

case object C64Model extends CBMComputerModel { override val modelName = "C64" }
case object C128Model extends CBMComputerModel { override val modelName = "C128" }
case object CBMIIModel extends CBMComputerModel { override val modelName = "CBM2" }
case object VIC20Model extends CBMComputerModel { override val modelName = "VIC20" }