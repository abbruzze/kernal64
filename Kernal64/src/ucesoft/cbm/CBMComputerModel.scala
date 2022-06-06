package ucesoft.cbm

sealed trait CBMComputerModel

case object C64Model extends CBMComputerModel
case object C128Model extends CBMComputerModel
case object CBMIIModel extends CBMComputerModel