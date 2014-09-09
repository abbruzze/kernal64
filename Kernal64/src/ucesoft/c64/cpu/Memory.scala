package ucesoft.c64.cpu

import ucesoft.c64.ChipID
import ucesoft.c64.Log
import ucesoft.c64.C64Component

trait Memory {
  val isRom: Boolean
  val length: Int
  val startAddress: Int
  lazy val endAddress = startAddress + length
  val name: String

  def init
  def isActive: Boolean  
  def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU)
  
  override def toString = s"${name}(${hex4(startAddress)}-${hex4(startAddress + length - 1)},active=${isActive})"
}

abstract class BridgeMemory extends RAMComponent {
  private[this] var bridges : List[(Int,Int,Memory)] = Nil
  
  def addBridge(m:Memory,start:Int= -1,length:Int= -1) {
    val startAddress = if (start != -1) start else m.startAddress
    val endAddress = startAddress + (if (length != -1) length else m.length) - 1
    
    bridges = (startAddress,endAddress,m) :: bridges
    Log.info(s"Added bridged memory ${m.name} to ${name} ${hex4(startAddress)}-${hex4(endAddress)}")
    m match {
      case mc : C64Component =>
        add(mc)
      case _ =>
    }
  }
  @inline private def select(address:Int) = {
    var ptr = bridges
    var found : Memory = null
    while (ptr.nonEmpty && found == null) {
      val mem = ptr.head
      if (address >= mem._1 && address <= mem._2) found = mem._3
      else ptr = ptr.tail
    }
    if (found == null) throw new IllegalArgumentException("Bad configuration of ram " + name + " while selecting address " + address + " " + bridges)
    found
  }
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = try {
    select(address).read(address,chipID)
  }
  catch {
    case ill:IllegalArgumentException =>
      defaultValue(address) match {
        case None => throw ill
        case Some(dv) => dv
      }
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = select(address).write(address,value,chipID)
  
  def defaultValue(address:Int) : Option[Int] = None
  override def toString = name + bridges.mkString("[",",","]")
}

object Memory {
  def dummyWith(address:Int,values:Int*) = new Memory {
    private val mem = values.toArray
    val isRom = false
    val length = values.length
    val startAddress = address
    val name = "DUMMY"
    
    def init {}
    val isActive = true
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address - startAddress)
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address - startAddress) = value
  }
}