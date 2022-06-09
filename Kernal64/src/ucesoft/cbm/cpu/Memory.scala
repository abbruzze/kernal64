package ucesoft.cbm.cpu

import ucesoft.cbm.{CBMComponent, ChipID, Log}

trait Memory {
  val isRom: Boolean
  val length: Int
  val startAddress: Int
  lazy val endAddress: Int = startAddress + length
  val name: String
  
  private[this] var forwardRead,forwardWrite = false
  protected[this] var forwardReadTo,forwardWriteTo : Memory = null
  
  final def isForwardRead: Boolean = forwardRead
  final def isForwardWrite: Boolean = forwardWrite
  final def setForwardReadTo(forwardReadTo:Option[Memory]) : Unit = {
    this.forwardReadTo = forwardReadTo match {
      case Some(fr) =>
        forwardRead = true
        fr
      case None =>
        forwardRead = false
        null
    }
  }
  final def setForwardWriteTo(forwardWriteTo:Option[Memory]) : Unit = {
    this.forwardWriteTo = forwardWriteTo match {
      case Some(fw) =>
        forwardWrite = true
        fw
      case None =>
        forwardWrite = false
        null
    }
  }

  def init() : Unit
  def isActive: Boolean  
  def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit
  def byteOnBUS : Int = 0
  
  override def toString = s"$name(${hex4(startAddress)}-${hex4(startAddress + length - 1)},active=$isActive)"
}

abstract class BridgeMemory extends RAMComponent {
  private[this] var bridges : List[(Int,Int,Memory)] = Nil
  // cache
  private[this] var bridgesStart : Array[Int] = _
  private[this] var bridgesEnd : Array[Int] = _
  private[this] var bridgesMem : Array[Memory] = _
  
  def addBridge(m:Memory,start:Int= -1,length:Int= -1,addComponent:Boolean=true) : Unit = {
    val startAddress = if (start != -1) start else m.startAddress
    val endAddress = startAddress + (if (length != -1) length else m.length) - 1
    
    bridges = (startAddress,endAddress,m) :: bridges
    Log.info(s"Added bridged memory ${m.name} to $name ${hex4(startAddress)}-${hex4(endAddress)}")
    m match {
      case mc : CBMComponent if addComponent =>
        add(mc)
      case _ =>
    }
    val ba = bridges.toArray
    bridgesStart = Array.ofDim[Int](ba.length)
    bridgesEnd = Array.ofDim[Int](ba.length)
    bridgesMem = Array.ofDim[Memory](ba.length)
    for(i <- 0 until ba.length) {
      bridgesStart(i) = ba(i)._1
      bridgesEnd(i) = ba(i)._2
      bridgesMem(i) = ba(i)._3
    }
  }
  @inline private def select(address:Int) = {
    var ptr = bridges
    var found : Memory = null
    var i = 0
    val length = bridgesStart.length
    while (found == null && i < length) {
      if (address >= bridgesStart(i) && address <= bridgesEnd(i)) found = bridgesMem(i)
      else i += 1
    }
    if (found == null) throw new IllegalArgumentException //("Bad configuration of ram " + name + " while selecting address " + address + " " + bridges)
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
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = try {
    select(address).write(address,value,chipID)
  }
  catch {
    case ill:IllegalArgumentException =>
      //println("Bad write on address " + Integer.toHexString(address))
  }
  
  def defaultValue(address:Int) : Option[Int] = None
  override def toString: String = name + bridges.mkString("[",",","]")
}

object Memory {
  val empty: Memory = dummyWith(0,0)
  
  def dummyWith(address:Int,values:Int*): Memory = new Memory {
    private val mem = values.toArray
    val isRom = false
    val length: Int = values.length
    val startAddress: Int = address
    val name = "DUMMY"
    
    def init  : Unit = {}
    val isActive = true
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = mem(address - startAddress) = value
  }
}