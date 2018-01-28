package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.Log
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import java.io.IOException

trait IECBusListener {
  val isController = false
  val busid : String
  
  def atnChanged(oldValue:Int,newValue:Int) {}
  def srqTriggered {}
}

object IECBusLine extends Enumeration {
  type Line = Value
  val ATN = Value
  val CLK = Value
  val DATA = Value
  val SRQ = Value
}

object IECBus {
  final val GROUND = 1
  final val VOLTAGE = 0
}

class IECBus extends CBMComponent {
  val componentID = "IEC Bus"
  val componentType = CBMComponentType.CHIP 
  
  import IECBus._
  private[this] var ATN = VOLTAGE
  private[this] var CLK = VOLTAGE
  private[this] var DATA = VOLTAGE
  private[this] var SRQ = VOLTAGE
  private[this] case class State(listener:IECBusListener,var atn:Int=VOLTAGE,var clk:Int=VOLTAGE,var data:Int=VOLTAGE,var srq:Int=VOLTAGE)
  private[this] var lines : List[State] = Nil
  private[this] var controller : State = null
  
  override def getProperties = {
    properties.setProperty("ATN",ATN.toString)
    properties.setProperty("CLK",CLK.toString)
    properties.setProperty("DATA",DATA.toString)
    properties.setProperty("SRQ",SRQ.toString)
    properties
  }
  
  final def unregisterListener(l:IECBusListener) {
    lines = lines filterNot { s => s.listener == l }
  }
  
  final def registerListener(l:IECBusListener) {
    unregisterListener(l)
    val state = State(l)
    lines = state :: lines
    Log.info(s"IECBus has registerd ${l.busid} as a listener")
    if (l.isController ) {
      controller = state
      Log.info(s"Found IECBus controller: ${l.busid}")
    }
  }
  
  final def setLine(id:String,line:IECBusLine.Line,value:Int) {
    //println(s"[${this}] ${id} set ${line} to ${value}")
    var l = lines
    while (l != Nil && l.head.listener.busid != id) l = l.tail
    if (l == Nil) throw new IllegalArgumentException(s"${id} listener not found as IECBus listener")
    line match {
      case IECBusLine.ATN => l.head.atn = value
      case IECBusLine.CLK => l.head.clk = value
      case IECBusLine.DATA => l.head.data = value
      case IECBusLine.SRQ => l.head.srq = value
    }
    updateLines
  }
  
  final def setLine(id:String,atnValue:Int,dataValue:Int,clockValue:Int) {
    var l = lines
    while (l != Nil && l.head.listener.busid != id) l = l.tail
    if (l == Nil) throw new IllegalArgumentException(s"${id} listener not found as IECBus listener")
    
    l.head.atn = atnValue
    l.head.clk = clockValue
    l.head.data = dataValue
    updateLines
  }
  
  final def triggerSRQ(id:String) {
    var l = lines
    while (l != Nil) {
      val listener = l.head.listener
      if (listener.busid != id) listener.srqTriggered
      l = l .tail
    }
  }
  
  def init {}
  def reset {
    var l = lines
    while (l != Nil) {
      l.head.atn = VOLTAGE
      l.head.clk = VOLTAGE
      l.head.data = VOLTAGE
      l.head.srq = VOLTAGE
      l = l .tail
    }
    
    updateLines
  }
  
  @inline private def updateLines {
    val preATN = ATN
    val preCLK = CLK
    val preDATA = DATA
    ATN = VOLTAGE
    CLK = VOLTAGE
    DATA = VOLTAGE
    SRQ = VOLTAGE
    var l = lines
    while (l != Nil) {
      val head = l.head
      if (head.atn == GROUND) ATN = GROUND
      if (head.clk == GROUND) CLK = GROUND
      if (head.data == GROUND) DATA = GROUND
      if (head.srq == GROUND) SRQ = GROUND
      l = l .tail
    }
    if (preATN != ATN) {
      var l = lines
  	  while (l != Nil) {
  	    if (preATN != ATN) l.head.listener.atnChanged(preATN,ATN)
  	    l = l .tail
  	  }
    }
  }
  
  final def atn = ATN
  final def clk = CLK
  final def data = DATA
  final def srq = SRQ
  
  override def toString = s"IECBus ATN=$ATN CLK=$CLK DATA=$DATA SRQ=$SRQ"  
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    for(l <- lines) {
      out.writeObject(l.listener.busid)
      out.writeInt(l.atn)
      out.writeInt(l.data)
      out.writeInt(l.clk)
      out.writeInt(l.srq)
    }
  }
  protected def loadState(in:ObjectInputStream) {
    for(i <- 0 until lines.length) {
      val id = in.readObject.asInstanceOf[String]
      lines find { _.listener.busid == id } match {
        case Some(l) =>
          l.atn = in.readInt
          l.data = in.readInt
          l.clk = in.readInt
          l.srq = in.readInt
        case None =>
          throw new IOException(s"Can't find busid $id")
      }
    }
    updateLines
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}