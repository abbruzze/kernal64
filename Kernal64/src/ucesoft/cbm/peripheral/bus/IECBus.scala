package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.Log
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

trait IECBusListener {
  val isController = false
  val busid : String
  private[bus] var bitmap = 0
  
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
  private[this] var ATN : Long = VOLTAGE
  private[this] var CLK : Long = VOLTAGE
  private[this] var DATA : Long = VOLTAGE
  private[this] var SRQ : Long = VOLTAGE
  private[this] var listeners : List[IECBusListener] = Nil
  private[this] var listenersBitMap = 0L
  
  override def getProperties = {
    properties.setProperty("ATN",normalize(ATN).toString)
    properties.setProperty("CLK",normalize(CLK).toString)
    properties.setProperty("DATA",normalize(DATA).toString)
    properties.setProperty("SRQ",normalize(SRQ).toString)
    properties
  }
    
  @inline private def findAndSetNextBit :Int = {
    for(b <- 0 to 63) {
      val p = 1 << b
      if ((listenersBitMap & p) == 0) {
        listenersBitMap |= p
        return b
      }
    }
    throw new IllegalArgumentException("Too many listeners")
  }
  
  @inline private def normalize(v:Long) = if (v > 0) GROUND else VOLTAGE
  
  final def unregisterListener(l:IECBusListener) {
    listeners = listeners filterNot { _.busid == l.busid }
    listenersBitMap &= ~l.bitmap
    ATN &= ~(1 << l.bitmap)
    DATA &= ~(1 << l.bitmap)
    CLK &= ~(1 << l.bitmap)
    SRQ &= ~(1 << l.bitmap)
  }
  
  final def registerListener(l:IECBusListener) {
    l.bitmap = findAndSetNextBit
    listeners = l :: listeners
    Log.info(s"IECBus has registerd ${l.busid}(${l.bitmap})(${l.getClass.getName}) as a listener")
    if (l.isController ) {
      Log.info(s"Found IECBus controller: ${l.busid}")
    }
  }
  
  final def setLine(l:IECBusListener,line:IECBusLine.Line,value:Int) {
    line match {
      case IECBusLine.ATN => 
        val preATN = ATN
        if (value == GROUND) ATN |= 1 << l.bitmap else ATN &= ~(1 << l.bitmap)
        if (preATN != ATN) notifyATNChange(preATN,ATN)
      case IECBusLine.CLK => 
        if (value == GROUND) CLK |= 1 << l.bitmap else CLK &= ~(1 << l.bitmap)
      case IECBusLine.DATA => 
        if (value == GROUND) DATA |= 1 << l.bitmap else DATA &= ~(1 << l.bitmap)
      case IECBusLine.SRQ => 
        val preSRQ = SRQ
        if (value == GROUND) SRQ |= 1 << l.bitmap else SRQ &= ~(1 << l.bitmap)
    }
  }
  
  @inline private def notifyATNChange(preATN:Long,ATN:Long) {
    var l = listeners
	  while (l != Nil) {
	    l.head.atnChanged(normalize(preATN),normalize(ATN))
	    l = l .tail
	  }
  }
  def triggerSRQ(caller:IECBusListener) {
    var l = listeners
	  while (l != Nil) {
	    if (caller.bitmap != l.head.bitmap) l.head.srqTriggered
	    l = l .tail
	  }
  }
  
  final def setLine(l:IECBusListener,atnValue:Int,dataValue:Int,clockValue:Int) {
    val preATN = ATN
    if (atnValue == GROUND) ATN |= 1 << l.bitmap else ATN &= ~(1 << l.bitmap)
    if (preATN != ATN) notifyATNChange(preATN,ATN)
    if (dataValue == GROUND) DATA |= 1 << l.bitmap else DATA &= ~(1 << l.bitmap)
    if (clockValue == GROUND) CLK |= 1 << l.bitmap else CLK &= ~(1 << l.bitmap)
  }
  
  def init {}
  def reset {
    ATN = VOLTAGE
    CLK = VOLTAGE
    DATA = VOLTAGE
    SRQ = VOLTAGE
  }
  
  final def atn = normalize(ATN)
  final def clk = normalize(CLK)
  final def data = normalize(DATA)
  final def srq = normalize(SRQ)
  
  override def toString = s"IECBus ATN=$ATN CLK=$CLK DATA=$DATA SRQ=$SRQ"  
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeInt(listeners.size)
    for(l <- listeners) out.writeObject(l.busid)
    out.writeLong(ATN)
    out.writeLong(CLK)
    out.writeLong(DATA)
    out.writeLong(SRQ)
  }
  protected def loadState(in:ObjectInputStream) {
    val listenerSize = in.readInt
    for(l <- 1 to listenerSize) {
      val id = in.readObject.toString
      val found = listeners exists { _.busid == id }
      if (!found) throw new IllegalArgumentException(s"Cannot find $id. Check peripherals")
    }
    ATN = in.readLong
    CLK = in.readLong
    DATA = in.readLong
    SRQ = in.readLong
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}