package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

trait IECBusListener {
  val isController = false
  val busid : String
  private[bus] var bitmap = 0
  
  def atnChanged(oldValue:Int,newValue:Int) : Unit = {}
  def srqTriggered()  : Unit = {}
}

object IECBusLine extends Enumeration {
  type Line = Value
  val ATN: IECBusLine.Value = Value
  val CLK: IECBusLine.Value = Value
  val DATA: IECBusLine.Value = Value
  val SRQ: IECBusLine.Value = Value
}

object IECBus {
  final val GROUND = 1
  final val VOLTAGE = 0
}

class IECBus extends CBMComponent {
  val componentID = "IEC Bus"
  val componentType: Type = CBMComponentType.CHIP
  
  import IECBus._
  private[this] var ATN : Long = VOLTAGE
  private[this] var CLK : Long = VOLTAGE
  private[this] var DATA : Long = VOLTAGE
  private[this] var SRQ : Long = VOLTAGE
  private[this] var listeners : List[IECBusListener] = Nil
  private[this] var listenersBitMap = 0L

  /* hack to enable driver's sound for freespin demo */
  var freeSpinStepperOn = false
  
  override def getProperties: Properties = {
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
  
  final def unregisterListener(l:IECBusListener) : Unit = {
    listeners = listeners filterNot { _.busid == l.busid }
    listenersBitMap &= ~(1 << l.bitmap)
    ATN &= ~(1 << l.bitmap)
    DATA &= ~(1 << l.bitmap)
    CLK &= ~(1 << l.bitmap)
    SRQ &= ~(1 << l.bitmap)
  }
  
  final def registerListener(l:IECBusListener) : Unit = {
    l.bitmap = findAndSetNextBit
    listeners = l :: listeners
    Log.info(s"IECBus has registerd ${l.busid}(${l.bitmap})(${l.getClass.getName}) as a listener")
    if (l.isController ) {
      Log.info(s"Found IECBus controller: ${l.busid}")
    }
  }
  
  final def setLine(l:IECBusListener,line:IECBusLine.Line,value:Int) : Unit = {
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
        if (value == GROUND) SRQ |= 1 << l.bitmap else SRQ &= ~(1 << l.bitmap)
    }
  }
  
  @inline private def notifyATNChange(preATN:Long,ATN:Long) : Unit = {
    var l = listeners
	  while (l != Nil) {
	    l.head.atnChanged(normalize(preATN),normalize(ATN))
	    l = l .tail
	  }
  }
  def triggerSRQ(caller:IECBusListener) : Unit = {
    var l = listeners
	  while (l != Nil) {
	    if (caller.bitmap != l.head.bitmap) l.head.srqTriggered()
	    l = l .tail
	  }
  }
  
  final def setLine(l:IECBusListener,atnValue:Int,dataValue:Int,clockValue:Int) : Unit = {
    val preATN = ATN
    if (atnValue == GROUND) ATN |= 1 << l.bitmap else ATN &= ~(1 << l.bitmap)
    if (preATN != ATN) notifyATNChange(preATN,ATN)
    if (dataValue == GROUND) DATA |= 1 << l.bitmap else DATA &= ~(1 << l.bitmap)
    if (clockValue == GROUND) CLK |= 1 << l.bitmap else CLK &= ~(1 << l.bitmap)
  }
  
  def init(): Unit = {}
  def reset(): Unit = {
    ATN = VOLTAGE
    CLK = VOLTAGE
    DATA = VOLTAGE
    SRQ = VOLTAGE
  }
  
  final def atn: Int = normalize(ATN)
  final def clk: Int = normalize(CLK)
  final def data: Int = normalize(DATA)
  final def srq: Int = normalize(SRQ)
  
  override def toString = s"IECBus ATN=$ATN CLK=$CLK DATA=$DATA SRQ=$SRQ"  
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(listeners.size)
    for(l <- listeners) out.writeObject(l.busid)
    out.writeLong(ATN)
    out.writeLong(CLK)
    out.writeLong(DATA)
    out.writeLong(SRQ)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
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
  protected def allowsStateRestoring : Boolean = true
}