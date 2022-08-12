package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType, CBMComputer, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}

object IEEE488Bus {
  object LineType extends Enumeration {
    val EOI: LineType.Value = Value(0)    // End Or Identify
    val REN: LineType.Value = Value(1)    // Remote Enable
    val DAV: LineType.Value = Value(2)    // Data Valid
    val NRFD: LineType.Value = Value(3)    // Not Ready For Data
    val NDAC: LineType.Value = Value(4)    // No Data Accepted
    val IFC: LineType.Value = Value(5)    // Interface Clear
    val SRQ: LineType.Value = Value(6)    // Service Request
    val ATN: LineType.Value = Value(7)    // Attention
  }

  object LineValue extends Enumeration {
    val RELEASED: LineValue.Value = Value(0)
    val PULLED: LineValue.Value = Value(1)

    def intValue(lv:LineValue.Value): Int = 1 - lv.id
    def fromValue(value:Long): LineValue.Value = value match {
      case 0 => RELEASED
      case _ => PULLED
    }
  }

  abstract class LineListener {
    val isController = false
    private var _id = 0L
    private final val changedFun : Array[(Long,LineValue.Value) => Unit] = Array(
      EOIchanged _,RENchanged _,DAVchanged _,NRFDchanged _,NDACchanged _,IFCchanged _,SRQchanged _,ATNchanged _
    )
    final def onLineChanged(id:Long,lineType:LineType.Value,value:LineValue.Value): Unit = changedFun(lineType.id)(id,value)

    final def id : Long = _id
    private[bus] final def setID(id:Long): Unit = _id = id

    protected def EOIchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def RENchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def DAVchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def NRFDchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def NDACchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def IFCchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def SRQchanged(id:Long,newValue:LineValue.Value): Unit = {}
    protected def ATNchanged(id:Long,newValue:LineValue.Value): Unit = {}
  }

  private class Line(val lineType:LineType.Value,lineListener:(Long,LineType.Value,LineValue.Value) => Unit) {
    private var line = 0L

    def reset(): Unit = {
      line = 0L
    }

    final def pull(pullID:Long): Unit = {
      val oldLine = line
      line |= pullID
      trigger(pullID,oldLine,line)
    }
    final def release(pullID:Long): Unit = {
      val oldLine = line
      line &= ~pullID
      trigger(pullID,oldLine,line)
    }

    final def get(): LineValue.Value = LineValue.fromValue(line)

    @inline private def trigger(id:Long,oldValue:Long,value:Long): Unit =
      if ((oldValue > 0 && value == 0) || (oldValue == 0 && value > 0)) {
        //println(s"IEEE $id has changed line $lineType from ${LineValue.fromValue(oldValue)} to new ${LineValue.fromValue(value)}")
        lineListener(id,lineType,LineValue.fromValue(value))
      }

    def loadState(in:ObjectInputStream): Unit = line = in.readLong()
    def saveState(out:ObjectOutputStream): Unit = out.writeLong(line)
  }
}

class IEEE488Bus extends CBMComponent {
  val componentID = "IEEE488 Bus"
  val componentType: Type = CBMComponentType.CHIP
  import IEEE488Bus._
  import LineType._

  private var DIO = 0
  private var listenersBitMap = 0L
  private var listeners : List[LineListener] = Nil
  private val lines : Array[Line] = Array(
    new Line(EOI,lineChanged _), new Line(REN,lineChanged _), new Line(DAV,lineChanged _), new Line(NRFD,lineChanged _), new Line(NDAC,lineChanged _), new Line(IFC,lineChanged _), new Line(SRQ,lineChanged _), new Line(ATN,lineChanged _)
  )

  private def lineChanged(id:Long,lineType:LineType.Value,value:LineValue.Value): Unit = {
    var lPtr = listeners
    while (lPtr.nonEmpty) {
      lPtr.head.onLineChanged(id,lineType,value)
      lPtr = lPtr.tail
    }
  }

  final def setDIO(dio:Int): Unit = DIO = dio
  final def getDIO(): Int = DIO

  final def getLine(lineType: LineType.Value): LineValue.Value = lines(lineType.id).get()

  final def pullLine(listener:LineListener,lineType:LineType.Value): Unit = lines(lineType.id).pull(listener.id)
  final def releaseLine(listener:LineListener,lineType:LineType.Value): Unit = lines(lineType.id).release(listener.id)

  final def registerListener(l:LineListener): Unit = {
    for (b <- 0 to 63) {
      val p = 1L << b
      if ((listenersBitMap & p) == 0) {
        listenersBitMap |= p
        l.setID(p)
        listeners = l :: listeners
        Log.info(s"IEEE488 has registerd ${l.id}(${l.getClass.getName}) as a listener")
        if (l.isController) {
          Log.info(s"Found IECBus controller: ${l.id}")
        }
        return
      }
    }
    throw new IllegalArgumentException("Too many listeners")
  }

  final def unregisterListener(l:LineListener): Unit = {
    listeners = listeners filterNot { _.id == l.id }
    listenersBitMap &= ~l.id
  }

  override def reset(): Unit = {
    DIO = 0
    lines.foreach(_.reset())
  }

  override def init(): Unit = {}

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeInt(DIO)
    lines.foreach(_.saveState(out))
  }

  override protected def loadState(in: ObjectInputStream): Unit = {
    DIO = in.readInt()
    lines.foreach(_.loadState(in))
  }

  override protected def allowsStateRestoring: Boolean = true
}
