package ucesoft.cbm.misc

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponent, CBMComponentType}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object Switcher {
  final val VIC = 0x01
  final val CIA = 0x02
  final val CRT = 0x04
  final val KB = 0x08
  final val EXT = 0x10
}

class Switcher(name:String,handler: (Boolean) => Unit) extends CBMComponent {
  val componentID = s"$name Switcher"
  val componentType: Type = CBMComponentType.INTERNAL

  private[this] var bus = 0

  override def getProperties: Properties = {
    properties.setProperty(componentID,bus.toBinaryString)
    properties
  }

  def init(): Unit = {}

  def reset(): Unit = {
    bus = 0
  }

  final def setLine(line:Int,set:Boolean) : Unit = {
    bus = if (set) bus | line else bus & ~line
    handler(bus > 0)
  }

  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(bus)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    bus = in.readInt()
  }
  protected def allowsStateRestoring : Boolean = true
}
