package ucesoft.cbm

import scala.collection.mutable.ListBuffer
import java.util.Properties
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import java.io.IOException

object CBMComponentType extends Enumeration {
  type Type = Value
  val CPU = Value
  val MEMORY = Value
  val CHIP = Value
  val INPUT_DEVICE = Value
  val OUTPUT_DEVICE = Value
  val DISK = Value
  val INTERNAL = Value
  val TAPE = Value
  val PRINTER = Value
  val USER_PORT = Value
  val FLOPPY = Value
  val CABLE = Value
}

trait CBMComponent {
  val componentID : String
  val componentType : CBMComponentType.Type
  protected val properties = new Properties  
  private[this] val _components = new ListBuffer[CBMComponent]
  
  def getProperties = properties
  
  final def add(c:CBMComponent) {
    val alreadyAdded = _components exists { _.componentID == c.componentID }
    if (!alreadyAdded) _components += c
  }
  
  def reset
  def init
  
  final def change(oldComponent:CBMComponent,newComponent:CBMComponent) {
    _components indexOf (oldComponent) match {
      case -1 => throw new IllegalArgumentException("Can't find component " + oldComponent)
      case i => _components(i) = newComponent
    }
  }
  
  def afterInitHook {}
  
  final def resetComponent : Unit = {
    Log.info(s"Resetting ${componentID}")
    _components foreach { c =>      
      c.resetComponent 
    }
    reset
  }
  def initComponent : Unit = {
    Log.info(s"Initializing ${componentID}")
    init
    _components foreach { c =>      
      c.initComponent 
    }
    afterInitHook
  }
  final def components = _components.toList
  final def printComponentsTree {
    def print(c:CBMComponent,ind:Int) {
      println(("\t" * ind) + c.componentID + " - " + c.componentType)
      c.components foreach { c => print(c,ind + 1) }
    }
    print(this,0)
  }
  
  protected def saveState(out:ObjectOutputStream)
  protected def loadState(in:ObjectInputStream)
  protected def allowsStateRestoring(parent:JFrame) : Boolean
  
  final def save(out:ObjectOutputStream) {
    Log.info(s"Saving $componentID/$componentType's state ...")
    out.writeObject(componentID)
    saveState(out)
    for(c <- _components) c.save(out)    
  }
  final def load(in:ObjectInputStream) {
    Log.info(s"Loading $componentID/$componentType's state ...")
    val id = in.readObject.asInstanceOf[String]
    if (id != componentID) throw new IOException(s"ID Mismatch: found $id, expected $componentID")
    loadState(in)
    for(c <- _components) c.load(in)
  }
  final def allowsState(parent:JFrame) : Boolean = {
    if (allowsStateRestoring(parent)) {
      _components forall { _.allowsStateRestoring(parent) }
    }
    else false
  }
  protected def saveClockEvents(out:ObjectOutputStream) {
    val events = Clock.systemClock.getSubIdListFor(componentID)
    out.writeObject(events)
    for(e <- events) Log.info(s"Saving event ${e._1} for cycle ${e._2} for $componentID")
  }
  protected def loadClockEvents(in:ObjectInputStream)(f:Function2[Int,Long,ClockEvent]) {
    val clk = Clock.systemClock
    val events = in.readObject.asInstanceOf[List[(Int,Long)]]
    for(e <- events) {
      Log.info(s"Loading event ${e._1} for cycle ${e._2} for $componentID")
      clk.schedule(f(e._1,e._2))
    }
  }
  protected def loadMemory[T](mem:Array[T],in:ObjectInputStream) {
    val ram = in.readObject.asInstanceOf[Array[T]]
    if (ram.length != mem.length) throw new IOException(s"ROM/RAM length mismatch while loading $componentID.Expected ${mem.length}, found ${ram.length}")
    Array.copy(ram,0,mem,0,ram.length)
  }
}