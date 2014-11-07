package ucesoft.c64

import scala.collection.mutable.ListBuffer
import java.util.Properties

object C64ComponentType extends Enumeration {
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
}

trait C64Component {
  val componentID : String
  val componentType : C64ComponentType.Type
  protected val properties = new Properties  
  private[this] val _components = new ListBuffer[C64Component]
  
  def getProperties = properties
  
  final def add(c:C64Component) {
    val alreadyAdded = _components exists { _.componentID == c.componentID }
    if (!alreadyAdded) _components += c
  }
  
  def reset
  def init
  
  final def change(oldComponent:C64Component,newComponent:C64Component) {
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
    def print(c:C64Component,ind:Int) {
      println(("\t" * ind) + c.componentID + " - " + c.componentType)
      c.components foreach { c => print(c,ind + 1) }
    }
    print(this,0)
  }
}