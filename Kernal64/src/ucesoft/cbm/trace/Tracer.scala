package ucesoft.cbm.trace

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.trace.TraceListener.{BreakType, StepType}

object Tracer {
  trait TracerListener {
    def stepInto(pc: Int): Unit
  }
  trait TracedDisplay {
    def getRasterLineAndCycle(): (Int,Int)
    def setDisplayRasterLine(line:Int): Unit
    def enableDisplayRasterLine(enabled:Boolean): Unit
  }

  case class TracedDevice(id:String,mem:Memory,listener:TraceListener,default:Boolean = false)
}

trait Tracer {
  import Tracer._
  def enableTracing(enabled:Boolean): Unit
  def isTracing(): Boolean
  def setBrk(brk:BreakType) : Unit

  def addListener(tl: TracerListener): Unit
  def removeListener(tl: TracerListener): Unit

  def setVisible(on:Boolean): Unit

  def addDevice(device:TracedDevice): Unit

  def removeDevice(device:TracedDevice): Unit

  def setDisplay(display:TracedDisplay): Unit

  def executeCommand(cmd:String): String

  def selectDevice(id:String): Boolean

  def step(stepType: StepType): Unit
}
