package ucesoft.cbm.trace

import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object TraceListener {
  case class BreakAccessType(read: Boolean, write: Boolean, execute: Boolean) {
    override def toString = {
      val r = if (read) "R" else ""
      val w = if (write) "W" else ""
      val x = if (execute) "X" else ""
      s"$r$w$x"
    }

    def hasAccess(other:BreakAccessType): Boolean = read && other.read || write && other.write || execute && other.execute
  }
  val ReadBreakAccess = BreakAccessType(true,false,false)
  val WriteBreakAccess = BreakAccessType(false,true,false)
  val ExecuteBreakAccess = BreakAccessType(false,false,true)

  sealed trait BreakInfo {
    private var _enabled = true
    def enabled: Boolean = _enabled
    def enabled_=(e:Boolean): Unit = _enabled = e
  }
  case class AddressBreakInfo(address:Int,access:BreakAccessType) extends BreakInfo
  sealed trait EventBreakInfo extends BreakInfo
  case class IRQBreakInfo() extends EventBreakInfo { override def toString = "IRQ" }
  case class NMIBreakInfo() extends EventBreakInfo { override def toString = "NMI" }
  case class ResetBreakInfo() extends EventBreakInfo { override def toString = "RESET" }

  trait BreakType {
    def isBreak(info:BreakInfo): Boolean
  }

  case class BreakSet(addressSet: Set[Int]) extends BreakType {
    def isBreak(info:BreakInfo): Boolean = {
      info match {
        case AddressBreakInfo(address, access) if access.execute =>
          addressSet.contains(address)
        case _ =>
          false
      }
    }

    override def toString: String = f"breaks in ${addressSet.map(a => f"$a%04X").mkString("{ ", ",", " }")}"
  }

  object NoBreak extends BreakType {
    def isBreak(info:BreakInfo) = false

    override def toString: String = "no breaks set"
  }

  trait TraceRegisterBuilder {
    def add(name:String,value:String): TraceRegisterBuilder
    def addIf(cond:Boolean,name:String,value:String): TraceRegisterBuilder
    def build(): List[TraceRegister]
  }
  object TraceRegister {
    def builder(): TraceRegisterBuilder = new TraceRegisterBuilder {
      val regs = new ListBuffer[TraceRegister]
      override def add(name: String, value: String): TraceRegisterBuilder = {
        regs += TraceRegister(name,value)
        this
      }
      override def addIf(cond:Boolean,name:String,value:String): TraceRegisterBuilder = {
        if (cond) add(name,value) else this
      }
      override def build(): List[TraceRegister] = regs.toList
    }
  }
  case class TraceRegister(name:String,value:String)
  case class CpuStepInfo(pc: Int, registers: List[TraceRegister],disassembled:String)

  sealed trait StepType
  case object StepIn extends StepType
  case object StepOut extends StepType
  case object StepOver extends StepType

  case class DisassembleInfo(dis:String,bytes:Int)

}

trait TraceListener {
  import TraceListener._
  val cycleModeSupported = false
  val supportTracing = true

  def setTraceOnFile(out:PrintWriter,enabled:Boolean) : Unit
  def setTrace(traceOn:Boolean) : Unit
  def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType) : Unit
  def setBreakAt(breakType:BreakType,callback:CpuStepInfo => Unit) : Unit
  def jmpTo(pc:Int) : Unit
  def disassemble(address:Int) : DisassembleInfo
  def setCycleMode(cycleMode:Boolean) : Unit
}