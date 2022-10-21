package ucesoft.cbm.trace

import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object TraceListener {
  trait BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean
  }

  case class BreakEqual(address: Int) extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = this.address == address

    override def toString: String = f"breaks at $address%04X"
  }

  case class BreakSet(addressSet: Set[Int]) extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = addressSet.contains(address)

    override def toString: String = f"breaks in ${addressSet.map(a => f"$a%04X").mkString("{ ", ",", " }")}"
  }

  case class BreakIn(fromAddress: Int, toAddress: Int) extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = address >= fromAddress && address <= toAddress

    override def toString: String = f"breaks between $fromAddress%04X and $toAddress%04X"
  }

  case class BreakOut(fromAddress: Int, toAddress: Int) extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = address < fromAddress || address > toAddress

    override def toString: String = f"breaks out of $fromAddress%04X and $toAddress%04X"
  }

  object NoBreak extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean) = false

    override def toString: String = "no breaks set"
  }

  object IRQBreak extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = irq

    override def toString: String = "breaks if IRQ"
  }

  object NMIBreak extends BreakType {
    def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = nmi

    override def toString: String = "breaks if NMI"
  }

  object BreakType {

    private def s2a(address: String) = address(0) match {
      case '$' => Integer.parseInt(address.substring(1), 16)
      case '%' => Integer.parseInt(address.substring(1), 2)
      case _ => address.toInt
    }

    def makeBreak(cmd: String): BreakType = {
      if (cmd.startsWith("in")) { // BreakIn
        val range = cmd.substring(2).trim.split(",")
        if (range.length != 2) throw new IllegalArgumentException("Bad range in the inclusive break type")
        return BreakIn(s2a(range(0)), s2a(range(1)))
      }
      if (cmd.startsWith("out")) { // BreakOut
        val range = cmd.substring(3).trim.split(",")
        if (range.length != 2) throw new IllegalArgumentException("Bad range in the out of range break type")
        return BreakOut(s2a(range(0)), s2a(range(1)))
      }
      if (cmd.startsWith("irq")) return IRQBreak
      if (cmd.startsWith("nmi")) return NMIBreak

      try {
        val addresses = cmd.split(",")
        if (addresses.length == 1) BreakEqual(s2a(cmd))
        else {
          val set = addresses map s2a toSet

          BreakSet(set)
        }
      }
      catch {
        case _: Exception =>
          throw new IllegalArgumentException("Bad break type")
      }
    }
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

  def setTraceOnFile(out:PrintWriter,enabled:Boolean) : Unit
  def setTrace(traceOn:Boolean) : Unit
  def step(updateRegisters: CpuStepInfo => Unit,stepType: StepType) : Unit
  def setBreakAt(breakType:BreakType,callback:CpuStepInfo => Unit) : Unit
  def jmpTo(pc:Int) : Unit
  def disassemble(address:Int) : DisassembleInfo
  def setCycleMode(cycleMode:Boolean) : Unit
}