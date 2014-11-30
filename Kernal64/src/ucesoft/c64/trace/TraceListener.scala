package ucesoft.c64.trace

sealed trait BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) : Boolean
}
case class BreakEqual(address:Int) extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = this.address == address
}
case class BreakIn(fromAddress:Int,toAddress:Int) extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = address >= fromAddress && address <= toAddress
}
case class BreakOut(fromAddress:Int,toAddress:Int) extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = address < fromAddress || address > toAddress
}
object NoBreak extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = false
}
object IRQBreak extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = irq
}
object NMIBreak extends BreakType {
  def isBreak(address:Int,irq:Boolean,nmi:Boolean) = nmi
}

object BreakType {
  
  private def s2a(address: String) = address(0) match {
    case '$' => Integer.parseInt(address.substring(1), 16)
    case '%' => Integer.parseInt(address.substring(1), 2)
    case _ => address.toInt
  }
  
  def makeBreak(cmd:String) : BreakType = {
    val c = cmd.charAt(0)
    if (c == '=' || c == '$' || c == '%' || c.isDigit) { // BreakEqual
      if (c == '=') return BreakEqual(s2a(cmd.substring(1).trim))
      return BreakEqual(s2a(cmd))
    }
    if (cmd.startsWith("in")) { // BreakIn
      val range = cmd.substring(2).trim.split(",")
      if (range.length != 2) throw new IllegalArgumentException("Bad range in the inclusive break type")
      return BreakIn(s2a(range(0)),s2a(range(1)))
    }
    if (cmd.startsWith("out")) { // BreakOut
      val range = cmd.substring(3).trim.split(",")
      if (range.length != 2) throw new IllegalArgumentException("Bad range in the out of range break type")
      return BreakOut(s2a(range(0)),s2a(range(1)))
    }
    if (cmd.startsWith("irq")) return IRQBreak
    if (cmd.startsWith("nmi")) return NMIBreak
    
    throw new IllegalArgumentException("Bad break type")
  }
}

trait TraceListener {
  def setTrace(traceOn:Boolean)
  def step(updateRegisters: (String) => Unit)
  def setBreakAt(breakType:BreakType,callback:(String) => Unit)
  def jmpTo(pc:Int)
}