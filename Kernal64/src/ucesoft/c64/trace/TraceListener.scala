package ucesoft.c64.trace

trait TraceListener {
  def setTrace(traceOn:Boolean)
  def step(updateRegisters: (String) => Unit)
  def setBreakAt(address:Int,callback:() => Unit)
  def jmpTo(pc:Int)
}