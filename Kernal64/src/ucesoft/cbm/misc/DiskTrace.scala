package ucesoft.cbm.misc

import ucesoft.cbm.Clock

import java.io.{FileWriter, IOException, PrintWriter}

object DiskTrace {
  private var trace : PrintWriter = _
  private val clk = Clock.systemClock

  def setTrace(file:String): Unit = {
    try {
      trace = new PrintWriter(new FileWriter(file,false))
      trace.println(s"Kernal64 disk trace started at ${new java.util.Date}")
    }
    catch {
      case io:IOException =>
        println(s"Cannot enable disk tracing on file $file: $io")
    }
  }

  def trace(log:String): Unit = if (trace != null) trace.println("[%10d]\t%s".format(clk.currentCycles,log))

  def close(): Unit = if (trace != null) {
    trace.close()
    trace = null
  }

  def isEnabled(): Boolean = trace != null
}
