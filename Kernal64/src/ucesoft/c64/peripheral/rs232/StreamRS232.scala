package ucesoft.c64.peripheral.rs232

import java.io.OutputStream
import java.io.InputStream
import ucesoft.c64.Log
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent

abstract class StreamRS232 extends AbstractRS232 with Runnable {
  private[this] var out : OutputStream = _
  private[this] var in : InputStream = _
  private[this] var thread : Thread = _
  
  override def setEnabled(enabled:Boolean) {
    super.setEnabled(enabled)
    if (!enabled) {
      if (thread != null) thread.interrupt      
    }
    else {
      dcd = RS232.DCD
      thread = new Thread(this,"StreamRS232")
      thread.start
    }
  }
  
  def setStreams(in:InputStream,out:OutputStream) {
    this.in = in
    this.out = out
  }
  
  def getStreams = (in,out)
  
  protected def sendOutByte(byte:Int) {
    try {
      out.write(byte)
      //print(byte.toChar)
      out.flush
    }
    catch {
      case t:Throwable =>
        Log.info(s"I/O error while writing from rs-232 ($componentID): " + t)
        t.printStackTrace
        setEnabled(false)
    }
  }
  
  override def reset {
    if (isEnabled) {
      thread.interrupt
      setEnabled(false)
    }
  }
  
  protected def canSend = rts && dtr
  
  def run {
    val clk = Clock.systemClock
    val waitLock = new Object
    var waitingClock = false
    while (isEnabled) {
      try {
        val byte = in.read
        //if (byte != -1) println("<" + byte.toChar)
        while (!canSend) Thread.sleep(1)
        
        if (byte != -1) {
          sendInByte(byte)
          waitingSending
          // wait some cycles before send another byte
          waitingClock = true
          clk.scheduleExternal(new ClockEvent("RS-232 receiveByte",clk.currentCycles + 10000, cycles => {
            waitLock.synchronized {
              waitingClock = false
              waitLock.notify
            }
          }))
          while (waitingClock) waitLock.synchronized { waitLock.wait }    
          }
        }
      catch {
        case i:InterruptedException =>
        case t:Throwable =>
          Log.info(s"I/O error while reading from rs-232 ($componentID): " + t)
          t.printStackTrace
          setEnabled(false)
      }
    }
  }

}