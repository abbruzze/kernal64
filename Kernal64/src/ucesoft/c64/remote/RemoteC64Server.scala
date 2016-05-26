package ucesoft.c64.remote

import java.io._
import java.net.Socket
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import javax.swing.SwingUtilities
import java.net.ServerSocket
import ucesoft.c64.Log

class RemoteC64Server(port:Int,keyListeners:List[KeyListener],videoBuffer:Array[Int],w:Int,h:Int,c1x:Int,c1y:Int,c2x:Int,c2y:Int,FR:Int = 1) extends Thread("C64Remote") with RemoteC64 {
  @volatile private[this] var sending = false
  private[this] val lock = new Object
  private[this] val width = c2x - c1x
  private[this] val height = c2y - c1y  
  private[this] var in : DataInputStream = _
  private[this] var x1,y1,x2,y2 = 0
  private[this] var fc = 0
  private[this] val dummyComponent = new javax.swing.JPanel
  private[this] var running = false
  private[this] var socket : Socket = _
  private[this] var connected = false
  private[this] val ss = new ServerSocket(port)
  
  private class KBThread extends Thread {
    override def run {
      while (!isInterrupted) {
        while (in == null) Thread.sleep(1000)
        
        try {
          in.readChar match {
            case 'K' =>
              val event = new KeyEvent(dummyComponent,in.readInt,System.currentTimeMillis,in.readInt,in.readInt,in.readChar,in.readInt)
              SwingUtilities.invokeLater(new Runnable {
                def run {
                  event.getID match {
                    case KeyEvent.KEY_PRESSED =>
                      for(kl <- keyListeners) kl.keyPressed(event)
                    case KeyEvent.KEY_RELEASED =>
                      for(kl <- keyListeners) kl.keyReleased(event)
                    case _ =>
                  }  
                }
              })              
            case _ =>
          }
        }
        catch {
          case _:IOException =>
            in = null
        }        
      }
    }
  }
  
  def updateVideo(x1:Int,y1:Int,x2:Int,y2:Int) {
    if (!sending) {     
      sending = true
      if (fc == 0) {
        this.x1 = c1x//x1
        this.y1 = c1y//y1
        this.x2 = c2x//x2
        this.y2 = c2y//y2
        fc = 0
      }
      else {
        this.x1 = x1
        this.y1 = y1
        this.x2 = x2 + 1
        this.y2 = y2 + 1
      }
      fc = (fc + 1) % FR
      lock.synchronized { lock.notify } 
    }
  }  
  
  def stopRemoting {
    running = false
    interrupt;
  }
  
  def isConnected : Boolean = connected
  
  override def run {
    running = true
    val keyb = new KBThread
    keyb.start
    
    while (running) {      
      var out : DataOutputStream = null      
      
      while (!connected && running) {
        try {          
          Log.info(s"Accepting remote connection on port $port ...")
          socket = ss.accept
          in = new DataInputStream(new BufferedInputStream(socket.getInputStream))
          out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
          Log.info(s"Remote connection accepted from ${socket.getInetAddress.getHostAddress}")
          connected = true
        }
        catch {
          case io:IOException =>
            Log.info(s"Waiting for connection on port:$port ...")
            Thread.sleep(10000)            
        }        
      }
      
      var y = 0
      var firstSend = true
      while (running && connected) {
        try {
          if (!sending) lock.synchronized {
            while (!sending) lock.wait
          } 
          
          // video          
          if (firstSend) {
            out.writeChar('W')
            out.writeInt(width)
            out.writeInt(height)
            firstSend = false
          }
          else {
            out.writeChar('P')
            out.writeInt(if (x1 - c1x < 0) 0 else x1 - c1x)
            out.writeInt(y1 - c1y)
            out.writeInt(x2 - c1x)
            out.writeInt(y2 - c1y)
            y = y1 * w
            val yEnd = y2 * w
            while (y < yEnd) {
              var x = x1
              val ny = y + w
              y += x       
              while (x < x2) {
                out.writeInt(videoBuffer(y))
                y += 1
                x += 1
              }
              y = ny
            }
          }
          sending = false
          out.flush
        }
        catch {
          case t:Throwable =>
            Log.info("Connection lost")
            connected = false
        }
      }
    }
    try {
      if (socket != null) socket.close
      ss.close
      keyb.interrupt
    }
    catch {
      case t:Throwable =>
    }
  }
}