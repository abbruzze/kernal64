package ucesoft.cbm.remote

import java.awt.{Color, Dimension, Graphics, Image}
import java.awt.event.{KeyEvent, KeyListener}
import java.awt.image.MemoryImageSource
import java.io._
import java.net.Socket
import javax.swing._

object RemoteC64Client extends App {
  if (args.length != 2) {
    println("Usage: <host> <port>")
    sys.exit(1)
  }
  try {
    val video = new RemoteC64Frame("Remote Kernal64")
    video.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)  
    video.setVisible(true)
    
    val server = new RemoteC64Client(args(0),args(1).toInt,video)
    server.listen()
  }
  catch {
    case t:Throwable =>
      t.printStackTrace()
      sys.exit(1)
  }
}

class RemoteC64Client(host:String,port:Int,video:RemoteC64Frame) extends KeyListener {
  private[this] var out : DataOutputStream = _
  private[this] val socket = new Socket(host,port)
  
  video.addKeyListener(this)
  
  private def sendKey(e:KeyEvent) : Unit = {
    out.writeChar('K')
    out.writeInt(e.getID)
    out.writeInt(e.getModifiers)
    out.writeInt(e.getKeyCode)
    out.writeChar(e.getKeyChar)
    out.writeInt(e.getKeyLocation)
    out.flush()
  }
  
  def keyPressed(e:KeyEvent) : Unit = {
    if (out != null) sendKey(e)    
  }
  def keyReleased(e:KeyEvent) : Unit = {
    if (out != null) sendKey(e)
  }
  def keyTyped(e:KeyEvent) : Unit = {}
  
  def listen() : Unit = {
    println("Connection established ...")
    try {
      val in = new DataInputStream(new BufferedInputStream(socket.getInputStream))
      out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
      
      var videoBuffer:Array[Int] = null
      
      var i = 0
      var w = 0
      while (true) {
        val cmd = in.readChar
        cmd match {
          case 'W' =>
            w = in.readInt
            val h = in.readInt
            videoBuffer = Array.ofDim[Int](w * h)
            video.initImage(w, h,videoBuffer)
          case 'P' =>
            val x1 = in.readInt
            val y1 = in.readInt
            val x2 = in.readInt
            val y2 = in.readInt
            var y = y1 * w
            val yEnd = y2 * w
            while (y < yEnd) {
              var x = x1
              val ny = y + w
              y += x       
              while (x < x2) {
                videoBuffer(y) = in.readInt
                y += 1
                x += 1
              }
              y = ny
            }
          
            video.updateScreen(x1,y1,x2,y2)
          case _ =>
        }
      }
    }
    catch {
      case _:IOException =>
        sys.exit(0)
    }
  }
}

class RemoteC64Frame(title:String) extends JFrame(title) {
  private[this] var imageSource : MemoryImageSource = _
  private[this] var screen : Image = _  
  private[this] var fc = 0
  private[this] var ts = 0L
  private[this] val panel = new ImagePanel
  
  getContentPane.add("Center",panel)
  
  def initImage(w:Int,h:Int,imageBuffer:Array[Int]) : Unit = {
    if (imageSource == null) {
      imageSource = new MemoryImageSource(w, h, imageBuffer, 0, w)
      imageSource.setAnimated(true);
      imageSource.setFullBufferUpdates(true)
      screen = createImage(imageSource)      
      panel.setPreferredSize(new Dimension(w,h))      
      pack()
    }
  }
  
  def updateScreen(x1:Int,y1:Int,x2:Int,y2:Int) : Unit = {
    imageSource.newPixels(x1,y1,x2,y2)
    repaint()
  }
  
  private class ImagePanel extends JComponent {
    override def paint(g:Graphics) : Unit = {
      if (screen != null) {
        g.drawImage(screen,0,0,getWidth,getHeight,null)
        fc += 1
        val now = System.currentTimeMillis
        if (now - ts >= 1000) {
          ts = now
          setTitle(s"$title - $fc fps")
          fc = 0
        }
      }
      else {
        g.setColor(Color.BLACK)
        g.drawString("Waiting connection...",0,20)
      }
    }    
  }
}