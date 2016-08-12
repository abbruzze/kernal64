package ucesoft.c64.peripheral.vic

import javax.swing._
import java.awt._
import java.awt.image.MemoryImageSource
import ucesoft.c64.Log
import ucesoft.c64.Clock
import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.event.MouseMotionListener
import java.awt.event.MouseEvent
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType

class Display(width: Int,height: Int, title: String, frame: JFrame) extends JComponent with MouseMotionListener with C64Component {
  val componentID = "Display"
  val componentType = C64ComponentType.OUTPUT_DEVICE  
  private[this] val dimension = new Dimension(0, 0)
  private[this] var clipArea: Tuple2[Point, Point] = null
  private[this] var frameCounter = 0L
  private[this] var framePerSecond = 0
  private[this] var ts = 0L
  val displayMem = Array.fill(width * height)(0xFF000000)
  private[this] val displayImage = new MemoryImageSource(width, height, displayMem, 0, width)
  private[this] var debugImage: Image = _
  private[this] val screen = {
    displayImage.setAnimated(true);
    displayImage.setFullBufferUpdates(true)
    createImage(displayImage)
  }
  private[this] var drawRasterLine = false
  private[this] var rasterLine = 0
  private[this] var lpX, lpY, mouseX, mouseY = 0
  private[this] var zoomFactorX,zoomFactorY = 0.0
  private[this] var remote : ucesoft.c64.remote.RemoteC64 = _
  private[this] var showRemotingLabel = true
  
  addMouseMotionListener(this)
  
  def init {}
  def reset {}
  
  // light pen events
  def mouseDragged(e:MouseEvent) { mouseMoved(e) }
  def mouseMoved(e:MouseEvent) {
    mouseX = (e.getX / zoomFactorX).toInt
    mouseY = (e.getY / zoomFactorY).toInt
    lpX =  mouseX + (if (clipArea != null) clipArea._1.x else 0)
    lpY = mouseY + (if (clipArea != null) clipArea._1.y else 0)
  }
  
  def getLightPenX = lpX
  def getLightPenY = lpY
  
  def getMouseX = mouseX
  def getMouseY = mouseY
  
  def getClipArea = clipArea
  def setRemote(remote:Option[ucesoft.c64.remote.RemoteC64]) {
    remote match {
      case Some(r) => this.remote = r
      case None => this.remote = null
    }    
  }
  def getDisplayDimension = new Dimension(width,height)

  def setClipArea(x1: Int, y1: Int, x2: Int, y2: Int) {
    clipArea = (new Point(x1, y1), new Point(x2, y2))
  }

  final override def update(g: Graphics) {
    paint(g)
  }

  def setDrawRasterLine(drawRasterLine: Boolean) = this.drawRasterLine = drawRasterLine
  def setRasterLineAt(rasterLine: Int) = {
    this.rasterLine = rasterLine
    repaint()
  }
  def getRasterLine = rasterLine

  override final def paint(g: Graphics) {
    if (dimension.width != getWidth || dimension.height != getHeight) {
      dimension.width = getWidth
      dimension.height = getHeight
      if (debugImage == null) debugImage = createImage(width, height)
      Log.debug(s"New screen dimension ${dimension.width} x ${dimension.height}")      
      zoomFactorX = dimension.width.toDouble / (if (clipArea != null) clipArea._2.x - clipArea._1.x else screen.getWidth(this))
      zoomFactorY = dimension.height.toDouble / (if (clipArea != null) clipArea._2.y - clipArea._1.y else screen.getHeight(this))
      println(s"New screen dimension ${dimension.width} x ${dimension.height} ${zoomFactorX} x ${zoomFactorY}")
    }
    val srcImage = if (drawRasterLine) {
      val dg = debugImage.getGraphics
      dg.drawImage(screen, 0, 0, null)
      dg.setColor(Color.RED)
      dg.drawLine(0, rasterLine, screen.getWidth(null), rasterLine)
      debugImage
    } else screen
    if (clipArea == null) g.drawImage(srcImage, 0, 0, dimension.width, dimension.height, null)
    else g.drawImage(srcImage, 0, 0, dimension.width, dimension.height, clipArea._1.x, clipArea._1.y, clipArea._2.x, clipArea._2.y, null)
  }
  
  final def showFrame(x1: Int, y1: Int, x2: Int, y2: Int) {
    if (x1 != -1) {
      displayImage.newPixels(x1, y1, x2, y2)      
      repaint()
      if (remote != null) remote.updateVideo(x1,y1,x2,y2)
    }
    else
    if (drawRasterLine) repaint()
    
    frameCounter += 1
    val now = System.currentTimeMillis
    if (ts == 0 || now - ts > 1000) {
      framePerSecond = math.round(frameCounter / ((now - ts) / 1000.0)).toInt
      ts = now
      frameCounter = 0
      val remoting = if (remote == null) "" 
                     else 
                     if (remote.isConnected) { 
                        showRemotingLabel = !showRemotingLabel
                        if (showRemotingLabel) "(R) " else "    "
                     }
                     else "(?) "
      frame.setTitle(title + " - " + remoting + framePerSecond + "fps - " + Clock.systemClock.getLastPerformancePerc + "%")
    }
  }

  def lastFramePerSecondCounter = framePerSecond

  def saveSnapshot(file: File) {
    val snap = createImage(getSize().width, getSize().height).asInstanceOf[BufferedImage]
    paint(snap.getGraphics)
    ImageIO.write(snap, "png", file)
  }
}