package ucesoft.cbm.peripheral.vic

import javax.swing._
import java.awt._
import java.awt.image.MemoryImageSource
import ucesoft.cbm.Log
import ucesoft.cbm.Clock
import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.event.MouseMotionListener
import java.awt.event.MouseEvent
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

class Display(width: Int,height: Int, title: String, frame: JFrame,clk:Clock = Clock.systemClock) extends JComponent with MouseMotionListener with CBMComponent {
  val componentID = "Display"
  val componentType = CBMComponentType.OUTPUT_DEVICE  
  private[this] val dimension = new Dimension(0, 0)
  private[this] var clipArea: Tuple2[Point, Point] = null
  private[this] var totalFrameCounter,frameCounter = 0L
  private[this] var framePerSecond = 0
  private[this] var ts = 0L
  private[this] val normalDisplayMem = Array.fill(width * height)(0xFF000000)
  private[this] val interlacedDisplayMem = Array.fill(width * height * 2)(0xFF000000)
  private[this] var ptrDisplayMem = normalDisplayMem
  private[this] val normalDisplayImage = new MemoryImageSource(width, height, normalDisplayMem, 0, width)
  private[this] val interlacedDisplayImage = new MemoryImageSource(width, height * 2, interlacedDisplayMem, 0, width)
  private[this] var displayImage = normalDisplayImage
  private[this] var debugImage: Image = _
  private[this] val normalScreen = {
    normalDisplayImage.setAnimated(true);
    normalDisplayImage.setFullBufferUpdates(false)
    createImage(normalDisplayImage)
  }
  private[this] val interlacedScreen = {
    interlacedDisplayImage.setAnimated(true);
    interlacedDisplayImage.setFullBufferUpdates(false)
    createImage(interlacedDisplayImage)
  }
  private[this] var screen = normalScreen
  private[this] var drawRasterLine = false
  private[this] var rasterLine = 0
  private[this] var lpX, lpY, mouseX, mouseY = 0
  private[this] var zoomFactorX,zoomFactorY = 0.0
  private[this] var remote : ucesoft.cbm.remote.RemoteC64 = _
  private[this] var showRemotingLabel = true
  private[this] var interlaced = false
  private[this] var renderingHints = RenderingHints.VALUE_INTERPOLATION_BICUBIC
  
  addMouseMotionListener(this)
  
  def setRenderingHints(hints:java.lang.Object) {
    renderingHints = hints
  }
  
  def displayMem : Array[Int] = ptrDisplayMem
  def setInterlaceMode(enabled:Boolean) {
    interlaced = enabled
    if (enabled) {
      ptrDisplayMem = interlacedDisplayMem
      displayImage = interlacedDisplayImage
      screen = interlacedScreen
    }
    else {
      ptrDisplayMem = normalDisplayMem
      displayImage = normalDisplayImage
      screen = normalScreen
    }
  }
  
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
  
  def getFrameCounter = totalFrameCounter
  
  def getClipArea = clipArea
  def setRemote(remote:Option[ucesoft.cbm.remote.RemoteC64]) {
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
      //println(s"New screen dimension ${dimension.width} x ${dimension.height} width/height=${dimension.width.toDouble/dimension.height}")
    }
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_INTERPOLATION,renderingHints)
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
    totalFrameCounter += 1
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
      frame.setTitle(title + " - " + remoting + framePerSecond + "fps - " + clk.getLastPerformancePerc + "%")
    }
  }

  def lastFramePerSecondCounter = framePerSecond

  def saveSnapshot(file: File) {
    val snap = createImage(getSize().width, getSize().height).asInstanceOf[BufferedImage]
    paint(snap.getGraphics)
    ImageIO.write(snap, "png", file)
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) {}
  protected def loadState(in:ObjectInputStream) {}
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}