package ucesoft.cbm.peripheral.vic

import java.awt._
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.image.{BufferedImage, MemoryImageSource}
import java.io.{File, ObjectInputStream, ObjectOutputStream}

import javax.imageio.ImageIO
import javax.swing._
import ucesoft.cbm.{CBMComponent, CBMComponentType, Clock, Log}

class Display(width: Int,height: Int, title: String, frame: JFrame,clk:Clock = Clock.systemClock) extends JComponent with MouseMotionListener with MouseListener with CBMComponent {
  val componentID = "Display"
  val componentType = CBMComponentType.OUTPUT_DEVICE  
  private[this] val dimension = new Dimension(0, 0)
  private[this] var dashIndex = 0
  private[this] val mouseZoomStartPoint,mouseZoomEndPoint = new Point
  private[this] val mouseZoomLineColors = Array(Color.WHITE,Color.YELLOW, Color.RED)
  private[this] var mouseZoomColorIndex = 0
  private[this] var mouseZoomEnabled = false
  private[this] var clipArea,zoomArea: Tuple2[Point, Point] = null
  private[this] var totalFrameCounter,frameCounter = 0L
  private[this] var framePerSecond = 0
  private[this] var ts = 0L
  private[this] var normalDisplayMem = Array.fill(width * height)(0xFF000000)
  private[this] var interlacedDisplayMem = Array.fill(width * height * 2)(0xFF000000)
  private[this] var ptrDisplayMem = normalDisplayMem
  private[this] var normalDisplayImage = new MemoryImageSource(width, height, normalDisplayMem, 0, width)
  private[this] var interlacedDisplayImage = new MemoryImageSource(width, height * 2, interlacedDisplayMem, 0, width)
  private[this] var displayImage = normalDisplayImage
  private[this] var normalScreen = {
    normalDisplayImage.setAnimated(true);
    normalDisplayImage.setFullBufferUpdates(false)
    createImage(normalDisplayImage)
  }
  private[this] var interlacedScreen = {
    interlacedDisplayImage.setAnimated(true);
    interlacedDisplayImage.setFullBufferUpdates(false)
    createImage(interlacedDisplayImage)
  }
  private[this] var screen = normalScreen
  private[this] var drawRasterLine = false
  private[this] var rasterLine = 0
  private[this] var lpX, lpY = 0
  private[this] var zoomFactorX,zoomFactorY = 0.0
  private[this] var remote : ucesoft.cbm.remote.RemoteC64 = _
  private[this] var showRemotingLabel = true
  private[this] var interlaced = false
  private[this] var renderingHints = RenderingHints.VALUE_INTERPOLATION_BILINEAR

  private[this] var rotationAngleRad = 0.0
  private[this] var flipX,flipY = false

  addMouseMotionListener(this)
  addMouseListener(this)

  def setRotationAngle(angleInDeg:Double) : Unit = {
    rotationAngleRad = math.toRadians(angleInDeg)
    repaint()
  }

  def setFlipXY(flipX:Boolean,flipY:Boolean) : Unit = {
    this.flipX = flipX
    this.flipY = flipY
    repaint()
  }

  def setNewResolution(height:Int,width:Int) {
    Log.debug(s"New resolution: $width x $height")
    normalDisplayMem = Array.fill(width * height)(0xFF000000)
    interlacedDisplayMem = Array.fill(width * height * 2)(0xFF000000)
    normalDisplayImage = new MemoryImageSource(width, height, normalDisplayMem, 0, width)
    interlacedDisplayImage = new MemoryImageSource(width, height * 2, interlacedDisplayMem, 0, width)
    normalDisplayImage.setAnimated(true);
    normalDisplayImage.setFullBufferUpdates(false)
    normalScreen = createImage(normalDisplayImage)
    interlacedDisplayImage.setAnimated(true);
    interlacedDisplayImage.setFullBufferUpdates(false)
    interlacedScreen = createImage(interlacedDisplayImage)
    setInterlaceMode(interlaced)
  }
  
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
  
  @inline private def isZoomEvent(e:MouseEvent) : Boolean = {
    import java.awt.event.InputEvent._
    val zoomMod = SHIFT_DOWN_MASK | CTRL_DOWN_MASK
    (e.getModifiersEx & zoomMod) == zoomMod
  }
  def mouseClicked(e:MouseEvent) {}
  def mousePressed(e:MouseEvent) {
    if (isZoomEvent(e)) {
      if (SwingUtilities.isRightMouseButton(e)) {
        mouseZoomEnabled = false
        zoomArea = null
        repaint()
      }
      else {
        setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR))
        mouseZoomEnabled = true
        mouseZoomStartPoint.x = e.getX
        mouseZoomStartPoint.y = e.getY
        mouseZoomEndPoint.x = e.getX
        mouseZoomEndPoint.y = e.getY
      }
    }
  }
  def mouseReleased(e:MouseEvent) {
    if (mouseZoomEnabled) {
      setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
      mouseZoomEnabled = false
      val zoomR = zoomRec
      val interlacedFactor = if (interlaced) 2.0 else 1.0
      zoomArea = (new Point((zoomR.x / zoomFactorX).toInt, (zoomR.y / zoomFactorY * interlacedFactor).toInt), new Point(((zoomR.x + zoomR.width) / zoomFactorX).toInt,((zoomR.y + zoomR.height) / zoomFactorY * interlacedFactor).toInt))
      repaint()
    }
  }
  def mouseEntered(e:MouseEvent) {}
  def mouseExited(e:MouseEvent) {}
  
  // light pen events
  def mouseDragged(e:MouseEvent) { mouseMoved(e) }
  def mouseMoved(e:MouseEvent) {
    val mX = (e.getX / zoomFactorX).toInt
    val mY = (e.getY / zoomFactorY).toInt
    lpX =  mX + (if (clipArea != null) clipArea._1.x else 0)
    lpY = mY + (if (clipArea != null) clipArea._1.y else 0)
    if (mouseZoomEnabled) {
      mouseZoomEndPoint.x = e.getX
      mouseZoomEndPoint.y = e.getY
    }
  }
  
  def getLightPenX = lpX
  def getLightPenY = lpY
  
  def getFrameCounter = totalFrameCounter
  
  def getClipArea = clipArea
  def setRemote(remote:Option[ucesoft.cbm.remote.RemoteC64]) {
    remote match {
      case Some(r) => this.remote = r
      case None => this.remote = null
    }    
  }

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
      Log.debug(s"New screen dimension ${dimension.width} x ${dimension.height}")      
      zoomFactorX = dimension.width.toDouble / (if (clipArea != null) clipArea._2.x - clipArea._1.x else screen.getWidth(null))
      zoomFactorY = dimension.height.toDouble / (if (clipArea != null) clipArea._2.y - clipArea._1.y else screen.getHeight(null))
      //println(s"New screen dimension ${dimension.width} x ${dimension.height} width/height=${dimension.width.toDouble/dimension.height}")
    }
    // interpolation
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_INTERPOLATION,renderingHints)
    // rotation
    if (rotationAngleRad != 0) g.asInstanceOf[Graphics2D].rotate(rotationAngleRad,dimension.width >> 1,dimension.height >> 1)
    // clipping
    var clip : Tuple2[Point,Point] = null
    if (clipArea != null) clip = clipArea
    if (zoomArea != null) {
      if (clip == null) clip = zoomArea
      else 
      clip = (new Point(clip._1.x + zoomArea._1.x,clip._1.y + zoomArea._1.y),new Point(clip._1.x + zoomArea._2.x,clip._1.y + zoomArea._2.y)) 
    }
    // flipping
    if (flipX) clip = (new Point(clip._2.x,clip._1.y),new Point(clip._1.x,clip._2.y))
    if (flipY) clip = (new Point(clip._1.x,clip._2.y),new Point(clip._2.x,clip._1.y))

    if (clip == null) g.drawImage(screen, 0, 0, dimension.width, dimension.height, null)
    else g.drawImage(screen, 0, 0, dimension.width, dimension.height, clip._1.x, clip._1.y, clip._2.x, clip._2.y, null)

    if (drawRasterLine) {
      g.setColor(Color.RED)
      if (clip == null) g.drawLine(0, rasterLine, dimension.width, rasterLine)
      else //g.drawLine(0, ((rasterLine - clip._1.y) * zoomFactorY).toInt, dimension.width, ((rasterLine - clip._1.y) * zoomFactorY).toInt)
      g.fillRect(0, ((rasterLine - clip._1.y) * zoomFactorY).toInt, dimension.width,zoomFactorY.toInt)
    }
    if (mouseZoomEnabled) {
      if ((totalFrameCounter % 10) == 0) mouseZoomColorIndex = (mouseZoomColorIndex + 1) % mouseZoomLineColors.length
      g.setColor(mouseZoomLineColors(mouseZoomColorIndex))
      val zoomR = zoomRec   
      dashIndex = (dashIndex + 1) % 10
      val dash = new BasicStroke(2, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL,10, Array(10.0f),dashIndex)
      g.asInstanceOf[Graphics2D].setStroke(dash)
      g.drawRect(zoomR.x,zoomR.y,zoomR.width,zoomR.height)
    }
  }
  
  @inline private def zoomRec : Rectangle = {
    val zx = if (mouseZoomStartPoint.x < mouseZoomEndPoint.x) mouseZoomStartPoint.x else mouseZoomEndPoint.x
    val zy = if (mouseZoomStartPoint.y < mouseZoomEndPoint.y) mouseZoomStartPoint.y else mouseZoomEndPoint.y
    val dx = math.abs(mouseZoomStartPoint.x - mouseZoomEndPoint.x)
    val dy = math.abs(mouseZoomStartPoint.y - mouseZoomEndPoint.y)
    new Rectangle(zx,zy,dx,dy)
  }
  
  final def showFrame(x1: Int, y1: Int, x2: Int, y2: Int) {
    if (x1 != -1) {      
      displayImage.newPixels(x1, y1, x2, y2)      
      repaint()
      if (remote != null) remote.updateVideo(x1,y1,x2,y2)
    }
    else
    if (drawRasterLine || mouseZoomEnabled) repaint()
    
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

  def setPaused : Unit = {
    frame.setTitle(s"$title - paused")
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
  protected def allowsStateRestoring : Boolean = true
}