package ucesoft.cbm.peripheral.crtc

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.peripheral.vic.Display

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object CRTC6845 {
  final val SCREEN_HEIGHT = 312 // PAL rows
  final val SCREEN_WIDTH = (SCREEN_HEIGHT * 2 * 4 / 3.0).toInt
  final val PREFERRED_FRAME_SIZE = new java.awt.Dimension(SCREEN_WIDTH,SCREEN_HEIGHT * 2)

  private object VideoMode extends Enumeration {
    val IDLE: VideoMode.Value = Value
    val TEXT: VideoMode.Value = Value
    val BLANK: VideoMode.Value = Value
  }
}

/**
  0/$00 Total number of horizontal character positions
  l/$01 Number of visible horizontal character positions
  2/$02 Horizontal sync position
  3/$03 Horizontal and vertical sync width
  4/$04 Total number of screen rows
  5/$05 Vertical fine adjustment
  6/$06 Number of visible screen rows
  7/$07 Vertical sync position
  8/$08 Interlace mode control register
  9/$09 Number of scan lines per character
  10/$OA Cursor mode control
  ll/$0B Ending scan line foT cursor
  12/$0C Screen memory starting address (high byte)
  13/$0D Screen memory starting address (low byte)
  14/$0E Cursor position address (high byte)
  15/$0F Cursor position address (low byte)
  16/$10 Light pen vertical position
  17/$11 Light pen horizontal position
 */
class CRTC6845(ram:Array[Int],var charRom:Array[Int],bytes_per_char:Int,retraceListener:Boolean => Unit = null) extends RAMComponent {
  import CRTC6845._

  val name = "CRTC6845"
  val componentID = "CRTC6845"
  val isRom = false
  val startAddress = 0xD800
  val length = 0x2
  val componentType: Type = CBMComponentType.MEMORY
  val isActive = true

  final private[this] var X_LEFT_CLIP_COLS = 0 //20 * 8 + 5 * 8
  final private[this] var X_RIGHT_CLIP_COLS = 0 //8 * 8 + 5 * 8
  final private[this] var Y_TOP_CLIP_ROWS = 0 //2 * 14//1 * 8
  final private[this] var Y_BOTTOM_CLIP_ROWS = 0 //0 * 8//3 * 8

  final private[this] val MAX_HEIGHT = 1024
  final private[this] val MIN_HEIGHT = 200

  private[this] var charAddressMask = 0

  private[this] var clk = Clock.systemClock
  private[this] var address_reg = 0
  private[this] val regs = Array.ofDim[Int](18)
  private[this] var vblank = false
  private[this] val debug = false
  private[this] var display : Display = _
  private[this] var bitmap : Array[Int] = _
  private[this] var videoMode = VideoMode.IDLE
  private[this] var screenHeight,currentScreenHeight = SCREEN_HEIGHT
  private[this] var screenWidth = SCREEN_WIDTH
  private[this] var oneLineDrawn = false
  // =====================================================
  private[this] var cycles_per_line,cycles_per_line_accu = 0
  private[this] var xchars_total = 0                // REG 0 Horizontal Total
  private[this] var ychars_total = 0                // REG 9 Character Total Vertical
  private[this] var cursor_pos = 0                  // REG 14-5 Cursor location HI/LO
  private[this] var cursorOn = true
  private[this] var ram_adr = 0                     // REG 12/13 ram adr
  private[this] var ypos = 0
  private[this] var ram_base_ptr = 0
  // light pen
  private[this] var lpFlag = 0                      // 0 = no new values of lp coords, 0x40 new values
  private[this] var clkStartLine = 0L               // clock cycles when the last line begun
  // -----------------------------------------------------
  private[this] var rasterLine = 0
  private[this] var currentCharScanLine = 0
  private[this] var visibleScreenHeightPix = 0      // total Y pixels
  private[this] var visibleTextRows = 0             // total rows
  private[this] var borderWidth = 0
  private[this] var interlaceMode = false
  private[this] var geometryUpdateListener : String => Unit = _
  private[this] var updateGeometryOnNextFrame = false
  private[this] var rowCounter,rowCounterY = 0
  private[this] var verticalAdjFlag = 0
  private[this] var adaptScreenResolution = true
  // caches
  private[this] val gfxBuffer = Array.ofDim[Int](256)
  // -----------------------------------------------------
  private[this] var deinterlaceMode = true
  private[this] var frameBit = 0
  // COLOR PALETTE =======================================
  private[this] final val BACKGROUND_COLOR = 0xFF000000
  private[this] final val FOREGROUND_COLOR = 0xFF00FF00

  // Clock management ====================================
  def pause() : Unit = {
    //if (clk != Clock.systemClock) clk.pause
    clk.cancel("CRTC_TICK")
  }
  def play() : Unit = {
    //if (clk != Clock.systemClock) clk.play
    pause
    reschedule
  }
  @inline private def reschedule() : Unit = {
    cycles_per_line_accu += cycles_per_line
    val next_clk = cycles_per_line_accu >> 16
    cycles_per_line_accu -= next_clk << 16
    clk.schedule(new ClockEvent("CRTC_TICK",clk.currentCycles + next_clk,drawLine _))
  }
  def setOwnThread(freq:Int,_play:Boolean = true) : Unit = {
    pause
    clk = Clock.makeClock("CRTCClock",Some(errorHandler _))((cycles) => {})
    clk.setClockHz(freq)
    clk.play
    if (_play) play
  }
  def stopOwnThread() : Unit = {
    if (clk != Clock.systemClock) {
      clk.halt
      clk = Clock.systemClock
      play
    }
  }

  def setCharAddressMask(mask:Int): Unit = charAddressMask = mask

  def setClipping(left:Int,right:Int,top:Int,bottom:Int): Unit = {
    X_LEFT_CLIP_COLS = left << 3
    X_RIGHT_CLIP_COLS = right << 3
    Y_TOP_CLIP_ROWS = top
    Y_BOTTOM_CLIP_ROWS = bottom
  }

  def getRasterLine(): Int = rasterLine
  def getRasterCycle(): Int = 0

  // =====================================================
  def setAdaptScreenResolution(adapt:Boolean) : Unit = {
    adaptScreenResolution = adapt
    if (!adapt) adaptScreenTo(SCREEN_HEIGHT)
  }

  def setDeinterlaceMode(on:Boolean) : Unit = {
    deinterlaceMode = on
  }

  def setGeometryUpdateListener(geometryUpdateListener:(String) => Unit) : Unit = {
    this.geometryUpdateListener = geometryUpdateListener
  }

  def setDisplay(display:Display) : Unit = {
    this.display = display
    bitmap = display.displayMem
  }

  final def init : Unit = {
    regs(0) = 126
    regs(1) = 102
    xchars_total = regs(0) + 1
    cycles_per_line = 0
    recalculate_xsync
    regs(4) = 39
    regs(5) = 0
    regs(6) = 25
    ychars_total = 13
    regs(9) = 13
    interlaceMode = false
    display.setInterlaceMode(false)
    bitmap = display.displayMem
    currentScreenHeight = SCREEN_HEIGHT
    oneLineDrawn = true
    rowCounter = 0
    vblank = false
    setScanLines(SCREEN_HEIGHT)
  }

  final def reset : Unit = {
    init
    play()
  }

  override def hardReset(): Unit = {
    java.util.Arrays.fill(regs,0)
    reset
  }

  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    address & 1 match {
      case 0 => read_status
      case 1 => read_regs
    }
  }

  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    address & 1 match {
      case 0 =>
        address_reg = value & 0x3F
      case 1 =>
        write_regs(value)
    }
  }

  @inline private[this] def read_status : Int = lpFlag | (if (vblank) 0x20 else 0)

  @inline private[this] def read_regs : Int = {
    if (address_reg > 17) return 0xFF

    address_reg match {
      case 16|17 =>
        lpFlag = 0
        regs(address_reg)
      case 14|15 =>
        regs(address_reg)
      case _ =>
        0xFF
    }
  }

  @inline private[this] def write_regs(value:Int) : Unit = {
    // REG 16 & 17 (light pen) are read only
    //if (address_reg == 16 || address_reg == 17 || address_reg > 17 || regs(address_reg) == value) return

    regs(address_reg) = value
    address_reg match {
      case 0 => // REG 0 Horizontal Total
        xchars_total = value + 1
        if (debug) println(s"VDC: REG 0 Horizontal Total: $xchars_total")
        recalculate_xsync
        updateGeometry
      case 1 => // REG 1 Horizontal Displayed
        if (debug) println(s"VDC: REG 1 Horizontal Displayed: $value")
        updateGeometryOnNextFrame = true
      case 2 => // REG 2 Horizontal Sync Pos
        if (debug) println(s"VDC: REG 2 Horizontal Sync Pos: $value")
        updateGeometryOnNextFrame = true
      case 3 => // REG 3 Horizontal/Vertical Sync widths
        if (debug) println(s"VDC: REG 3 Horizontal/Vertical Sync widths: $value")
      case 4 => // REG 4 Vertical Total
        if (debug) println(s"VDC: REG 4 Vertical Total :$value")
      case 5 => // REG 5 Vertical Total Fine Adjust
        if (debug) println(s"VDC: REG 5 Vertical Total Fine Adjust :$value")
      case 6 => // REG 6 Vertical Displayed
        if (debug) println(s"VDC: REG 6 Vertical Displayed: $value")
        updateGeometry
      case 7 => // REG 7 Vertical Sync Position
        if (debug) println(s"VDC: REG 7 Vertical Sync Position: $value")
      case 8 => // REG 8 Interlace
        if (debug) println(s"VDC: REG 8 Interlace: $value")
        val newInterlaceMode = (value & 3) == 3
        if (newInterlaceMode != interlaceMode) {
          setInterlaceMode(newInterlaceMode)
          recalculate_xsync
        }
      case 9 => // REG 9 Character Total Vertical
        ychars_total = value & 0x1F
        updateGeometryOnNextFrame = true
        if (debug) println(s"VDC: REG 9 Character Total Vertical: $value ychars_total=$ychars_total bytes_per_char=$bytes_per_char rasterLine=$rasterLine vblank=$vblank")
      case 10 => // R10  Cursor Mode, Start Scan
        if (debug) println(s"VDC: R10  Cursor Mode, Start Scan: ${value & 0x1F} mode: ${(value & 0xE0) >> 5}")
      case 11 => // R11 Cursor End Scan
        if (debug) println(s"VDC R11 Cursor End Scan: $value")
      case 12|13 => // R12  Display Start Address hi, R13  Display Start Address lo
        if (debug) println(s"VDC: new Screen Address($address_reg): ${Integer.toHexString(regs(12) << 8 | regs(13))} rasterLine=$rasterLine vblank=$vblank")
      case 14|15 =>  // REG 14-5 Cursor location HI/LO
        cursor_pos = regs(14) << 8 | regs(15)
        if (debug) println(s"VDC: cursor pos: $cursor_pos")
      case _ =>
    }
  }

  private def setInterlaceMode(newInterlaceMode:Boolean) : Unit = {
    interlaceMode = newInterlaceMode
    display.setInterlaceMode(interlaceMode)
    bitmap = display.displayMem
    if (debug) {
      val nextFrameScreenHeight = if (interlaceMode) currentScreenHeight << 1 else currentScreenHeight
      println(s"Interlace mode changed to $interlaceMode nextFrameScreenHeight=$nextFrameScreenHeight")
    }
    updateGeometryOnNextFrame = true
  }

  // ============================================================================
  @inline private def recalculate_xsync() : Unit = {
    val charWidth = 8

    val exact_cycles_per_line = (xchars_total * charWidth) / 7.90 - 1
    val new_cycles_per_line = (exact_cycles_per_line * 65536).asInstanceOf[Int]
    if (new_cycles_per_line != cycles_per_line) {
      cycles_per_line = new_cycles_per_line
      cycles_per_line_accu = 0
      if (debug) println(s"VDC: cycles per line set to ${cycles_per_line >> 16}, xchars_total=$xchars_total charWidth=$charWidth")
    }
  }

  // =======================================================================

  private def errorHandler(t:Throwable) : Unit = {
    t.printStackTrace()
  }

  @inline private def latch_addresses(): Unit = {
    // update video address for the next frame
    ram_adr = regs(12) << 8 | regs(13)
    ram_base_ptr = ram_adr
  }

  @inline private def vsync() : Unit = {
    rasterLine = 0
    nextFrame
  }

  private def drawLine(cycles:Long) : Unit = {
    clkStartLine = cycles
    reschedule

    if (rowCounter == visibleTextRows) {
      vblank = true
      if (retraceListener != null) retraceListener(true)
    }
    if (rowCounter == visibleTextRows && currentCharScanLine == 0) latch_addresses

    if (rowCounter > regs(4)) {
      if (verticalAdjFlag == 2 || (regs(5) & 0x1F) == 0) {
        rowCounter = 0
        rowCounterY = 0
        verticalAdjFlag = 0
        currentCharScanLine = 0//regs(24) & 0x1F
        vblank = false
        if (retraceListener != null) retraceListener(false)
        ypos = 0
        latch_addresses
      }
      else verticalAdjFlag = 1
    }
    if (rowCounter == regs(7) && rowCounterY == 0) vsync

    val interlacing = !deinterlaceMode && interlaceMode
    val skipLineForInterlace = interlacing && ((rasterLine & 1) != frameBit)

    //if (!useCacheForNextFrame || interlacing) {
      oneLineDrawn = true

      if (vblank) drawTextLine(true)
      else
      if (skipLineForInterlace) {
        val vm = videoMode
        videoMode = VideoMode.IDLE
        drawTextLine(false)
        videoMode = vm
      }
      else {
        videoMode = VideoMode.TEXT
        drawTextLine(false)
      }
    //}
    // NEXT RASTER LINE =====================================================
    rasterLine += 1
    if (regs(7) > regs(4) && rasterLine >= screenHeight) {
      // hack used to force vsync when no vsync has been reached at the end of raster path
      if (!(regs(7) == regs(4) + 1 && regs(5) > 0)) vsync
    }

    val screenWidth = regs(1)
    if (!vblank && videoMode != VideoMode.IDLE) currentCharScanLine += 1

    if (currentCharScanLine > ychars_total) {
      currentCharScanLine = 0
      ypos += 1
      if (videoMode == VideoMode.TEXT) ram_base_ptr += screenWidth
    }

    rowCounterY += 1
    if (verticalAdjFlag == 1) {
      if (rowCounterY == (regs(5) & 0x1F)) verticalAdjFlag = 2
    }
    else
    if (rowCounterY > ychars_total) {
      rowCounterY = 0
      rowCounter += 1
    }
  }

  @inline private def updateGeometry() : Unit = {
    visibleTextRows = regs(6)
    visibleScreenHeightPix = visibleTextRows * (ychars_total + 1)
    val charWidth = 8

    val htotal = xchars_total
    val hdisplayed = regs(1)
    val hsync = regs(2)

    val BORDER = -2
    var lborder = htotal - hsync + BORDER
    var rborder = htotal - hdisplayed - lborder

    if (rborder < 0) rborder = 0
    if (lborder < 0) lborder = 0

    borderWidth = lborder * charWidth

    val newScreenWidth = htotal * charWidth

    if (newScreenWidth != screenWidth) {
      screenWidth = newScreenWidth
      display.setNewResolution(currentScreenHeight,screenWidth)
      bitmap = display.displayMem
      if (borderWidth < X_LEFT_CLIP_COLS) display.setClipArea(borderWidth,Y_TOP_CLIP_ROWS,screenWidth,screenHeight - Y_BOTTOM_CLIP_ROWS)
      else display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS * (if (interlaceMode) 2 else 1),screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS * (if (interlaceMode) 2 else 1))
      //println(s"New screen width: $screenWidth")
    }

    if (debug) println(s"New screen res. width=$screenWidth htotal=$htotal hdisplayed=$hdisplayed hsync=$hsync hsync_width=${(regs(3) & 0x0F) - 1} rborder=$rborder lborder=$lborder rester=$rasterLine")
    if (borderWidth < 0) borderWidth = 0
    if (geometryUpdateListener != null) {
      val charInfo = s"${charWidth}x${if (interlaceMode) (ychars_total + 1) >> 1 else (ychars_total + 1)}}"
      geometryUpdateListener(s"Text mode ${regs(1)}x$visibleTextRows $charInfo ${if (interlaceMode) "interlaced" else ""}")
    }
    if (debug) println(s"VDC: updated geometry. Interlaced=$interlaceMode ${regs(1) * 8}x$visibleScreenHeightPix new borderWidth=$borderWidth")
  }

  @inline private def nextFrame() : Unit = {
    frameBit = (frameBit + 1) & 1
    if (oneLineDrawn) display.showFrame(0,0,screenWidth,screenHeight)
    else display.showFrame(-1,-1,-1,-1)

    oneLineDrawn = false

    // cursor blinking
    val cursorMode = regs(10) & 0x60
    val cursor_change = if (cursorMode == 0x40) (display.getFrameCounter % 16) == 0 // 1/16
    else
    if (cursorMode == 0x60) (display.getFrameCounter % 32) == 0 // 1/32
    else false
    if (cursor_change) cursorOn = !cursorOn

    if (updateGeometryOnNextFrame) {
      updateGeometryOnNextFrame = false
      updateGeometry
    }
    // check new screen's height
    var newScreenHeight = (regs(4) + 1) * (ychars_total + 1) + (regs(5) & 0x1F)
    if (interlaceMode) newScreenHeight >>= 1
    val currentHeight = if (interlaceMode) screenHeight >> 1 else screenHeight
    // don't know if the screen height must be checked on every frame
    if (adaptScreenResolution && newScreenHeight > MIN_HEIGHT && newScreenHeight < MAX_HEIGHT && newScreenHeight != currentHeight) adaptScreenTo(newScreenHeight)
  }

  @inline private def adaptScreenTo(newScreenHeight:Int) : Unit = {
    setScanLines(newScreenHeight)
    if (borderWidth < X_LEFT_CLIP_COLS) display.setClipArea(borderWidth,Y_TOP_CLIP_ROWS,screenWidth,screenHeight - Y_BOTTOM_CLIP_ROWS)
    else display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS * (if (interlaceMode) 2 else 1),screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS * (if (interlaceMode) 2 else 1))
  }

  @inline private def fetchGFX() : Unit = {
    var c = 0
    while (c < regs(1) + 1) {
      gfxBuffer(c) = ram((ram_base_ptr + c) % ram.length)
      if (c >= 82) gfxBuffer(c - 41) = gfxBuffer(c)
      c += 1
    }
  }

  @inline private def drawTextLine(vsync:Boolean) : Unit = {
    val bitmapOffset = rasterLine * screenWidth

    var ram_ptr = ram_base_ptr
    var col = 0
    val charWidth = 8
    val rightBorderPix = borderWidth + regs(1) * charWidth
    var colPix = 0
    var char_col = 0
    val firstRowLine = currentCharScanLine == 0
    if (!vsync && firstRowLine) fetchGFX

    while (col < xchars_total) {
      val outRow = colPix < borderWidth || colPix >= rightBorderPix
      if (vsync || videoMode == VideoMode.IDLE || videoMode == VideoMode.BLANK || outRow) {
        var x = 0
        val bkc = BACKGROUND_COLOR //if (videoMode == VideoMode.BLANK || (videoMode == VideoMode.IDLE && !outRow)) PALETTE(0) else backgroundColor
        while (x < charWidth && colPix < screenWidth) {
          // check used to avoid index error when screen height is adjusting
          if (bitmapOffset + colPix < bitmap.length) bitmap(bitmapOffset + colPix) = bkc
          x += 1
          colPix += 1
        }
      }
      else {
        var reverse = false

        val charCode = gfxBuffer(char_col)
        val cursorMode = regs(10) & 0x60
        val cursorTopLine = regs(10) & 0x1F
        val cursorBottomLine = regs(11) & 0x1F
        val showCursor = ram_ptr == cursor_pos && cursorMode != 0x20

        ram_ptr += 1

        if (showCursor) {
          val isCursorLine = if (cursorTopLine < cursorBottomLine) currentCharScanLine >= cursorTopLine && currentCharScanLine <= cursorBottomLine
          else currentCharScanLine < cursorBottomLine || currentCharScanLine > cursorTopLine
          reverse ^= isCursorLine
          if (cursorMode != 0x00 && isCursorLine) reverse ^= cursorOn // cursor blinking
        }

        val char_ptr = charCode * bytes_per_char + currentCharScanLine
        val charBitmap = charRom(char_ptr | charAddressMask)
        var bitmapPtr = bitmapOffset + colPix
        var gfx = charBitmap
        var xpos = 0

        while (xpos < charWidth && colPix < rightBorderPix) {
          val bit = (gfx & 0x80) > 0
          val color = if (bit ^ reverse) FOREGROUND_COLOR else BACKGROUND_COLOR

          gfx <<= 1
          xpos += 1

          // check used to avoid index error when screen height is adjusting
          if (bitmapPtr < bitmap.length) bitmap(bitmapPtr) = color

          bitmapPtr += 1
          colPix += 1
        }
        char_col += 1
      }
      col += 1
    }
  }

  private def setScanLines(lines:Int) : Unit = {
    display.setNewResolution(lines,screenWidth)
    bitmap = display.displayMem
    currentScreenHeight = lines
    screenHeight = if (interlaceMode) lines << 1 else lines
    updateGeometryOnNextFrame = true
  }

  def triggerLightPen() : Unit = {
    lpFlag = 0x40
    regs(16) = ypos
    val delta = clk.currentCycles - clkStartLine
    regs(17) = ((delta / cycles_per_line.toDouble) * xchars_total).toInt
  }

  // =============== Properties ==========================
  override def getProperties: Properties = {
    properties.setProperty("Status",read_status.toString)
    properties.setProperty("Video mode",videoMode.toString)
    properties.setProperty("Cycles per line",cycles_per_line.toString)
    properties.setProperty("VBlank",vblank.toString)
    for(i <- 0 until regs.length) {
      properties.setProperty(s"Regs($i)",s"0x${regs(i).toHexString}")
    }
    super.getProperties
  }

  // state -----------------------------------------------
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(address_reg)
    out.writeBoolean(vblank)
    out.writeObject(ram)
    out.writeObject(regs)
    out.writeInt(videoMode.id)
    out.writeInt(screenHeight)
    out.writeInt(currentScreenHeight)
    out.writeInt(screenWidth)
    out.writeInt(cycles_per_line)
    out.writeInt(cycles_per_line_accu)
    out.writeInt(xchars_total)
    out.writeInt(ychars_total)
    out.writeInt(cursor_pos)
    out.writeInt(ram_adr)
    out.writeInt(ypos)
    out.writeInt(ram_base_ptr)
    out.writeInt(rasterLine)
    out.writeInt(currentCharScanLine)
    out.writeInt(visibleScreenHeightPix)
    out.writeInt(visibleTextRows)
    out.writeInt(borderWidth)
    out.writeBoolean(interlaceMode)
    out.writeInt(rowCounter)
    out.writeInt(rowCounterY)
    out.writeInt(verticalAdjFlag)
    out.writeObject(gfxBuffer)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    address_reg = in.readInt
    vblank = in.readBoolean
    loadMemory[Int](ram,in)
    loadMemory[Int](regs,in)
    videoMode = VideoMode(in.readInt)
    screenHeight = in.readInt
    currentScreenHeight = in.readInt
    screenWidth = in.readInt
    cycles_per_line = in.readInt
    cycles_per_line_accu = in.readInt
    xchars_total = in.readInt
    ychars_total = in.readInt
    cursor_pos = in.readInt
    ram_adr = in.readInt
    ypos = in.readInt
    ram_base_ptr = in.readInt
    rasterLine = in.readInt
    currentCharScanLine = in.readInt
    visibleScreenHeightPix = in.readInt
    visibleTextRows = in.readInt
    borderWidth = in.readInt
    interlaceMode = in.readBoolean
    rowCounter = in.readInt
    rowCounterY = in.readInt
    verticalAdjFlag = in.readInt
    loadMemory[Int](gfxBuffer,in)
    setInterlaceMode(interlaceMode)
    display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS,screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS)
    display.setNewResolution(currentScreenHeight,screenWidth)
    bitmap = display.displayMem
    play
  }
  protected def allowsStateRestoring = true
}
