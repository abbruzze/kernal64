package ucesoft.c64.peripheral.vic

import ucesoft.c64.cpu.Memory
import ucesoft.c64.Chip
import ucesoft.c64.ChipID
import ucesoft.c64.Log
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent
import java.awt.Image
import java.awt.geom.Rectangle2D
import Palette._
import java.awt.Graphics2D
import ucesoft.c64.cpu.CPU6510Mems
import java.awt.event.ActionEvent
import java.util.Arrays
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.cpu.RAMComponent
import annotation.switch

/*
object VIC extends App {
  import CPU6510Mems._
  import javax.swing._
  System.setProperty("sun.java2d.opengl","true")
  System.setProperty("sun.java2d.accthreshold","0")
  val mem = new MAIN_MEMORY(true)
  mem.init
  var base = 1024
  for(m <- 0 until 1000) if (m < 256) mem.write(base + m,m) else mem.write(base + m,32)
  for(c <- 0xD800 until 0xD800 + 1000) mem.write(c,14)
  val bankedMem = new BankedMemory(mem,mem.CHAR_ROM,mem.COLOR_RAM)
  //Log.setDebug
  val frame = new JFrame("Test")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  val vic = new VIC(bankedMem,x => {},x => {})
  val display = new Display(vic.SCREEN_WIDTH,vic.SCREEN_HEIGHT,"C64",frame)
  vic.setDisplay(display)
  vic.write(0xD011,0x9B)
  vic.write(0xD016,8)
  vic.write(0xD018,0x14)
  vic.write(0xD021,6)
  vic.write(0xD022,1)
  vic.write(0xD023,2)
  vic.write(0xD020,14)
  // sprite #x --------
  for(i <- 0 to 7) mem.write(2040 + i,1)  
  //for(i <- 0 to 62) mem.write(64+i,255)
  
  for(y <- 0 to 20) {
    for(x <- 0 to 2) if (y == 0 || y == 20) mem.write(64 + y*3 + x,0x78) else mem.write(64 + y*3 + x,255)
  }
  
  /*
  for(y <- 0 to 20;x <- 0 to 2) x match {
    case 0 => if (y == 0 || y == 20) mem.write(16 + y*3 + x,255) else mem.write(16 + y*3 + x,128)
    case 1 => if (y == 0 || y == 20) mem.write(16 + y*3 + x,255) else mem.write(16 + y*3 + x,0)
    case 2 => if (y == 0 || y == 20) mem.write(16 + y*3 + x,255) else mem.write(16 + y*3 + x,1)
  }
  */ 
  for(y <- 0 to 20) println("%3d %3d %3d".format(mem.read(64 + y*3),mem.read(64 + y*3 + 1),mem.read(64 + y*3 + 2)))
  var x = 24
  var y = 229
  for(i <- 0 to 0) {
    vic.write(0xD000 + i*2,x)
    vic.write(0xD000 + i*2 + 1,y)
    x += 12
    //y += 12
  }
  mem.write(base + 40 * 24,1)
  /*
  vic.write(0xD000,24)
  vic.write(0xD001,51) 
  vic.write(0xD002,49)
  vic.write(0xD003,61)
  */
  vic.write(0xD01B,1)
  //vic.write(0xD000,63)
  //vic.write(0xD010,1)
  vic.write(0xD015,255)  // #0+1
  // colors
  for(i <- 0 to 7) vic.write(0xD027 + i,i)
  //vic.write(0xD017,1)
  //vic.write(0xD01D,1)
  vic.write(0xD01C,0) // multi
  vic.write(0xD025,2)
  vic.write(0xD026,3)
  //vic.write(0xD01A,255)
  // ------------------
  frame.setSize(320,200)
  val button = new JButton("Clock")
  button.addActionListener(new java.awt.event.ActionListener {
    def actionPerformed(e:java.awt.event.ActionEvent) { for(_ <- 1 to 63 * 8) vic.clock(0) }
  })
  frame.getContentPane.add("Center",display)
  //frame.getContentPane.add("North",button)
  frame.setVisible(true)
  Clock.setSystemClock() { cycles =>
    vic.clock(cycles)
  }
  Clock.systemClock.play
}
*/
final class VIC(mem: Memory,
  colorMem:Memory,
  irqAction: (Boolean) => Unit,
  baLow: (Boolean) => Unit,
  debug: Boolean = false,
  clip: Boolean = true) extends Chip with RAMComponent {
  override lazy val componentID = "VIC 6569"
  val name = "VIC"
  val isRom = false
  val length = 1024
  val isActive = true
  val startAddress = 0xD000
  val id = ChipID.VIC

  // ----------------------- Constants --------------------------------------------------------------------
  final private[this] val CYCLE_FOR_FIRST_XCOORD = 13
  final private[this] val RASTER_LINES = 312
  final private[this] val RASTER_CYCLES = 63
  final val SCREEN_WIDTH = RASTER_CYCLES * 8
  final val SCREEN_HEIGHT = RASTER_LINES
  final private[this] val PIXELS_PER_LINE = RASTER_CYCLES * 8
  final private[this] val LEFT_RIGHT_FF_COMP = Array(Array(0x1F, 0x14F), Array(0x18, 0x158)) // first index is CSEL's value 0 or 1, second index is 0=left, 1=right 
  final private[this] val TOP_BOTTOM_FF_COMP = Array(Array(0x37, 0xF7), Array(0x33, 0xFB)) // first index is RSEL's value 0 or 1, second index is 0=top, 1=bottom
  final private[this] val COLOR_ADDRESS = 0xD800
  final private[this] val BLANK_TOP_LINE = 15
  final private[this] val BLANK_BOTTOM_LINE = 300
  final private[this] val BLANK_LEFT_CYCLE = 9
  final private[this] val BLANK_RIGHT_CYCLE = 61
  final private[this] val BA_CYCLES_FOR_CHARS = 40
  final private[this] val BA_CYCLES_PER_SPRITE = 3
  final val VISIBLE_SCREEN_WIDTH = 403//(BLANK_RIGHT_CYCLE - BLANK_LEFT_CYCLE - 1) * 8
  final val VISIBLE_SCREEN_HEIGHT = BLANK_BOTTOM_LINE - BLANK_TOP_LINE - 1
  final val SCREEN_ASPECT_RATIO = VISIBLE_SCREEN_WIDTH.toDouble / VISIBLE_SCREEN_HEIGHT
  // ----------------------- INTERNAL REGISTERS -----------------------------------------------------------
  private[this] var videoMatrixAddress = 0
  private[this] var characterAddress = 0
  private[this] var bitmapAddress = 0
  final private[this] val vml_p = Array.fill(40)(0) // video matrix line for characters pointers
  final private[this] val vml_c = Array.fill(40)(0) // video matrix line for characters colors
  private[this] var vcbase, vc = 0
  private[this] var rc = 0 // row counter
  private[this] var vmli = 0 // video matrix line index
  private[this] var isInDisplayState = false // display or idle state
  //private[this] var spriteCrunch = false
  private[this] var yscroll = 0 // y scroll value 0-7
  private[this] var xscroll = 0 // x scroll value 0-7
  private[this] var rsel = 0 // 1 => 25 rows, 0 => 24 rows
  private[this] var den = false // blank screen if false
  private[this] var denOn30 = false // den value at raster line $30
  private[this] var badLine = false
  private[this] var bmm = false // bitmap mode: true enabled
  private[this] var ecm = false // extended color mode: true enabled
  private[this] var csel = 0 // 1 => 40 cols, 0 => 38 cols
  private[this] var mcm = false // multi color mode: true enabled
  private[this] var res = false // video enabled: false enabled
  // borders
  private[this] var mainBorderFF = false // main border flip flop
  private[this] var verticalBorderFF = false // vertical border flip flop

  private[this] var rasterCycle = 1 // horizontal cycle 1-63
  // graphics management
  private[this] var isBlank = false // valid inside drawCycle: tells if we are in the blank area
  private[this] var display: Display = null // the display
  private[this] var displayMem: Array[Int] = null
  private[this] var firstModPixelX, firstModPixelY = 0 // first x,y pixel coordinate modified
  private[this] var lastModPixelX, lastModPixelY = 0 // last x,y pixel coordinate modified
  private[this] var lightPenEnabled = false
  private[this] var _baLow = false
  // --------------------- DEBUG --------------------------------------------------------------------------
  private[this] var traceRasterLineInfo = false
  final private[this] val traceRasterLineBuffer = Array.fill(SCREEN_WIDTH)("")
  private[this] var traceRasterLine = 0
  // ------------------------ PUBLIC REGISTERS ------------------------------------------------------------
  /*
   * $D000 - $D00F
   * x,y coordinate of sprite i, 0 <= i <= 7
   */
  final private[this] val spriteXYCoord = Array.fill(16)(0)
  /*
   * $D010
   * 9th bit of x coordinate of sprite i
   */
  private[this] var spriteXCoord9thBit = 0
  /*
   * $D011
	Initial Value: %10011011
	Bit responsibilities:
	Bit#0-#2: Screen Soft Scroll Vertical
	Bit#3: Switch between 25 or 24 visible rows
	Bit#4: Switch VIC-II output on/off
	Bit#5: Turn Bitmap Mode on/off
	Bit#6: Turn Extended Color Mode on/off
	Bit#7: 9th Bit for $D012 Rasterline counter 
  */
  private[this] var controlRegister1 = 0x9B
  /*
   * $D012
    When Reading:Return current Rasterline
    When Writing:Define Rasterline for Interrupt triggering
    Bit#7 of $D011 is (to be) set if line number exceeds 255
   */
  private[this] var rasterLine = 0 // real raster line 0-8 bits  
  private[this] var rasterLatch = 0
  /*
   * $D013 - $D014
   * Light Pen XY Coord
   */
  final private[this] val lightPenXYCoord = Array(0, 0)
  private[this] var canUpdateLightPenCoords = true
  /*
   * $D015
   * Each Bit corresponds to a Sprite. If set high the corresponding Sprite is enabled on Screen
   */
  private[this] var spriteEnableRegister = 0
  /*
   * $D016
   * Initial Value: %00001000
	 Bit responsibilities:
	 Bit#0-#2: Screen Soft Scroll Horizontal
 	 Bit#3: Switch betweem 40 or 38 visible columns
	 Bit#4: Turn Multicolor Mode on/off
	 Bit#5-#7: not used 
   */
  private[this] var controlRegister2 = 8
  /*
   * $D017
   * Every Bit corresponds to one Sprite. If set high, the Sprite will be stretched vertically x2
   */
  private[this] var spriteYExpansion = 0
  /*
   * $D018
   * Initial Value: %00010100
	 Bit responsibilities:
	 Bit#0: not used
	 Bit#1-#3: Address Bits 11-13 of the Character Set (*2048)
	 Bit#4-#7: Address Bits 10-13 of the Screen RAM (*1024) 
   */
  private[this] var vicBaseAddress = 0x14
  /*
   * $D019
   * Initial Value: %00001111 (for latch)
	 Bit responsibilities:
	 Bit#0: Interrupt by Rasterline triggered when high
	 Bit#1: Interrupt by Sprite-Background collision triggered when high
	 Bit#2: Interrupt by Sprite-Sprite collision triggered when high
	 Bit#3: Interrupt by Lightpen impulse triggered when high
	 Bit#4-#6: not used
	 Bit#7: If set high at least one of the Interrupts above were triggered 
   */
  private[this] var interruptControlRegister = 0
  /*
   * $D01A
   * Initial Value: %00000000
	 Bit responsibilities:
	 Bit#0: Request Interrupt by Rasterline by setting high
	 Bit#1: Request Interrupt by Spite-Background collision by setting high
	 Bit#2: Request Interrupt by Sprite-Sprite collision by setting high
	 Bit#3: Request Interrupt by Lightpen impulse by setting high
	 Bit#4-#7: not used  
   */
  private[this] var interruptMaskRegister = 0
  /*
   * $D01B
   * Each Bit corresponds to a Sprite. If set high, the Background overlays the Sprite, if set low, the Sprite overlays Background.
   */
  private[this] var spriteCollisionPriority = 0
  /*
   * $D01C
   * Each Bit correspondents to a Sprite. If set high, the Sprite is considered to be a Multicolor-Sprite
   */
  private[this] var spriteMulticolor = 0
  /*
   * $D01D
   * Each Bit corresponds to a Sprite. If set high, the Sprite will be stretched horzontally x2
   */
  private[this] var spriteXExpansion = 0
  /*
   * $D01E
   * Each Bit corresponds to a Sprite. 
   * If two sprites collide, then corresponding Bits involved in the collision are set to high. 
   * This event will also set Bit#2 of the Interrupt Request Register high.
   */
  private[this] var spriteSpriteCollision = 0
  /*
   * $D01F
   * Each Bit corresponds to a Sprite. 
   * If a sprite collides with the backgroud, then its Bit is set to high. This event will also set Bit#1 of the Interrupt Request Register high.
   */
  private[this] var spriteBackgroundCollision = 0
  /*
   * $D020
   * Set Border Color to one of the 16 Colors ($00-$0F)
   */
  private[this] var borderColor = 0
  /*
   * $D021 - $D024
   * Set Background Color 0 - 3 to one of the 16 Colors ($00-$0F)
   */
  final private[this] var backgroundColor = Array(0, 0, 0, 0)
  /*
   * $D025 - $D026
   * Set Color 1 - 2 shared by Multicolor Sprites
   */
  final private[this] var spriteMulticolor01 = Array(0, 0)
  /*
   * $D027 - $D02E
   * Set individual color for Sprite#0 - #7
   */
  final private[this] var spriteColor = Array(0, 0, 0, 0, 0, 0, 0, 0)
  
  // ---------------------------- PIXELS --------------------------------------------------
  // |doc 1|doc 0|sprite priority|sp 2|sp 1|sp 0|transparent|background/foreground|c3|c2|c1|c0|
  // |  11 | 10  |       9       | 8  | 7  | 6  |     5     |          4          | 3| 2| 1| 0|
  
  // |doc 1|doc 0|
  // | 0   |  0  | = 'N'
  // | 0   |  1  | = 'B'
  // | 1   |  0  | = 'G'
  // | 1   |  1  | = 'S'
  
  final private[this] val PIXEL_FOREGROUND = 1 << 4
  final private[this] val PIXEL_TRANSPARENT = 1 << 5
  final private[this] val PIXEL_DOX_N = 0
  final private[this] val PIXEL_DOX_B = 1 << 10
  final private[this] val PIXEL_DOX_G = 2 << 10
  final private[this] val PIXEL_DOX_S = 3 << 10
  final private[this] val PIXEL_SPRITE_PRIORITY = 1 << 9
  final private[this] val PIXEL_BLACK = 0

  // ----------------------------- SPRITE -------------------------------------------------
  private[this] class Sprite(index: Int,
    var x: Int = 0,
    var y: Int = 0,
    var xexp: Boolean = false,
    var color: Int = 0,
    var isMulticolor: Boolean = false,
    var dataPriority: Boolean = false) extends C64Component {
    val componentID = "Sprite " + index
    val componentType = C64ComponentType.INTERNAL
    
    private[this] val DMA_INDEX = 1 << index
    private[this] var _enabled = false
    private[this] val mask = 1 << index
    private[this] var counter = 0
    private[this] var gdata = 0
    private[this] var _yexp = false
    private[this] var memoryPointer = 0
    private[this] var mcbase, mc = 0
    var dma = false
    private[this] var expansionFF = true
    private[this] var xexpCounter = 0
    private[this] var display = false
    //private[this] var lastLine = false
    var painting = false
    var hasPixels = false
    private[this] val pixels = Array.fill[Int](8)(PIXEL_TRANSPARENT)
    private[this] val ALL_TRANSPARENT = Array.fill(8)(PIXEL_TRANSPARENT)
    
    def enabled = _enabled
    def enabled_=(enabled:Boolean) = {
      _enabled = enabled
    }
    
    override def toString = s"Sprite #${index} cnt=${counter} data=${Integer.toBinaryString(gdata & 0xFFFFFF)} en=${_enabled} hasPixels=${hasPixels} x=${x} y=${y} xexp=${xexp} yexp=${_yexp} color=${color} mcm=${isMulticolor} pr=${dataPriority} memP=${memoryPointer} mcbase=${mcbase} mc=${mc} dma=${dma} display=${display} ff=${expansionFF}"

    override def getProperties = {
      properties.setProperty("Enabled",_enabled.toString)
      properties.setProperty("X",x.toString)
      properties.setProperty("Y",y.toString)
      properties.setProperty("X expansion",xexp.toString)
      properties.setProperty("Y expansion",_yexp.toString)
      properties.setProperty("Memory pointer",Integer.toHexString(memoryPointer))
      properties.setProperty("Multi color mode",isMulticolor.toString)
      properties.setProperty("Color",Integer.toHexString(color))
      properties
    }
    
    final def yexp = _yexp
    final def displayable = display

    final def yexp_=(v: Boolean) {
      if (!v && !expansionFF) {
        if (rasterCycle == 15) mc = (0x2A & (mcbase & mc)) | (0x15 & (mcbase | mc)) // patch from VIC-Addendum, sprite crunch
        expansionFF = true
      }
//      if (!v && !expansionFF) { // sprite crunch
//        if (rasterCycle == 15) mcbase = (0x2A & (mcbase & mc)) | (0x15 & (mcbase | mc)) // patch from VIC-Addentum
//        expansionFF = true
//      }
      _yexp = v
    }

    final def getPixels = pixels

    final def reset {
      _enabled = false
      x = 0
      y = 0
      xexp = false
      color = 0
      isMulticolor = false
      dataPriority = false
      gdata = 0
      _yexp = false
      memoryPointer = 0
      mcbase = 0
      mc = 0
      dma = false
      expansionFF = true
      xexpCounter = 0
      display = false
      //lastLine = false
      painting = false
      hasPixels = false
      counter = 0
      resetSprite
    }
    
    def init {}

    @inline final def resetSprite {
      if (hasPixels) {
        hasPixels = false
        Array.copy(ALL_TRANSPARENT,0,pixels,0,8)
      }
      if (!displayable) spritesDisplayedMask &= ~mask
    }

    @inline final def producePixels {
      val xcoord = xCoord
      var i = 0
      while (i < 8) {
        if (!painting && x == xcoord + i && display) painting = true
        if (painting && !isFinished) {
          pixels(i) = shift
          //println(s"Sprite #${s} shifted a pixel ${sprite.pixels(i)} in ${i} raster=${rasterLine} cycle=${rasterCycle} ${sprites(s)}")
        }
        i += 1
      }
    }

    @inline final def isFinished = {
      val finished = counter == (if (xexp) 48 else 24)
      if (finished) {
        painting = false
//        if (lastLine) {
//          lastLine = false          
//        }
      }
      finished
    }

    final private def shift = {
      var pixel = if (!isMulticolor) { // no multicolor        
        if (xexp) {
          if (xexpCounter == 0) gdata <<= 1
          xexpCounter = (xexpCounter + 1) % 2
        } else gdata <<= 1
        val cbit = (gdata & 0x1000000) == 0x1000000
        if (cbit) color else PIXEL_TRANSPARENT
      } else { // multicolor
        if ((counter & 1) == 0) {
          if (xexp) {
            if (xexpCounter == 0) gdata <<= 2
            xexpCounter += 1
            if (xexpCounter == 2) xexpCounter = 0
          } else gdata <<= 2
        }
        val cbit = (gdata & 0x3000000)
        (cbit : @switch) match {
          case 0x0000000 => PIXEL_TRANSPARENT
          case 0x1000000 => spriteMulticolor01(0)
          case 0x2000000 => color
          case 0x3000000 => spriteMulticolor01(1)
        }
      }
      counter += 1
      if ((pixel & PIXEL_TRANSPARENT) == 0) hasPixels = true
      if (dataPriority) pixel |= PIXEL_SPRITE_PRIORITY
      pixel |= index << 6
      if (traceRasterLineInfo) pixel |= PIXEL_DOX_S
      pixel
    }

    @inline final def readMemoryData(first:Boolean) {            
      // s-accesses
      if (first) memoryPointer = mem.read(videoMatrixAddress + 1016 + index) << 6
      if (dma) {    
        if (first) {          
          // p-accesses          
          gdata = (mem.read(memoryPointer + mc) & 0xFF) << 16
        }
        else {
          gdata |= (mem.read(memoryPointer + mc) & 0xFF) << 8
          mc = (mc + 1) & 0x3F
          gdata |= (mem.read(memoryPointer + mc) & 0xFF)
          counter = 0
          hasPixels = false
        } 
        mc = (mc + 1) & 0x3F
      }
      else 
      if (!first) mem.read(0x3FFF) // idle access
    }

    final def checkForCycle(cycle: Int) = (cycle : @switch) match {
      case 16 =>
        if (expansionFF) {
          mcbase = mc
          if (mcbase == 63) {
            dma = false
            spriteDMAon &= ~DMA_INDEX
          }
        }
      case 55|56 =>
        if (cycle == 55 && _yexp) expansionFF = !expansionFF
        if (_enabled && (y) == (rasterLine & 0xFF) && !dma) {
          dma = true
          spriteDMAon |= DMA_INDEX
          mcbase = 0
          if (_yexp) expansionFF = false
          //Log.debug(s"Sprite #${index}. " + this)
          //println(s"Sprite #${index} DMA=true on rasterLine=${rasterLine}")
        }
      case 58 =>
        mc = mcbase
        if (dma) {
          if (_enabled && (y) == (rasterLine & 0xFF)) {
            display = true
            spritesDisplayedMask |= mask
          }          
        }
        else display = false 
        
      case _ => // do nothing
    }
  }
  final private[this] val sprites = Array(new Sprite(0), new Sprite(1), new Sprite(2), new Sprite(3), new Sprite(4), new Sprite(5), new Sprite(6), new Sprite(7))
  private[this] var spritesDisplayedMask = 0
  private[this] var spriteDMAon = 0
  // -------------------------------- VIDEO CACHE -----------------------------------------
  // | 43 - 40 | 39 - 36 | 35 - 32 | 31 - 28 | 27 - 24 | 23 - 16 | 15 - 8  | 7 - 0 |
  // |  back 3 |  back 2 |  back 1 | back 0  | border c| $d018   | $d016   | $d011 |
  
  private[this] val hashVideoCache = Array.fill[Long](RASTER_LINES,RASTER_CYCLES)(-1)
  private[this] val pixelVideoCache = Array.fill[Int](RASTER_LINES,RASTER_CYCLES,8)(-1)
  private[this] val gfxVideoCache = Array.fill[Long](RASTER_LINES,RASTER_CYCLES)(-2)
  
  @inline private[this] def getHashVideoCache = backgroundColor(3).toLong << 40 |
    											backgroundColor(2) << 36 |
    											backgroundColor(1) << 32 |
    											backgroundColor(0) << 28 |
    											borderColor << 24 |
    											vicBaseAddress << 16 | 
  												controlRegister2 << 8 | 
  												controlRegister1 & 0x7F
 						
  // ------------------------------ SHIFTERS ----------------------------------------------    
  private[this] object BorderShifter {
    private[this] var xcoord = 0
    private[this] var color = 0
    private[this] val ALL_TRANSPARENT = Array.fill(8)(PIXEL_TRANSPARENT)
    private[this] val pixels = Array.fill(8)(PIXEL_TRANSPARENT)
    private[this] var drawBorder = true

    final def getPixels = if (drawBorder) pixels else ALL_TRANSPARENT//if (drawBorder) pixelVideoCache(rasterLine)(rasterCycle) else ALL_TRANSPARENT//if (drawBorder) pixels else ALL_TRANSPARENT

    final def producePixels {
      // optimization           
      drawBorder = !den ||
        rasterLine <= TOP_BOTTOM_FF_COMP(rsel)(0) ||
        rasterLine >= TOP_BOTTOM_FF_COMP(rsel)(1) ||
        (rasterCycle > 8 && rasterCycle < 17) || (rasterCycle > 53 && rasterCycle < 61)
      if (!drawBorder) return
      xcoord = xCoord
      color = borderColor
      if (traceRasterLineInfo) color |= PIXEL_DOX_B
      /*
      val hash = getHashVideoCache
      if (hashVideoCache(rasterLine)(rasterCycle) == hash) return
      */
      
      var i = 0
      while (i < 8) {
        val hasBorder = checkBorderFF(xcoord + i)
        pixels(i) = if (hasBorder) color else PIXEL_TRANSPARENT
        //pixelVideoCache(rasterLine)(rasterCycle)(i) = shift
        i += 1
      }
    }
  }
  /**
   * Graphics Shifter for text/bitmap & blank lines
   */
  private object GFXShifter {
    private[this] var counter = 0
    private[this] var gdata = 0
    private[this] var xscrollBuffer = Array.fill(8)(PIXEL_BLACK)
    private[this] var firstPixel = true
    private[this] val pixels = Array.fill(8)(PIXEL_TRANSPARENT)

    private[this] var mcm, ecm, isBlank = false
    private[this] var vml_p, vml_c: Array[Int] = _
    private[this] var vmli = 0
    private[this] var isInvalidMode = false
    private[this] var isInDisplayState = false

    final def getPixels = pixels//pixelVideoCache(rasterLine)(rasterCycle)//pixels

    final def setData(gdata: Int) {
      this.gdata = gdata      
      counter = 0
    }

    final def clear {
      counter = 0
      reset
    }
    final def reset = firstPixel = true
    @inline final protected def shift = {
      var pixel = if (isInvalidMode || isBlank || gdata < 0) PIXEL_BLACK
      else if (!bmm) { // text mode        
        val mc = (vml_c(vmli) & 8) == 8
        val multicolor = mcm && mc
        if (!multicolor) { // standard
          gdata <<= 1
          val cbit = (gdata & 0x100) == 0x100
          if (cbit) { // foreground
            if (isInDisplayState) (if (mcm) vml_c(vmli) & 7 else vml_c(vmli)) | PIXEL_FOREGROUND
            else PIXEL_FOREGROUND
          }
          else { // background
            if (isInDisplayState) { 
              val backIndex = if (ecm) (vml_p(vmli) >> 6) & 3 else 0
              backgroundColor(backIndex)
            }
            else backgroundColor(0)
          }
        } else { // multi color mode          
          if ((counter & 1) == 0) gdata <<= 2
          val cbit = (gdata & 0x300) >> 8
          (cbit : @switch) match {
            case 0 => if (isInDisplayState) backgroundColor(cbit) // background
            		  else backgroundColor(0)
            case 1 => if (isInDisplayState) backgroundColor(cbit) // background
            		  else 0
            case 2 => if (isInDisplayState) backgroundColor(cbit) | PIXEL_FOREGROUND // foreground
            		  else PIXEL_FOREGROUND
            case 3 => if (isInDisplayState) (vml_c(vmli) & 7) | PIXEL_FOREGROUND // foreground
            		  else PIXEL_FOREGROUND
          }
        }
      } else { // bitmap mode
        if (!mcm) { // standard mode
          gdata <<= 1
          val cbit = (gdata & 0x100) == 0x100
          val col0 = vml_p(vmli) & 0x0F
          val col1 = (vml_p(vmli) >> 4) & 0x0F
          if (cbit) {
            if (isInDisplayState) col1 | PIXEL_FOREGROUND // foreground
            else PIXEL_FOREGROUND 
          }
          else {
            if (isInDisplayState) col0 // background
            else 0
          }
        } else { // multi color mode          
          if ((counter & 1) == 0) gdata <<= 2
          val cbit = gdata & 0x300
          (cbit : @switch) match {
            case 0x00 => if (isInDisplayState) backgroundColor(0) // background
            			 else backgroundColor(0)
            case 0x100 => if (isInDisplayState) (vml_p(vmli) >> 4) & 0x0F // background
            			  else 0
            case 0x200 => if (isInDisplayState) (vml_p(vmli) & 0x0F) | PIXEL_FOREGROUND// foreground
            			  else PIXEL_FOREGROUND
            case 0x300 => if (isInDisplayState) vml_c(vmli) | PIXEL_FOREGROUND// foreground
            		 	  else PIXEL_FOREGROUND
          }
        }
      }
      counter += 1
      if (xscroll == 0) {
        if (traceRasterLineInfo && pixel != PIXEL_BLACK) pixel |= PIXEL_DOX_G
        pixel
      } else {
        if (firstPixel) {
          firstPixel = false
          var i = 0
          // insert xscroll black pixels
          while (i < xscroll) {
            xscrollBuffer(i) = backgroundColor(0) //BLACK_PIXEL
            i += 1
          }
        }
        xscrollBuffer(xscroll) = pixel
        // shift xscrollBuffer
        var headPixel = xscrollBuffer(0)
        var i = 0
        while (i < xscroll) {
          xscrollBuffer(i) = xscrollBuffer(i + 1)
          i += 1
        }
        if (traceRasterLineInfo && pixel != PIXEL_BLACK) headPixel |= PIXEL_DOX_G
        headPixel
      }
    }

    final def producePixels {
      isBlank = VIC.this.isBlank
      mcm = VIC.this.mcm
      ecm = VIC.this.ecm
      vml_p = VIC.this.vml_p
      vml_c = VIC.this.vml_c
      vmli = VIC.this.vmli
      isInvalidMode = mcm && ecm || bmm && ecm
      isInDisplayState = VIC.this.isInDisplayState
      
      /*
      val data = gfxVideoCache(rasterLine)(rasterCycle) & 0xFF
      val color = gfxVideoCache(rasterLine)(rasterCycle) >> 8
      if (data == gdata && color == vml_c(vmli)) {
        val hash = getHashVideoCache
        if (hashVideoCache(rasterLine)(rasterCycle) == hash) return
      }
      gfxVideoCache(rasterLine)(rasterCycle) = gdata | vml_c(vmli) << 8
      */
      // light pen checking
      var xcoord = 0
      var lpx = 0
      var baseX = 0
      val checkLP = lightPenEnabled && rasterLine == display.getLightPenY
      if (checkLP) {
        xcoord = xCoord
        lpx = display.getLightPenX
        baseX = rasterCycle << 3
      }
      counter = 0
      while (counter < 8) {
        pixels(counter) = shift
        //pixelVideoCache(rasterLine)(rasterCycle)(counter) = shift
        if (checkLP && baseX + counter == lpx) triggerLightPen
      }
    }
  }

  def init {
    sprites foreach { add _ }    
  }

  override def getProperties = {    
      properties.setProperty("Video matrix addess",Integer.toHexString(videoMatrixAddress))
      properties.setProperty("Character address",Integer.toHexString(characterAddress))
      properties.setProperty("Bitmap address",Integer.toHexString(bitmapAddress))
      properties.setProperty("X scroll",xscroll.toString)
      properties.setProperty("Y scroll",yscroll.toString)
      properties.setProperty("Bitmap mode",bmm.toString)
      properties.setProperty("Extended color mode",ecm.toString)
      properties.setProperty("Multicolor mode",mcm.toString)
      properties.setProperty("Raster line",Integer.toHexString(rasterLine))
      properties.setProperty("Sprite enable register",Integer.toHexString(spriteEnableRegister))
      properties.setProperty("Control register 1",Integer.toHexString(controlRegister1))
      properties.setProperty("Control register 2",Integer.toHexString(controlRegister2))
      properties.setProperty("VIC base address",Integer.toHexString(vicBaseAddress))
      properties.setProperty("IRQ control register",Integer.toHexString(interruptControlRegister))
      properties.setProperty("IRQ mask register",Integer.toHexString(interruptMaskRegister))
      properties.setProperty("Raster latch",Integer.toHexString(rasterLatch))
      properties.setProperty("Raster cycle",Integer.toHexString(rasterCycle))
      properties.setProperty("Main border FF",mainBorderFF.toString)
      properties.setProperty("Vertical border FF",verticalBorderFF.toString)
      properties.setProperty("Den",den.toString)
      properties.setProperty("BA",_baLow.toString)
      super.getProperties
    }

  def reset {
    videoMatrixAddress = 0
    characterAddress = 0
    bitmapAddress = 0
    vcbase = 0
    vc = 0
    rc = 0
    vmli = 0
    isInDisplayState = false
    yscroll = 0
    xscroll = 0
    rsel = 0
    den = false
    denOn30 = false
    bmm = false
    ecm = false
    csel = 0
    mcm = false
    res = false
    mainBorderFF = false
    verticalBorderFF = false
    rasterCycle = 1
    spriteXCoord9thBit = 0
    controlRegister1 = 0x9B
    rasterLine = 0
    rasterLatch = 0
    spriteEnableRegister = 0
    controlRegister2 = 8
    spriteYExpansion = 0
    vicBaseAddress = 0x14
    interruptControlRegister = 0
    interruptMaskRegister = 0
    spriteCollisionPriority = 0
    spriteMulticolor = 0
    spriteXExpansion = 0
    spriteSpriteCollision = 0
    spriteBackgroundCollision = 0
    spriteDMAon = 0
    GFXShifter.clear
  }

  def setDisplay(display: Display) = {
    this.display = display
    displayMem = display.displayMem
    if (clip)
      display.setClipArea((BLANK_LEFT_CYCLE + 1) * 8 - 3, BLANK_TOP_LINE + 1, 480, BLANK_BOTTOM_LINE)
  }

  def enableLightPen(enabled: Boolean) = lightPenEnabled = enabled

  def triggerLightPen {
    if (canUpdateLightPenCoords) {
      canUpdateLightPenCoords = false
      lightPenXYCoord(0) = (xCoord >> 1) & 0xFF
      lightPenXYCoord(1) = rasterLine & 0xFF
      interruptControlRegister |= 8
      // check if we must set interrupt
      if ((interruptControlRegister & interruptMaskRegister & 0x0f) != 0) irqRequest
    }
  }

  @inline private def decodeAddress(address: Int) = (address - startAddress) % 64

  final def read(address: Int, chipID: ChipID.ID): Int = {
    val offset = decodeAddress(address)
    if (offset <= 0xF) spriteXYCoord(offset)
    else if (offset >= 0x2F && offset <= 0x3F) 0xFF
    else
      (offset : @switch) match {
        case 16 => spriteXCoord9thBit
        case 17 => controlRegister1
        case 18 => rasterLine & 0xFF
        case 19 => lightPenXYCoord(0)
        case 20 => lightPenXYCoord(1)
        case 21 => spriteEnableRegister
        case 22 => controlRegister2 | 0xC0 // bit 6 & 7 always 1
        case 23 => spriteYExpansion
        case 24 => vicBaseAddress | 1 // bit 0 always 1
        case 25 => interruptControlRegister | 0x70 //& 0x8F // bit 4,5,6 always 1
        case 26 => interruptMaskRegister | 0xF0 // bit 7,6,5,4 always 1
        case 27 => spriteCollisionPriority
        case 28 => spriteMulticolor
        case 29 => spriteXExpansion
        case 30 =>
          val tmp = spriteSpriteCollision; spriteSpriteCollision = 0; tmp // cleared after reading
        case 31 =>
          val tmp = spriteBackgroundCollision; spriteBackgroundCollision = 0; tmp // cleared after reading
        case 32 => borderColor | 0xF0 // bit 7,6,5,4 always 1
        case 33 => backgroundColor(0) | 0xF0 // bit 7,6,5,4 always 1
        case 34 => backgroundColor(1) | 0xF0 // bit 7,6,5,4 always 1
        case 35 => backgroundColor(2) | 0xF0 // bit 7,6,5,4 always 1
        case 36 => backgroundColor(3) | 0xF0 // bit 7,6,5,4 always 1
        case 37 => spriteMulticolor01(0) | 0xF0 // bit 7,6,5,4 always 1
        case 38 => spriteMulticolor01(1) | 0xF0 // bit 7,6,5,4 always 1
        case 39 => spriteColor(0) | 0xF0 // bit 7,6,5,4 always 1
        case 40 => spriteColor(1) | 0xF0 // bit 7,6,5,4 always 1
        case 41 => spriteColor(2) | 0xF0 // bit 7,6,5,4 always 1
        case 42 => spriteColor(3) | 0xF0 // bit 7,6,5,4 always 1
        case 43 => spriteColor(4) | 0xF0 // bit 7,6,5,4 always 1
        case 44 => spriteColor(5) | 0xF0 // bit 7,6,5,4 always 1
        case 45 => spriteColor(6) | 0xF0 // bit 7,6,5,4 always 1
        case 46 => spriteColor(7) | 0xF0 // bit 7,6,5,4 always 1
        case _ => 0xFF // $D02F-$D03F
      }
  }
  
  final def write(address: Int, value: Int, chipID: ChipID.ID) = {
    val offset = decodeAddress(address)
    if (offset <= 0xF) {
      spriteXYCoord(offset) = value
      val sindex = offset >> 1 /// 2
      val isX = (offset & 1) == 0
      if (isX) {
        val bit9 = sprites(sindex).x & 0x100
        sprites(sindex).x = value | bit9
      } else sprites(sindex).y = value
      //Log.fine(s"Sprite #${sindex} update coord ${if (isX) "x" else "y"} = ${value}")
    } else
      (offset : @switch) match {
        case 16 =>
          spriteXCoord9thBit = value
          var i = 0
          var tmp = spriteXCoord9thBit
          while (i < 8) {
            if ((tmp & 1) == 1) sprites(i).x |= 0x100 else sprites(i).x &= 0xFF
            tmp >>= 1
            i += 1
          }
        //Log.fine((for(i <- 0 to 7) yield "Sprite x updated to" + sprites(i).x) mkString("\n"))
        case 17 =>
          //val clk = Clock.systemClock
          //clk.schedule(new ClockEvent("$D011",clk.currentCycles + 1,(cycles) => {
            controlRegister1 = value
            val prevRasterLatch = rasterLatch
            if ((controlRegister1 & 128) == 128) rasterLatch |= 0x100 else rasterLatch &= 0xFF
            //if (rasterLatch > RASTER_LINES) rasterLatch &= 0xFF
            if (prevRasterLatch != rasterLatch && rasterLine == rasterLatch) rasterLineEqualsLatch
            yscroll = controlRegister1 & 7
            rsel = (controlRegister1 & 8) >> 3
            den = (controlRegister1 & 16) == 16
            bmm = (controlRegister1 & 32) == 32
            ecm = (controlRegister1 & 64) == 64            
            badLine = isBadLine
            if (rasterLine == 0x30 && den) denOn30 = true
          //}))
          if (debug) Log.info("VIC control register set to %s, yscroll=%d rsel=%d den=%b bmm=%b ecm=%b. Raster latch set to %4X".format(Integer.toBinaryString(controlRegister1), yscroll, rsel, den, bmm, ecm, rasterLatch))
        //Log.info("VIC control register set to %s, yscroll=%d rsel=%d den=%b bmm=%b ecm=%b. Raster latch set to %4X".format(Integer.toBinaryString(controlRegister1),yscroll,rsel,den,bmm,ecm,rasterLatch))
          //hashVideoCache(rasterLine)(rasterCycle) = getHashVideoCache
    	case 18 =>
          //Log.debug("VIC previous value of raster latch is %4X".format(rasterLatch))
          val rst8 = rasterLatch & 0x100
          val prevRasterLatch = rasterLatch
          rasterLatch = value | rst8
          //if (rasterLatch > RASTER_LINES) rasterLatch &= 0xFF
          if (prevRasterLatch != rasterLatch && rasterLine == rasterLatch) rasterLineEqualsLatch
          if (debug) Log.info("VIC raster latch set to %4X value=%2X".format(rasterLatch, value))
        //else Log.debug("VIC raster latch set to %4X value=%2X".format(rasterLatch,value))
        case 19 | 20 => // light pen ignored
        case 21 =>
          spriteEnableRegister = value
          var i = 7
          while (i >= 0) {
            sprites(i).enabled = ((spriteEnableRegister >> i) & 1) == 1
            i -= 1
            //if (sprites(i).enabled) Log.debug(s" Sprite #${i} enabled. " + sprites(i))
          }
        //Log.fine("Sprite enable register se to " + Integer.toBinaryString(spriteEnableRegister))
        case 22 =>
          //val clk = Clock.systemClock
          //clk.schedule(new ClockEvent("$D016",clk.currentCycles + 1,(cycles) => {
          controlRegister2 = value
          xscroll = controlRegister2 & 7
          csel = (controlRegister2 & 8) >> 3
          mcm = (controlRegister2 & 16) == 16
          res = (controlRegister2 & 32) == 32
          //}))
          if (debug) Log.info("VIC control register 2 set to %s, xscroll=%d csel=%d mcm=%b rasterLine=%d".format(Integer.toBinaryString(controlRegister2), xscroll, csel, mcm, rasterLine))
        //Log.info("VIC control register 2 set to %s, xscroll=%d csel=%d mcm=%b rasterLine=%d".format(Integer.toBinaryString(controlRegister2),xscroll,csel,mcm,rasterLine))
          //hashVideoCache(rasterLine)(rasterCycle) = getHashVideoCach
        case 23 =>
          spriteYExpansion = value
          var i = 7
          while (i >= 0) {
            sprites(i).yexp = ((spriteYExpansion >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite Y expansion se to " + Integer.toBinaryString(spriteYExpansion))
        case 24 =>
          //val clk = Clock.systemClock
          //clk.schedule(new ClockEvent("$D018",clk.currentCycles + 4,(cycles) => {
          vicBaseAddress = value
          videoMatrixAddress = ((vicBaseAddress >> 4) & 0x0F) << 10 //* 1024
          characterAddress = ((vicBaseAddress >> 1) & 0x07) << 11 //* 2048
          bitmapAddress = ((vicBaseAddress >> 3) & 1) << 13 //* 8192
          //}))
          //hashVideoCache(rasterLine)(rasterCycle) = getHashVideoCache
          if (debug) Log.info(s"VIC base pointer set to ${Integer.toBinaryString(vicBaseAddress)} video matrix=${videoMatrixAddress} char address=${characterAddress} bitmap address=${bitmapAddress}")
        //Log.info(s"VIC base pointer set to ${Integer.toBinaryString(vicBaseAddress)} video matrix=${videoMatrixAddress} char address=${characterAddress} bitmap address=${bitmapAddress} raster=${rasterLine}")          
        case 25 =>
          interruptControlRegister &= ~value & 0x0F
          // check if we must clear interrupt
          if ((interruptControlRegister & interruptMaskRegister & 0x0f) == 0) {
            //Log.debug("VIC clearing interrupt...")
            interruptControlRegister &= 0x7F
            irqAction(false)
          }
          else interruptControlRegister |= 0x80
        // light pen ignored
        //Log.debug("VIC interrupt control register set to " + Integer.toBinaryString(interruptControlRegister))
        case 26 =>
          interruptMaskRegister = value & 0x0F
          // check if we must set interrupt
          if ((interruptControlRegister & interruptMaskRegister & 0x0f) != 0) {
            //Log.debug("VIC#26 setting interrupt...")
            irqRequest
          } else {
            interruptControlRegister &= 0x7F
            irqAction(false)
          }

        //Log.debug("VIC interrupt mask register set to " + Integer.toBinaryString(interruptMaskRegister))
        case 27 =>
          spriteCollisionPriority = value
          var i = 7
          while (i >= 0) {
            sprites(i).dataPriority = ((spriteCollisionPriority >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite collision priority set to " + Integer.toBinaryString(spriteCollisionPriority))
        case 28 =>
          spriteMulticolor = value
          var i = 7
          while (i >= 0) {
            sprites(i).isMulticolor = ((spriteMulticolor >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite multicolor set to " + Integer.toBinaryString(spriteMulticolor))
        case 29 =>
          spriteXExpansion = value
          var i = 7
          while (i >= 0) {
            sprites(i).xexp = ((spriteXExpansion >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite X expansion set to " + Integer.toBinaryString(spriteXExpansion))
        case 30 | 31 => // can't be written
        case 32 =>
          borderColor = value & 0x0F
          //hashVideoCache(rasterLine)(rasterCycle) = getHashVideoCache
        //Log.debug("VIC border color set to " + borderColor)
        case 33 | 34 | 35 | 36 =>
          backgroundColor(offset - 33) = value & 0x0F
          //hashVideoCache(rasterLine)(rasterCycle) = getHashVideoCache
        //Log.debug("VIC background color #%d set to %d".format(offset - 33,value))
        case 37 | 38 =>
          spriteMulticolor01(offset - 37) = value & 0x0F
        //Log.fine("Sprite multicolor #%d set to %d".format(offset - 37,value))
        case 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 =>
          val index = offset - 39
          spriteColor(index) = value & 0x0F
          sprites(index).color = value & 0x0F
        //Log.fine("Sprite #%d color set to %d".format(offset - 39,value))
        case _ => // $D02F-$D03F
      }
  }

  @inline private def spriteCheck {
    var c = 0
    while (c < 8) {
      sprites(c).checkForCycle(rasterCycle)
      c += 1
    }
  }
  
  private[this] var ref = 0
  
  @inline private def setBaLow(low:Boolean) {
    if (low != _baLow) {
      _baLow = low
      baLow(low)
    }
  }
  
  final def clock(cycles: Long) {
    val outOfScreen = rasterLine <= BLANK_TOP_LINE || rasterLine >= BLANK_BOTTOM_LINE
    isBlank = outOfScreen || rasterCycle <= BLANK_LEFT_CYCLE || rasterCycle >= BLANK_RIGHT_CYCLE

    if (badLine) isInDisplayState = true

    //if (!outOfScreen)
    (rasterCycle: @switch) match {       
      case 1 =>                
        // check raster line with raster latch if irq enabled     
        if (rasterLine > 0 && rasterLine == rasterLatch) rasterLineEqualsLatch
        // check den on $30
        if (rasterLine == 0x30) denOn30 = den
        badLine = isBadLine        
        sprites(3).readMemoryData(true)
        setBaLow((spriteDMAon & 0x18) > 0) // 3,4
        drawCycle(-1)
      case 2 =>
        // check raster line with raster latch if irq enabled    
        if (rasterLine == 0 && rasterLine == rasterLatch) rasterLineEqualsLatch
        sprites(3).readMemoryData(false)
        setBaLow((spriteDMAon & 0x38) > 0) // 3,4,5
        drawCycle(-1)
      case 3 =>
        sprites(4).readMemoryData(true)
        setBaLow((spriteDMAon & 0x30) > 0) // 4,5
        drawCycle(-1)
      case 4 =>
        sprites(4).readMemoryData(false)
        setBaLow((spriteDMAon & 0x70) > 0) // 4,5,6
        drawCycle(-1)
      case 5 =>
        sprites(5).readMemoryData(true)
        setBaLow((spriteDMAon & 0x60) > 0) // 5,6
        drawCycle(-1)
      case 6 =>
        sprites(5).readMemoryData(false)
        setBaLow((spriteDMAon & 0xE0) > 0) // 5,6,7
        drawCycle(-1)
      case 7 =>
        sprites(6).readMemoryData(true)
        setBaLow((spriteDMAon & 0xC0) > 0) // 6,7
        drawCycle(-1)
      case 8 =>
        sprites(6).readMemoryData(false)
        drawCycle(-1)
      case 9 =>
        sprites(7).readMemoryData(true)
        setBaLow((spriteDMAon & 0x80) > 0) // 7
        drawCycle(-1)
      case 10 =>
        sprites(7).readMemoryData(false)
        setBaLow((spriteDMAon & 0x80) > 0) // 7
        //setBaLow(false) // ***
        drawCycle(-1)
      case 11 =>
        mem.read(0x3F00 | ref) ; ref = (ref - 1) & 0xFF // DRAM REFRESH
        setBaLow(false)
        drawCycle(-1)
      // ---------------------------------------------------------------
      case 55 =>
        spriteCheck
        setBaLow((spriteDMAon & 0x01) > 0) // 0
        if (isInDisplayState) {
          // g-access
          drawCycle(readCharFromMemory)          
          vc = (vc + 1) & 0x3FF //% 1024
          vmli = (vmli + 1) & 0x3F 
        }
        else drawCycle(mem.read(if (ecm) 0x39ff else 0x3fff))
      case 56 =>
        mem.read(0x3FFF)
        spriteCheck
        setBaLow((spriteDMAon & 0x01) > 0) // 0
        drawCycle(-1)
      case 57 =>
        mem.read(0x3FFF)
        setBaLow((spriteDMAon & 0x03) > 0)  // 0,1
        drawCycle(-1)	
      // ---------------------------------------------------------------
      case 58 =>     
        spriteCheck
        if (rc == 7) {
          isInDisplayState = false
          vcbase = vc
          //Log.fine(s"VIC cycle 58 vcbase=${vcbase}")
        }
        if (badLine || isInDisplayState) {
          rc = (rc + 1) & 7
          isInDisplayState = true
        }
        sprites(0).readMemoryData(true)
        setBaLow((spriteDMAon & 0x03) > 0) // 0,1
        drawCycle(-1)
      // ---------------------------------------------------------------
      case 59 =>        
        sprites(0).readMemoryData(false)
        setBaLow((spriteDMAon & 0x07) > 0) // 0,1,2
        drawCycle(-1)
      case 60 =>
        sprites(1).readMemoryData(true)
        setBaLow((spriteDMAon & 0x06) > 0) // 1,2
        drawCycle(-1)
      case 61 =>
        sprites(1).readMemoryData(false)
        setBaLow((spriteDMAon & 0x0E) > 0) // 1,2,3
        drawCycle(-1)
      case 62 =>
        sprites(2).readMemoryData(true)
        setBaLow((spriteDMAon & 0x0C) > 0) // 2,3       
        drawCycle(-1)
      // ---------------------------------------------------------------
      case 63 =>        
        sprites(2).readMemoryData(false)
        setBaLow((spriteDMAon & 0x1C) > 0) // 2,3,4
        GFXShifter.reset
        drawCycle(-1)
      // ---------------------------------------------------------------
      case _ => // 12 - 54
        setBaLow(badLine) 
        if (isInDisplayState && rasterCycle >= 16) {
          //if (badLine) readAndStoreVideoMemory
                    
          // g-access
          drawCycle(readCharFromMemory)          
          vc = (vc + 1) & 0x3FF //% 1024
          vmli += 1
          // c-access
          if (_baLow) readAndStoreVideoMemory
        } 
        else 
        if (!isInDisplayState && rasterCycle >= 16) {
          //vml_c(vmli) = 0
          drawCycle(mem.read(if (ecm) 0x39ff else 0x3fff))
          //drawCycle(0)
        } 
        else drawCycle(-1)
        (rasterCycle : @switch) match {
          case 12 =>
            mem.read(0x3F00 | ref) ; ref = (ref - 1) & 0xFF // DRAM REFRESH
          case 13 =>
            mem.read(0x3F00 | ref) ; ref = (ref - 1) & 0xFF // DRAM REFRESH
          case 14 =>
            mem.read(0x3F00 | ref) ; ref = (ref - 1) & 0xFF // DRAM REFRESH
            vc = vcbase
            vmli = 0
            if (badLine) rc = 0
          case 15 =>
            mem.read(0x3F00 | ref) ; ref = (ref - 1) & 0xFF // DRAM REFRESH
            // c-access
            if (_baLow) readAndStoreVideoMemory
          case 16 =>
            spriteCheck
          case _ =>
        }               
    }
    
    if (rasterCycle == RASTER_CYCLES) {
      rasterCycle = 1 
      updateRasterLine
    }
    else {
      rasterCycle += 1
      if (showDebug) {
        display.showFrame(firstModPixelX, firstModPixelY, lastModPixelX, lastModPixelY)
      }
    }
  }
  
  private[this] var showDebug = false
  def setShowDebug(showDebug:Boolean) = this.showDebug = showDebug
  
  @inline private[this] def updateRasterLine {
    rasterLine += 1
    if (rasterLine == RASTER_LINES) rasterLine = 0
    // update the 8th bit of raster in control register 1
    if (rasterLine > 0xFF) controlRegister1 |= 0x80 else controlRegister1 &= 0x7F
    if (rasterLine == 0) {
      canUpdateLightPenCoords = true
      display.showFrame(firstModPixelX, firstModPixelY, lastModPixelX, lastModPixelY)
      firstModPixelX = -1
      lastModPixelX = -1
      ref = 0xFF
      vcbase = 0
      vc = 0
    }
  }
  
  @inline private[this] def tracePixel(pixel: Int, x: Int) {
    val source = (pixel >> 10) & 3 match {
      case 0 => 'N'
      case 1 => 'B'
      case 2 => 'G'
      case 3 => 'S'
    }
    val color = pixel & 0x0F
    val isForeground = (pixel & PIXEL_FOREGROUND) > 0
    traceRasterLineBuffer(x) = "%c%2d%c%s%s".format(source,color, if (isForeground) 'F' else 'B', if (mcm) "M" else "", if (ecm) "E" else "")
  }
  @inline private[this] def drawPixel(x: Int, y: Int, pixel: Int) = {
    val index = y * SCREEN_WIDTH + x
    val color = VIC_RGB(pixel & 0x0F)
    if (displayMem(index) != color) {
      displayMem(index) = color
      if (firstModPixelX == -1) {
        firstModPixelX = x
        firstModPixelY = y
      }

      lastModPixelX = x
      lastModPixelY = y
    }
    if (traceRasterLineInfo && y == traceRasterLine) tracePixel(pixel, x)
  }

  private[this] def drawCycle(gdata: Int) {
    if (isBlank) return

    val y = rasterLine
    val x = (rasterCycle - 1) << 3
    var s, i = 0

    // --------------------- GFX -------------------------
    val borderOnOpt = verticalBorderFF && rasterLine != TOP_BOTTOM_FF_COMP(rsel)(0)
    if ((!borderOnOpt) && gdata >= 0) {
      GFXShifter.setData(gdata)
      GFXShifter.producePixels
    }
    // ------------------- Sprites -----------------------
    val almostOneSprite = spritesDisplayedMask > 0
    //var almostOneSprite = false
    
    if (almostOneSprite)
    while (s < 8) {
      if (sprites(s).displayable) {
        sprites(s).producePixels
        //almostOneSprite = true
      }
      s += 1
    }
    // ------------------- Border ------------------------
    //BorderShifter.setData(0)
    BorderShifter.producePixels
    // ************************** RENDERING ************************************

    val gfxPixels = GFXShifter.getPixels
    val borderPixels = BorderShifter.getPixels
    while (i < 8) { // scan each bit      
      s = 7
      var pixel = PIXEL_TRANSPARENT
      if (almostOneSprite) 
      while (s >= 0) {
        if (sprites(s).hasPixels) { // scan each sprite      
          val spritePixels = sprites(s).getPixels
          if ((spritePixels(i) & PIXEL_TRANSPARENT) == 0) {
            if ((pixel & PIXEL_TRANSPARENT) == 0) spriteSpriteCollision((pixel >> 6) & 7, (spritePixels(i) >> 6) & 7) // sprite-sprite collision detected
            pixel = spritePixels(i)
          }
        }
        s -= 1
      }
      val gfxPixel = if (verticalBorderFF) backgroundColor(0) else if (gdata < 0) backgroundColor(0) else gfxPixels(i)
      if (!verticalBorderFF && (gfxPixel & 0x10) > 0 && (pixel & PIXEL_TRANSPARENT) == 0) spriteDataCollision((pixel >> 6) & 7) // sprite-data collision detected
      if ((pixel & PIXEL_TRANSPARENT) > 0 || ((gfxPixel & 0x10) > 0 && (pixel & PIXEL_SPRITE_PRIORITY) > 0)) pixel = gfxPixel
      if ((borderPixels(i) & PIXEL_TRANSPARENT) == 0) drawPixel(x + i, y, borderPixels(i))
      else drawPixel(x + i, y, pixel)
      
      i += 1
    }
    // ************************** RESET SPRITES ********************************
    s = 0
    if (almostOneSprite) 
    while (s < 8) {
      //if (sprites(s).hasPixels) sprites(s).reset
      sprites(s).resetSprite
      s += 1
    }
    // *************************************************************************

    //if (debug) { display.showFrame }
  }

  @inline private def irqRequest {
   if ((interruptControlRegister & 0x80) == 0) {
      interruptControlRegister |= 0x80
      irqAction(true)
   }
  }

  @inline private def rasterLineEqualsLatch {
    if ((interruptControlRegister & 1) == 0) {
      interruptControlRegister |= 1
      if ((interruptMaskRegister & 1) == 1) {
        //Log.debug(s"VIC - raster interrupt request raster=${rasterLine}")
        irqRequest
      }
    }
  }

  @inline private def spriteSpriteCollision(i: Int, j: Int) {
    val mask = (1 << i) | (1 << j)
    val ssCollision = spriteSpriteCollision
    spriteSpriteCollision |= mask
    if (ssCollision == 0) {     
      interruptControlRegister |= 4
      if ((interruptMaskRegister & 4) == 4) {
        //Log.debug(s"Sprite-sprite collision detected between ${i} and ${j} ss=${Integer.toBinaryString(spriteSpriteCollision)} icr=${Integer.toBinaryString(interruptControlRegister)}")
        irqRequest
      }
    }
  }
  @inline private def spriteDataCollision(i: Int) {
    val mask = (1 << i)
    val sbc = spriteBackgroundCollision
    spriteBackgroundCollision |= (1 << i)
    if (sbc == 0) {      
      interruptControlRegister |= 2
      if ((interruptMaskRegister & 2) == 2) {
        //Log.debug(s"Sprite-data collision detected for ${i} sd=${Integer.toBinaryString(spriteBackgroundCollision)} icr=${Integer.toBinaryString(interruptControlRegister)}")
        irqRequest
      }
    }
  }

  /**
   * To be called on bad lines
   */
  @inline private def readAndStoreVideoMemory {
    val charCode = mem.read(videoMatrixAddress | vc)
    val color = colorMem.read(COLOR_ADDRESS | vc) & 0x0F
    vml_p(vmli) = charCode
    vml_c(vmli) = color
    //Log.fine(s"Reading video memory at ${videoMatrixAddress + offset}: charCode=${charCode} color=${color}")
  }
  
  @inline private def readCharFromMemory : Int = {
    if (bmm) {
      val offset = bitmapAddress | ((vc & 0x3ff) << 3) | rc
      val bitmap = mem.read(offset)
      //Log.debug(s"Reading bitmap at ${offset} = ${bitmap}")
      bitmap
    } else {
      val charCode = if (ecm) vml_p(vmli) & 0x3F else vml_p(vmli)
      val char = mem.read(characterAddress | (charCode << 3) | rc)
      //Log.fine(s"Reading char at ${characterAddress} for char code ${charCode} with rc=${rc} pattern=${char}")
      char
    }
  }

  /*
   * A Bad Line Condition is given at any arbitrary clock cycle, if at the
 	negative edge of ø0 at the beginning of the cycle RASTER >= $30 and RASTER
 	<= $f7 and the lower three bits of RASTER are equal to YSCROLL and if the
 	DEN bit was set during an arbitrary cycle of raster line $30.
   */
  @inline private def isBadLine = rasterLine >= 0x30 && rasterLine <= 0xF7 && ((rasterLine & 7) == yscroll) && denOn30

  /**
   * Get the x coordinate based on current raster cycle.
   */
  @inline private def xCoord = {
    if (rasterCycle >= CYCLE_FOR_FIRST_XCOORD) (rasterCycle - CYCLE_FOR_FIRST_XCOORD) << 3
    else 0x198 + (rasterCycle - 1) << 3
//    var xcoord = (rasterCycle - CYCLE_FOR_FIRST_XCOORD) << 3
//    if (xcoord < 0) xcoord += PIXELS_PER_LINE
//    xcoord
  }

  @inline private def checkBorderFF(xcoord: Int) = {
    // 1
    if (xcoord == LEFT_RIGHT_FF_COMP(csel)(1)) mainBorderFF = true
    // 2
    if (rasterCycle == RASTER_CYCLES && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(1)) verticalBorderFF = true
    else
    // 3
    if (rasterCycle == RASTER_CYCLES && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(0) && den) verticalBorderFF = false
    // 4
    if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(1)) verticalBorderFF = true
    else
    // 5
    if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(0) && den) verticalBorderFF = false
    // 6
    if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && !verticalBorderFF) mainBorderFF = false
    
    mainBorderFF// || verticalBorderFF
  }

  def enableTraceRasterLine(enabled: Boolean) = traceRasterLineInfo = enabled
  def setTraceRasterLineAt(traceRasterLine: Int) = this.traceRasterLine = traceRasterLine
  def getTraceRasterLineInfo = {
    val line = traceRasterLine + "=" + (traceRasterLineBuffer mkString "|")
    for (i <- 0 until traceRasterLineBuffer.length) traceRasterLineBuffer(i) = ""
    line
  }

  def getMemory = mem

  def dump = {
    val sb = new StringBuffer("VIC dump:\n")
    mem match {
      case bm: BankedMemory => sb.append(s"Video Bank:\t\t\t${bm.getBank} = ${Integer.toHexString(bm.getBankAddress)}\n")
      case _ =>
    }
    sb.append(if (bmm) "Bitmap mode\n" else "Text mode\n")
    sb.append(s"Extended color mode\t\t${ecm}\n")
    sb.append(s"Multi color mode\t\t${mcm}\n")
    sb.append(s"Raster line\t\t\t${rasterLine}\n")
    sb.append(s"Display enabled\t\t\t${den}\n")
    sb.append(s"Display size\t\t\t${if (csel == 1) "40" else "38"}x${if (rsel == 1) "25" else "24"}\n")
    sb.append(s"Raster IRQ on\t\t\t${rasterLatch}\n")
    sb.append(s"Video matrix address\t\t${Integer.toHexString(videoMatrixAddress)}\n")
    sb.append(s"Char address\t\t\t${Integer.toHexString(characterAddress)}\n")
    sb.append(s"Bitmap address\t\t\t${Integer.toHexString(bitmapAddress)}\n")
    sb.toString
  }
}