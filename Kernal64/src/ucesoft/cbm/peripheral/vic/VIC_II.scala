package ucesoft.cbm.peripheral.vic

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm._
import ucesoft.cbm.cpu.{Memory, RAMComponent}
import ucesoft.cbm.peripheral.vic.Palette._
import ucesoft.cbm.peripheral.vic.coprocessor.{VICContext, VICCoprocessor}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties
import scala.annotation.switch


final class VIC_II(mem: VIC_II_Memory,
                   colorMem:Memory,
                   irqAction: Boolean => Unit,
                   baLow: Boolean => Unit,
                   isVICIIe:Boolean = false) extends RAMComponent with VICContext {
  override val componentType: Type = CBMComponentType.CHIP
  override lazy val componentID = "VICII"
  val name = "VIC"
  val isRom = false
  val length = 1024
  val isActive = true
  val startAddress = 0xD000
  val id: ID = ChipID.VIC

  private[this] var model : VIC_II_Model = _

  setVICModel(VIC_II_PAL)

  def setVICModel(model:VIC_II_Model) : Unit = {
    this.model = model
    lastModPixelY = model.RASTER_LINES
    lastModPixelX = model.BLANK_RIGHT_CYCLE << 3
    firstModPixelX = -1
    firstModPixelY = 0
    xCoord = model.XCOORD
  }

  override def getVICModel : VICModel = model

  // ----------------------- Constants --------------------------------------------------------------------
  final private[this] val LEFT_RIGHT_FF_COMP = Array(Array(0x1F, 0x14F), Array(0x18, 0x158)) // first index is CSEL's value 0 or 1, second index is 0=left, 1=right
  final private[this] val TOP_BOTTOM_FF_COMP = Array(Array(0x37, 0xF7), Array(0x33, 0xFB)) // first index is RSEL's value 0 or 1, second index is 0=top, 1=bottom
  final private[this] val COLOR_ADDRESS = 0xD800
  // ------------------------------------------------------------------------------------------------------
  def SCREEN_WIDTH : Int = model.RASTER_CYCLES << 3
  def SCREEN_HEIGHT: Int = model.RASTER_LINES
  def VISIBLE_SCREEN_WIDTH : Int = (model.BLANK_RIGHT_CYCLE - model.BLANK_LEFT_CYCLE) << 3
  def VISIBLE_SCREEN_HEIGHT: Int = model.BLANK_BOTTOM_LINE - model.BLANK_TOP_LINE - 1
  def SCREEN_ASPECT_RATIO: Double = VISIBLE_SCREEN_WIDTH.toDouble / VISIBLE_SCREEN_HEIGHT
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
  private[this] var yscroll = 0 // y scroll value 0-7
  private[this] var xscroll = 0 // x scroll value 0-7
  private[this] var rsel = 0 // 1 => 25 rows, 0 => 24 rows
  private[this] var den = false // blank screen if false
  private[this] var denOn30 = false // den value at raster line $30
  private[this] var badLine = false
  private[this] var bmm,bmmDelay = false // bitmap mode: true enabled
  private[this] var ecm,ecmDelay = false // extended color mode: true enabled
  private[this] var csel = 0 // 1 => 40 cols, 0 => 38 cols
  private[this] var mcm = false // multi color mode: true enabled
  private[this] var res = false // video enabled: false enabled
  private[this] var internalDataBus = 0
  // borders
  private[this] var mainBorderFF = false // main border flip flop
  private[this] var verticalBorderFF = false // vertical border flip flop

  private[this] var rasterCycle = 1 // horizontal cycle 1-63
  // graphics management
  private[this] var isBlank = false // valid inside drawCycle: tells if we are in the blank area
  private[this] var display: Display = null // the display
  private[this] var displayMem: Array[Int] = null
  private[this] var firstModPixelX, firstModPixelY = 0 // first x,y pixel coordinate modified
  private[this] var lastModPixelX = model.BLANK_RIGHT_CYCLE << 3
  private[this] var lastModPixelY = 0 // last y pixel coordinate modified
  private[this] var lightPenEnabled = false
  private[this] var _baLow = false
  private[this] var baLowFirstCycle = 0L
  private[this] val clk = Clock.systemClock
  private[this] var ref = 0
  private[this] var rasterIRQTriggered = false
  private[this] var xCoord : Array[Int] = _
  private[this] var drawBorderOpt = true
  private[this] var pendingDrawBorderModeChange = false
  private[this] var isNewVICModel = isVICIIe // 8565 for PAL, 8562 for NTSC
  // ------------------------ C128 $D030 test bit & others ------------------------------------------------
  private[this] var c128TestBitEnabled = false
  private[this] var refreshCycle = false
  private[this] var _2MhzMode = false
  // ------------------------ COPROCESSOR -----------------------------------------------------------------
  private[this] var coprocessor : VICCoprocessor = null
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
  private[this] var displayLine = 0
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
  final private[this] val backgroundColor = Array(0, 0, 0, 0)

  // Last background drawn
  private[this] var lastBackground = 0
  /*
   * $D025 - $D026
   * Set Color 1 - 2 shared by Multicolor Sprites
   */
  final private[this] val spriteMulticolor01 = Array(0, 0)
  /*
   * $D027 - $D02E
   * Set individual color for Sprite#0 - #7
   */
  // see Sprite class

  private[this] var dataToDraw,vmliToDraw = 0
  private[this] var lastColorReg = 0xFF // for grey dot bug
  // sprite
  final private[this] val sprites = Array(new Sprite(0), new Sprite(1), new Sprite(2), new Sprite(3), new Sprite(4), new Sprite(5), new Sprite(6), new Sprite(7))
  private[this] var spritesDisplayedMask = 0
  private[this] var spriteDMAon = 0

  // PIPELINE =============================================================================
  /*
  private[this] var dataToDrawPipe = 0L
  private[this] var vmliToDrawPipe = 0
  private[this] var rasterCycleToDrawPipe = 0
  private[this] var displayLineToDrawPipe = 0
  private[this] var rasterCycleToDraw, displayLineToDraw = 0
  final private[this] val shadowBackgroundColor = Array(0, 0, 0, 0)
  private[this] var backgroundColorChangedIndex = -1
  private[this] var shadowBorderColor = -1

   */
  // ======================================================================================

  // ---------------------------- PIXELS --------------------------------------------------
  //  grey dot gfx info |sprite priority|sp 2|sp 1|sp 0|transparent|background/foreground|c3|c2|c1|c0|
  //                  10|       9       | 8  | 7  | 6  |     5     |          4          | 3| 2| 1| 0|

  final private[this] val PIXEL_FOREGROUND = 1 << 4
  final private[this] val PIXEL_TRANSPARENT = 1 << 5
  final private[this] val PIXEL_SPRITE_PRIORITY = 1 << 9
  final private[this] val PIXEL_BLACK = 0

  // ----------------------------- SPRITE -------------------------------------------------
  private[this] class Sprite(index: Int,
                             var x: Int = 0,
                             var y: Int = 0,
                             var xexp: Boolean = false,
                             var isMulticolor : Boolean = false,
                             var color: Int = 0,
                             var dataPriority: Boolean = false) extends CBMComponent {
    val componentID: String = "Sprite " + index
    val componentType: Type = CBMComponentType.INTERNAL

    private[this] val DMA_INDEX = 1 << index
    private[this] var _enabled = false
    private[this] var mcCounter = 0
    private[this] var gdata = 0
    private[this] var _yexp = false
    private[this] var memoryPointer = 0
    private[this] var mcbase, mc = 0
    var dma = false
    private[this] var expansionFF = true
    private[this] var xexpCounter = 0
    private[this] var display = false
    private[this] var latchedMc = false
    var painting = false
    var hasPixels = false
    private[this] val pixels = Array.fill[Int](8)(PIXEL_TRANSPARENT)
    private[this] val ALL_TRANSPARENT = Array.fill(8)(PIXEL_TRANSPARENT)

    def enabled: Boolean = _enabled
    def enabled_=(enabled:Boolean): Unit = {
      _enabled = enabled
    }

    override def toString = s"Sprite #$index mcCounter=$mcCounter data=${Integer.toBinaryString(gdata & 0xFFFFFF)} en=${_enabled} hasPixels=$hasPixels x=$x y=$y xexp=$xexp yexp=${_yexp} color=$color mcm=$isMulticolor pr=$dataPriority memP=$memoryPointer mcbase=$mcbase mc=$mc dma=$dma display=$display ff=$expansionFF"

    override def getProperties: Properties = {
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

    final def yexp: Boolean = _yexp
    final def displayable: Boolean = display

    final def yexp_=(v: Boolean) : Unit = {
      if (!v && _yexp) {
        if (rasterCycle == 15) mc = (0x2A & (mcbase & mc)) | (0x15 & (mcbase | mc)) // patch from VIC-Addendum, sprite crunch
        expansionFF = true
      }
      _yexp = v
    }

    final def getPixels: Array[Int] = pixels

    final def reset : Unit = {
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
      painting = false
      hasPixels = false
      mcCounter = 0
      Array.copy(ALL_TRANSPARENT,0,pixels,0,8)
    }

    def init : Unit = {}

    @inline final def resetSprite() : Unit = {
      if (hasPixels) {
        hasPixels = false
        Array.copy(ALL_TRANSPARENT,0,pixels,0,8)
      }
      if (!display) spritesDisplayedMask &= ~DMA_INDEX
    }

    final def producePixels() : Unit = {
      //var xcoord = xCoord(rasterCycleToDraw) // TODO: PIPE
      var xcoord = xCoord(rasterCycle)
      var i = 0
      var finished = false

      while (i < 8 && !finished) {
        if (!painting && x == xcoord && x < 0x1F8) {
          painting = true
          latchedMc = isMulticolor
          mcCounter = 0
          xexpCounter = 0
        } else xcoord += 1

        if (painting) {
          finished = (gdata & 0x03FFFFFF) == 0
          if (finished) painting = false
          else {
            if (i == 3) {
              if (latchedMc ^ isMulticolor) {
                latchedMc = isMulticolor
                //mcCounter = 0
              }
            }
            pixels(i) = shift(latchedMc)
          }
        }
        i += 1
      }
    }

    @inline final private def shift(mc:Boolean) = {
      var pixel = if (!mc) { // no multicolor
        if (xexp) {
          if (xexpCounter == 0) gdata <<= 1
          xexpCounter ^= 1
        } else gdata <<= 1
        val cbit = (gdata & 0x1000000) == 0x1000000
        if (cbit) color else PIXEL_TRANSPARENT
      } else { // multicolor
        if (mcCounter == 0) {
          if (xexp) {
            if (xexpCounter == 0) gdata <<= 2
            xexpCounter ^= 1
          } else gdata <<= 2
        }

        mcCounter ^= 1
        val cbit = gdata & 0x3000000
        (cbit : @switch) match {
          case 0x0000000 => PIXEL_TRANSPARENT
          case 0x1000000 => spriteMulticolor01(0)
          case 0x2000000 => color
          case 0x3000000 => spriteMulticolor01(1)
        }
      }

      if ((pixel & PIXEL_TRANSPARENT) == 0) hasPixels = true
      if (dataPriority) pixel |= PIXEL_SPRITE_PRIORITY
      pixel |= index << 6
      pixel
    }

    @inline final def readMemoryData(first:Boolean) : Unit = {
      // p-access phi1
      if (first) memoryPointer = mem.read(videoMatrixAddress + 1016 + index,ChipID.VIC) << 6
      if (dma) {
        if (first) {
          // s-access phi2
          gdata <<= 8
          gdata |= (mem.read(memoryPointer + mc,ChipID.VIC) & 0xFF)
        }
        else {
          // s-accesses phi2
          gdata <<= 8
          gdata |= (mem.readPhi2(memoryPointer + mc) & 0xFF)
          mc = (mc + 1) & 0x3F
          gdata <<= 8
          gdata |= (mem.readPhi2(memoryPointer + mc) & 0xFF)
          //hasPixels = false
        }
        mc = (mc + 1) & 0x3F
      }
      else
      if (first) {
        gdata <<= 8
        gdata |= internalDataBus
      }
      else {
        val ghost = mem.read(0x3FFF,ChipID.VIC)
        gdata <<= 16
        gdata |= ghost << 8 | internalDataBus
        //hasPixels = false
      } // idle access
    }

    final def check16() : Unit = {
      if (expansionFF) {
        mcbase = mc
        if (mcbase == 63) {
          dma = false
          spriteDMAon &= ~DMA_INDEX
        }
      }
    }

    final def isReadyForDMA : Boolean = _enabled && y == (rasterLine & 0xFF)

    final def check55_56(is55:Boolean) : Unit = {
      if (is55 && _yexp) expansionFF = !expansionFF
      if (_enabled && y == (rasterLine & 0xFF) && !dma) {
        dma = true
        spriteDMAon |= DMA_INDEX
        mcbase = 0
        if (_yexp) expansionFF = false
      }
    }

    final def check58() : Unit = {
      mc = mcbase
      if (dma) {
        if (_enabled && y == (rasterLine & 0xFF)) {
          display = true
          spritesDisplayedMask |= DMA_INDEX
        }
      }
      else {
        display = false
        spritesDisplayedMask &= ~DMA_INDEX
      }
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeBoolean(_enabled)
      out.writeInt(x)
      out.writeInt(y)
      out.writeBoolean(xexp)
      out.writeInt(color)
      out.writeBoolean(isMulticolor)
      out.writeBoolean(dataPriority)
      out.writeInt(gdata)
      out.writeBoolean(_yexp)
      out.writeInt(memoryPointer)
      out.writeInt(mcbase)
      out.writeInt(mc)
      out.writeBoolean(dma)
      out.writeBoolean(expansionFF)
      out.writeInt(xexpCounter)
      out.writeBoolean(display)
      out.writeBoolean(painting)
      out.writeBoolean(hasPixels)
      out.writeInt(mcCounter)
      out.writeObject(pixels)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      _enabled = in.readBoolean
      x = in.readInt
      y = in.readInt
      xexp = in.readBoolean
      color = in.readInt
      isMulticolor = in.readBoolean
      dataPriority = in.readBoolean
      gdata = in.readInt
      _yexp = in.readBoolean
      memoryPointer = in.readInt
      mcbase = in.readInt
      mc = in.readInt
      dma = in.readBoolean
      expansionFF = in.readBoolean
      xexpCounter = in.readInt
      display = in.readBoolean
      painting = in.readBoolean
      hasPixels = in.readBoolean
      mcCounter = in.readInt
      loadMemory[Int](pixels,in)
    }
    protected def allowsStateRestoring : Boolean = true
  }
  // ------------------------------ SHIFTERS ----------------------------------------------
  private[this] final val borderShifter = new BorderShifter
  private[this] val gfxShifter = new GFXShifter

  private[this] class BorderShifter extends CBMComponent {
    val componentID = "BorderShifter"
    val componentType: Type = CBMComponentType.INTERNAL
    private[this] val ALL_TRANSPARENT = Array.fill(8)(PIXEL_TRANSPARENT)
    private[this] val pixels = Array.fill(8)(PIXEL_TRANSPARENT)
    private[this] var lastBorderColor = -1

    def init : Unit = {}
    def reset : Unit = {}

    final def getPixels : Array[Int] = if (isBlank || !drawBorderOpt) ALL_TRANSPARENT else pixels

    final def producePixels()  : Unit = {
      if (!drawBorderOpt) return

      if (!isBlank) {
        //var xcoord = xCoord(rasterCycleToDraw) // TODO : PIPE
        var xcoord = xCoord(rasterCycle)

        checkVertical

        var i = 0
        while (i < 8) {
          /*
          val color = if (i < 1) {
            if (shadowBorderColor == -1) borderColor else shadowBorderColor
          } else borderColor
           */ // TODO : PIPE
          val color = borderColor
          val hasBorder = checkBorderFF(xcoord)
          pixels(i) = if (hasBorder) {
            if (isVICIIe && i == 0 && lastColorReg == 0xFA) 0x0F else color
          } else PIXEL_TRANSPARENT
          i += 1
          xcoord += 1
        }
      }
      lastBorderColor = borderColor
    }

    @inline private def checkVertical(): Unit = {
      //if (rasterCycle == model.RASTER_CYCLES) {
        // 2 If the Y coordinate reaches the bottom comparison value in cycle 63, the
        //   vertical border flip flop is set.
        if (rasterLine == TOP_BOTTOM_FF_COMP(rsel)(1)) verticalBorderFF = true
        else
        // 3 If the Y coordinate reaches the top comparison value in cycle 63 and the
        //   DEN bit in register $d011 is set, the vertical border flip flop is
        //   reset.
        if (rasterLine == TOP_BOTTOM_FF_COMP(rsel)(0) && den) verticalBorderFF = false
      //}
    }

    @inline private def checkBorderFF(xcoord: Int) = {
      // 1 If the X coordinate reaches the right comparison value, the main border
      //   flip flop is set.
      if (xcoord == LEFT_RIGHT_FF_COMP(csel)(1)) mainBorderFF = true
      else
      // 4 If the X coordinate reaches the left comparison value and the Y
      //   coordinate reaches the bottom one, the vertical border flip flop is set.
      if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(1)) verticalBorderFF = true
      else
      // 5 If the X coordinate reaches the left comparison value and the Y
      //   coordinate reaches the top one and the DEN bit in register $d011 is set,
      //   the vertical border flip flop is reset.
      if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && rasterLine == TOP_BOTTOM_FF_COMP(rsel)(0) && den) verticalBorderFF = false
      // 6 If the X coordinate reaches the left comparison value and the vertical
      //   border flip flop is not set, the main flip flop is reset.
      if (xcoord == LEFT_RIGHT_FF_COMP(csel)(0) && !verticalBorderFF) mainBorderFF = false

      mainBorderFF
    }

    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeObject(pixels)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      loadMemory[Int](pixels,in)

    }
    protected def allowsStateRestoring : Boolean = true
  }

  /**
   * Graphics Shifter for text/bitmap & blank lines
   */
  private class GFXShifter extends CBMComponent {
    val componentID = "GFXShifter"
    val componentType: Type = CBMComponentType.INTERNAL
    private[this] var gdata,gdataLatch = 0
    private[this] val pixels = Array.fill(8)(PIXEL_TRANSPARENT)
    private[this] var mcFlop = 0

    final def getPixels: Array[Int] = pixels

    final def init : Unit = {}
    final def reset : Unit = {
      gdata = 0
      gdataLatch = 0
    }

    @inline private def shift(counter:Int) = {
      val vmli = if (counter < xscroll) {
        if (vmliToDraw > 0) vmliToDraw - 1 else 0
      }
      else vmliToDraw

      if (counter == xscroll) {
        gdata = gdataLatch
        mcFlop = 0
      }

      //val backgroundColor = if (counter < 1) shadowBackgroundColor else VIC.this.backgroundColor // TODO : PIPE
      val backgroundColor = VIC_II.this.backgroundColor

      val pixel = if (isBlank || gdata < 0) PIXEL_BLACK
      else if (!bmm) { // text mode
        val mc = (vml_c(vmli) & 8) == 8
        val multicolor = mcm && mc
        if (!multicolor) { // standard
          gdata <<= 1
          val cbit = (gdata & 0x100) == 0x100
          if (cbit) { // foreground
            if (mcm & ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid text mode (foreground)
            else
            if (isInDisplayState) (if (mcm) vml_c(vmli) & 7 else vml_c(vmli)) | PIXEL_FOREGROUND
            else PIXEL_FOREGROUND
          }
          else { // background
            if (mcm & ecm) PIXEL_BLACK // invalid text mode (background)
            else
            if (isInDisplayState) {
              val backIndex = if (ecm) (vml_p(vmli) >> 6) & 3 else 0
              backgroundColor(backIndex) | (backIndex + 1) << 10
            }
            else backgroundColor(0)
          }
        }
        else { // multi color mode
          if (mcFlop == 0) gdata <<= 2
          val cbit = (gdata & 0x300) >> 8
          (cbit : @switch) match {
            case 0 => if (ecm) PIXEL_BLACK // invalid text mode (background)
            else
            if (isInDisplayState) backgroundColor(cbit) | (cbit + 1) << 10  // background
            else backgroundColor(0)
            case 1 => if (ecm) PIXEL_BLACK // invalid text mode (background)
            else
            if (isInDisplayState) backgroundColor(cbit) | (cbit + 1) << 10 // background
            else PIXEL_BLACK
            case 2 => if (ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid text mode (foreground)
            else
            if (isInDisplayState) backgroundColor(cbit) | PIXEL_FOREGROUND | (cbit + 1) << 10 // foreground
            else PIXEL_FOREGROUND
            case 3 => if (ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid text mode (foreground)
            else
            if (isInDisplayState) (vml_c(vmli) & 7) | PIXEL_FOREGROUND // foreground
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
            if (ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid bitmap mode (foreground)
            else
            if (isInDisplayState) col1 | PIXEL_FOREGROUND // foreground
            else PIXEL_FOREGROUND
          }
          else {
            if (ecm) PIXEL_BLACK // invalid bitmap mode (background)
            else
            if (isInDisplayState) col0 // background
            else PIXEL_BLACK
          }
        } else { // multi color mode
          if (mcFlop == 0) gdata <<= 2
          val cbit = gdata & 0x300
          (cbit : @switch) match {
            case 0x00 => if (ecm) PIXEL_BLACK // invalid bitmap mode (background)
            else
            if (isInDisplayState) backgroundColor(0) | 1 << 10 // background
            else backgroundColor(0)
            case 0x100 => if (ecm) PIXEL_BLACK // invalid bitmap mode (background)
            else
            if (isInDisplayState) (vml_p(vmli) >> 4) & 0x0F // background
            else PIXEL_BLACK
            case 0x200 => if (ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid bitmap mode (foreground)
            else
            if (isInDisplayState) (vml_p(vmli) & 0x0F) | PIXEL_FOREGROUND// foreground
            else PIXEL_FOREGROUND
            case 0x300 => if (ecm) PIXEL_BLACK | PIXEL_FOREGROUND // invalid bitmap mode (foreground)
            else
            if (isInDisplayState) vml_c(vmli) | PIXEL_FOREGROUND// foreground
            else PIXEL_FOREGROUND
          }
        }
      }
      mcFlop ^= 1

      if (isInDisplayState && (pixel & PIXEL_FOREGROUND) == 0) lastBackground = pixel

      pixel
    }

    final def producePixels(gdata:Int) : Unit = {
      gdataLatch = gdata
      // light pen checking
      var xcoord = 0
      var lpx = 0
      var baseX = 0
      //val checkLP = lightPenEnabled && displayLineToDraw == display.getLightPenY // TODO : PIPE
      val checkLP = lightPenEnabled && displayLine == display.getLightPenY
      if (checkLP) {
        //xcoord = xCoord(rasterCycleToDraw) // TODO : PIPE
        xcoord = xCoord(rasterCycle)
        lpx = display.getLightPenX
        // baseX = rasterCycleToDraw << 3 // TODO : PIPE
        baseX = rasterCycle << 3
      }
      var counter = 0
      while (counter < 8) {
        pixels(counter) = shift(counter)
        if (isNewVICModel) {
          val greyReg = ((pixels(counter) >> 10) & 7) - 1
          if (counter == 0 && lastColorReg == greyReg) pixels(counter) = 0x0F
        }

        if (checkLP && baseX + counter == lpx) triggerLightPen
        counter += 1
      }
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeInt(gdata)
      out.writeInt(gdataLatch)
      out.writeInt(mcFlop)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      gdata = in.readInt
      gdataLatch = in.readInt
      mcFlop = in.readInt

    }
    protected def allowsStateRestoring : Boolean = true
  }

  def init : Unit = {
    add(borderShifter)
    add(gfxShifter)
    sprites foreach { add _ }
  }

  override def getProperties: Properties = {
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
    properties.setProperty("Raster cycle",rasterCycle.toString)
    properties.setProperty("Badline",badLine.toString)
    properties.setProperty("Main border FF",mainBorderFF.toString)
    properties.setProperty("Vertical border FF",verticalBorderFF.toString)
    properties.setProperty("Den",den.toString)
    properties.setProperty("BA",_baLow.toString)
    properties.setProperty("Model",model.VIC_TYPE.toString)
    super.getProperties
  }

  def reset : Unit = {
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
    controlRegister1 = 0
    rasterLine = 0
    displayLine = 0
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
    spritesDisplayedMask = 0
    ref = 0
    lastModPixelY = model.RASTER_LINES
    lastModPixelX = model.BLANK_RIGHT_CYCLE << 3
    firstModPixelX = -1//model.BLANK_LEFT_CYCLE << 3
    firstModPixelY = 0
    lastBackground = 0
    /*
    dataToDrawPipe = 0
    displayLineToDrawPipe = 0
    rasterCycleToDrawPipe = 0
    vmliToDrawPipe = 0
    */ // TODO : PIPE
    if (coprocessor != null) coprocessor.reset
  }

  def setNEWVICModel(newModel:Boolean) : Unit = isNewVICModel = newModel || isVICIIe

  def setDrawBorder(on:Boolean) : Unit = {
    drawBorderOpt = on
    pendingDrawBorderModeChange = true
  }

  def setCoprocessor(cop:VICCoprocessor) : Unit = {
    if (cop != null) {
      if (coprocessor != null) {
        remove(coprocessor)
        coprocessor.disinstall
      }
      add(cop)
      cop.install
    }
    else if (coprocessor != null) {
      remove(coprocessor)
      coprocessor.disinstall
    }
    this.coprocessor = cop
  }

  def getCoprocessor : Option[VICCoprocessor] = Option(coprocessor)

  def setDisplay(display: Display): Unit = {
    this.display = display
    displayMem = display.displayMem

    display.setClipArea(model.BLANK_LEFT_CYCLE << 3, model.BLANK_TOP_LINE + 1,model.BLANK_RIGHT_CYCLE << 3, model.BLANK_BOTTOM_LINE)
  }

  def enableLightPen(enabled: Boolean): Unit = lightPenEnabled = enabled

  def triggerLightPen() : Unit = {
    if (canUpdateLightPenCoords) {
      canUpdateLightPenCoords = false
      lightPenXYCoord(0) = ((xCoord(rasterCycle) >> 1) & 0xFF) + (if (isNewVICModel) 1 else 2)
      lightPenXYCoord(1) = displayLine & 0xFF
      interruptControlRegister |= 8
      // check if we must set interrupt
      //if ((interruptControlRegister & interruptMaskRegister & 0x0f) != 0) irqRequest
      checkAndSendIRQ
    }
  }

  @inline private def decodeAddress(address: Int) = address & 0x3F

  final def read(address: Int, chipID: ChipID.ID): Int = {
    val offset = decodeAddress(address)
    internalDataBus = if (coprocessor != null && coprocessor.isActive && address > coprocessor.readOffset) coprocessor.readReg(address)
    else if (offset <= 0xF) spriteXYCoord(offset)
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
        case 25 => interruptControlRegister | (if (coprocessor != null && coprocessor.isActive) coprocessor.controlRegisterMask else 0x70) // bit 4,5,6 always 1
        case 26 => interruptMaskRegister | (if (coprocessor != null && coprocessor.isActive) coprocessor.interruptMaskRegisterMask else 0xF0) // bit 7,6,5,4 always 1
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
        case 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 =>
          val index = offset - 39
          sprites(index).color | 0xF0 // bit 7,6,5,4 always 1
        case _ => 0xFF // $D02F-$D03F
      }
    internalDataBus
  }

  final def write(address: Int, value: Int, chipID: ChipID.ID) : Unit = {
    internalDataBus = value
    if (coprocessor != null && chipID != ChipID.VIC_COP) coprocessor.writeReg(address,value)

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
          controlRegister1 = value
          val prevRasterLatch = rasterLatch
          if ((controlRegister1 & 128) == 128) rasterLatch |= 0x100 else rasterLatch &= 0xFF
          if (prevRasterLatch != rasterLatch) checkRasterIRQ

          yscroll = controlRegister1 & 7
          rsel = (controlRegister1 & 8) >> 3
          den = (controlRegister1 & 16) == 16
          bmmDelay = bmm
          ecmDelay = ecm
          bmm = (controlRegister1 & 32) == 32
          ecm = (controlRegister1 & 64) == 64

          badLine = isBadLine

        //Log.info("VIC control register set to %s, yscroll=%d rsel=%d den=%b bmm=%b ecm=%b. Raster latch set to %4X".format(Integer.toBinaryString(controlRegister1),yscroll,rsel,den,bmm,ecm,rasterLatch))
        case 18 =>
          //Log.debug("VIC previous value of raster latch is %4X".format(rasterLatch))
          val rst8 = rasterLatch & 0x100
          val prevRasterLatch = rasterLatch
          rasterLatch = value | rst8
          if (prevRasterLatch != rasterLatch) checkRasterIRQ

        //if (debug) Log.info("VIC raster latch set to %4X value=%2X".format(rasterLatch, value))
        case 19 | 20 => // light pen ignored
        case 21 =>
          spriteEnableRegister = value
          var i = 7
          while (i >= 0) {
            sprites(i).enabled = ((spriteEnableRegister >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite enable register se to " + Integer.toBinaryString(spriteEnableRegister))
        case 22 =>
          controlRegister2 = value
          xscroll = controlRegister2 & 7
          csel = (controlRegister2 & 8) >> 3
          mcm = (controlRegister2 & 16) == 16
          res = (controlRegister2 & 32) == 32
        //Log.info("VIC control register 2 set to %s, xscroll=%d csel=%d mcm=%b rasterLine=%d".format(Integer.toBinaryString(controlRegister2),xscroll,csel,mcm,rasterLine))
        case 23 =>
          spriteYExpansion = value
          var i = 7
          while (i >= 0) {
            sprites(i).yexp = ((spriteYExpansion >> i) & 1) == 1
            i -= 1
          }
        //Log.fine("Sprite Y expansion se to " + Integer.toBinaryString(spriteYExpansion))
        case 24 =>
          vicBaseAddress = value
          videoMatrixAddress = ((vicBaseAddress >> 4) & 0x0F) << 10 //* 1024
          characterAddress = ((vicBaseAddress >> 1) & 0x07) << 11 //* 2048
          bitmapAddress = ((vicBaseAddress >> 3) & 1) << 13 //* 8192
        //Log.info(s"VIC base pointer set to ${Integer.toBinaryString(vicBaseAddress)} video matrix=${videoMatrixAddress} char address=${characterAddress} bitmap address=${bitmapAddress} raster=${rasterLine}")
        case 25 =>
          interruptControlRegister &= (~value & 0x0F) | 0x80
          checkAndSendIRQ
        // light pen ignored
        //Log.debug("VIC interrupt control register set to " + Integer.toBinaryString(interruptControlRegister))
        case 26 =>
          interruptMaskRegister = value & (if (coprocessor != null && coprocessor.isActive) ~coprocessor.interruptMaskRegisterMask & 0xFF else 0x0F)
          checkAndSendIRQ
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
          // shadowBorderColor = borderColor // TODO : PIPE
          borderColor = value & 0x0F
          lastColorReg = 0xFA // value for border reg
        //Log.debug("VIC border color set to " + borderColor)
        case 33 | 34 | 35 | 36 =>
          // shadowBackgroundColor(offset - 33) = backgroundColor(offset - 33) // TODO : PIPE
          // backgroundColorChangedIndex = offset - 33 // TODO : PIPE
          backgroundColor(offset - 33) = value & 0x0F
          lastColorReg = offset - 33
        //Log.debug("VIC background color #%d set to %d".format(offset - 33,value))
        case 37 | 38 =>
          spriteMulticolor01(offset - 37) = value & 0x0F
          lastColorReg = offset - 33
        //Log.fine("Sprite multicolor #%d set to %d".format(offset - 37,value))
        case 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 =>
          val index = offset - 39
          sprites(index).color = value & 0x0F
          lastColorReg = offset - 33
        //Log.fine("Sprite #%d color set to %d".format(offset - 39,value))
        case _ => // $D02F-$D03F
      }
  }

  @inline private def setBaLow(low:Boolean) : Unit = {
    if (low != _baLow) {
      _baLow = low

      baLowFirstCycle = clk.currentCycles
      if (!(_2MhzMode && low)) baLow(low)
    }
  }

  @inline private def checkRasterIRQ() : Unit = {
    if (rasterLine == rasterLatch) {
      if (!rasterIRQTriggered) {
        rasterIRQTriggered = true
        rasterLineEqualsLatch
      }
    }
    else rasterIRQTriggered = false
  }

  final def clock() : Unit = {
    if (c128TestBitEnabled) {
      rasterCycle = 0
      updateRasterLine
      gfxShifter.reset
      return
    }
    refreshCycle = false

    // End of PHI2 phase

    if (badLine && rasterCycle > 14 && rasterCycle < 55) cAccess

    drawCycle

    /*
    if (backgroundColorChangedIndex != -1) {
      shadowBackgroundColor(backgroundColorChangedIndex) = backgroundColor(backgroundColorChangedIndex)
      backgroundColorChangedIndex = -1
    }
    if (shadowBorderColor != -1) {
      shadowBorderColor = -1
    }
    */ // TODO : PIPE

    if (rasterCycle == model.RASTER_CYCLES) {
      rasterCycle = 0
      updateRasterLine
      gfxShifter.reset
    }

    rasterCycle += 1

    if (showDebug) display.showFrame(firstModPixelX, firstModPixelY, lastModPixelX, lastModPixelY)

    // var dataToDraw = -1 // TODO : PIPE
    dataToDraw = -1
    // vmliToDrawPipe = vmliToDrawPipe << 8 | vmli // TODO : PIPE
    vmliToDraw = vmli

    if (rasterLine == 0x30) denOn30 |= den

    badLine = isBadLine

    if (badLine) isInDisplayState = true

    // ----------------------------------------------

    (rasterCycle: @switch) match {
      case 1 =>
        // check raster line with raster latch if irq enabled
        if (rasterLine > 0) checkRasterIRQ
      case 2 =>
        // check raster line with raster latch if irq enabled
        if (rasterLine == 0) checkRasterIRQ
      case 3|4|5|6|7|8|9|10 =>
      case 11 =>
        refreshAccess
      case 55 =>
        var c = 0
        while (c < 8) {
          sprites(c).check55_56(true)
          c += 1
        }
        if (isInDisplayState) {
          // g-access
          dataToDraw = gAccess
          vc = (vc + 1) & 0x3FF //% 1024
          vmli = (vmli + 1) & 0x3F
        }
        else dataToDraw = idleAccess
      case 56 =>
        var c = 0
        while (c < 8) {
          sprites(c).check55_56(false)
          c += 1
        }
      case 57 =>
      case 58 =>
        var c = 0
        while (c < 8) {
          sprites(c).check58
          c += 1
        }
        if (rc == 7) {
          isInDisplayState = false
          vcbase = vc
          //Log.fine(s"VIC cycle 58 vcbase=${vcbase}")
        }
        if (badLine || isInDisplayState) {
          rc = (rc + 1) & 7
          isInDisplayState = true
        }
      // ---------------------------------------------------------------
      case 59|60|61|62|63 =>
      case 12|13 =>
        setBaLow(badLine)
        refreshAccess
      case 14 =>
        setBaLow(badLine)
        refreshAccess
        vc = vcbase
        vmli = 0
        if (badLine) rc = 0
      case 15 =>
        setBaLow(badLine)
        refreshAccess
      case _ if rasterCycle < 55 => // 16 - 54
        setBaLow(badLine)
        if (isInDisplayState) {
          // g-access
          dataToDraw = gAccess
          vc = (vc + 1) & 0x3FF
          vmli = (vmli + 1) & 0x3F
        }
        else dataToDraw = idleAccess

        if (rasterCycle == 16) {
          var c = 0
          while (c < 8) {
            sprites(c).check16
            c += 1
          }
        }

      case _ => // rasterCycle > 54
    }

    // SPRITE & BA Handling -------------------------
    val sprInfo = model.SPRITE_BA_INFO(rasterCycle)
    if ((sprInfo & (1 << 17)) == 0) {
      val sprite = (sprInfo >> 8) & 0xFF
      if (sprite > 0) {
        val first = (sprInfo & (1 << 16)) > 0
        sprites(sprite - 1).readMemoryData(first)
      }
      if (sprite == 0 && !refreshCycle && dataToDraw == -1) mem.read(0x3FFF,ChipID.VIC)
      val ba = sprInfo & 0xFF
      setBaLow((spriteDMAon & ba) > 0)
    }
    // ----------------------------------------------

    internalDataBus = 0xFF
    if (coprocessor != null && coprocessor.isActive) coprocessor.cycle(rasterLine,rasterCycle)

    // PIPELINE
    /*
    dataToDrawPipe = dataToDrawPipe << 32 | (dataToDraw & 0xFFFFFFFFL)
    displayLineToDrawPipe = displayLineToDrawPipe << 16 | displayLine
    rasterCycleToDrawPipe = rasterCycleToDrawPipe << 8 | rasterCycle
    */ // TODO : PIPE
    bmmDelay = bmm
    ecmDelay = ecm
  }

  private def refreshAccess() : Unit = {
    mem.read(0x3F00 | ref,ChipID.VIC)
    ref = (ref - 1) & 0xFF // DRAM REFRESH
    refreshCycle = true
  }

  @inline private def idleAccess : Int = {
    if (_2MhzMode) mem.byteOnBUS
    else {
      val cop = if (coprocessor != null && coprocessor.isActive) coprocessor.g_access(rasterCycle) else -1
      if (cop == -1) mem.read(if (ecm) 0x39ff else 0x3fff,ChipID.VIC) else cop
    }
  }

  private[this] var showDebug = false
  def setShowDebug(showDebug:Boolean) : Unit = this.showDebug = showDebug

  @inline private[this] def updateRasterLine() : Unit = {
    displayLine += 1
    if (displayLine == model.RASTER_LINES) displayLine = 0
    rasterLine = displayLine + model.RASTER_OFFSET
    if (rasterLine >= model.RASTER_LINES) rasterLine -= model.RASTER_LINES
    // update the 8th bit of raster in control register 1
    if (rasterLine > 0xFF) controlRegister1 |= 0x80 else controlRegister1 &= 0x7F
    if (displayLine == 0) {
      denOn30 = false
      canUpdateLightPenCoords = true
      display.showFrame(firstModPixelX, firstModPixelY, lastModPixelX, lastModPixelY + 1)
      firstModPixelX = -1
      ref = 0xFF
      vcbase = 0
      vc = 0
      if (pendingDrawBorderModeChange) {
        pendingDrawBorderModeChange = false
        if (!drawBorderOpt) {
          mainBorderFF = false
          verticalBorderFF = false
        }
      }
    }
  }

  @inline private[this] def drawPixel(index: Int, y: Int, pixel: Int): Unit = {
    val color = VIC_II_RGB(pixel & 0x0F)
    if (displayMem(index) != color) {
      displayMem(index) = color
      if (firstModPixelX == -1) {
        firstModPixelY = y
        firstModPixelX = model.BLANK_LEFT_CYCLE << 3
      }
      lastModPixelY = y
    }
  }

  @inline private def drawCycle() : Unit = {
    // PIPELINE ===================
    /*
    rasterCycleToDraw = (rasterCycleToDrawPipe >> 8) & 0xFF
    displayLineToDraw = (displayLineToDrawPipe >> 16) & 0xFFFF
    dataToDraw = ((dataToDrawPipe >> 32) & 0xFFFFFFFF).toInt
    vmliToDraw = (vmliToDrawPipe >> 8) & 0xFF
     */ // TODO : PIPE
    // ============================
    val almostOneSprite = spritesDisplayedMask > 0

    // val outOfYScreen = displayLineToDraw <= model.BLANK_TOP_LINE || displayLineToDraw >= model.BLANK_BOTTOM_LINE // TODO : PIPE
    val outOfYScreen = displayLine <= model.BLANK_TOP_LINE || displayLine >= model.BLANK_BOTTOM_LINE
    isBlank = outOfYScreen
    if (outOfYScreen) return
    // val outOfXScreen = rasterCycleToDraw < model.BLANK_LEFT_CYCLE || rasterCycleToDraw > model.BLANK_RIGHT_CYCLE // TODO : PIPE
    val outOfXScreen = rasterCycle < model.BLANK_LEFT_CYCLE || rasterCycle > model.BLANK_RIGHT_CYCLE
    isBlank |= outOfXScreen

    val y = displayLine
    //val x = (rasterCycleToDraw - 1) << 3 // TODO : PIPE
    val x = (rasterCycle - 1) << 3
    var index = y * SCREEN_WIDTH + x
    var s, i = 0

    // --------------------- GFX -------------------------
    val borderOnOpt = verticalBorderFF && displayLine != TOP_BOTTOM_FF_COMP(rsel)(0)
    if (!borderOnOpt && dataToDraw >= 0) gfxShifter.producePixels(dataToDraw)
    // ------------------- Sprites -----------------------

    if (almostOneSprite)
      while (s < 8) {
        if (sprites(s).displayable) {
          sprites(s).producePixels
        }
        s += 1
      }
    // ------------------- Border ------------------------
    borderShifter.producePixels
    // ************************** RENDERING ************************************

    val gfxPixels = gfxShifter.getPixels
    val borderPixels = borderShifter.getPixels
    while (i < 8) { // scan each bit
      s = 7
      var pixel = PIXEL_TRANSPARENT
      // ---------------------------- Sprite pixel --------------------------
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
      // --------------------------------------------------------------------
      val gfxPixel = if (verticalBorderFF) backgroundColor(0) else if (dataToDraw < 0) backgroundColor(0) else gfxPixels(i)
      if (!verticalBorderFF && (gfxPixel & PIXEL_FOREGROUND) > 0 && (pixel & PIXEL_TRANSPARENT) == 0) spriteDataCollision((pixel >> 6) & 7) // sprite-data collision detected
      if ((borderPixels(i) & PIXEL_TRANSPARENT) == 0) pixel = borderPixels(i) // border is on
      else
      if ((pixel & PIXEL_TRANSPARENT) > 0 || ((gfxPixel & PIXEL_FOREGROUND) > 0 && (pixel & PIXEL_SPRITE_PRIORITY) > 0)) pixel = gfxPixel

      drawPixel(index, y, pixel)
      i += 1
      index += 1
    }
    // ************************** RESET SPRITES ********************************
    s = 0
    if (almostOneSprite)
      while (s < 8) {
        sprites(s).resetSprite
        s += 1
      }
    // *************************************************************************

    lastColorReg = 0xFF
  }

  @inline private def checkAndSendIRQ() : Unit = {
    if ((interruptControlRegister & interruptMaskRegister) == 0) {
      if ((interruptControlRegister & 0x80) != 0) {
        interruptControlRegister &= 0x7f
        irqAction(false)
      }
    }
    else
    if ((interruptControlRegister & 0x80) == 0) {
      interruptControlRegister |= 0x80
      irqAction(true)
    }
  }
  @inline private def rasterLineEqualsLatch() : Unit = {
    if ((interruptControlRegister & 1) == 0) {
      interruptControlRegister |= 1
      checkAndSendIRQ
    }
  }

  @inline private def spriteSpriteCollision(i: Int, j: Int) : Unit = {
    val mask = (1 << i) | (1 << j)
    val ssCollision = spriteSpriteCollision
    spriteSpriteCollision |= mask
    if (ssCollision == 0) {
      interruptControlRegister |= 4
      checkAndSendIRQ
    }
  }
  @inline private def spriteDataCollision(i: Int) : Unit = {
    val sbc = spriteBackgroundCollision
    spriteBackgroundCollision |= (1 << i)
    if (sbc == 0) {
      interruptControlRegister |= 2
      checkAndSendIRQ
    }
  }

  /**
   * To be called on bad lines
   */
  @inline private def cAccess() : Unit = {
    if (_baLow) {
      val busAvailable = clk.currentCycles - baLowFirstCycle > 2
      if (busAvailable) {
        val charCode = if (_2MhzMode) internalDataBus else mem.readPhi2(videoMatrixAddress | vc)
        val color = if (_2MhzMode) mem.byteOnBUS & 0x0F else colorMem.read(COLOR_ADDRESS | vc,ChipID.VIC) & 0x0F
        vml_p(vmli) = charCode
        vml_c(vmli) = color
      }
      else {
        vml_p(vmli) = 0xFF
        vml_c(vmli) = mem.readPCOpcode & 0x0F//0xFF // it should read memory at PC but...
      }
      //Log.fine(s"Reading video memory at ${videoMatrixAddress + offset}: charCode=${charCode} color=${color}")
    }
  }

  @inline private def gAccess : Int = {
    val coprocessor_gdata = if (!isVICIIe && coprocessor != null && coprocessor.isActive) coprocessor.g_access(rasterCycle) else -1
    if (coprocessor_gdata != -1) coprocessor_gdata
    else
    if (_2MhzMode) mem.byteOnBUS
    else {
      if ((bmm ^ bmmDelay) && (model == VIC_II_PAL && !(isVICIIe || isNewVICModel))) {
        val adrFrom = gAccessAddress(bmmDelay,ecmDelay)
        val adrTo = gAccessAddress(bmm,ecm)
        val address = if (!mem.isCharROMAddress(adrFrom) && mem.isCharROMAddress(adrTo)) {
          (adrFrom & 0xff) | (adrTo & 0x3f00)
        } else adrTo
        mem.read(address,ChipID.VIC)
      }
      else mem.read(gAccessAddress(bmm,ecm),ChipID.VIC)
    }

  }

  @inline private def gAccessAddress(bmm:Boolean,ecm:Boolean) : Int = {
    if (bmm) bitmapAddress | ((vc & 0x3ff) << 3) | rc
    else {
      val charCode = if (ecm) vml_p(vmli) & 0x3F else vml_p(vmli)
      characterAddress | (charCode << 3) | rc
    }
  }

  /*
   * A Bad Line Condition is given at any arbitrary clock cycle, if at the
 	negative edge of phi0 at the beginning of the cycle RASTER >= $30 and RASTER
 	<= $f7 and the lower three bits of RASTER are equal to YSCROLL and if the
 	DEN bit was set during an arbitrary cycle of raster line $30.
   */
  @inline private def isBadLine = rasterLine >= 0x30 && rasterLine <= 0xF7 && ((rasterLine & 7) == yscroll) && denOn30

  def getMemory: VIC_II_Memory = mem

  def c128TestBitEnabled(enabled:Boolean) : Unit = c128TestBitEnabled = enabled

  def isRefreshCycle: Boolean = refreshCycle

  def set2MhzMode(enabled:Boolean) : Unit = _2MhzMode = enabled

  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(videoMatrixAddress)
    out.writeInt(characterAddress)
    out.writeInt(bitmapAddress)
    out.writeInt(vcbase)
    out.writeInt(vc)
    out.writeInt(rc)
    out.writeInt(vmli)
    out.writeBoolean(isInDisplayState)
    out.writeInt(yscroll)
    out.writeInt(xscroll)
    out.writeInt(rsel)
    out.writeBoolean(den)
    out.writeBoolean(denOn30)
    out.writeBoolean(bmm)
    out.writeBoolean(ecm)
    out.writeInt(csel)
    out.writeBoolean(mcm)
    out.writeBoolean(res)
    out.writeBoolean(mainBorderFF)
    out.writeBoolean(verticalBorderFF)
    out.writeInt(rasterCycle)
    out.writeInt(spriteXCoord9thBit)
    out.writeInt(controlRegister1)
    out.writeInt(rasterLine)
    out.writeInt(displayLine)
    out.writeInt(rasterLatch)
    out.writeInt(spriteEnableRegister)
    out.writeInt(controlRegister2)
    out.writeInt(spriteYExpansion)
    out.writeInt(vicBaseAddress)
    out.writeInt(interruptControlRegister)
    out.writeInt(interruptMaskRegister)
    out.writeInt(spriteCollisionPriority)
    out.writeInt(spriteMulticolor)
    out.writeInt(spriteXExpansion)
    out.writeInt(spriteSpriteCollision)
    out.writeInt(spriteBackgroundCollision)
    out.writeInt(spriteDMAon)
    out.writeInt(spritesDisplayedMask)
    out.writeInt(dataToDraw)
    out.writeInt(vmliToDraw)
    out.writeBoolean(lightPenEnabled)
    out.writeBoolean(_baLow)
    out.writeBoolean(canUpdateLightPenCoords)
    out.writeInt(ref)
    out.writeInt(borderColor)
    out.writeBoolean(rasterIRQTriggered)
    out.writeObject(spriteXYCoord)
    out.writeObject(lightPenXYCoord)
    out.writeObject(backgroundColor)
    out.writeObject(spriteMulticolor01)
    out.writeObject(vml_p)
    out.writeObject(vml_c)
    out.writeInt(lastBackground)
    out.writeBoolean(isNewVICModel)
    /*
    out.writeObject(shadowBackgroundColor)
    out.writeInt(backgroundColorChangedIndex)
    out.writeInt(shadowBorderColor)
     */ // TODO : PIPE
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    videoMatrixAddress = in.readInt
    characterAddress = in.readInt
    bitmapAddress = in.readInt
    vcbase = in.readInt
    vc = in.readInt
    rc = in.readInt
    vmli = in.readInt
    isInDisplayState = in.readBoolean
    yscroll = in.readInt
    xscroll = in.readInt
    rsel = in.readInt
    den = in.readBoolean
    denOn30 = in.readBoolean
    bmm = in.readBoolean
    ecm = in.readBoolean
    csel = in.readInt
    mcm = in.readBoolean
    res = in.readBoolean
    mainBorderFF = in.readBoolean
    verticalBorderFF = in.readBoolean
    rasterCycle = in.readInt
    spriteXCoord9thBit = in.readInt
    controlRegister1 = in.readInt
    rasterLine = in.readInt
    displayLine = in.readInt
    rasterLatch = in.readInt
    spriteEnableRegister = in.readInt
    controlRegister2 = in.readInt
    spriteYExpansion = in.readInt
    vicBaseAddress = in.readInt
    interruptControlRegister = in.readInt
    interruptMaskRegister = in.readInt
    spriteCollisionPriority = in.readInt
    spriteMulticolor = in.readInt
    spriteXExpansion = in.readInt
    spriteSpriteCollision = in.readInt
    spriteBackgroundCollision = in.readInt
    spriteDMAon = in.readInt
    spritesDisplayedMask = in.readInt
    dataToDraw = in.readInt
    vmliToDraw = in.readInt
    lightPenEnabled = in.readBoolean
    _baLow = in.readBoolean
    canUpdateLightPenCoords = in.readBoolean
    ref = in.readInt
    borderColor = in.readInt
    rasterIRQTriggered = in.readBoolean
    loadMemory[Int](spriteXYCoord,in)
    loadMemory[Int](lightPenXYCoord,in)
    loadMemory[Int](backgroundColor,in)
    loadMemory[Int](spriteMulticolor01,in)
    loadMemory[Int](vml_p,in)
    loadMemory[Int](vml_c,in)
    lastBackground = in.readInt
    isNewVICModel = in.readBoolean
    /*
    loadMemory[Int](shadowBackgroundColor,in)
    backgroundColorChangedIndex = in.readInt
    shadowBorderColor = in.readInt
     */ // TODO : PIPE
  }
  protected def allowsStateRestoring : Boolean = true

  def getRasterLine : Int = rasterLine
  def getRasterCycle : Int = rasterCycle //if (rasterCycle == model.RASTER_CYCLES) 1 else rasterCycle + 1

  // COPROCESSOR ============================================================================================
  override def turnOnInterruptControlRegisterBits(value:Int) : Unit = {
    interruptControlRegister |= value
    checkAndSendIRQ
  }

  override def isAECAvailable: Boolean = {
    val baLowNext = baOnCycle(if (rasterCycle < model.RASTER_CYCLES) rasterCycle + 1 else 1)
    if (!baLowNext) true
    else {
      if (!_baLow) true // first cycle ba goes down
      else clk.currentCycles - baLowFirstCycle < 2
    }
  }

  private def baOnCycle(rasterCycle:Int) : Boolean = {
    val sprInfo = model.SPRITE_BA_INFO(rasterCycle)
    if ((sprInfo & (1 << 17)) == 0) {
      val ba = sprInfo & 0xFF
      rasterCycle match {
        case 55|56 =>
          ba > 0 && !sprites(ba - 1).dma && sprites(ba - 1).isReadyForDMA
        case _ =>
          (spriteDMAon & ba) > 0
      }
    }
    else badLine
  }
}

