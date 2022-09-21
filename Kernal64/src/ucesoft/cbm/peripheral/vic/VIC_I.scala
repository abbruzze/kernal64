package ucesoft.cbm.peripheral.vic
import ucesoft.cbm.{ChipID, Clock}
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.sid.AudioDriverDevice
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.peripheral.vic.coprocessor.VICCoprocessor

import java.awt.Dimension
import java.io.{ObjectInputStream, ObjectOutputStream}

class VIC_I(mem:Memory,audioDriver:AudioDriverDevice) extends VIC {
  override val componentID: String = "VIC_I"
  override val length: Int = 0x100
  override val startAddress: Int = 0x9000
  override val name: String = "VIC_I"

  override type Model = VICModel

  private type VState = Int
  private type HState = Int

  private final val TOP_BORDER          = 0
  private final val START_DISPLAY_AREA  = 1
  private final val BOTTOM_BORDER       = 2
  private final val DISPLAY_AREA        = 3
  //
  private final val IDLE_FETCH          = 0
  private final val FETCH_DELAY         = 1
  private final val FETCH_MATRIX        = 2
  private final val FETCH_CHAR          = 3
  private final val FETCH_MATRIX_2      = 4
  private final val FETCH_CHAR_2        = 5
  private final val FETCH_MATRIX_DRAW   = 6
  private final val FETCH_CHAR_DRAW     = 7
  private final val MATRIX_DRAW_LAST    = 8
  private final val MATRIX_DRAW_LAST2   = 9
  private final val MATRIX_DRAW_LAST3   = 10
  private final val MATRIX_DRAW_LAST4   = 11
  private final val END_FETCH           = 12

  /*
    36864 $8666 VICCR0
    Bit 7: Interlace scan bit. Default value: 0
    Bits 6-0: Horizontal TV picture origin. Default value: 5.

    These bits can be used to adjust the position where the first
    character appears on the left side of the TV picture. Possible values
    in this location are between 0 and 127 (although those above 16
    seem to confuse BASIC), with larger numbers moving the characters
    to the right. Every increase or decrease of this number by one shifts
    the TV display four pixels right or left. If this location is set to 0
    column 3 would be on the extreme left edge of the TV picture.
   */
  final private val VIC_CR0_HORIGIN = 0
  /*
    36865 $9001 V1CCR1
    Bits 7-0: Vertical TV picture origin.

    This location specifies where the top line of characters is dis
    played on the TV. The picture can be relocated by the addition or
    subtraction of one from this location; subtraction raises the picture
    on the screen, and addition lowers it. Each change of one in this
    value moves the TV display two pixels. A value of zero here causes
    the middle of the fourth line to be at the top of the TV.
   */
  final private val VIC_CR1_VORIGIN = 1
  /*
    36866 $8662 VICCR2
    Bit 7: Default value: 1. This bit serves as bit 9 of the 14-bit
    screen map address used by the VIC chip.
    If this bit is set to 0, the screen map RAM is located on a 1024-
    byte boundary, and the color map begins at location 37888 ($9400).
    When this bit is set to 1, the screen map RAM starts on a 512-byte
    boundary and the color map is at location 38400 ($9600).
    Bits 6-0: Default value: 22. These bits contain the number of
    character columns displayed on each TV display line.
   */
  final private val VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7 = 2
  /*
    36867 $9003 V1CCR3
    Bit 7: Default value: 1/0. Raster beam location bit 0.
    Bits 6-1: Default value: 46. Number of character lines displayed on
    the TV picture multiplied by two.
    Bit 0: Default value: 0. Character size 8 x 8, or 8 x 16 pixels.
    A zero in this bit position specifies 8x8 character size.
   */
  final private val VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE = 3
  /*
    36868 $9004 VICCR4
    Bits 7-0: When combined with the high order bit of location 36867
    ($9003) as the low order bit, this value tracks the location of the
    electron beam as it refreshes the TV picture.
   */
  final private val VIC_CR4_RASTER_H = 4
  /*
    36869 $9085 VICCR5
    Bits 7-4: Default value: 240. These bits serve as bits 13-10 of the
    screen map address, combined with bit 7 of 36866 ($9002) to form
    the 14-bit VIC chip screen map address.
    Bits 3-0: Default value: 0. These serve as bits 13-10 of the character
    map 14-bit address. They are used to form the 14-bit VIC chip
    address that points to the beginning of the current 2048-byte (2K)
    character map or custom character set.

    bits      address   Type of chars
    -------------------------------------
    3210      32768     uppercase
    0000      33792     uppercase rev
    0001      34816     lowercase
    O010      35840     lowercase rev
    O011      36864     VIC Chip !!
    0100      37888     Color Map !!
    0101      38912     I/O Block !!
    0110      39936     I/O Block !!
    0111      0000      Low RAM
    1000      1024      VIC can't access
    1001      2048      VIC can't access
    1010      3072      VIC can't access
    1011      4096      custom char set
    1100      5120      custom char set
    1101      6144      custom char set
    1111      7168      custom char set
   */
  final private val VIC_CR5_SCREEN_MAP_CHAR_MAP_ADDRESS = 5
  /*
    36670 $9006 VICCR6
    This address contains the pixel location of the light pen photo
    cell from the left side of the TV.
   */
  final private val VIC_CR6_H_LIGHTPEN = 6
  /*
    36871 $9007 VICCR7
    This address contains the pixel location of the light pen photo
    cell from the top of the TV.
   */
  final private val VIC_CR7_V_LIGHTPEN = 7
  /*
    36872 $9008 VICCR8
    The analog-to-digital converters in the VIC chip can convert
    variable resistance to digital values ranging from 0 to 255,
    incremented by one for every IK ohms of resistance. No connection
    means infinite resistance, so 255 is the default value in this location.
    The game port pin 9 is used for this X value, pin 5 for the Y value,
    and pin 8 for a common ground.
    Paddle values: right=0, left=255.
   */
  final private val VIC_CR8_PADDLE_X = 8
  /*
    36873 $9009 VICCR9
    Potentiometer Y/Paddle Y value.
   */
  final private val VIC_CR9_PADDLE_Y = 9
  /*
    36874 S900A VICCRA
    Bit 7: Switch to enable (1) or disable (0) this oscillator.
    Bits 6-0: Low sound voice (sawtooth waveform).
   */
  final private val VIC_CRA_SOUND_OSC_1 = 0xA
  /*
    36875 S900B VICCRB
    Bit 7: Switch to enable (1) or disable (0) this oscillator.
    Bits 6-0: Medium sound voice (pulse waveform).
   */
  final private val VIC_CRB_SOUND_OSC_2 = 0xB
  /*
    36876 $900C VICCRC
    Bit 7: Switch to enable (1) or disable (0) this oscillator.
    Bits 6-0: High sound voice (pulse waveform).
   */
  final private val VIC_CRC_SOUND_OSC_3 = 0xC
  /*
    36877 $900D VICCRD
    Bit 7: Switch to enable (1) or disable (0) this oscillator.
    Bits 6-0: Noise voice (square waveform).
   */
  final private val VIC_CRD_SOUND_OSC_4 = 0xD
  /*
    Bits 7-4: Auxiliary color for multicolor mode. A bit value of 11
    selects this auxiliary color. You can use any of the 16 colors:
    Black 0   Orange      8
    White 1   Lt Orange   9
    Red   2   Pink        10
    Cyan  3   Lt Cyan     11
    Purple4   Lt Purple   12
    Green 5   Lt Green    13
    Blue  6   Lt Blue     14
    Yellow7   Lt Yellow   15
    Bits 3-0: Sound volume 0 (low) to 15 (high).
   */
  final private val VIC_CRE_SOUND_VOLUME_AUX_COLOR = 0xE
  /*
    Bits 7-4: Default value: 1. These four bits determine the background
    color of the screen. Colors available on the VIC are:
    Black 0   Orange      8
    White 1   Lt Orange   9
    Red   2   Pink        10
    Cyan  3   Lt Cyan     11
    Purple4   Lt Purple   12
    Green 5   Lt Green    13
    Blue  6   Lt Blue     14
    Yellow7   Lt Yellow   15
    Bit 3: Default value: 1. This bit serves as the inverse color switch.
    When set to one, the background and foreground colors are in their
    respective places. Setting this to zero, however, inverts that scheme.
    The foreground color will be used for the background and all the
    characters are shaded in the background color.
    This bit has no effect when multicolor mode is in effect for
    individual characters.
    Bits 2-0: Default value: 3. These three bits control the border color
    surrounding the screen. Available border colors are:
    Black 0   Purple  4
    White 1   Green   5
    Red   2   Blue    6
    Cyan  3   Yellow  7
   */
  final private val VIC_CRF_BACKGROUND_BORDER_INV = 0xF

  final private val FETCH_DELAY_COUNT_PAL = 3
  final private val FETCH_DELAY_COUNT_NTSC = 2


  // VIC's registers
  final private val regs = Array.ofDim[Int](16)
  // Vertical & Horizontal state
  private var vState : VState = TOP_BORDER
  private var hState : HState = IDLE_FETCH
  // VIC Model
  private var model : Model = VIC_I_PAL
  // raster line
  private var rasterLine = 0
  // raster cycle
  private var rasterCycle = 0
  // display pointer
  private var displayPtr = 0
  // line increment
  private var displayPtrInc = 0
  // row counter
  private var rowCounter = 0
  // Y position within current row
  private var rowY = 0
  // latched value of number of columns
  private var latchedColumns = 0
  // latched value of number of rows
  private var latchedRows = 0
  // char height : 8 or 16
  private var charHeight = 8
  // fetched char code
  private var charCode = 0
  // graphic buffer pipeline
  private val gBuf = Array.ofDim[Int](0xFF)
  // color buffer pipeline
  private val cBuf = Array.ofDim[Int](0xFF)
  // column index
  private var displayCol = 0
  // canvas X position
  private var xpos = 0

  private var lightPenEnabled,lightPenTriggered = false
  private var lpX,lpY = 0

  private var display: Display = _
  private var displayMem: Array[Int] = _
  private val palette = Palette.VIC_RGB

  private var firstModPixelX, firstModPixelY = 0 // first x,y pixel coordinate modified
  private var lastModPixelY = 0 // last y pixel coordinate modified

  private var potx, poty = 0xFF

  private val audio = new VIC_I_Audio(audioDriver)

  private var testBenchMode = false

  // Constructor
  Palette.setPalette(PaletteType.VIC20_VICE)
  setVICModel(VIC_I_PAL)
  audio.setCPUFrequency(Clock.systemClock.getClockHz)
  Clock.systemClock.addChangeFrequencyListener(audio.setCPUFrequency _)

  override def SCREEN_WIDTH: Int = model.RASTER_CYCLES << 2
  override def SCREEN_HEIGHT: Int = model.RASTER_LINES
  override def VISIBLE_SCREEN_WIDTH: Int = (model.BLANK_RIGHT_CYCLE - model.BLANK_LEFT_CYCLE) << 2
  override def VISIBLE_SCREEN_HEIGHT: Int = model.BLANK_BOTTOM_LINE - model.BLANK_TOP_LINE
  override def SCREEN_ASPECT_RATIO: Double = VISIBLE_SCREEN_WIDTH.toDouble / VISIBLE_SCREEN_HEIGHT
  override def STANDARD_DIMENSION : Dimension = model match {
    case VIC_I_PAL => new Dimension(746,VISIBLE_SCREEN_HEIGHT << 1)
    case VIC_I_NTSC => new Dimension(730,VISIBLE_SCREEN_HEIGHT << 1)
  }
  override def TESTBENCH_DIMENSION : Dimension = new Dimension(568,284)

  def setTestBenchMode(enabled:Boolean): Unit = {
    testBenchMode = enabled
    if (enabled) display.setClipArea(0, 28,284,312)
    else display.setClipArea(model.BLANK_LEFT_CYCLE << 2, model.BLANK_TOP_LINE, model.BLANK_RIGHT_CYCLE << 2, model.BLANK_BOTTOM_LINE)
  }

  override def getRasterLine = rasterLine

  override def getRasterCycle = rasterCycle

  override def setShowDebug(showDebug:Boolean) : Unit = {}
  def setCoprocessor(cop:VICCoprocessor) : Unit = {}
  def getCoprocessor : Option[VICCoprocessor] = None

  def setDrawBorder(on:Boolean) : Unit = {}

  def enableLightPen(enabled: Boolean): Unit = lightPenEnabled = enabled

  def triggerLightPen(): Unit = {
    if (!lightPenTriggered) {
      lightPenTriggered = true
      lpX = (xpos >> 1) & 0xFF
      lpY = (rasterLine >> 1) & 0xFF
    }
  }

  override def setDisplay(display: Display): Unit = {
    this.display = display
    displayMem = display.displayMem

    display.setClipArea(model.BLANK_LEFT_CYCLE << 2, model.BLANK_TOP_LINE, model.BLANK_RIGHT_CYCLE << 2, model.BLANK_BOTTOM_LINE)
  }


  override def setVICModel(model: Model): Unit = {
    this.model = model
    lastModPixelY = model.RASTER_LINES
    firstModPixelX = -1
    firstModPixelY = 0
  }

  override def getVICModel(): Model = model

  override def reset(): Unit = {
    java.util.Arrays.fill(regs,0)
    rasterCycle = 0
    rasterLine = 0
    charHeight = 8
    displayPtr = 0
    rowCounter = 0
    displayPtrInc = 0
    displayCol = 0
    rowY = 0
    xpos = 0

    lastModPixelY = model.RASTER_LINES
    firstModPixelX = -1
    firstModPixelY = 0

    potx = 0xFF
    poty = 0xFF

    vState = TOP_BORDER
    hState = IDLE_FETCH
  }
  override def init(): Unit = {
    add(audio)
  }

  override def read(address: Int, chipID: ID): Int = {
    address & 0xF match {
      case VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE =>
        regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) | (rasterLine & 1) << 7
      case VIC_CR4_RASTER_H =>
        rasterLine >> 1
      case VIC_CR6_H_LIGHTPEN => lpX
      case VIC_CR7_V_LIGHTPEN => lpY
      case VIC_CR8_PADDLE_X => potx
      case VIC_CR9_PADDLE_Y => poty
      case adr =>
        regs(adr)
    }
  }
  override def write(address: Int, value: Int, chipID: ID): Unit = {
    regs(address & 0xF) = value
    address & 0xF match {
      case VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE =>
        charHeight = if ((value & 1) == 0) 8 else 16
      case VIC_CRA_SOUND_OSC_1 =>
        audio.writeOsc(0,value)
      case VIC_CRB_SOUND_OSC_2 =>
        audio.writeOsc(1,value)
      case VIC_CRC_SOUND_OSC_3 =>
        audio.writeOsc(2,value)
      case VIC_CRD_SOUND_OSC_4 =>
        audio.writeOsc(3,value)
      case VIC_CRE_SOUND_VOLUME_AUX_COLOR =>
        audio.setVolume(value & 0xF)
      case _ =>
    }
  }

  @inline private def openVerticalBorder(): Unit = {
    vState = START_DISPLAY_AREA
    if (latchedRows == 0) closeVerticalBorder()
  }

  @inline private def closeVerticalBorder(): Unit = {
    vState = BOTTOM_BORDER
    // TODO Display one more line if h-flipflop is already open
  }
  @inline private def openHorizontalBorder(): Unit = {
    hState = FETCH_DELAY
    if (vState == START_DISPLAY_AREA) {
      // first text character
      vState = DISPLAY_AREA
    }
    if (latchedColumns == 0) closeHorizontalBorder()
    displayCol = if (model == VIC_I_PAL) FETCH_DELAY_COUNT_PAL else FETCH_DELAY_COUNT_NTSC
    displayPtrInc = 0
  }
  @inline private def closeHorizontalBorder(): Unit = {
    hState = END_FETCH
  }

  @inline private def endOfLine(): Unit = {
    if (testBenchMode) {
      while (xpos < SCREEN_WIDTH) drawBorderCycle()
    }

    rasterLine += 1
    rasterCycle = 0
    hState = IDLE_FETCH
    xpos = 0

    if (vState == DISPLAY_AREA) {
      rowY += 1

      if ((rowY & (charHeight - 1)) == 0) { // go next line
        rowY = 0
        rowCounter += 1
        if (rowCounter == latchedRows) closeVerticalBorder()
        displayPtrInc = latchedColumns
      }

      displayPtr += displayPtrInc
      displayPtrInc = 0
    }
  }
  @inline private def endOfFrame(): Unit = {
    if (vState != BOTTOM_BORDER) closeVerticalBorder()

    displayPtr = 0
    displayPtrInc = 0
    rowCounter = 0
    rasterLine = 0
    vState = TOP_BORDER
    rowY = 0
    lightPenTriggered = false

    //display.showFrame(0,0,SCREEN_WIDTH,SCREEN_HEIGHT)
    display.showFrame(firstModPixelX, firstModPixelY, SCREEN_WIDTH, lastModPixelY + 1)
    firstModPixelX = -1
  }
  @inline private def latchRowsNumber(): Unit = {
    latchedRows = (regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) >> 1) & 0x3F
  }

  @inline private def latchColumnsNumber(): Unit = {
    val colsCandidate = regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x7F
    latchedColumns = if (colsCandidate > model.MAX_COLUMNS) model.MAX_COLUMNS else colsCandidate
  }

  @inline private def screenAddr(): Int = {
    val addr = ((regs(VIC_CR5_SCREEN_MAP_CHAR_MAP_ADDRESS) & 0xF0) << 6 | (regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x80) << 2)  + displayPtr + displayCol
    val msb = ~((addr & 0x2000) << 2) & 0x8000
    (addr & 0x1fff) | msb
  }

  @inline private def charMap(conf:Int): Int = {
    val k = conf & 0x7
    (((conf >> 3) & 1) ^ 1) << 15 | k << 10
  }

  @inline private def fetchMatrix(): Unit = {
    // fetches both char code and color code
    charCode = mem.read(screenAddr(),ChipID.VIC)
    val colorBaseAddress = 0x9400 + ((regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x80) << 2)
    var colorAddress = colorBaseAddress + displayPtr + displayCol
    if (colorAddress > 0x97FF) colorAddress = 0x9400 + (colorAddress & 0x3FF) // color address wrap-around
    cBuf(displayCol) = mem.read(colorAddress,ChipID.VIC)
  }
  @inline private def fetchChar(): Unit = {
    val charShift = if ((regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) & 1) == 0) 3 else 4
    val offset = (charCode << charShift) + (rowY & (charHeight - 1))
    val blocks = offset >> 10
    val conf = (regs(VIC_CR5_SCREEN_MAP_CHAR_MAP_ADDRESS) + blocks) & 0xF
    val target = charMap(conf) + (offset & 0x3FF)
    gBuf(displayCol) = mem.read(target,ChipID.VIC)

    displayCol += 1
    if (rowY == (charHeight - 1)) displayPtrInc = displayCol
  }

  @inline private def drawPixel(index:Int,color:Int): Unit = {
    if (displayMem(index) != color) {
      displayMem(index) = color
      if (firstModPixelX == -1) {
        firstModPixelY = rasterLine
        firstModPixelX = 0//model.BLANK_LEFT_CYCLE << 2
      }
      lastModPixelY = rasterLine
    }

    if (lightPenEnabled) {
      if (rasterLine == display.getLightPenY && xpos == display.getLightPenX) triggerLightPen()
    }
  }

  private def drawDisplayCycle(): Unit = {
    val index = if (displayCol - 2 >= 0) 2 else 1
    var gData = gBuf(displayCol - index)

    val ypos = rasterLine * SCREEN_WIDTH

    val backgroundColorIndex = regs(VIC_CRF_BACKGROUND_BORDER_INV) >> 4
    val invertColors = (regs(VIC_CRF_BACKGROUND_BORDER_INV) & 8) == 0

    val mc = (cBuf(displayCol - index) & 8) > 0
    val fgColorIndex = cBuf(displayCol - index) & 7

    var p = 0
    if (!mc) { // ========== HiRes mode =============================
      val fgColor = palette(if (!invertColors) fgColorIndex else backgroundColorIndex)
      val bgColor = palette(if (!invertColors) backgroundColorIndex else fgColorIndex)

      while (p < 4) {
        val pixel = gData & 0x80
        gData <<= 1
        drawPixel(ypos + xpos,if (pixel == 0) bgColor else fgColor)
        xpos += 1
        p += 1
      }
    }
    else { // ========== Multicolor mode ========================
      val auxColor = palette(regs(VIC_CRE_SOUND_VOLUME_AUX_COLOR) >> 4)
      val fgColor = palette(fgColorIndex)
      val bgColor = palette(backgroundColorIndex)
      val borderColor = palette(regs(VIC_CRF_BACKGROUND_BORDER_INV) & 7)

      while (p < 2) {
        val pixel = gData & 0xC0
        val mcColor = pixel match {
          case 0x00 /* 00 */ => bgColor
          case 0x40 /* 01 */ => borderColor
          case 0x80 /* 10 */ => fgColor
          case 0xC0 /* 11 */ => auxColor
        }
        gData <<= 2
        drawPixel(ypos + xpos,mcColor)
        drawPixel(ypos + xpos + 1,mcColor)
        xpos += 2
        p += 1
      }
    }

    gBuf(displayCol - index) = gData
  }

  private def drawBorderCycle(): Unit = {
    val ypos = rasterLine * SCREEN_WIDTH

    val borderColor = palette(regs(VIC_CRF_BACKGROUND_BORDER_INV) & 7)
    var p = 0
    while (p < 4) {
      drawPixel(ypos + xpos,borderColor)
      xpos += 1
      p += 1
    }
  }

  @inline private def fetchCycle(): Unit = {
    hState match {
      case IDLE_FETCH|END_FETCH =>
        // idle fetch => do nothing
        drawBorderCycle()
      case FETCH_DELAY =>
        if (displayCol == 0) hState = FETCH_MATRIX
        else displayCol -= 1
      case FETCH_MATRIX =>
        fetchMatrix()
        hState = FETCH_CHAR
      case FETCH_CHAR =>
        fetchChar()
        hState = if (displayCol == latchedColumns) MATRIX_DRAW_LAST else FETCH_MATRIX_2
      case FETCH_MATRIX_2 =>
        fetchMatrix()
        hState = FETCH_CHAR_2
      case FETCH_CHAR_2 =>
        fetchChar()
        hState = if (displayCol == latchedColumns) MATRIX_DRAW_LAST else FETCH_MATRIX_DRAW
      case FETCH_MATRIX_DRAW =>
        drawDisplayCycle()
        fetchMatrix()
        hState = FETCH_CHAR_DRAW
      case FETCH_CHAR_DRAW =>
        drawDisplayCycle()
        fetchChar()
        hState = if (displayCol == latchedColumns) MATRIX_DRAW_LAST else FETCH_MATRIX_DRAW
      case MATRIX_DRAW_LAST =>
        drawDisplayCycle()
        hState = MATRIX_DRAW_LAST2
      case MATRIX_DRAW_LAST2 =>
        drawDisplayCycle()
        displayCol += 1
        if (latchedColumns == 1) closeHorizontalBorder()
        else hState = MATRIX_DRAW_LAST3
      case MATRIX_DRAW_LAST3 =>
        drawDisplayCycle()
        hState = MATRIX_DRAW_LAST4
      case MATRIX_DRAW_LAST4 =>
        drawDisplayCycle()
        closeHorizontalBorder()
    }
  }

  final def clock() : Unit = {
    // check vertical border for opening
    if (vState == TOP_BORDER && regs(VIC_CR1_VORIGIN) == rasterLine >> 1) {
      openVerticalBorder()
    }

    rasterCycle += 1
    // check end of line
    if (rasterCycle == model.RASTER_CYCLES) {
      endOfLine()
      if (rasterLine == model.RASTER_LINES) endOfFrame()
    }

    // check horizontal border for opening
    if (vState == START_DISPLAY_AREA || vState == DISPLAY_AREA) {
      if (hState == IDLE_FETCH && (regs(VIC_CR0_HORIGIN) & 0x7F) == rasterCycle) openHorizontalBorder()
    }
    // first raster check
    if (rasterLine == 0) {
      // latch rows number
      if (rasterCycle == 2) latchRowsNumber()
    }
    // latch columns number
    if (rasterCycle == 1) {
      latchColumnsNumber()
    }

    fetchCycle()

    audio.audioClock()
  }

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeObject(regs)
    out.writeInt(vState)
    out.writeInt(hState)
    // model is injected from outside
    out.writeInt(rasterLine)
    out.writeInt(rasterCycle)
    out.writeInt(displayPtr)
    out.writeInt(displayPtrInc)
    out.writeInt(rowCounter)
    out.writeInt(rowY)
    out.writeInt(latchedColumns)
    out.writeInt(latchedRows)
    out.writeInt(charHeight)
    out.writeInt(charCode)
    out.writeObject(gBuf)
    out.writeObject(cBuf)
    out.writeInt(displayCol)
    out.writeInt(xpos)
  }
  override protected def loadState(in: ObjectInputStream): Unit = {
    loadMemory(regs,in)
    vState = in.readInt()
    hState = in.readInt()
    rasterLine = in.readInt()
    rasterCycle = in.readInt()
    displayPtr = in.readInt()
    displayPtrInc = in.readInt()
    rowCounter = in.readInt()
    rowY = in.readInt()
    latchedColumns = in.readInt()
    latchedRows = in.readInt()
    charHeight = in.readInt()
    charCode = in.readInt()
    loadMemory(gBuf,in)
    loadMemory(cBuf,in)
    displayCol = in.readInt()
    xpos = in.readInt()
  }
  override protected def allowsStateRestoring: Boolean = true
}
