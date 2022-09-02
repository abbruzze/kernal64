package ucesoft.cbm.peripheral.vic
import ucesoft.cbm.{ChipID, Clock}
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.sid.AudioDriverDevice
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.peripheral.vic.coprocessor.VICCoprocessor

import java.io.{File, ObjectInputStream, ObjectOutputStream}

class VIC_I(mem:Memory,audioDriver:AudioDriverDevice) extends VIC {
  override val componentID: String = "VIC_I"
  override val length: Int = 0x110
  override val startAddress: Int = 0x9000
  override val name: String = "VIC_I"

  override type Model = VICModel

  private trait VState
  private case object TOP_BORDER extends VState
  private case object START_DISPLAY_AREA extends VState
  private case object BOTTOM_BORDER extends VState
  private case object DISPLAY_AREA extends VState

  private trait HState
  private case object IDLE_FETCH extends HState
  private case object FETCH_MATRIX extends HState
  private case object FETCH_CHAR extends HState
  private case object END_FETCH extends HState

  // ======================== Sound stuff ==============================
  private final val noisepattern = Array(7, 30, 30, 28, 28, 62, 60, 56, 120, 248, 124, 30, 31, 143, 7, 7, 193, 192, 224, 241, 224, 240, 227, 225, 192, 224, 120, 126, 60, 56, 224, 225, 195, 195, 135, 199, 7, 30, 28, 31, 14, 14, 30, 14, 15, 15, 195, 195, 241, 225, 227, 193, 227, 195, 195, 252, 60, 30, 15, 131, 195, 193, 193, 195, 195, 199, 135, 135, 199, 15, 14, 60, 124, 120, 60, 60, 60, 56, 62, 28, 124, 30, 60, 15, 14, 62, 120, 240, 240, 224, 225, 241, 193, 195, 199, 195, 225, 241, 224, 225, 240, 241, 227, 192, 240, 224, 248, 112, 227, 135, 135, 192, 240, 224, 241, 225, 225, 199, 131, 135, 131, 143, 135, 135, 199, 131, 195, 131, 195, 241, 225, 195, 199, 129, 207, 135, 3, 135, 199, 199, 135, 131, 225, 195, 7, 195, 135, 135, 7, 135, 195, 135, 131, 225, 195, 199, 195, 135, 135, 143, 15, 135, 135, 15, 207, 31, 135, 142, 14, 7, 129, 195, 227, 193, 224, 240, 224, 227, 131, 135, 7, 135, 142, 30, 15, 7, 135, 143, 31, 7, 135, 193, 240, 225, 225, 227, 199, 15, 3, 143, 135, 14, 30, 30, 15, 135, 135, 15, 135, 31, 15, 195, 195, 240, 248, 240, 112, 241, 240, 240, 225, 240, 224, 120, 124, 120, 124, 112, 113, 225, 225, 195, 195, 199, 135, 28, 60, 60, 28, 60, 124, 30, 30, 30, 28, 60, 120, 248, 248, 225, 195, 135, 30, 30, 60, 62, 15, 15, 135, 31, 142, 15, 15, 142, 30, 30, 30, 30, 15, 15, 143, 135, 135, 195, 131, 193, 225, 195, 193, 195, 199, 143, 15, 15, 15, 15, 131, 199, 195, 193, 225, 224, 248, 62, 60, 60, 60, 60, 60, 120, 62, 30, 30, 30, 15, 15, 15, 30, 14, 30, 30, 15, 15, 135, 31, 135, 135, 28, 62, 31, 15, 15, 142, 62, 14, 62, 30, 28, 60, 124, 252, 56, 120, 120, 56, 120, 112, 248, 124, 30, 60, 60, 48, 241, 240, 112, 112, 224, 248, 240, 248, 120, 120, 113, 225, 240, 227, 193, 240, 113, 227, 199, 135, 142, 62, 14, 30, 62, 15, 7, 135, 12, 62, 15, 135, 15, 30, 60, 60, 56, 120, 241, 231, 195, 195, 199, 142, 60, 56, 240, 224, 126, 30, 62, 14, 15, 15, 15, 3, 195, 195, 199, 135, 31, 14, 30, 28, 60, 60, 15, 7, 7, 199, 199, 135, 135, 143, 15, 192, 240, 248, 96, 240, 240, 225, 227, 227, 195, 195, 195, 135, 15, 135, 142, 30, 30, 63, 30, 14, 28, 60, 126, 30, 60, 56, 120, 120, 120, 56, 120, 60, 225, 227, 143, 31, 28, 120, 112, 126, 15, 135, 7, 195, 199, 15, 30, 60, 14, 15, 14, 30, 3, 240, 240, 241, 227, 193, 199, 192, 225, 225, 225, 225, 224, 112, 225, 240, 120, 112, 227, 199, 15, 193, 225, 227, 195, 192, 240, 252, 28, 60, 112, 248, 112, 248, 120, 60, 112, 240, 120, 112, 124, 124, 60, 56, 30, 62, 60, 126, 7, 131, 199, 193, 193, 225, 195, 195, 195, 225, 225, 240, 120, 124, 62, 15, 31, 7, 143, 15, 131, 135, 193, 227, 227, 195, 195, 225, 240, 248, 240, 60, 124, 60, 15, 142, 14, 31, 31, 14, 60, 56, 120, 112, 112, 240, 240, 248, 112, 112, 120, 56, 60, 112, 224, 240, 120, 241, 240, 120, 62, 60, 15, 7, 14, 62, 30, 63, 30, 14, 15, 135, 135, 7, 15, 7, 199, 143, 15, 135, 30, 30, 31, 30, 30, 60, 30, 28, 62, 15, 3, 195, 129, 224, 240, 252, 56, 60, 62, 14, 30, 28, 124, 30, 31, 14, 62, 28, 120, 120, 124, 30, 62, 30, 60, 31, 15, 31, 15, 15, 143, 28, 60, 120, 248, 240, 248, 112, 240, 120, 120, 60, 60, 120, 60, 31, 15, 7, 134, 28, 30, 28, 30, 30, 31, 3, 195, 199, 142, 60, 60, 28, 24, 240, 225, 195, 225, 193, 225, 227, 195, 195, 227, 195, 131, 135, 131, 135, 15, 7, 7, 225, 225, 224, 124, 120, 56, 120, 120, 60, 31, 15, 143, 14, 7, 15, 7, 131, 195, 195, 129, 240, 248, 241, 224, 227, 199, 28, 62, 30, 15, 15, 195, 240, 240, 227, 131, 195, 199, 7, 15, 15, 15, 15, 15, 7, 135, 15, 15, 14, 15, 15, 30, 15, 15, 135, 135, 135, 143, 199, 199, 131, 131, 195, 199, 143, 135, 7, 195, 142, 30, 56, 62, 60, 56, 124, 31, 28, 56, 60, 120, 124, 30, 28, 60, 63, 30, 14, 62, 28, 60, 31, 15, 7, 195, 227, 131, 135, 129, 193, 227, 207, 14, 15, 30, 62, 30, 31, 15, 143, 195, 135, 14, 3, 240, 240, 112, 224, 225, 225, 199, 142, 15, 15, 30, 14, 30, 31, 28, 120, 240, 241, 241, 224, 241, 225, 225, 224, 224, 241, 193, 240, 113, 225, 195, 131, 199, 131, 225, 225, 248, 112, 240, 240, 240, 240, 240, 112, 248, 112, 112, 97, 224, 240, 225, 224, 120, 113, 224, 240, 248, 56, 30, 28, 56, 112, 248, 96, 120, 56, 60, 63, 31, 15, 31, 15, 31, 135, 135, 131, 135, 131, 225, 225, 240, 120, 241, 240, 112, 56, 56, 112, 224, 227, 192, 224, 248, 120, 120, 248, 56, 241, 225, 225, 195, 135, 135, 14, 30, 31, 14, 14, 15, 15, 135, 195, 135, 7, 131, 192, 240, 56, 60, 60, 56, 240, 252, 62, 30, 28, 28, 56, 112, 240, 241, 224, 240, 224, 224, 241, 227, 224, 225, 240, 240, 120, 124, 120, 60, 120, 120, 56, 120, 120, 120, 120, 112, 227, 131, 131, 224, 195, 193, 225, 193, 193, 193, 227, 195, 199, 30, 14, 31, 30, 30, 15, 15, 14, 14, 14, 7, 131, 135, 135, 14, 7, 143, 15, 15, 15, 14, 28, 112, 225, 224, 113, 193, 131, 131, 135, 15, 30, 24, 120, 120, 124, 62, 28, 56, 240, 225, 224, 120, 112, 56, 60, 62, 30, 60, 30, 28, 112, 60, 56, 63)
  private class Oscillator {
    var shift = 0
    var counter = 0
    var frequency = 0
    var enabled = false

    def reset(): Unit = {
      shift = 0
      counter = 0
      frequency = 0
      enabled = false
    }

    def amp(): Int = if ((shift & 0x80) == 0x80) 0xFF else 0
    def set(value: Int): Unit = {
      enabled = (value & 0x80) == 0x80
      frequency = value & 0x7F
    }

    def generate(): Unit = {
      if (counter >= 127) {
        counter = frequency
        shift = shift << 1 | (shift >> 7 ^ 1) & (if (enabled) 1 else 0)
      }
      else counter += 1
    }
  }
  private class Noise extends Oscillator {
    private var noisectr = 0

    override def reset(): Unit = {
      super.reset()
      noisectr = 0
    }

    override def generate(): Unit = {
      if (counter >= 127) {
        counter = frequency
        val ff = enabled && (noisepattern(noisectr >> 3 & 0x3FF) >> (noisectr & 7) & 1) == 1
        shift = if (ff) 0xFF else 0
        noisectr += 1
      }
      else counter += 1
    }
  }

  @inline private def getAudioSample(): Int = (osc(0).amp() + osc(1).amp() + osc(2).amp() + osc(3).shift + 1024) * soundVolume

  def setCPUFrequency(f: Double): Unit = {
    val CPU_FREQ = f.toInt
    AUDIO_CLOCKS_PER_SAMPLE = CPU_FREQ / audioDriver.sampleRate
  }

  private final val osc = Array(new Oscillator,new Oscillator,new Oscillator,new Noise)
  private var soundVolume = 0
  private val clk = Clock.systemClock
  private var AUDIO_CLOCKS_PER_SAMPLE = clk.getClockHz.toInt / audioDriver.sampleRate
  private var audioClocksPerSampleCounter = 0
  // ===================================================================
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

  // VIC's registers
  final private val regs = Array.ofDim[Int](16)
  // Vertical & Horizontal state
  private var vState : VState = TOP_BORDER
  private var hState : HState = IDLE_FETCH
  // VIC Model
  private var model : Model = _
  // raster line
  private var rasterLine = 0
  // raster cycle
  private var rasterCycle = 0
  // display pointer
  private var displayPtr = 0
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
  // graphic buffer
  private var gBuf = 0
  // color buffer
  private var cBuf = 0
  // column index
  private var displayCol = 0
  // canvas X position
  private var xpos = 0

  private var drawBorderOn = true
  private var lightPenEnabled = false

  private var display: Display = _
  private var displayMem: Array[Int] = _
  private val palette = Palette.VIC_RGB

  // Constructor
  Palette.setPalette(PaletteType.VIC20_VICE)
  setVICModel(VIC_I_PAL)
  setCPUFrequency(Clock.systemClock.getClockHz)
  clk.addChangeFrequencyListener(setCPUFrequency _)

  override def SCREEN_WIDTH: Int = model.RASTER_CYCLES << 2
  override def SCREEN_HEIGHT: Int = model.RASTER_LINES
  override def VISIBLE_SCREEN_WIDTH: Int = (model.BLANK_RIGHT_CYCLE - model.BLANK_LEFT_CYCLE) << 2
  override def VISIBLE_SCREEN_HEIGHT: Int = model.BLANK_BOTTOM_LINE - model.BLANK_TOP_LINE
  override def SCREEN_ASPECT_RATIO: Double = VISIBLE_SCREEN_WIDTH.toDouble / VISIBLE_SCREEN_HEIGHT

  override def getRasterLine = rasterLine

  override def getRasterCycle = rasterCycle

  override def setShowDebug(showDebug:Boolean) : Unit = {}
  def setCoprocessor(cop:VICCoprocessor) : Unit = {}
  def getCoprocessor : Option[VICCoprocessor] = None

  def setDrawBorder(on:Boolean) : Unit = drawBorderOn = on

  def enableLightPen(enabled: Boolean): Unit = lightPenEnabled = enabled

  def triggerLightPen(): Unit = {}

  override def setDisplay(display: Display): Unit = {
    this.display = display
    displayMem = display.displayMem

    display.setClipArea(model.BLANK_LEFT_CYCLE << 2, model.BLANK_TOP_LINE, model.BLANK_RIGHT_CYCLE << 2, model.BLANK_BOTTOM_LINE)
  }


  override def setVICModel(model: Model): Unit = {
    this.model = model
  }

  override def getVICModel(): Model = model

  override def reset(): Unit = {
    soundVolume = 0
    for(o <- osc) o.reset()
    // TODO
  }
  override def init(): Unit = {}

  override def read(address: Int, chipID: ID): Int = {
    address & 0xF match {
      case VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE =>
        regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) | (rasterLine & 1) << 7
      case VIC_CR4_RASTER_H =>
        rasterLine >> 1
      case adr =>
        regs(adr)
    }
  }
  override def write(address: Int, value: Int, chipID: ID): Unit = {
    regs(address & 0xF) = value
    address & 0xF match {
      case VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE =>
        charHeight = if ((value & 1) == 0) 8 else 16
      case 0xA =>
        osc(0).set(value)
      case 0xB =>
        osc(1).set(value)
      case 0xC =>
        osc(2).set(value)
      case 0xD =>
        osc(3).set(value)
      case 0xE =>
        soundVolume = value & 0xF
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
    hState = FETCH_MATRIX
    if (vState == START_DISPLAY_AREA) {
      // first text character
      vState = DISPLAY_AREA
    }
    if (latchedColumns == 0) closeHorizontalBorder()
    displayCol = 0
  }
  @inline private def closeHorizontalBorder(): Unit = {
    hState = END_FETCH
  }

  @inline private def startOfTextLine(): Unit = {}
  @inline private def endOfLine(): Unit = {
    rasterCycle = 0
    xpos = 0
    hState = IDLE_FETCH
    rasterLine += 1

    if (vState == DISPLAY_AREA) {
      rowY += 1

      if (rowY == charHeight) { // go next line
        rowY = 0
        rowCounter += 1
        if (rowCounter == latchedRows) closeVerticalBorder()
        displayPtr += latchedColumns
      }
    }
  }
  @inline private def endOfFrame(): Unit = {
    if (vState != BOTTOM_BORDER) closeVerticalBorder()

    displayPtr = 0
    rowCounter = 0
    rasterLine = 0
    vState = TOP_BORDER
    rowY = 0

    display.showFrame(0,0,SCREEN_WIDTH,SCREEN_HEIGHT)
  }
  @inline private def latchRowsNumber(): Unit = {
    latchedRows = (regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) >> 1) & 0x3F
  }

  @inline private def latchColumnsNumber(): Unit = {
    val colsCandidate = regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x7F
    latchedColumns = if (colsCandidate > model.MAX_COLUMNS) model.MAX_COLUMNS else colsCandidate
  }

  @inline private def screenMap(): Int = {
    (regs(VIC_CR5_SCREEN_MAP_CHAR_MAP_ADDRESS) & 0x70) << 6 | // mask to 0 bit 7, mapping the VIC pag. 129
    (regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x80) << 2
  }

  @inline private def charMap(conf:Int): Int = {
    val k = conf & 0x7
    (((conf >> 3) & 1) ^ 1) << 15 | k << 10
  }

  @inline private def fetchMatrix(): Unit = {
    // fetches both char code and color code
    gBuf = mem.read(screenMap() + displayPtr + displayCol,ChipID.VIC)
    val colorBaseAddress = 0x9400 + ((regs(VIC_CR2_COLS_SCREEN_MAP_ADDRESS_7) & 0x80) << 2)
    cBuf = mem.read(colorBaseAddress + displayPtr + displayCol,ChipID.VIC)
  }
  @inline private def fetchChar(): Unit = {
    val charShift = if ((regs(VIC_CR3_TEXT_ROW_DISPLAYED_RASTER_L_CHAR_SIZE) & 1) == 0) 3 else 4
    val offset = (gBuf << charShift) + rowY
    val blocks = offset >> 10
    val conf = (regs(VIC_CR5_SCREEN_MAP_CHAR_MAP_ADDRESS) + blocks) & 0xF
    val target = charMap(conf) + (offset & 0x3FF)
    gBuf = mem.read(target,ChipID.VIC)

    displayCol += 1
  }

  private def drawDisplayCycle(): Unit = {
    val ypos = rasterLine * SCREEN_WIDTH

    val backgroundColorIndex = regs(VIC_CRF_BACKGROUND_BORDER_INV) >> 4
    val invertColors = (regs(VIC_CRF_BACKGROUND_BORDER_INV) & 8) == 0

    val mc = (cBuf & 8) > 0
    val fgColorIndex = cBuf & 7

    var p = 0
    if (!mc) { // ========== HiRes mode =============================
      val fgColor = palette(if (!invertColors) fgColorIndex else backgroundColorIndex)
      val bgColor = palette(if (!invertColors) backgroundColorIndex else fgColorIndex)

      while (p < 8) {
        val pixel = gBuf & 0x80
        gBuf <<= 1
        displayMem(ypos + xpos) = if (pixel == 0) bgColor else fgColor
        xpos += 1
        p += 1
      }
    }
    else { // ========== Multicolor mode ========================
      val auxColor = palette(regs(VIC_CRE_SOUND_VOLUME_AUX_COLOR) >> 4)
      val fgColor = palette(fgColorIndex)
      val bgColor = palette(backgroundColorIndex)
      val borderColor = palette(regs(VIC_CRF_BACKGROUND_BORDER_INV) & 7)

      while (p < 4) {
        val pixel = gBuf & 0xC0
        val mcColor = pixel match {
          case 0x00 /* 00 */ => bgColor
          case 0x40 /* 01 */ => borderColor
          case 0x80 /* 10 */ => fgColor
          case 0xC0 /* 11 */ => auxColor
        }
        gBuf <<= 2
        displayMem(ypos + xpos) = mcColor
        displayMem(ypos + xpos + 1) = mcColor
        xpos += 2
        p += 1
      }
    }
  }

  private def drawBorderCycle(): Unit = {
    val ypos = rasterLine * SCREEN_WIDTH

    val borderColor = palette(regs(VIC_CRF_BACKGROUND_BORDER_INV) & 7)
    var p = 0
    while (p < 4) {
      displayMem(ypos + xpos) = borderColor
      xpos += 1
      p += 1
    }
  }

  @inline private def fetchCycle(): Unit = {
    hState match {
      case IDLE_FETCH|END_FETCH =>
        // idle fetch => do nothing
        drawBorderCycle()
      case FETCH_MATRIX =>
        fetchMatrix()
        hState = FETCH_CHAR
      case FETCH_CHAR =>
        fetchChar()
        drawDisplayCycle()

        if (displayCol == latchedColumns)
          closeHorizontalBorder()
        else
          hState = FETCH_MATRIX
    }
  }

  final def clock() : Unit = {
    // check vertical border for opening
    if (vState == TOP_BORDER && regs(VIC_CR1_VORIGIN) == rasterLine >> 1) {
      openVerticalBorder()
    }

    // check end of line
    if (rasterCycle == model.RASTER_CYCLES) {
      endOfLine()
      if (rasterLine == model.RASTER_LINES) endOfFrame()
    }
    // check horizontal border for opening
    if (vState == START_DISPLAY_AREA || vState == DISPLAY_AREA) {
      if (hState == IDLE_FETCH && (regs(VIC_CR0_HORIGIN) & 0x7F) == rasterCycle) openHorizontalBorder()
      // check for start of line
      if (vState == DISPLAY_AREA && rasterCycle == 0) startOfTextLine()
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
    rasterCycle += 1

    audioClock()
  }

  private def audioClock(): Unit = {
    val cycles = clk.currentCycles
    if ((cycles & 4) == 4) {
      osc(2).generate()
      if ((cycles & 8) == 8) {
        osc(1).generate()
        if ((cycles & 0x10) == 0x10) {
          osc(0).generate()
          if ((cycles & 0x20) == 0x20) {
            osc(3).generate()
          }
        }
      }
    }
    audioClocksPerSampleCounter += 1
    if (audioClocksPerSampleCounter == AUDIO_CLOCKS_PER_SAMPLE) {
      audioClocksPerSampleCounter = 0
      audioDriver.addSample(getAudioSample())
    }
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???
  override protected def loadState(in: ObjectInputStream): Unit = ???
  override protected def allowsStateRestoring: Boolean = true
}
