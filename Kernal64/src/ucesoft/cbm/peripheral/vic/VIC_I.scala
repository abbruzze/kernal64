package ucesoft.cbm.peripheral.vic
import ucesoft.cbm.ChipID.ID

import java.io.{ObjectInputStream, ObjectOutputStream}

class VIC_I extends VIC {
  override val componentID: String = "VIC_I"
  override val length: Int = 0x110
  override val startAddress: Int = 0x9000
  override val name: String = "VIC_I"

  private trait VState
  private object TOP_BORDER extends VState
  private object BOTTOM_BORDER extends VState
  private object START_DISPLAY_AREA extends VState
  private object END_DISPLAY_AREA extends VState
  private object DISPLAY_AREA extends VState

  private trait HState
  private object IDLE_FETCH extends HState
  private object START_FETCH extends HState
  private object FETCH_MATRIX extends HState
  private object FETCH_CHAR extends HState
  private object END_FETCH extends HState

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
  private var vstate : VState = TOP_BORDER
  private var hstate : HState = IDLE_FETCH
  // VIC Model
  private var model : VICModel = _
  // raster line
  private var rasterLine = 0
  // raster cycle
  private var cycle = 0
  // number of columns so far
  private var textCols = 0
  // number of rows so far
  private var textRows = 0
  // Y position within current row
  private var rowY = 0
  // absolute position within screen area
  private var screenX, screenY = 0
  // latched value of number of columns
  private var latched_columns = 0
  // latched value of number of rows
  private var latched_rows = 0
  // char height : 8 or 16
  private var charHeight = 8


  override def setVICModel(model: VICModel): Unit = {
    this.model = model
  }

  override def reset(): Unit = ???
  override def init(): Unit = ???

  override def read(address: Int, chipID: ID): Int = ???
  override def write(address: Int, value: Int, chipID: ID): Unit = ???

  final def clock() : Unit = {

  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???
  override protected def loadState(in: ObjectInputStream): Unit = ???
  override protected def allowsStateRestoring: Boolean = true
}
