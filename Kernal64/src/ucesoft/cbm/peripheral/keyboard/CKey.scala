package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.{C128Model, C64Model, CBMComputerModel, CBMIIModel, VIC20Model}

object CKey extends Enumeration {
  type Key = Value
  /* ============================ C64 =============================
        7   6   5   4   3   2   1   0
      --------------------------------
    7| STP  /   ,   N   V   X  LSH CDN    STP=RUN/STOP, LSH=Left SHIFT, CDN=CRSR-Down
     |
    6|  Q  UPA  @   O   U   T   E  F5     UPA=Up Arrow
     |
    5| CBM  =   :   K   H   F   S  F3     CBM=Commodore logo
     |
    4| SPC RSH  .   M   B   C   Z  F1     RSH=Right SHIFT
     |
    3|  2  HOM  -   0   8   6   4  F7     HOM=CLR/HOME
     |
    2| CTL  ;   L   J   G   D   A  CRT    CTL=CTRL, CRT=CRSR-Right
     |
    1| LFA  *   P   I   Y   R   W  RET    LFA=Left Arrow, RET=RETURN
     |
    0|  1  BP   +   9   7   5   3  DEL    BP=British Pound/Lira
   */
  // row 0
  val INST_DEL = Value(0x00)
  val RETURN = Value(0x01)
  val CRSR_LR = Value(0x02)
  val F7 = Value(0x03)
  val F1 = Value(0x04)
  val F3 = Value(0x05)
  val F5 = Value(0x06)
  val CRSR_UD = Value(0x07)
  // row 1
  val N_3 = Value(0x10)
  val W = Value(0x11)
  val A = Value(0x12)
  val N_4 = Value(0x13)
  val Z = Value(0x14)
  val S = Value(0x15)
  val E = Value(0x16)
  val L_SHIFT = Value(0x17)
  // row 2
  val N_5 = Value(0x20)
  val R = Value(0x21)
  val D = Value(0x22)
  val N_6 = Value(0x23)
  val C = Value(0x24)
  val F = Value(0x25)
  val T = Value(0x26)
  val X = Value(0x27)
  // row 3
  val N_7 = Value(0x30)
  val Y = Value(0x31)
  val G = Value(0x32)
  val N_8 = Value(0x33)
  val B = Value(0x34)
  val H = Value(0x35)
  val U = Value(0x36)
  val V = Value(0x37)
  // row 4
  val N_9 = Value(0x40)
  val I = Value(0x41)
  val J = Value(0x42)
  val N_0 = Value(0x43)
  val M = Value(0x44)
  val K = Value(0x45)
  val O = Value(0x46)
  val N = Value(0x47)
  // row 5
  val PLUS = Value(0x50)
  val P = Value(0x51)
  val L = Value(0x52)
  val MINUS = Value(0x53)
  val PERIOD = Value(0x54)
  val COLON = Value(0x55)
  val AT = Value(0x56)
  val COMMA = Value(0x57)
  // row 6
  val LIRA = Value(0x60)
  val STAR = Value(0x61)
  val SEMICOL = Value(0x62)
  val CLR_HOME = Value(0x63)
  val R_SHIFT = Value(0x64)
  val EQUAL = Value(0x65)
  val EXP = Value(0x66)
  val SLASH = Value(0x67)
  // row 7
  val N_1 = Value(0x70)
  val L_ARROW = Value(0x71)
  val CTRL = Value(0x72)
  val N_2 = Value(0x73)
  val SPACE = Value(0x74)
  val CBM = Value(0x75)
  val Q = Value(0x76)
  val RUN_STOP = Value(0x77)
  /* ============================ C128 ============================
          K2  K1  K0  7   6   5   4   3   2   1   0
     ------------ - -------------------------------
   7| NOS KP3 KP1   STP  /   ,   N   V   X  LSH CDN    STP=RUN/STOP, LSH=Left SHIFT,
    |                                                  CDN=CRSR-Down
   6| KPR KP9 KP7    Q  UPA  @   O   U   T   E  F5     UPA=Up Arrow
    |
   5| KPL KP6 KP4   CBM  =   :   K   H   F   S  F3     CBM=Commodore logo
    |
   4| KPD ENT KP2   SPC RSH  .   M   B   C   Z  F1     RSH=Right SHIFT
    |
   3| KPU LNF TAB    2  HOM  -   0   8   6   4  F7     HOM=CLR/HOME
    |
   2| KP. KP- KP5   CTL  ;   L   J   G   D   A  CRT    CTL=CTRL, CRT=CRSR-Right
    |
   1| KP0 KP+ KP8   LFA  *   P   I   Y   R   W  RET    LFA=Left Arrow, RET=RETURN
    |
   0| ALT ESC HLP    1  BP   +   9   7   5   3  DEL    BP=British Pound
   */
  // C128 rows, add an offset 0x100
  // row 0
  val HELP = Value(0x100)
  val KEYPAD_N_8 = Value(0x101)
  val KEYPAD_N_5 = Value(0x102)
  val TAB = Value(0x103)
  val KEYPAD_N_2 = Value(0x104)
  val KEYPAD_N_4 = Value(0x105)
  val KEYPAD_N_7 = Value(0x106)
  val KEYPAD_N_1 = Value(0x107)
  // row 1
  val ESC = Value(0x110)
  val KEYPAD_PLUS = Value(0x111)
  val KEYPAD_MINUS = Value(0x112)
  val LINE_FEED = Value(0x113)
  val KEYPAD_ENTER = Value(0x114)
  val KEYPAD_N_6 = Value(0x115)
  val KEYPAD_N_9 = Value(0x116)
  val KEYPAD_N_3 = Value(0x117)
  // row 2
  val ALT = Value(0x120)
  val KEYPAD_N_0 = Value(0x121)
  val KEYPAD_DOT = Value(0x122)
  val KEYPAD_UP = Value(0x123)
  val KEYPAD_DOWN = Value(0x124)
  val KEYPAD_LEFT = Value(0x125)
  val KEYPAD_RIGHT = Value(0x126)
  val NO_SCROLL = Value(0x127)
  // special
  val _40_80 = Value(0x128)
  val CAPS_LOCK = Value(0x129)

  val RESTORE = Value(0xFF)

  // ============================ CBM2 ============================
  // CBM2 rows, add an offset 0x200
  // A
  val CBM2_F1 = Value(0x200)
  val CBM2_F2 = Value(0x201)
  val CBM2_F3 = Value(0x202)
  val CBM2_F4 = Value(0x203)
  val CBM2_F5 = Value(0x204)
  val CBM2_F6 = Value(0x205)
  val CBM2_F7 = Value(0x206)
  val CBM2_F8 = Value(0x207)
  val CBM2_F9 = Value(0x208)
  val CBM2_F10 = Value(0x209)
  val CBM2_DOWN = Value(0x20A)
  val CBM2_UP = Value(0x20B)
  val CBM2_HOME = Value(0x20C)
  val CBM2_RVS = Value(0x20D)
  val CBM2_GRAPH = Value(0x20E)
  val CBM2_STOP = Value(0x20F)
  // B
  val CBM2_ESC = Value(0x210)
  val CBM2_1 = Value(0x211)
  val CBM2_2 = Value(0x212)
  val CBM2_3 = Value(0x213)
  val CBM2_4 = Value(0x214)
  val CBM2_5 = Value(0x215)
  val CBM2_7 = Value(0x216)
  val CBM2_8 = Value(0x217)
  val CBM2_9 = Value(0x218)
  val CBM2_0 = Value(0x219)
  val CBM2_EQUAL = Value(0x21A)
  val CBM2_LEFT = Value(0x21B)
  val CBM2_KP_QMARK = Value(0x21C)
  val CBM2_KP_CE = Value(0x21D)
  val CBM2_KP_MUL = Value(0x21E)
  val CBM2_KP_DIV = Value(0x21F)
  // C
  val CBM2_TAB = Value(0x220)
  val CBM2_Q = Value(0x221)
  val CBM2_W = Value(0x222)
  val CBM2_E = Value(0x223)
  val CBM2_R = Value(0x224)
  val CBM2_6 = Value(0x225)
  val CBM2_U = Value(0x226)
  val CBM2_I = Value(0x227)
  val CBM2_O = Value(0x228)
  val CBM2_MINUS = Value(0x229)
  val CBM2_BARROW = Value(0x22A)
  val CBM2_RIGHT = Value(0x22B)
  val CBM2_KP_7 = Value(0x22C)
  val CBM2_KP_8 = Value(0x22D)
  val CBM2_KP_9 = Value(0x22E)
  val CBM2_KP_MINUS = Value(0x22F)
  // D
  val CBM2_A = Value(0x231)
  val CBM2_S = Value(0x232)
  val CBM2_D = Value(0x233)
  val CBM2_T = Value(0x234)
  val CBM2_Y = Value(0x235)
  val CBM2_J = Value(0x236)
  val CBM2_K = Value(0x237)
  val CBM2_L = Value(0x238)
  val CBM2_P = Value(0x239)
  val CBM2_CLOSED_SBRACKET = Value(0x23A)
  val CBM2_DEL = Value(0x23B)
  val CBM2_KP_4 = Value(0x23C)
  val CBM2_KP_5 = Value(0x23D)
  val CBM2_KP_6 = Value(0x23E)
  val CBM2_KP_PLUS = Value(0x23F)
  // E
  val CBM2_SHIFT = Value(0x240)
  val CBM2_Z = Value(0x241)
  val CBM2_X = Value(0x242)
  val CBM2_F = Value(0x243)
  val CBM2_G = Value(0x244)
  val CBM2_H = Value(0x245)
  val CBM2_M = Value(0x246)
  val CBM2_COMMA = Value(0x247)
  val CBM2_SEMICOL = Value(0x248)
  val CBM2_OPEN_SBRACKET = Value(0x249)
  val CBM2_RETURN = Value(0x24A)
  val CBM2_COMMODORE = Value(0x24B)
  val CBM2_KP_1 = Value(0x24C)
  val CBM2_KP_2 = Value(0x24D)
  val CBM2_KP_3 = Value(0x24E)
  val CBM2_KP_ENTER = Value(0x24F)
  // F
  val CBM2_CONTROL = Value(0x250)
  val CBM2_C = Value(0x252)
  val CBM2_V = Value(0x253)
  val CBM2_B = Value(0x254)
  val CBM2_N = Value(0x255)
  val CBM2_SPACE = Value(0x256)
  val CBM2_DOT = Value(0x257)
  val CBM2_SLASH = Value(0x258)
  val CBM2_DB_QUOTE = Value(0x259)
  val CBM2_PI = Value(0x25A)
  val CBM2_KP_0 = Value(0x25C)
  val CBM2_KP_DOT = Value(0x25D)
  val CBM2_KP_00 = Value(0x25E)

  /* ============================ VIC20 ===========================
      7   6   5   4   3   2   1   0
     --------------------------------
   7| F7  F5  F3  F1  CDN CRT RET DEL    CRT=Cursor-Right, CDN=Cursor-Down
    |
   6| HOM UA  =   RSH /   ;   *   BP     BP=British Pound, RSH=Should be Right-SHIFT,
    |                                    UA=Up Arrow
   5| -   @   :   .   ,   L   P   +
    |
   4| 0   O   K   M   N   J   I   9
    |
   3| 8   U   H   B   V   G   Y   7
    |
   2| 6   T   F   C   X   D   R   5
    |
   1| 4   E   S   Z   LSH A   W   3      LSH=Should be Left-SHIFT
    |
   0| 2   Q   CBM SPC STP CTL LA  1      LA=Left Arrow, CTL=Should be CTRL, STP=RUN/STOP
    |                                    CBM=Commodore key
   */
  // row 7
  val VIC20_INST_DEL = Value(0x470)
  val VIC20_RETURN = Value(0x471)
  val VIC20_CRSR_LR = Value(0x472)
  val VIC20_CRSR_UD = Value(0x473)
  val VIC20_F1 = Value(0x474)
  val VIC20_F3 = Value(0x475)
  val VIC20_F5 = Value(0x476)
  val VIC20_F7 = Value(0x477)
  // row 6
  val VIC20_LIRA = Value(0x460)
  val VIC20_STAR = Value(0x461)
  val VIC20_SEMICOL = Value(0x462)
  val VIC20_SLASH = Value(0x463)
  val VIC20_R_SHIFT = Value(0x464)
  val VIC20_EQUAL = Value(0x465)
  val VIC20_EXP = Value(0x466)
  val VIC20_CLR_HOME = Value(0x467)
  // row 5
  val VIC20_PLUS = Value(0x450)
  val VIC20_P = Value(0x451)
  val VIC20_L = Value(0x452)
  val VIC20_COMMA = Value(0x453)
  val VIC20_PERIOD = Value(0x454)
  val VIC20_COLON = Value(0x455)
  val VIC20_AT = Value(0x456)
  val VIC20_MINUS = Value(0x457)
  // row 4
  val VIC20_N_9 = Value(0x440)
  val VIC20_I = Value(0x441)
  val VIC20_J = Value(0x442)
  val VIC20_N = Value(0x443)
  val VIC20_M = Value(0x444)
  val VIC20_K = Value(0x445)
  val VIC20_O = Value(0x446)
  val VIC20_N_0 = Value(0x447)
  // row 3
  val VIC20_N_7 = Value(0x430)
  val VIC20_Y = Value(0x431)
  val VIC20_G = Value(0x432)
  val VIC20_V = Value(0x433)
  val VIC20_B = Value(0x434)
  val VIC20_H = Value(0x435)
  val VIC20_U = Value(0x436)
  val VIC20_N_8 = Value(0x437)
  // row 2
  val VIC20_N_5 = Value(0x420)
  val VIC20_R = Value(0x421)
  val VIC20_D = Value(0x422)
  val VIC20_X = Value(0x423)
  val VIC20_C = Value(0x424)
  val VIC20_F = Value(0x425)
  val VIC20_T = Value(0x426)
  val VIC20_N_6 = Value(0x427)
  // row 1
  val VIC20_N_3 = Value(0x410)
  val VIC20_W = Value(0x411)
  val VIC20_A = Value(0x412)
  val VIC20_L_SHIFT = Value(0x413)
  val VIC20_Z = Value(0x414)
  val VIC20_S = Value(0x415)
  val VIC20_E = Value(0x416)
  val VIC20_N_4 = Value(0x417)
  // row 0
  val VIC20_N_1 = Value(0x400)
  val VIC20_L_ARROW = Value(0x401)
  val VIC20_CTRL = Value(0x402)
  val VIC20_RUN_STOP = Value(0x403)
  val VIC20_SPACE = Value(0x404)
  val VIC20_CBM = Value(0x405)
  val VIC20_Q = Value(0x406)
  val VIC20_N_2 = Value(0x407)

  val VIC20_RESTORE = Value(0x4FF)

  def is128Key(k: Key): Boolean = (k.id & 0x100) > 0

  def isCBM2Key(k: Key): Boolean = (k.id & 0x200) > 0

  def isVIC20Key(k: Key): Boolean = (k.id & 0x400) > 0

  def isC64Key(k: Key): Boolean = k.id < 0x100

  def isShift(key: Key): Boolean = key == R_SHIFT || key == L_SHIFT || key == VIC20_L_SHIFT || key == VIC20_R_SHIFT || key == CBM2_SHIFT

  def getKey(key: String, model: CBMComputerModel): CKey.Value = {
    val name = model match {
      case C64Model | C128Model =>
        key
      case VIC20Model =>
        if (!key.startsWith("VIC20_")) s"VIC20_$key" else key
      case CBMIIModel =>
        if (!key.startsWith("CBM2_")) s"CBM2_$key" else key
    }
    CKey.withName(name)
  }

  def getKeyWithoutPrefix(key: CKey.Value, model: CBMComputerModel): String = {
    val k = key.toString
    model match {
      case C64Model | C128Model => k
      case VIC20Model => k.substring("VIC20_".length)
      case CBMIIModel => k.substring("CBM2_".length)
    }
  }

	def isLetterKey(key:CKey.Value, model: CBMComputerModel): Boolean = {
		val name = getKeyWithoutPrefix(key,model)
		name.length == 1 && name.charAt(0).isLetter
	}
  def getRowCol(k: Key): (Int, Int) = {
    val id = k.id
    val col = id & 0xf
    val row = (id >> 4) & 0xf
    (row, col)
  }
}