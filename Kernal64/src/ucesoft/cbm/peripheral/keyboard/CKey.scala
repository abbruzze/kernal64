package ucesoft.cbm.peripheral.keyboard

object CKey extends Enumeration {
	  type Key = Value
		// ============================ C64 =============================
	  // row 0
	  val INST_DEL: CKey.Value = Value(0x00)
	  val RETURN: CKey.Value = Value(0x01)
	  val CRSR_LR: CKey.Value = Value(0x02)
	  val F7: CKey.Value = Value(0x03)
	  val F1: CKey.Value = Value(0x04)
	  val F3: CKey.Value = Value(0x05)
	  val F5: CKey.Value = Value(0x06)
	  val CRSR_UD: CKey.Value = Value(0x07)
	  // row 1
	  val N_3: CKey.Value = Value(0x10)
	  val W: CKey.Value = Value(0x11)
	  val A: CKey.Value = Value(0x12)
	  val N_4: CKey.Value = Value(0x13)
	  val Z: CKey.Value = Value(0x14)
	  val S: CKey.Value = Value(0x15)
	  val E: CKey.Value = Value(0x16)
	  val L_SHIFT: CKey.Value = Value(0x17)
	  // row 2
	  val N_5: CKey.Value = Value(0x20)
	  val R: CKey.Value = Value(0x21)
	  val D: CKey.Value = Value(0x22)
	  val N_6: CKey.Value = Value(0x23)
	  val C: CKey.Value = Value(0x24)
	  val F: CKey.Value = Value(0x25)
	  val T: CKey.Value = Value(0x26)
	  val X: CKey.Value = Value(0x27)
	  // row 3
	  val N_7: CKey.Value = Value(0x30)
	  val Y: CKey.Value = Value(0x31)
	  val G: CKey.Value = Value(0x32)
	  val N_8: CKey.Value = Value(0x33)
	  val B: CKey.Value = Value(0x34)
	  val H: CKey.Value = Value(0x35)
	  val U: CKey.Value = Value(0x36)
	  val V: CKey.Value = Value(0x37)
	  // row 4
	  val N_9: CKey.Value = Value(0x40)
	  val I: CKey.Value = Value(0x41)
	  val J: CKey.Value = Value(0x42)
	  val N_0: CKey.Value = Value(0x43)
	  val M: CKey.Value = Value(0x44)
	  val K: CKey.Value = Value(0x45)
	  val O: CKey.Value = Value(0x46)
	  val N: CKey.Value = Value(0x47)
	  // row 5
	  val PLUS: CKey.Value = Value(0x50)
	  val P: CKey.Value = Value(0x51)
	  val L: CKey.Value = Value(0x52)
	  val MINUS: CKey.Value = Value(0x53)
	  val PERIOD: CKey.Value = Value(0x54)
	  val COLON: CKey.Value = Value(0x55)
	  val AT: CKey.Value = Value(0x56)
	  val COMMA: CKey.Value = Value(0x57)
	  // row 6
	  val LIRA: CKey.Value = Value(0x60)
	  val STAR: CKey.Value = Value(0x61)
	  val SEMICOL: CKey.Value = Value(0x62)
	  val CLR_HOME: CKey.Value = Value(0x63)
	  val R_SHIFT: CKey.Value = Value(0x64)
	  val EQUAL: CKey.Value = Value(0x65)
	  val EXP: CKey.Value = Value(0x66)
	  val SLASH: CKey.Value = Value(0x67)
	  // row 7
	  val N_1: CKey.Value = Value(0x70)
	  val L_ARROW: CKey.Value = Value(0x71)
	  val CTRL: CKey.Value = Value(0x72)
	  val N_2: CKey.Value = Value(0x73)
	  val SPACE: CKey.Value = Value(0x74)
	  val CBM: CKey.Value = Value(0x75)
	  val Q: CKey.Value = Value(0x76)
	  val RUN_STOP: CKey.Value = Value(0x77)
		// ============================ C128 ============================
	  // C128 rows, add an offset 0x100
	  // row 0
	  val HELP: CKey.Value = Value(0x100)
	  val KEYPAD_N_8: CKey.Value = Value(0x101)
	  val KEYPAD_N_5: CKey.Value = Value(0x102)
	  val TAB: CKey.Value = Value(0x103)
	  val KEYPAD_N_2: CKey.Value = Value(0x104)
	  val KEYPAD_N_4: CKey.Value = Value(0x105)
	  val KEYPAD_N_7: CKey.Value = Value(0x106)
	  val KEYPAD_N_1: CKey.Value = Value(0x107)
	  // row 1
	  val ESC: CKey.Value = Value(0x110)
	  val KEYPAD_PLUS: CKey.Value = Value(0x111)
	  val KEYPAD_MINUS: CKey.Value = Value(0x112)
	  val LINE_FEED: CKey.Value = Value(0x113)
	  val KEYPAD_ENTER: CKey.Value = Value(0x114)
	  val KEYPAD_N_6: CKey.Value = Value(0x115)
	  val KEYPAD_N_9: CKey.Value = Value(0x116)
	  val KEYPAD_N_3: CKey.Value = Value(0x117)
	  // row 2
	  val ALT: CKey.Value = Value(0x120)
	  val KEYPAD_N_0: CKey.Value = Value(0x121)
	  val KEYPAD_DOT: CKey.Value = Value(0x122)
	  val KEYPAD_UP: CKey.Value = Value(0x123)
	  val KEYPAD_DOWN: CKey.Value = Value(0x124)
	  val KEYPAD_LEFT: CKey.Value = Value(0x125)
	  val KEYPAD_RIGHT: CKey.Value = Value(0x126)
	  val NO_SCROLL: CKey.Value = Value(0x127)
	  // special
	  val _40_80: CKey.Value = Value(0x128)
	  val CAPS_LOCK: CKey.Value = Value(0x129)
	  
	  val RESTORE: CKey.Value = Value(0xFF)

		// ============================ CBM2 ============================
		// CBM2 rows, add an offset 0x200
		// A
		val CBM2_F1: CKey.Value = Value(0x200)
		val CBM2_F2: CKey.Value = Value(0x201)
		val CBM2_F3: CKey.Value = Value(0x202)
		val CBM2_F4: CKey.Value = Value(0x203)
		val CBM2_F5: CKey.Value = Value(0x204)
		val CBM2_F6: CKey.Value = Value(0x205)
		val CBM2_F7: CKey.Value = Value(0x206)
		val CBM2_F8: CKey.Value = Value(0x207)
		val CBM2_F9: CKey.Value = Value(0x208)
		val CBM2_F10: CKey.Value = Value(0x209)
		val CBM2_DOWN: CKey.Value = Value(0x20A)
		val CBM2_UP: CKey.Value = Value(0x20B)
		val CBM2_HOME: CKey.Value = Value(0x20C)
		val CBM2_RVS: CKey.Value = Value(0x20D)
		val CBM2_GRAPH: CKey.Value = Value(0x20E)
		val CBM2_STOP: CKey.Value = Value(0x20F)
		// B
		val CBM2_ESC: CKey.Value = Value(0x210)
		val CBM2_1: CKey.Value = Value(0x211)
		val CBM2_2: CKey.Value = Value(0x212)
		val CBM2_3: CKey.Value = Value(0x213)
		val CBM2_4: CKey.Value = Value(0x214)
		val CBM2_5: CKey.Value = Value(0x215)
		val CBM2_7: CKey.Value = Value(0x216)
		val CBM2_8: CKey.Value = Value(0x217)
		val CBM2_9: CKey.Value = Value(0x218)
		val CBM2_0: CKey.Value = Value(0x219)
		val CBM2_EQUAL: CKey.Value = Value(0x21A)
		val CBM2_LEFT: CKey.Value = Value(0x21B)
		val CBM2_KP_? : CKey.Value = Value(0x21C)
		val CBM2_KP_CE: CKey.Value = Value(0x21D)
		val CBM2_KP_* : CKey.Value = Value(0x21E)
		val CBM2_KP_/ : CKey.Value = Value(0x21F)
		// C
		val CBM2_TAB: CKey.Value = Value(0x220)
		val CBM2_Q: CKey.Value = Value(0x221)
		val CBM2_W: CKey.Value = Value(0x222)
		val CBM2_E: CKey.Value = Value(0x223)
		val CBM2_R: CKey.Value = Value(0x224)
		val CBM2_6: CKey.Value = Value(0x225)
		val CBM2_U: CKey.Value = Value(0x226)
		val CBM2_I: CKey.Value = Value(0x227)
		val CBM2_O: CKey.Value = Value(0x228)
		val CBM2_- : CKey.Value = Value(0x229)
		val CBM2_BARROW: CKey.Value = Value(0x22A)
		val CBM2_RIGHT: CKey.Value = Value(0x22B)
		val CBM2_KP_7: CKey.Value = Value(0x22C)
		val CBM2_KP_8: CKey.Value = Value(0x22D)
		val CBM2_KP_9: CKey.Value = Value(0x22E)
		val CBM2_KP_- : CKey.Value = Value(0x22F)
		// D
		val CBM2_A: CKey.Value = Value(0x231)
		val CBM2_S: CKey.Value = Value(0x232)
		val CBM2_D: CKey.Value = Value(0x233)
		val CBM2_T: CKey.Value = Value(0x234)
		val CBM2_Y: CKey.Value = Value(0x235)
		val CBM2_J: CKey.Value = Value(0x236)
		val CBM2_K: CKey.Value = Value(0x237)
		val CBM2_L: CKey.Value = Value(0x238)
		val CBM2_P: CKey.Value = Value(0x239)
		val CBM2_CL_SQ_BR: CKey.Value = Value(0x23A)
		val CBM2_DEL: CKey.Value = Value(0x23B)
		val CBM2_KP_4: CKey.Value = Value(0x23C)
		val CBM2_KP_5: CKey.Value = Value(0x23D)
		val CBM2_KP_6: CKey.Value = Value(0x23E)
		val CBM2_KP_+ : CKey.Value = Value(0x23F)
		// E
		val CBM2_SHIFT: CKey.Value = Value(0x240)
		val CBM2_Z: CKey.Value = Value(0x241)
		val CBM2_X: CKey.Value = Value(0x242)
		val CBM2_F: CKey.Value = Value(0x243)
		val CBM2_G: CKey.Value = Value(0x244)
		val CBM2_H: CKey.Value = Value(0x245)
		val CBM2_M: CKey.Value = Value(0x246)
		val CBM2_COMMA: CKey.Value = Value(0x247)
		val CBM2_SEMICOL: CKey.Value = Value(0x248)
		val CBM2_OP_SQ_BR: CKey.Value = Value(0x249)
		val CBM2_RETURN: CKey.Value = Value(0x24A)
		val CBM2_COMMODORE: CKey.Value = Value(0x24B)
		val CBM2_KP_1: CKey.Value = Value(0x24C)
		val CBM2_KP_2: CKey.Value = Value(0x24D)
		val CBM2_KP_3: CKey.Value = Value(0x24E)
		val CBM2_KP_ENTER: CKey.Value = Value(0x24F)
		// F
		val CBM2_CONTROL: CKey.Value = Value(0x250)
		val CBM2_C: CKey.Value = Value(0x252)
		val CBM2_V: CKey.Value = Value(0x253)
		val CBM2_B: CKey.Value = Value(0x254)
		val CBM2_N: CKey.Value = Value(0x255)
		val CBM2_SPACE: CKey.Value = Value(0x256)
		val CBM2_DOT: CKey.Value = Value(0x257)
		val CBM2_/ : CKey.Value = Value(0x258)
		val CBM2_DB_QUOTE: CKey.Value = Value(0x259)
		val CBM2_PI: CKey.Value = Value(0x25A)
		val CBM2_KP_0: CKey.Value = Value(0x25C)
		val CBM2_KP_DOT: CKey.Value = Value(0x25D)
		val CBM2_KP_00: CKey.Value = Value(0x25E)

	  def is128Key(k:Key): Boolean = (k.id & 0x100) > 0
		def isCBM2Key(k:Key): Boolean = (k.id & 0x200) > 0
	  
	  def getRowCol(k:Key): (Int,Int) = {
	    val id = k.id
	    val col = id & 0xf
	    val row = (id >> 4) & 0xf
	    (row,col)
	  }
	}