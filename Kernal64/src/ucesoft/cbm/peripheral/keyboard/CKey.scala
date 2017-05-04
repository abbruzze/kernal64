package ucesoft.cbm.peripheral.keyboard

object CKey extends Enumeration {
	  type Key = Value
	  // row 0
	  val INST_DEL 	= Value(0x00)
	  val RETURN	= Value(0x01)
	  val CRSR_LR	= Value(0x02)
	  val F7		= Value(0x03)
	  val F1		= Value(0x04)
	  val F3		= Value(0x05)
	  val F5		= Value(0x06)
	  val CRSR_UD	= Value(0x07)
	  // row 1
	  val N_3	 	= Value(0x10)
	  val W			= Value(0x11)
	  val A			= Value(0x12)
	  val N_4		= Value(0x13)
	  val Z			= Value(0x14)
	  val S			= Value(0x15)
	  val E			= Value(0x16)
	  val L_SHIFT	= Value(0x17)
	  // row 2
	  val N_5	 	= Value(0x20)
	  val R			= Value(0x21)
	  val D			= Value(0x22)
	  val N_6		= Value(0x23)
	  val C			= Value(0x24)
	  val F			= Value(0x25)
	  val T			= Value(0x26)
	  val X			= Value(0x27)
	  // row 3
	  val N_7	 	= Value(0x30)
	  val Y			= Value(0x31)
	  val G			= Value(0x32)
	  val N_8		= Value(0x33)
	  val B			= Value(0x34)
	  val H			= Value(0x35)
	  val U			= Value(0x36)
	  val V			= Value(0x37)
	  // row 4
	  val N_9	 	= Value(0x40)
	  val I			= Value(0x41)
	  val J			= Value(0x42)
	  val N_0		= Value(0x43)
	  val M			= Value(0x44)
	  val K			= Value(0x45)
	  val O			= Value(0x46)
	  val N			= Value(0x47)
	  // row 5
	  val PLUS	 	= Value(0x50)
	  val P			= Value(0x51)
	  val L			= Value(0x52)
	  val MINUS		= Value(0x53)
	  val PERIOD	= Value(0x54)
	  val COLON		= Value(0x55)
	  val AT		= Value(0x56)
	  val COMMA		= Value(0x57)
	  // row 6
	  val LIRA	 	= Value(0x60)
	  val STAR		= Value(0x61)
	  val SEMICOL	= Value(0x62)
	  val CLR_HOME	= Value(0x63)
	  val R_SHIFT	= Value(0x64)
	  val EQUAL		= Value(0x65)
	  val EXP		= Value(0x66)
	  val SLASH		= Value(0x67)
	  // row 7
	  val N_1	 	= Value(0x70)
	  val L_ARROW	= Value(0x71)
	  val CTRL		= Value(0x72)
	  val N_2		= Value(0x73)
	  val SPACE		= Value(0x74)
	  val CBM		= Value(0x75)
	  val Q			= Value(0x76)
	  val RUN_STOP	= Value(0x77)
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
	  
	  val RESTORE 	= Value(0xFF)
	  
	  def is128Key(k:Key) = k.id > 0xFF
	  
	  def getRowCol(k:Key) = {
	    val id = k.id
	    val col = id & 0xf
	    val row = (id >> 4) & 0xf
	    (row,col)
	  }
	}