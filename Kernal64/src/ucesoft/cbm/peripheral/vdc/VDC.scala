package ucesoft.cbm.peripheral.vdc

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.Log
import ucesoft.cbm.Clock
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.ClockEvent

object VDC {
  final val SCREEN_HEIGHT = 312 // PAL rows
  final val SCREEN_WIDTH = (SCREEN_HEIGHT * 2 * 4 / 3.0).toInt
  final val PREFERRED_FRAME_SIZE = new java.awt.Dimension(SCREEN_WIDTH,SCREEN_HEIGHT * 2)

  private object VideoMode extends Enumeration {
    val IDLE = Value
    val TEXT = Value
    val BITMAP = Value
    val BLANK = Value
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
  18/$12 Current memory address (high byte)
  19/$13 Current memory address (low byte)
  20/$14 Attribute memory starting address (high byte)
  21/$15 Attribute memory starting address {low byte)
  22/$16 Character horizontal size control register
  23/$17 Character vertical size control register
  24/$18 Vertical smooth scrolling and control register
  25/$19 Horizontal smooth scrolling and control register
  26/$lA Fore ground/background color register
  27/$lB Address increment per row
  28/$lC Character set address and memory type register
  29/$lD Underline scan-line-position register
  30/$lE Number of bytes for block write or copy
  31/$1F Memory read/write register
  32/$20 Block copy source address (high byte)
  33/$21 Block copy source address (low byte)
  34/$22 Beginning position for horizontal blanking
  35/$23 Ending position for horizontal blanking
  36/$24 Number of memory refresh cycles per scan line
 */
class VDC extends RAMComponent {
  import VDC._

  val name = "VDC"
  val componentID = "C128_VDC"
  val isRom = false
  val startAddress = 0xD600
  val length = 0x2
  val componentType = CBMComponentType.MEMORY
  val isActive = true

  final private[this] val X_LEFT_CLIP_COLS = 20 * 8
  final private[this] val X_RIGHT_CLIP_COLS = 8 * 8
  final private[this] val Y_TOP_CLIP_ROWS = 1 * 8
  final private[this] val Y_BOTTOM_CLIP_ROWS = 3 * 8
  final private[this] val CPU_CLOCK_HZ = 985248.0
  final private[this] val VDC_CLOCK_HZ = 16000000.0

  final private[this] val MAX_HEIGHT = 1024
  final private[this] val MIN_HEIGHT = 200

  final private[this] val VDC_REVISION_0 = 0 /* 8563 R7A */
  final private[this] val VDC_REVISION_1 = 1 /* 8563 R8/R9 */
  final private[this] val VDC_REVISION_2 = 2 /* 8568 */

  private[this] var clk = Clock.systemClock
  private[this] val ram = Array.ofDim[Int](0x10000)
  private[this] var address_reg = 0
  private[this] val regs = Array.ofDim[Int](37)
  private[this] var vblank = false
  private[this] val debug = false
  private[this] val vdc_revision = VDC_REVISION_2
  private[this] val reg_mask = Array(0x00, 0x00, 0x00, 0x00, 0x00, 0xE0, 0x00, 0x00,
    0xFC, 0xE0, 0x80, 0xE0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE0,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xE0, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xF0)
  private[this] var display : Display = _
  private[this] var bitmap : Array[Int] = _
  private[this] var videoMode = VideoMode.IDLE
  private[this] var screenHeight,currentScreenHeight = SCREEN_HEIGHT
  private[this] var screenWidth = SCREEN_WIDTH
  private[this] var oneLineDrawn = false
  // =====================================================
  private[this] var busyFlagClearCycle = 0L         // used to keep track of copy&fill operation cycles
  private[this] var cycles_per_line,cycles_per_line_accu = 0
  private[this] var xchars_total = 0                // REG 0 Horizontal Total
  private[this] var ychars_total = 0                // REG 9 Character Total Vertical
  private[this] var ychars_visible = 0              // REG 23 Character vertical size control
  private[this] var bytes_per_char = 0              // REG 9 Character Total Vertical
  private[this] var cursor_pos = 0                  // REG 14-5 Cursor location HI/LO
  private[this] var charVisibleWidth = 0            // REG 22
  private[this] var cursorOn = true
  private[this] var charBlinkOn = true
  private[this] var xsmooth = 0                     // REG 25
  private[this] var chargen_adr = 0                 // REG 26 chargen_adr
  private[this] var attr_adr = 0                    // REG 20/21 attribute adr
  private[this] var attr_offset,ram_base_offset = 0 // used for 8x1 trick
  private[this] var ram_adr = 0                     // REG 12/13 ram adr
  private[this] var ypos = 0
  private[this] var ram_base_ptr,attr_base_ptr = 0
  // light pen
  private[this] var lpFlag = 0                      // 0 = no new values of lp coords, 0x40 new values
  private[this] var clkStartLine = 0L               // clock cycles when the last line begun
  // -----------------------------------------------------
  private[this] var rasterLine,screenLine = 0
  private[this] var currentCharScanLine = 0
  private[this] var visibleScreenHeightPix = 0      // total Y pixels
  private[this] var visibleTextRows = 0             // total rows
  private[this] var vsyncPos,borderWidth = 0    // top char position
  private[this] var interlaceMode = false
  private[this] var geometryUpdateListener : String => Unit = _
  private[this] var useCacheForNextFrame,writeOnPrevFrame = false
  private[this] var updateGeometryOnNextFrame = false
  private[this] var blankMode = false // TO BE IMPLEMENTED PROPERLY
  // -----------------------------------------------------
  private[this] var deinterlaceMode = true
  private[this] var frameBit = 0
  // COLOR PALETTE =======================================
  private[this] val PALETTE = Array(
    0xFF000000,  // 00 Black
    0xFF555555,  // 01 Medium Gray
    0xFF000078,  // 02 Blue (78)
    0xFF5555FF,  // 03 Light blue
    0xFF007800,  // 04 Green (78)
    0xFF55FF55,  // 05 Light green
    0xFF007878,  // 06 Dark cyan (78)
    0xFF55FFFF,  // 07 Light cyan
    0xFF780000,  // 08 Dark red (78)
    0xFFFF5555,  // 09 Light red
    0xFF780078,  // 10 Dark purple (78)
    0xFFFF55FF,  // 11 Light purple
    0xFF785500,  // 12 Brown (78)
    0xFFFFFF55,  // 13 Yellow
    0xFF787878,  // 14 Light Gray (78)
    0xFFFFFFFF   // 15 White
  )
  // Clock management ====================================
  def pause {
    //if (clk != Clock.systemClock) clk.pause
    clk.cancel("VDC_TICK")
  }
  def play {
    //if (clk != Clock.systemClock) clk.play
    pause
    reschedule
  }
  @inline private def reschedule {
    cycles_per_line_accu += cycles_per_line
    val next_clk = cycles_per_line_accu >> 16
    cycles_per_line_accu -= next_clk << 16
    clk.schedule(new ClockEvent("VDC_TICK",clk.currentCycles + next_clk,drawLine _))
  }
  def setOwnThread {
    pause
    clk = Clock.makeClock("VDCClock",Some(errorHandler _))((cycles) => {})
    clk.play
    play
  }
  def stopOwnThread {
    if (clk != Clock.systemClock) {
      clk.halt
      clk = Clock.systemClock
      play
    }
  }

  // =====================================================
  def setDeinterlaceMode(on:Boolean) : Unit = {
    deinterlaceMode = on
    writeOnPrevFrame = true
  }

  def setGeometryUpdateListener(geometryUpdateListener:(String) => Unit) {
    this.geometryUpdateListener = geometryUpdateListener
  }

  def setDisplay(display:Display) {
    this.display = display
    bitmap = display.displayMem
  }

  final def init {
    reset
    var v = 0xFF
    for(i <- 0 until ram.length) {
      ram(i) = v
      v ^= 0xFF
    }
  }

  final def reset {
    busyFlagClearCycle = 0
    charVisibleWidth = 8
    regs(22) = 0x78
    regs(25) = 0x47
    xsmooth = 7
    regs(0) = 126
    regs(1) = 102
    xchars_total = regs(0) + 1
    cycles_per_line = 0
    recalculate_xsync
    regs(4) = 39
    regs(5) = 0
    regs(6) = 25
    ychars_total = 7
    regs(9) = 7
    bytes_per_char = 16
    interlaceMode = false
    display.setInterlaceMode(false)
    bitmap = display.displayMem
    currentScreenHeight = SCREEN_HEIGHT
    vsyncPos = 0
    setScanLines(SCREEN_HEIGHT)
    play
  }

  @inline private def ram_adr(address:Int) : Int = {
    if ((regs(28) & 0x10) == 0x10) address & 0xFFFF
    else (address & 0x3F00) << 1 | (address & 0x81FF) // see vdc dump https://csdb.dk/release/?id=157510, patterns.txt
  }

  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    address & 1 match {
      case 0 => read_status
      case 1 => read_regs
    }
  }

  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    address & 1 match {
      case 0 =>
        address_reg = value & 0x3F
      case 1 =>
        writeOnPrevFrame = true
        write_regs(value)
    }
  }

  @inline private[this] def read_status : Int = {
    val busyFlag = if (clk.currentCycles > busyFlagClearCycle) 0x80 else 0
    lpFlag | busyFlag | (if (vblank) 0x20 else 0) | vdc_revision
  }
  @inline private[this] def currentMemoryAddress = regs(18) << 8 | regs(19)
  @inline private[this] def incCurrentMemoryAddress {
    regs(19) += 1
    if (regs(19) == 0x100) {
      regs(19) = 0
      regs(18) += 1
      if (regs(18) == 0x100) regs(18) = 0
    }
  }

  @inline private[this] def read_regs : Int = {
    if (address_reg > 36) return 0xFF

    address_reg match {
      case 16|17 =>
        lpFlag = 0
        regs(address_reg)
      case 28 => // to set the memory size 16K or 64K
        regs(28)
      case 31 =>
        if (busyFlagClearCycle < clk.currentCycles) {
          val adr = currentMemoryAddress
          incCurrentMemoryAddress
          busyFlagClearCycle = clk.currentCycles + 37
          ram(ram_adr(adr))
        }
        else {
          //println(s"Reading too early $busyFlagClearCycle ${clk.currentCycles}")
          0
        }
      case _ =>
        regs(address_reg) | reg_mask(address_reg)
    }
  }

  @inline private[this] def write_regs(value:Int) : Unit = {
    // REG 16 & 17 (light pen) are read only
    if (address_reg == 16 || address_reg == 17 || address_reg > 36 || (regs(address_reg) == value && (address_reg != 31 && address_reg != 30))) return
    val oldValue = regs(address_reg)
    regs(address_reg) = value
    address_reg match {
      case 0 => // REG 0 Horizontal Total
        xchars_total = value + 1
        if (debug) println(s"VDC: REG 0 Horizontal Total: $xchars_total")
        recalculate_xsync
        updateGeometryOnNextFrame = true
      case 1 => // REG 1 Horizontal Displayed
        if (debug) println(s"VDC: REG 1 Horizontal Displayed: $value")
        updateGeometryOnNextFrame = true
      case 2 => // REG 2 Horizontal Sync Pos
        if (debug) println(s"VDC: REG 2 Horizontal Sync Pos: $value")
        updateGeometryOnNextFrame = true
      case 3 => // REG 3 Horizontal/Vertical Sync widths
        if (debug) println(s"VDC: REG 3 Horizontal/Vertical Sync widths: $value")
        updateGeometryOnNextFrame = true
      case 4 => // REG 4 Vertical Total
        if (debug) println(s"VDC: REG 4 Vertical Total :$value")
        //updateGeometryOnNextFrame = true
        updateGeometry
      case 5 => // REG 5 Vertical Total Fine Adjust
        if (debug) println(s"VDC: REG 5 Vertical Total Fine Adjust :$value")
        updateGeometryOnNextFrame = true
      case 6 => // REG 6 Vertical Displayed
        if (debug) println(s"VDC: REG 6 Vertical Displayed: $value")
        //updateGeometryOnNextFrame = true
        updateGeometry
      //updateVertical
      case 7 => // REG 7 Vertical Sync Position
        if (debug) println(s"VDC: REG 7 Vertical Sync Position: $value")
      //updateVertical
      case 8 => // REG 8 Interlace
        if (debug) println(s"VDC: REG 8 Interlace: $value")
        val newInterlaceMode = (value & 3) == 3
        if (newInterlaceMode != interlaceMode) setInterlaceMode(newInterlaceMode)
      case 9 => // REG 9 Character Total Vertical
        ychars_total = value & 0x1F
        bytes_per_char = if (ychars_total < 16) 16 else 32
        if (ychars_total == 0) {
          if (ypos == 0) {
            ram_base_offset = 34 * regs(1)
            attr_offset = -regs(1)
          }
          else attr_offset = 3

          updateGeometry
        }
        else {
          ram_base_offset = 0
          attr_offset = 0
          updateGeometryOnNextFrame = true
        }
        if (debug) println(s"VDC: REG 9 Character Total Vertical: $value ychars_total=$ychars_total ychars_visible=$ychars_visible bytes_per_char=$bytes_per_char attr_offset=$attr_offset rasterLine=$rasterLine vblank=$vblank")
      //updateGeometryOnNextFrame = true
      //updateVertical
      case 10 => // R10  Cursor Mode, Start Scan
        if (debug) println(s"VDC: R10  Cursor Mode, Start Scan: ${value & 0x1F} mode: ${(value & 0xE0) >> 5}")
      case 11 => // R11 Cursor End Scan
        if (debug) println(s"VDC R11 Cursor End Scan: $value")
      case 12|13 => // R12  Display Start Address hi, R13  Display Start Address lo
        if (debug) println(s"VDC: new Screen Address($address_reg): ${Integer.toHexString(regs(12) << 8 | regs(13))} rasterLine=$rasterLine vblank=$vblank")
      case 14|15 =>  // REG 14-5 Cursor location HI/LO
        cursor_pos = regs(14) << 8 | regs(15)
      //if (debug) println(s"VDC: new cursor pos($address_reg): ${Integer.toHexString(cursor_pos)}")
      case 18|19 =>
        if (busyFlagClearCycle < clk.currentCycles) busyFlagClearCycle = clk.currentCycles + 37
      case 20|21 => // REG 20-1 Attribute Start Address hi/lo
        //attr_adr = regs(20) << 8 | regs(21)
        if (debug) println(s"VDC: new Attribute Address($address_reg): ${Integer.toHexString(regs(20) << 8 | regs(21))}")
      case 22 => // REG 22 Character Horizontal Size Control
        if (value != oldValue) {
          charVisibleWidth = value & 0x0F
          if (debug) println(s"VCD: REG 22 char visible width: $charVisibleWidth total char width: " + (1 + (regs(22) >> 4)))
          updateGeometryOnNextFrame = true
          recalculate_xsync
        }
      case 23 => // REG 23 Vert. Character Pxl Spc
        ychars_visible = value & 0x1F
        if (debug) println(s"VDC: REG 23 Vert. Character Pxl Spc: $ychars_visible")
      case 24 => // REG 24 Vertical Smooth Scroll + Blink frequency + Screen reverse mode
        if (debug) println(s"VDC: REG 24 Vertical Smooth Scroll: ${value & 0x1F} Blink frequency: ${if ((value & 0x20) > 0) "1/32" else "1/16"} Screen reverse mode: ${if ((value & 0x40) > 0) "reverse" else "normal"}")
      case 25 => // REG 25 Video mode,Color source,semi-graphics mode, double pixel mode
        if ((value & 0x0F) != (oldValue & 0x0F)) { // Horizontal smooth scroll
          if (vdc_revision == VDC_REVISION_0) xsmooth = ((regs(22) >> 4) - (value & 0x0F)) & 0x0F
          else xsmooth = value & 0x0F
        }
        if ((value & 0x80) != (oldValue & 0x80)) updateGeometry // bitmap
        if ((value & 0x10) != (oldValue & 0x10)) recalculate_xsync
        if (debug) println(s"VDC: REG 25 xsmooth=$xsmooth Video Mode = ${if ((value & 0x80) > 0) "bitmap" else "text"} Color source ${if ((value & 0x40) > 0) "attribute space" else "REG 26"} Semigraph-mode ${if ((value & 0x20) > 0) "on" else "off"} Double pixel mode ${if ((value & 0x10) > 0) "on" else "off"} rasterLine=$rasterLine vblank=$vblank")
      case 26 => // REG 26 background, foregorund colors
      //if (debug) println(s"VCD: REG 26 background color ${value & 0xF} foreground color ${(value >> 4) & 0xF}")
      case 27 => // REG 27 Row/Adrs. Increment
        if (debug) println(s"VDC REG 27 Row/Adrs. Increment $value")
      case 28 => // R28 chargen address
        chargen_adr = (value << 8) & 0xE000
        if (debug) println(s"VDC: REG 28 chargen_adr ${Integer.toHexString(chargen_adr)} 64K=${(value & 0x10) == 0x10}")
      case 29 => // REG 29 Underline scal-line control
        if (debug) println(s"VDC: REG 29 Underline scal-line control $value")
      case 30 =>
        copyorfill
      case 31 =>
        if (busyFlagClearCycle < clk.currentCycles) {
          val addr = currentMemoryAddress
          incCurrentMemoryAddress
          ram(ram_adr(addr)) = value
          busyFlagClearCycle = clk.currentCycles + 15
        }
      //if (debug) println(s"VDC write ${Integer.toHexString(addr)} = ${Integer.toHexString(value)}")
      case 34|35 => // REG 34-5 Display Enable begin/end
        if (debug) println(s"VDC: REG$address_reg Display Enable begin ${regs(34)} end ${regs(35)}")
      case _ =>
    }
  }

  private def setInterlaceMode(newInterlaceMode:Boolean) {
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
  @inline private def recalculate_xsync {
    val charWidth = if((regs(25) & 0x10) > 0) (regs(22) >> 4) << 1 /* double pixel a.k.a 40column mode */
    else 1 + (regs(22) >> 4)

    val exact_cycles_per_line = xchars_total * charWidth * CPU_CLOCK_HZ / VDC_CLOCK_HZ - 0.2
    val new_cycles_per_line = (exact_cycles_per_line * 65536).asInstanceOf[Int]
    if (new_cycles_per_line != cycles_per_line) {
      cycles_per_line = new_cycles_per_line
      cycles_per_line_accu = 0
      if (debug) println(s"VDC: cycles per line set to ${cycles_per_line >> 16}, xchars_total=$xchars_total charWidth=$charWidth")
    }
  }

  @inline private[this] def copyorfill {
    val length = if (regs(30) == 0) 0x100 else regs(30)
    // ------------------------------
    // To be confirmed: copyfill cycles = length / 20MHz * 8 bit
    busyFlagClearCycle = clk.currentCycles + (length * 0.45).toInt
    // ------------------------------
    var address = currentMemoryAddress
    if ((regs(24) & 0x80) > 0) { // copy
      var source_copy_address = regs(32) << 8 | regs(33)
      Log.debug(s"VDC copying from ${Integer.toHexString(source_copy_address)} to ${Integer.toHexString(address)} length=${Integer.toHexString(length)}")
      //if (debug) println(s"VDC copying from ${Integer.toHexString(source_copy_address)} to ${Integer.toHexString(address)} length=${Integer.toHexString(length)}")
      var i = 0
      while (i < length) {
        ram(ram_adr(address)) = ram(ram_adr(source_copy_address))
        //if (debug) println(s"Copying from ${Integer.toHexString(source_copy_address)} to ${Integer.toHexString(address)} = ${Integer.toHexString(ram(source_copy_address & RAM_ADDR_MASK))}")
        i += 1
        address += 1
        source_copy_address += 1
      }
      regs(31) = ram(ram_adr(source_copy_address - 1)) // from vdc-mem.c
      regs(32) = (source_copy_address >> 8) & 0xFF
      regs(33) = source_copy_address & 0xFF
    }
    else { // fill
      val value = regs(31)
      Log.debug(s"VDC filling from ${Integer.toHexString(address)} length=${Integer.toHexString(length)} with ${Integer.toHexString(value)}")
      //if (debug) println(s"VDC filling from ${Integer.toHexString(address)} length=${Integer.toHexString(length)} with ${Integer.toHexString(value)}")
      var i = 0
      while (i < length) {
        ram(ram_adr(address)) = value
        //if (debug) println(s"Filling ${Integer.toHexString(address)} = ${Integer.toHexString(value)}")
        i += 1
        address += 1
      }
    }

    regs(18) = (address >> 8) & 0xFF
    regs(19) = address & 0xFF
  }

  // =======================================================================

  private def errorHandler(t:Throwable) {
    t.printStackTrace
  }

  @inline private def vsync: Unit = {
    vblank = false
    screenLine = 0
    ypos = 0
    currentCharScanLine = regs(24) & 0x1F // vertical smooth scrolling
    if (currentCharScanLine > ychars_total) currentCharScanLine = 0 // ?? maybe
  }

  @inline private def latch_addresses: Unit = {
    // update video & attributes address for the next frame
    ram_adr = regs(12) << 8 | regs(13)
    attr_adr = regs(20) << 8 | regs(21)
    ram_base_ptr = ram_adr
    attr_base_ptr = attr_adr

    // vsync update
    // from vdc101 tests seems this is the correct place to check vsync updates
    var _vsyncPos = (regs(4) - regs(7)) * (ychars_total + 1) -          // vsync pulse
         //(((regs(9) & 0x1f) - (regs(24) & 0x1f)) & 0x1f) -            // vscroll adjustment
         ((regs(3) >> 4) & 0x0F)                                        // vsync pulse width
    if (_vsyncPos > screenHeight) _vsyncPos = 0 // what to do when vsync > screenHeight ?
    else
    if (_vsyncPos < 0) _vsyncPos = screenHeight - regs(4) * (ychars_total + 1) // maybe ? seems correct for vdc101

    if (_vsyncPos != vsyncPos) {
      vsyncPos = _vsyncPos
      //println(s"New vsync: regs(4)=${regs(4)} regs(7)=${regs(7)} ychars_total=${ychars_total} vsync=$vsyncPos")
    }
  }

  final def drawLine(cycles:Long) {
    clkStartLine = cycles
    reschedule

    if (rasterLine == vsyncPos) vsync
    else
    if (screenLine == visibleScreenHeightPix) vblank = true
    else
    if (screenLine == screenHeight || screenLine == visibleScreenHeightPix + 2) latch_addresses

    val interlacing = !deinterlaceMode && interlaceMode
    val skipLineForInterlace = interlacing && ((rasterLine & 1) != frameBit)

    if (!useCacheForNextFrame || interlacing)
      try {
        oneLineDrawn = true
        val isIdle = currentCharScanLine > ychars_visible ||
                     ((regs(25) & 0x10) == 0 && xsmooth > ((regs(22) >> 4) & 0x0F)) ||  // xscroll > char_width in single pixel mode
                     ((regs(25) & 0x10) > 0 && xsmooth >= ((regs(22) >> 4) & 0x0F))     // xscroll >= char_width in double pixel mode
        val allBlank = ((regs(25) & 0x10) == 0 && regs(34) > regs(0)) || regs(34) == regs(35)
        if (allBlank) {
          videoMode = VideoMode.BLANK
          drawTextLine(false)
        }
        else
        if (vblank) drawTextLine(true)
        else
        if (skipLineForInterlace || isIdle) {
          val vm = videoMode
          videoMode = VideoMode.IDLE
          drawTextLine(false)
          videoMode = vm
        }
        else
        if ((regs(25) & 0x80) == 0) {
          videoMode = VideoMode.TEXT
          drawTextLine(false)
        }
        else {
          videoMode = VideoMode.BITMAP
          drawBitmapLine
        }
      }
      catch {
        case _:ArrayIndexOutOfBoundsException =>
      }
    // NEXT RASTER LINE =====================================================
    rasterLine += 1
    screenLine += 1

    if (rasterLine > screenHeight) {
      rasterLine = 0
      nextFrame // we are on last line
      //latch_addresses
    }

    val virtualScreenWidth = (regs(1) + regs(27))
    if (!vblank && videoMode != VideoMode.IDLE) {
      if (videoMode == VideoMode.BITMAP) {
        if (interlaceMode) {
          if ((rasterLine & 1) == 1) ram_base_ptr += virtualScreenWidth
        }
        else ram_base_ptr += virtualScreenWidth
      }
      currentCharScanLine += 1
    }

    if (currentCharScanLine > ychars_total) {
      currentCharScanLine = 0
      ypos = (ypos + 1) % regs(6)
      //ypos += 1
      if (videoMode != VideoMode.IDLE) attr_base_ptr += virtualScreenWidth
      if (videoMode == VideoMode.TEXT) ram_base_ptr += virtualScreenWidth
    }

  }

  @inline private def updateGeometry {
    visibleTextRows = regs(6)
    visibleScreenHeightPix = visibleTextRows * (ychars_total + 1)
    val charWidth = if((regs(25) & 0x10) > 0) (regs(22) >> 4) << 1 /* double pixel a.k.a 40column mode */
    else 1 + (regs(22) >> 4)

    val htotal = xchars_total
    val hdisplayed = regs(1)
    val hsync = regs(2)

    val BORDER = 4
    val hsync_width = (regs(3) & 0x0F) - 1
    //var rborder = hsync - hdisplayed - BORDER
    var lborder = htotal - hsync + BORDER //htotal - (hsync + hsync_width)
    var rborder = htotal - hdisplayed - lborder

    if (rborder < 0) rborder = 0
    if (lborder < 0) lborder = 0

    val newScreenWidth = htotal * charWidth // (hdisplayed + lborder + rborder) * charWidth
    if (newScreenWidth != screenWidth) {
      screenWidth = newScreenWidth
      //display.setClipArea(X_LEFT_CLIP_COLS,0,screenWidth - X_RIGHT_CLIP_COLS,screenHeight)
      display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS,screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS)
      display.setNewResolution(currentScreenHeight,screenWidth)
      bitmap = display.displayMem
    }

    borderWidth = lborder * charWidth
    //borderWidth = (xchars_total - regs(2) - ((regs(3) & 0x0F))) * charWidth
    if (debug) println(s"New screen res. width=$screenWidth htotal=$htotal hdisplayed=$hdisplayed hsync=$hsync hsync_width=$hsync_width rborder=$rborder lborder=$lborder rester=$rasterLine")
    if (borderWidth < 0) borderWidth = 0
    val textMode = (regs(25) & 0x80) == 0
    if (geometryUpdateListener != null) {
      val doublePixFeature = (regs(25) & 0x10) > 0
      val semigraphicMode = (regs(25) & 0x20) > 0
      val charInfo = s"${charWidth}x${if (interlaceMode) (ychars_total + 1) >> 1 else (ychars_total + 1)} ${if (doublePixFeature) "D" else ""}${if (semigraphicMode) "S" else ""}"
      if (textMode) geometryUpdateListener(s"Text mode ${regs(1)}x${visibleTextRows} ${charInfo} ${if (interlaceMode) "interlaced" else ""}")
      else geometryUpdateListener(s"Bitmap mode ${regs(1) * charVisibleWidth}x$visibleScreenHeightPix ${charInfo} ${if (interlaceMode) "interlaced" else ""}")
    }
    if (debug) println(s"VDC: updated geometry. Text mode=$textMode interlaced=$interlaceMode ${regs(1) * charVisibleWidth}x${visibleScreenHeightPix} new borderWidth=$borderWidth")
  }

  @inline private def nextFrame {
    frameBit = (frameBit + 1) & 1
    if (oneLineDrawn) display.showFrame(0,0,screenWidth,screenHeight)
    else display.showFrame(-1,-1,-1,-1)

    oneLineDrawn = false

    // char blinking
    val blinkMode = regs(24) & 0x20
    val change = if (blinkMode == 0) (display.getFrameCounter & 7) == 0 // 1/16
    else (display.getFrameCounter & 0xF) == 0 // 1/32
    if (change) charBlinkOn = !charBlinkOn
    // cursor blinking
    val cursorMode = regs(10) & 0x60
    val cursor_change = if (cursorMode == 0x40) (display.getFrameCounter & 7) == 0 // 1/16
    else
    if (cursorMode == 0x60) (display.getFrameCounter % 0xF) == 0 // 1/32
    else false
    if (cursor_change) cursorOn = !cursorOn

    if (writeOnPrevFrame) {
      writeOnPrevFrame = false
      useCacheForNextFrame = false
    }
    else useCacheForNextFrame = true
    if (useCacheForNextFrame) {
      if ((regs(25) & 0x80) == 0) useCacheForNextFrame = !(change || cursor_change) // text mode
    }

    if (updateGeometryOnNextFrame) {
      updateGeometryOnNextFrame = false
      updateGeometry
    }
    // check new screen's height
    var newScreenHeight = (regs(4) + 1) * (ychars_total + 1) + (regs(5) & 0x0F)
    if (interlaceMode) newScreenHeight >>= 1
    val currentHeight = if (interlaceMode) screenHeight >> 1 else screenHeight
    // don't know if the screen height must be checked on every frame
    if (newScreenHeight > MIN_HEIGHT && newScreenHeight < MAX_HEIGHT && newScreenHeight != currentHeight) {
      setScanLines(newScreenHeight)
      display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS,screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS)
      //println(s"New screen height: $newScreenHeight")
    }
  }

  @inline private def drawTextLine(vsync:Boolean) {
    val backgroundColor = PALETTE(regs(26) & 0x0F)
    val bitmapOffset = rasterLine * screenWidth
    val blankColLeft = regs(35) % regs(0)
    val blankColRight = regs(34) % regs(0)

    val foregroundColor = PALETTE((regs(26) >> 4) & 0x0F)
    val virtualScreenWidth = regs(1) + regs(27)
    val yoffset = ram_adr + ypos * virtualScreenWidth

    var ram_ptr = ram_base_ptr
    var attr_ptr = attr_base_ptr
    var col = 0
    val doublePixFeature = (regs(25) & 0x10) > 0
    val realCharWidth = if (doublePixFeature) regs(22) >> 4 else 1 + (regs(22) >> 4)
    val charWidth = if (doublePixFeature) realCharWidth << 1 else realCharWidth
    val realCharVisibleWidth = if (doublePixFeature) this.charVisibleWidth - 1 else this.charVisibleWidth
    val rightBorderPix = borderWidth + regs(1) * charWidth
    var colPix = 0
    var char_col = 0
    val useAttributes = (regs(25) & 0x40) > 0
    val cursorMode = regs(10) & 0x60
    val cursorTopLine = regs(10) & 0x1F
    val cursorBottomLine = regs(11) & 0x1F
    val semigraphicMode = (regs(25) & 0x20) > 0

    while (col < xchars_total) {
      // TODO
      //      if (col == blankColLeft) blankMode = true
      //      else
      //      if (col == blankColRight) blankMode = false

      val outRow = colPix < borderWidth || colPix >= rightBorderPix
      if (vsync || videoMode == VideoMode.IDLE || videoMode == VideoMode.BLANK || outRow) {
        var x = 0
        val bkc = if (blankMode || videoMode == VideoMode.BLANK || (videoMode == VideoMode.IDLE && !outRow)) PALETTE(0) else backgroundColor
        while (x < charWidth && colPix + x < screenWidth) {
          bitmap(bitmapOffset + colPix + x) = bkc
          x += 1
        }
        colPix += charWidth
      }
      else {
        // pick char code
        val charCode = ram(ram_adr(ram_ptr))
        ram_ptr += 1

        var reverse = (regs(24) & 0x40) > 0
        var alternateCharSetOfs = 0
        var blink = false
        var underline = false
        var fg = foregroundColor

        if (useAttributes) {
          val attr = ram(ram_adr(attr_ptr))
          attr_ptr += 1
          if ((attr & 0x80) > 0) alternateCharSetOfs = if (ychars_total > 15) 0x2000 else 0x1000
          reverse ^= (attr & 0x40) > 0
          underline = (attr & 0x20) > 0 && currentCharScanLine == (regs(29) & 0x1F)
          blink = (attr & 0x10) > 0
          fg = PALETTE(attr & 0x0F)
        }

        val showCursor = yoffset + char_col == cursor_pos && cursorMode != 0x20

        if (showCursor) {
          val isCursorLine = if (cursorTopLine < cursorBottomLine) currentCharScanLine >= cursorTopLine && currentCharScanLine <= cursorBottomLine
          else currentCharScanLine < cursorBottomLine || currentCharScanLine > cursorTopLine
          reverse ^= isCursorLine
          if (cursorMode != 0x00 && isCursorLine) reverse ^= cursorOn // cursor blinking
        }

        val char_address = if (ychars_total > 15) (regs(28) & 0xC0) << 8 else chargen_adr
        val char_ptr = char_address + alternateCharSetOfs + charCode * bytes_per_char + currentCharScanLine
        val charBitmap = ram(ram_adr(char_ptr))
        var showChar = true

        if (blink) showChar ^= charBlinkOn

        val virtualScreenWidth = regs(1) + regs(27)
        val xscroll = realCharWidth - xsmooth
        val xscrollCount = if (char_col == 0) xscroll - 1 else 0
        var bitmapPtr = bitmapOffset + colPix
        var gfx = if (char_col >= virtualScreenWidth) 0 else charBitmap << xscrollCount
        var xpos = xscrollCount
        var doubleCount = 0
        var semigraphicStop = false
        while (xpos < realCharWidth && colPix < rightBorderPix) {
          val bit = if (xpos > realCharVisibleWidth && !semigraphicStop) false else showChar && ((gfx & 0x80) > 0 || underline)
          val color = if (bit ^ reverse) fg else backgroundColor

          if (semigraphicMode && xpos == realCharVisibleWidth) semigraphicStop = true

          if (doublePixFeature) {
            if (doubleCount == 1) {
              if (!semigraphicStop) gfx <<= 1
              xpos += 1
            }
            doubleCount ^= 1
          }
          else {
            if (!semigraphicStop) gfx <<= 1
            xpos += 1
          }
          bitmap(bitmapPtr) = if (blankMode) PALETTE(0) else color

          bitmapPtr += 1
          colPix += 1
        }
        char_col += 1
      }
      col += 1
    }
    //    if (blankColRight > xtotal) blankMode = false
  }

  @inline private def drawBitmapLine {
    val doublePixFeature = (regs(25) & 0x10) > 0
    val realCharWidth = if (doublePixFeature) regs(22) >> 4 else 1 + (regs(22) >> 4)
    val charWidth = if (doublePixFeature) realCharWidth << 1 else realCharWidth
    val realCharVisibleWidth = if (doublePixFeature) this.charVisibleWidth - 1 else this.charVisibleWidth
    val backgroundColor = PALETTE(regs(26) & 0x0F)
    val bitmapOffset = rasterLine * screenWidth
    val foregroundColor = PALETTE((regs(26) >> 4) & 0x0F)
    val virtualScreenWidth = regs(1) + regs(27)
    val interlaceRamOffset = if (interlaceMode && (rasterLine & 1) == 1) virtualScreenWidth * (visibleScreenHeightPix >> 1) else 0
    val interlaceAttrOffset = if (interlaceMode && (rasterLine & 1) == 1) visibleTextRows * virtualScreenWidth else 0
    var ram_ptr = ram_base_ptr + interlaceRamOffset + ram_base_offset
    var attr_ptr = attr_base_ptr + interlaceAttrOffset + attr_offset

    if (interlaceMode) ram_ptr += (((regs(5) & 0x1F) + 1) >> 1) * virtualScreenWidth

    var col = 0
    val rightBorderPix = borderWidth + regs(1) * charWidth
    var colPix = 0
    var char_col = 0
    val useAttributes = (regs(25) & 0x40) > 0
    val reverse = (regs(24) & 0x40) > 0
    val semigraphicMode = (regs(25) & 0x20) > 0

    while (col < xchars_total) {
      if (colPix < borderWidth || // TODO Horizontal blanking pos.
        colPix >= rightBorderPix) {
        var x = 0
        while (x < charWidth && colPix + x < screenWidth) {
          bitmap(bitmapOffset + colPix + x) = backgroundColor
          x += 1
        }
        colPix += charWidth
      }
      else {
        // pick gfx
        val bmpgfx = ram(ram_adr(ram_ptr))
        ram_ptr += 1

        var fg = foregroundColor
        var bg = backgroundColor

        if (useAttributes) {
          val attr = ram(ram_adr(attr_ptr))
          attr_ptr += 1

          fg = PALETTE(attr & 0x0F)
          bg = PALETTE((attr >> 4) & 0x0F)
        }

        val xscroll = realCharWidth - xsmooth
        val xscrollCount = if (char_col == 0) xscroll - 1 else 0
        var bitmapPtr = bitmapOffset + colPix
        var gfx = if (char_col >= virtualScreenWidth) 0 else bmpgfx << xscrollCount
        var xpos = xscrollCount
        var doubleCount = 0
        var semigraphicStop = false
        while (xpos < realCharWidth && colPix < rightBorderPix) {
          val bit = if (xpos > realCharVisibleWidth && !semigraphicStop) false else (gfx & 0x80) > 0
          val color = if (bit ^ reverse) fg else bg

          if (semigraphicMode && xpos == realCharVisibleWidth) semigraphicStop = true

          if (doublePixFeature) {
            if (doubleCount == 1) {
              if (!semigraphicStop) gfx <<= 1
              xpos += 1
            }
            doubleCount ^= 1
          }
          else {
            if (!semigraphicStop) gfx <<= 1
            xpos += 1
          }
          bitmap(bitmapPtr) = if (blankMode) PALETTE(0) else color

          bitmapPtr += 1
          colPix += 1
        }
        char_col += 1
      }
      col += 1
    }
  }

  private def setScanLines(lines:Int) {
    display.setNewResolution(lines,screenWidth)
    bitmap = display.displayMem
    currentScreenHeight = lines
    screenHeight = if (interlaceMode) lines << 1 else lines
    updateGeometryOnNextFrame = true
    writeOnPrevFrame = true
  }

  def triggerLightPen {
    lpFlag = 0x40
    regs(16) = ypos
    val delta = clk.currentCycles - clkStartLine
    regs(17) = ((delta / cycles_per_line.toDouble) * xchars_total).toInt
  }

  // =============== Properties ==========================
  override def getProperties = {
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
  protected def saveState(out:ObjectOutputStream) {
    out.writeObject(ram)
    out.writeObject(regs)
    out.writeInt(videoMode.id)
    out.writeInt(screenHeight)
    out.writeInt(currentScreenHeight)
    out.writeInt(screenWidth)
    out.writeInt(cycles_per_line)
    out.writeInt(xchars_total)
    out.writeInt(ychars_total)
    out.writeInt(ychars_visible)
    out.writeInt(bytes_per_char)
    out.writeInt(cursor_pos)
    out.writeInt(charVisibleWidth)
    out.writeInt(xsmooth)
    out.writeInt(chargen_adr)
    out.writeInt(attr_adr)
    out.writeInt(attr_offset)
    out.writeInt(ram_adr)
    out.writeInt(ypos)
    out.writeInt(ram_base_ptr)
    out.writeInt(attr_base_ptr)
    out.writeInt(rasterLine)
    out.writeInt(currentCharScanLine)
    out.writeInt(visibleScreenHeightPix)
    out.writeInt(visibleTextRows)
    out.writeInt(vsyncPos)
    out.writeInt(borderWidth)
    out.writeBoolean(interlaceMode)
  }
  protected def loadState(in:ObjectInputStream) {
    loadMemory[Int](ram,in)
    loadMemory[Int](regs,in)
    videoMode = VideoMode(in.readInt)
    screenHeight = in.readInt
    currentScreenHeight = in.readInt
    screenWidth = in.readInt
    cycles_per_line = in.readInt
    xchars_total = in.readInt
    ychars_total = in.readInt
    ychars_visible = in.readInt
    bytes_per_char = in.readInt
    cursor_pos = in.readInt
    charVisibleWidth = in.readInt
    xsmooth = in.readInt
    chargen_adr = in.readInt
    attr_adr = in.readInt
    attr_offset = in.readInt
    ram_adr = in.readInt
    ypos = in.readInt
    ram_base_ptr = in.readInt
    attr_base_ptr = in.readInt
    rasterLine = in.readInt
    currentCharScanLine = in.readInt
    visibleScreenHeightPix = in.readInt
    visibleTextRows = in.readInt
    vsyncPos = in.readInt
    borderWidth = in.readInt
    interlaceMode = in.readBoolean
    setInterlaceMode(interlaceMode)
    display.setClipArea(X_LEFT_CLIP_COLS,Y_TOP_CLIP_ROWS,screenWidth - X_RIGHT_CLIP_COLS,screenHeight - Y_BOTTOM_CLIP_ROWS)
    display.setNewResolution(currentScreenHeight,screenWidth)
    bitmap = display.displayMem
    play
  }
  protected def allowsStateRestoring(parent:JFrame) = true
}
