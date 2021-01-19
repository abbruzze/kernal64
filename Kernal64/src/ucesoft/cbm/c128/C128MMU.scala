package ucesoft.cbm.c128

import ucesoft.cbm.cpu.{CPU65xx, Memory, RAMComponent, ROM, Z80}
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import javax.swing.JFrame
import ucesoft.cbm.expansion.ExpansionPortConfigurationListener
import ucesoft.cbm.c64.ExtendedROM
import ucesoft.cbm.Log
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.c64.MemConfig
import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.peripheral.vdc.VDC
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.vic.VICMemory
import ucesoft.cbm.misc.TestCart

trait MMUChangeListener {
  def frequencyChanged(f:Int) : Unit // 1 or 2
  def cpuChanged(is8502:Boolean) : Unit
  def c64Mode(c64Mode:Boolean) : Unit
  def fastSerialDirection(input:Boolean) : Unit
  def _1571mode(_1571Mode:Boolean) : Unit
}

class C128MMU(mmuChangeListener : MMUChangeListener) extends RAMComponent with ExpansionPortConfigurationListener with VICMemory with Z80.IOMemory {
  import ROM._
  val componentID = "128 MMU"
  val componentType = CBMComponentType.MEMORY
  
  val isRom = false
  val name = "128_MMU"
  val startAddress = 0x0
  val length = 0x10000
  final val isActive = true
    
  // Addresses ---------------------------------
  // 64
  final private[this] val BASIC64_ADDR = 0xA000
  final private[this] val KERNAL64_ADDR = 0xE000
  final private[this] val CHARACTERS64_ADDR = 0xD000
  final private[this] val ROML64_ADDR = 0x8000
  // 128
  final private[this] val KERNAL_ADDR = 0xC000
  final private[this] val BASIC_LOW_ADDR = 0x4000
  final private[this] val Z80_BIOS_ADDR = 0xD000
  final private[this] val CHARACTERS128_ADDR = 0xD000
  // RAM ---------------------------------------
  final private[this] val ram = new C128RAM
  // Color & ROMS ------------------------------
  final private[this] val COLOR_RAM = new ColorRAM
  // 64
  final private[this] val KERNAL64_ROM = new ROM(ram, "KERNAL64", KERNAL64_ADDR, 0x2000,C64_KERNAL_ROM_PROP)
  final private[this] val BASIC64_ROM = new ROM(ram, "BASIC64", BASIC64_ADDR, 0x2000, C64_BASIC_ROM_PROP)
  final private[this] val CHARACTERS64_ROM = new ROM(ram, "CHARACTERS64", CHARACTERS64_ADDR, 0x1000, C128_CHAR_ROM_PROP,0x0)
  final private[this] val ROML = new ExtendedROM(ram,"ROML",ROML64_ADDR)
  final private[this] val ROMH = new ExtendedROM(ram,"ROMH",BASIC64_ADDR)
  final private[this] val ROMH_ULTIMAX = new ExtendedROM(ram,"ROMH_ULTIMAX",KERNAL64_ADDR)
  // 128  
  final private[this] val BASIC128_ROM = new ROM(ram,"BASIC128_LOW_HI",BASIC_LOW_ADDR,0x8000,C128_BASIC_ROM_PROP)
  final private[this] val KERNAL128_ROM = new ROM(ram, "KERNAL128", KERNAL_ADDR, 0x4000,C128_KERNAL_ROM_PROP)
  final private[this] val CHARACTERS128_ROM = new ROM(ram, "CHARACTERS128", CHARACTERS128_ADDR, 0x1000, C128_CHAR_ROM_PROP,0x1000)
  private[this] var c128Mode = true
  private[this] val expansionPort = ExpansionPort.getExpansionPort
  // MMU ======================================================================================
  final private[this] val MMU_VER_NUMBER = 0
  final private[this] val MMU_CR1 = 0xD500
  final private[this] val MMU_CR = 0xFF00
  final private[this] val D500_REGS = Array.ofDim[Int](0xB) // index 0 not used, see cr_reg, 0xB is not used
  final private[this] var D508Latch,D50ALatch = 0
  final private[this] val FF00_REGS = Array.ofDim[Int](0x5) // index 0 not used, see cr_reg  
  private[this] var cr_reg = 0
  private[this] var z80enabled = true
  private[this] var ramBank,vicBank = 0
  private[this] var ioacc = false
  private[this] var lastByteOnBUS = 0
  // IO =======================================================================================
  private[this] var cia_dc00,cia_dd00 : CIA = _
  private[this] var vic : VIC = _
  private[this] var sid : SID = _
  private[this] var vdc : VDC = _
  // Extended VIC registers ===================================================================
  final private[this] val VIC_XSCAN_REG = 0x2F
  final private[this] val VIC_CLKRATE_REG = 0x30
  private[this] var vic_xscan_reg,vic_clkrate_reg = 0
  // 0/1 CAPSLOCK & 40/80 senses ==============================================================
  private[this] var keyboard : Keyboard = _
  // 0/1 datassette lines =====================================================================
  private[this] var datassette : Datassette = _
  // ==========================================================================================
  private[this] var _0 = 0
  private[this] var _1 = 0
  // C64 memory configuration =================================================================
  private[this] var c64MemConfig = -1
  private[this] var c64MC : MemConfig = _
  private[this] val C64_MEM_CONFIG = MemConfig.MEM_CONFIG
  private[this] var ULTIMAX = false  
  // VIC stuff ================================================================================
  private[this] var videoBank = 0
  private[this] var vicBaseAddress = 0
  private[this] var memLastByteRead = 0
  // Internal & External function ROM =========================================================
  private[this] var internalFunctionROM,internalFunctionROM_mid,internalFunctionROM_high,externalFunctionROM_mid,externalFunctionROM_high : Array[Int] = _
  private[this] var internalROMType : FunctionROMType.Value = FunctionROMType.NORMAL
  // ==========================================================================================
  private[this] var cpu : CPU65xx = _

  def setCPU(cpu:CPU65xx) : Unit = this.cpu = cpu

  def getBank0RAM : Memory = ram.getBank0
  def colorRAM = COLOR_RAM
  def RAM = ram
  def CHAR_ROM = CHARACTERS128_ROM
  
  override def getProperties = {
    properties.setProperty("VIC bank",vicBank.toString)
    properties.setProperty("RAM bank",ramBank.toString)
    properties.setProperty("IO access",ioacc.toString)
    properties.setProperty("Video bank",videoBank.toString)
    properties.setProperty("VIC base address",Integer.toHexString(vicBaseAddress))
    properties.setProperty("Char rom active",CHARACTERS128_ROM.isActive.toString)    
    properties
  }
  
  def setKeyboard(keyboard:Keyboard) : Unit = {
    this.keyboard = keyboard
  }
  
  final def isIOACC = ioacc
    
  final def init  : Unit = {
    if (cia_dc00 == null ||
        cia_dd00 == null ||
        vic == null ||
        sid == null ||
        vdc == null) throw new IllegalStateException("I/O components not configured properly")
    if (datassette == null) throw new IllegalStateException("Datassette not set")
    if (keyboard == null) throw new IllegalStateException("Keyboard not set")
    Log.info("Initializing 128 main memory ...")
    add(ram)
    add(COLOR_RAM)
    // 64
    add(KERNAL64_ROM)
    add(BASIC64_ROM)
    add(CHARACTERS64_ROM)
    add(ROML)
    add(ROMH)
    add(ROMH_ULTIMAX)
    // 128
    add(BASIC128_ROM)
    add(KERNAL128_ROM)
    add(CHARACTERS128_ROM)
    add(cia_dc00)
    add(cia_dd00)
    add(vic)
    add(sid)
    add(vdc)    
  }
  
  def configureFunctionROM(internal:Boolean,_rom:Array[Byte],romType:FunctionROMType.Value) : Unit = {
    if (_rom == null) {
      if (internal) {
        internalFunctionROM_mid = null
        internalFunctionROM_high = null
        internalFunctionROM = null
      }
      else {
        externalFunctionROM_mid = null
        externalFunctionROM_high = null
      }
      return  
    }
    internalROMType = romType
    val rom = _rom map { _.toInt & 0xFF }
    if (_rom.length <= 16384) { // only mid affected      
      if (internal) {
        internalFunctionROM_mid = rom
        internalFunctionROM_high = rom
      } else {
        externalFunctionROM_mid = rom
        externalFunctionROM_high = rom
      }
    }
    else {
      val midRom = Array.ofDim[Int](16384)
      Array.copy(rom,0,midRom,0,16384)
      if (internal) internalFunctionROM_mid = midRom else externalFunctionROM_mid = midRom
      val highRom = Array.ofDim[Int](16384)
      Array.copy(rom,16384,highRom,0,16384)
      if (internal) internalFunctionROM_high = highRom else externalFunctionROM_high = highRom
      if (romType != FunctionROMType.NORMAL) internalFunctionROM = rom
    }
    //else throw new IllegalArgumentException("Invalid function ROM: size must be less than 32K")
  }
  
  private def setInternalROMPage(page:Int) : Unit = {
    val offset = page << 15
    Array.copy(internalFunctionROM,offset,internalFunctionROM_mid,0,16384)
    Array.copy(internalFunctionROM,offset + 16384,internalFunctionROM_high,0,16384)
  }
  
  override def afterInitHook  : Unit = {
    check128_1
  }
  
  final def reset  : Unit = {
    Log.info("Resetting 128 main memory ...")
    _0 = 0
    _1 = 0//read128_1
    c128Mode = true
    c64MemConfig = -1
    ULTIMAX = false
    z80enabled = true
    mmuChangeListener.cpuChanged(false)
    mmuChangeListener.c64Mode(false)
    mmuChangeListener.frequencyChanged(1)
    java.util.Arrays.fill(D500_REGS,0)
    java.util.Arrays.fill(FF00_REGS,0)
    cr_reg = 0
    vic_xscan_reg = 0
    vic_clkrate_reg = 0
    ramBank = 0
    vicBank = 0
    videoBank = 0
    vicBaseAddress = 0
    memLastByteRead = 0
    ioacc = false
    check128_1

  }
  
  final def setIO(cia_dc00:CIA,cia_dd00:CIA,vic:VIC,sid:SID,vdc:VDC) : Unit = {
    this.cia_dc00 = cia_dc00
    this.cia_dd00 = cia_dd00
    this.vic = vic
    this.sid = sid
    this.vdc = vdc
  }
  
  final def setDatassette(datassette:Datassette) : Unit = {
    this.datassette = datassette
  }
  
  /**
   * Used in C64 mode
   */
  final def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) : Unit = {
    val newMemConfig = (~_0 | _1) & 0x7 | (if (game) 1 << 3 else 0) | (if (exrom) 1 << 4 else 0)
    if (c64MemConfig == newMemConfig) return
    c64MemConfig = newMemConfig
    c64MC = C64_MEM_CONFIG(c64MemConfig)
    ULTIMAX = c64MC.romhultimax
  }

  final override def byteOnBUS: Int = lastByteOnBUS
    
  final def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    val address = _address & 0xFFFF
    if (chipID == ChipID.VIC) return vicRead(address)
    ioacc = false
    lastByteOnBUS = if (c128Mode) {
      if (z80enabled) readZ80(address) else read128(address)
    }
    else read64(address)
    lastByteOnBUS
  }
  final def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    val address = _address & 0xFFFF
    ioacc = false
    if (c128Mode) {
      if (z80enabled) writeZ80(address,value) else write128(address,value)
    }
    else write64(address,value)
    lastByteOnBUS = value
  }
  
  /**
   *  Z80 I/O memory handling ===================================================
   *  Z-80 I/O mapped area:

      In addition to the normal memory mapping which includes RAM and
      ROM, the Z-80 has another addressing mode which is called I/O
      mapping.  This is used to access the chip registers and is
      similar to memory mapping except the Z-80 IN and OUT instructions
      must be used instead of LD type instructions.  The two most
      commonly used ones are:
       
      IN A,(C)    which will read the value of the I/O port (chip
                  register) addressed by .BC into the .A register
      OUT (C),A   which will write the value of .A to the I/O port
                  addressed by .BC
      
      In both cases, .BC is a 16 bit address and .A is an 8 bit value. 
      The C-128 I/O chips appear at their normal address locations in
      the I/O area as outlined below and can be programmed directly by
      the experienced user.  Note that you must be careful when playing
      with register values because CP/M expects most of them to be set
      in certain ways.  Changing these settings may cause a system
      crash.  If no chip register is present at the specified address,
      the underlying RAM or ROM is written/read.
      
      0000 to 0FFF                  image of RAM mapped down from D000 to
      DFFF of BANK 0.
      
      1000 to 13FF      VICCOLOR    color map for 40 col screen  
   */
  final def in(addressHI:Int,addressLO:Int) : Int = {
    val address = addressHI << 8 | addressLO
    if (address < 0x1000) return ram.read(0xD000 | address,0)
    if (address < 0x1400) return COLOR_RAM.read(address & 0x3FF)
    if (address >= 0xD800 && address < 0xDC00) return ram.read(address)
    if (address >= 0xD000 && address < 0xE000) return mem_read_0xD000_0xDFFF(address)
    read128(address)
  }
  final def out(addressHI:Int,addressLO:Int,value:Int) : Unit = {
    val address = addressHI << 8 | addressLO
    if (address < 0x1000) ram.write(0xD000 | address,0,value)
    else if (address < 0x1400) COLOR_RAM.write(address & 0x3FF,value)
    else if (address >= 0xD800 && address < 0xDC00) ram.write(address,value)
    else if (address >= 0xD000 && address < 0xE000) mem_write_0xD000_0xDFFF(address,value)
    else write128(address,value)    
  }      
  // Z80 ========================================================================
  /**
   * Z-80 
   * BANK 2 :    MMU configuration register value $3E, or preconfig
    register #3 (FF03)
     
    This bank, which I have arbitrarily called BANK 2, is mostly the
    same as BANK 0 with the exception that the 40 column video color
    and I/O are mapped into context:
    
    1000 to 13FF      VICCOLOR    color map for 40 col screen
    
    Note:       This bank must also be in context to access the MMU
                chip registers at D500 in the Z-80 I/O mapped area.
   */
  @inline private[this] def readZ80(address:Int) : Int = {
    if (address < 0x1000) {
      if (ramBank == 0) return KERNAL128_ROM.read(Z80_BIOS_ADDR | address)
      return ram.read(address)
    }
    if (address < 0x1400 && cr_reg == 0x3E) return COLOR_RAM.read(address & 0x3FF)
    // FF00-FF04 --------------------------------------------
    if (address == 0xFF00) return cr_reg
    if (address > 0xFF00 && address < 0xFF05) return D500_REGS(address & 7) // read the preconfiguration registers
    // 0002-3FFF --------------------------------------------
    if (address < 0x4000) return ram.read(address)
    // 4000-7FFF --------------------------------------------
    if (address < 0x8000) return mem_read_0x4000_0x7FFF(address)
    // 8000-BFFF --------------------------------------------
    if (address < 0xC000) return mem_read_0x8000_0xBFFF(address)
    // I/O --------------------------------------------------
    if (address >= 0xD000 && address < 0xE000) return ram.read(address)
    // C000-FFFF --------------------------------------------
    mem_read_0xC000_0xFFFF(address,false)
  }
  @inline private[this] def writeZ80(address:Int,value:Int) : Unit = {
    if (address < 0x1000) {
      if (ramBank == 0) KERNAL128_ROM.write(Z80_BIOS_ADDR | address,value) // ?
      else ram.write(address,value)
    }
    else if (address < 0x1400 && cr_reg == 0x3E) COLOR_RAM.write(address & 0x3FF,value)
    // FF00-FF04 --------------------------------------------
    else if (address == 0xFF00) MMU_CR_write(value)
    else if (address > 0xFF00 && address < 0xFF05) MMU_FF01_4_write(address & 7) // load cr from preconfiguration registers
    else ram.write(address,value)
  }
  // 8502 =======================================================================
  @inline private[this] def ioenabled : Boolean = (cr_reg & 1) == 0
  @inline private[this] def read128(address: Int): Int = {
    if (isForwardRead) forwardReadTo.read(address)
    // 0/1 ports --------------------------------------------
    if (address == 0) return _0
    if (address == 1) return read128_1
    // FF00-FF04 --------------------------------------------
    if (address == 0xFF00) return cr_reg
    if (address > 0xFF00 && address < 0xFF05) return D500_REGS(address & 7) // read the preconfiguration registers
    // 0002-3FFF --------------------------------------------
    if (address < 0x4000) return ram.read(address)
    // 4000-7FFF --------------------------------------------
    if (address < 0x8000) return mem_read_0x4000_0x7FFF(address)
    // 8000-BFFF --------------------------------------------
    if (address < 0xC000) return mem_read_0x8000_0xBFFF(address)
    // D000-DFFF I/O ----------------------------------------
    if (address >= 0xD000 && address < 0xE000) {
      if (ioenabled) return mem_read_0xD000_0xDFFF(address)
      return mem_read_0xC000_0xFFFF(address,true)
    }
    // C000-FFFF --------------------------------------------
    mem_read_0xC000_0xFFFF(address,false)
  }
  @inline private[this] def write128(address: Int, value: Int) = {  
    if (isForwardWrite) forwardWriteTo.write(address,value)
    // 0/1 ports --------------------------------------------
    if (address == 0) { _0 = value ; check128_1 }
    else if (address == 1) { _1 = value & 0x7F ; check128_1 }
    // FF00-FF04 --------------------------------------------
    else if (address == 0xFF00) MMU_CR_write(value)
    else if (address > 0xFF00 && address < 0xFF05) MMU_FF01_4_write(address & 7) // load cr from preconfiguration registers
    // 0002-3FFF --------------------------------------------
    else if (address < 0x4000) ram.write(address,value)
    // 4000-7FFF --------------------------------------------
    else if (address < 0x8000) ram.write(address,value)
    // 8000-BFFF --------------------------------------------
    else if (address < 0xC000) ram.write(address,value)
    // D000-DFFF I/O ----------------------------------------
    else if (ioenabled && address >= 0xD000 && address < 0xE000) mem_write_0xD000_0xDFFF(address,value)
    // C000-FFFF --------------------------------------------
    else ram.write(address,value)
  }
  
  @inline private[this] def read128_1 : Int = {
    val playSense = if ((_0 & 0x10) > 0) _1 & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
    val capsLockSense = if (keyboard.isCapsLockPressed) 0x0 else 0x1
    var one = _1 & 0x2F | capsLockSense << 6 | playSense | ((_0 & 0x7) ^ 0x7) // pull up resistors
    
    if ((_0 & 0x20) == 0) one &= 0xDF
    one
  }
  
  @inline private[this] def check128_1  : Unit = {
    val pr = read128_1
    // check color bank
    COLOR_RAM.setProcessorAndVICColorBanks(pr)
    // check Charen
    CHARACTERS128_ROM.setActive((pr & 4) == 0)
    // check tape motor
    datassette.setMotor((_0 & 0x20) > 0 && (_1 & 0x20) == 0)
    datassette.setWriteLine((_0 & 0x08) > 0 && (_1 & 0x08) > 0)
  }
  
  // MMU regs handling ==========================================================
  @inline private[this] def MMU_CR_write(value:Int) : Unit = {
    cr_reg = value
    ramBank = (cr_reg & 0xC0) >> 6
    ram.setProcessorBank(ramBank)
    Log.debug(s"MMU CR set to 0x${Integer.toHexString(cr_reg)} %${Integer.toBinaryString(cr_reg)}")
    //println(s"MMU CR set to 0x${Integer.toHexString(cr_reg)} %${Integer.toBinaryString(cr_reg)}")
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D505_read : Int = {
    var d505 = D500_REGS(5) | 0x36 // (xx11x11x)
    // game & exrom
    if ((d505 & 0x10) > 0 && !expansionPort.GAME) d505 &= 0xEF // clear game bit
    if ((d505 & 0x20) > 0 && !expansionPort.EXROM) d505 &= 0xDF // clear exrom bit
    //println(s"Reading D505 exrom=${expansionPort.EXROM} game=${expansionPort.GAME}")
    // 40/80 cols
    var _d505 = d505
    if ((d505 & 0x80) > 0) {
      if (!keyboard.is4080Pressed) _d505 |= 0x80 else _d505 &= 0x7F
    }
    Log.debug(s"Reading D505 register: 0x${Integer.toHexString(_d505)}")
    _d505
  }
  // ----------------------------------------------------------------------------
  def go64  : Unit = {
    MMU_D505_write(D500_REGS(5) | 0x41)
  }
  @inline private[this] def MMU_D505_write(value:Int) : Unit = {
    D500_REGS(5) = value
    // Z80/8502 check
    val oldZ80enabled = z80enabled
    z80enabled = (value & 1) == 0    
    // FSDIR on bit 3
    mmuChangeListener.fastSerialDirection((value & 8) == 0)
    // 64/128 mode
    val old128Mode = c128Mode
    c128Mode = (value & 0x40) == 0
    if (z80enabled != oldZ80enabled) mmuChangeListener.cpuChanged(!z80enabled)
    if (!c128Mode && old128Mode && !z80enabled) { 
      mmuChangeListener.c64Mode(true)
      check64_1
    }
    Log.debug(s"Writing D505 register: z80enabled=$z80enabled c128Mode=$c128Mode fastSerialDir=${(value & 8) == 0}")
    //println(s"Writing D505 register: z80enabled=$z80enabled c128Mode=$c128Mode")
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D506_write(value:Int) : Unit = {
    D500_REGS(6) = value
    // can be optimized in one call ...
    ram.setCommonAreaAndSize((value & 0xC) >> 2,value & 3)
    vicBank = (value & 0xC0) >> 6
    ram.setVICBank(vicBank)
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D507_write(value:Int) : Unit = {
    D500_REGS(7) = value
    D500_REGS(8) = D508Latch | 0xF0
    ram.setDivertedPage(0,value,D500_REGS(8))
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D509_write(value:Int) : Unit = {
    D500_REGS(9) = value
    D500_REGS(0xA) = D50ALatch | 0xF0
    ram.setDivertedPage(1,value,D500_REGS(0xA))
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D50B_read : Int = MMU_VER_NUMBER | ram.getBanksNumber << 4
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_FF01_4_write(index:Int) : Unit = {
    //println(s"Setting CR from PCR($index) = ${D500_REGS(index)}")
    MMU_CR_write(D500_REGS(index))
  }
  // Memory handling ============================================================
  // basic low
  @inline private[this] def mem_read_0x4000_0x7FFF(address:Int) : Int = {
    if ((cr_reg & 2) == 0) BASIC128_ROM.read(address)
    else ram.read(address)
  }
  // basic hi
  @inline private[this] def mem_read_0x8000_0xBFFF(address:Int) : Int = {
    val conf = (cr_reg & 0xC) >> 2
    if (conf == 0) BASIC128_ROM.read(address)
    else 
    if (conf == 3) ram.read(address)
    else mem_read_function_rom_0x8000_0xBFFF(conf == 1,address)    
  }
  // kernal
  @inline private[this] def mem_read_0xC000_0xFFFF(address:Int,withinIODisabled:Boolean) : Int = {
    val conf = (cr_reg & 0x30) >> 4
    if (conf == 0) {
      if (withinIODisabled) CHARACTERS128_ROM.read(address)
      else KERNAL128_ROM.read(address)
    }
    else 
    if (conf == 3) ram.read(address)
    else mem_read_function_rom_0xC000_0xFFFF(conf == 1,address)
  }
  // I/O
  @inline private[this] def mem_read_0xD000_0xDFFF(address:Int) : Int = {
    ioacc = true
    // VIC ---------------------------------------------------------------
    if (address < 0xD400) {
      // additional VIC registers D02F & D030
      val reg = address & 0x3F
      if (reg == VIC_XSCAN_REG) return vic_xscan_reg | 0xF8
      if (reg == VIC_CLKRATE_REG) return vic_clkrate_reg | 0xFC
      return vic.read(address)
    }
    // SID ---------------------------------------------------------------
    if (address < 0xD500) return sid.read(address)
    // MMU REGS ----------------------------------------------------------
    if (address == MMU_CR1) return cr_reg
    if (address == 0xD505) return MMU_D505_read
    if (address < 0xD50B) return D500_REGS(address & 0xF)
    if (address == 0xD50B) return MMU_D50B_read
    if (address < 0xD600) return 0xFF // unused MMU space
    // VDC ---------------------------------------------------------------
    if (address < 0xD700) return vdc.read(address) 
    // Unused I/O --------------------------------------------------------
    if (address < 0xD800) return 0
    // Color RAM ---------------------------------------------------------
    if (address < 0xDC00) return COLOR_RAM.read(address) & 0x0F | lastByteRead & 0xF0
    // CIA 1 -------------------------------------------------------------
    if (address < 0xDD00) return cia_dc00.read(address)
    // CIA 2 -------------------------------------------------------------
    if (address < 0xDE00) return cia_dd00.read(address)
    // I/O expansion slots 1 & 2 -----------------------------------------
    expansionPort.read(address)
  }
  @inline private[this] def mem_write_D030(reg:Int,value:Int) : Unit = {
    val old_clkrate = vic_clkrate_reg & 1
    vic_clkrate_reg = value
    val clkrate = value & 1
    if (clkrate != old_clkrate) mmuChangeListener.frequencyChanged(clkrate + 1)
    //if ((value & 2) > 0) println("VIC D030 trick mode...")
    vic.c128TestBitEnabled((value & 2) > 0)
    Log.debug(s"Clock frequency set to ${clkrate + 1} Mhz")
  }
  @inline private[this] def mem_write_D5xx(address:Int,value:Int) : Unit = {
    if (address == 0xD505) MMU_D505_write(value)
    else if (address == 0xD506) MMU_D506_write(value)
    else if (address == 0xD507) MMU_D507_write(value)
    else if (address == 0xD508) D508Latch = value
    else if (address == 0xD509) MMU_D509_write(value)
    else if (address == 0xD50A) D50ALatch = value
    else if (address != 0xD50B) {
      D500_REGS(address & 0xF) = value
      Log.debug(s"D505_REG(${address & 0xF})=${Integer.toHexString(value)}")
    }
  }
  @inline private[this] def mem_write_0xD000_0xDFFF(address:Int,value:Int) : Unit = {
    ioacc = true
    // TestCart
    if (TestCart.enabled) TestCart.write(address,value)
    // VIC ---------------------------------------------------------------
    if (address < 0xD400) {
      // additional VIC registers D02F & D030
      val reg = address & 0x3F
      if (reg == VIC_XSCAN_REG) {
        vic_xscan_reg = value
        keyboard.selectC128ExtendedRow(value)
      }
      else if (reg == VIC_CLKRATE_REG) mem_write_D030(reg,value)
      else vic.write(address,value)
    }
    // SID ---------------------------------------------------------------
    else if (address < 0xD500) sid.write(address,value)
    // MMU REGS ----------------------------------------------------------
    else if (address == MMU_CR1) MMU_CR_write(value)
    else if (address < 0xD50C) mem_write_D5xx(address,value)
    else if (address < 0xD600) return // unused MMU space
    // VDC ---------------------------------------------------------------
    else if (address < 0xD700) vdc.write(address,value)
    // Unused I/O --------------------------------------------------------
    else if (address < 0xD800) {
      if (address == 0xD700 && internalROMType == FunctionROMType.MEGABIT) setInternalROMPage(value)
      return
    }
    // Color RAM ---------------------------------------------------------
    else if (address < 0xDC00) COLOR_RAM.write(address,value & 0x0F)
     // CIA 1 -------------------------------------------------------------
    else if (address < 0xDD00) cia_dc00.write(address,value)
    // CIA 2 -------------------------------------------------------------
    else if (address < 0xDE00) cia_dd00.write(address,value)
    // I/O expansion slots 1 & 2 -----------------------------------------
    else expansionPort.write(address,value)
  }
  /*
   * internal & external function ROM
   */
  @inline private[this] def mem_read_function_rom_0x8000_0xBFFF(internal:Boolean,_address:Int) : Int = {
    val address = _address & 0x3FFF
    if (internal) {
      if (internalFunctionROM_mid == null) return 0
      if (address >= internalFunctionROM_mid.length) return 0
      internalFunctionROM_mid(address)
    }
    else {
      if (externalFunctionROM_mid == null) return 0
      if (address >= externalFunctionROM_mid.length) return 0      
      externalFunctionROM_mid(address)
    }
  }
  @inline private[this] def mem_read_function_rom_0xC000_0xFFFF(internal:Boolean,_address:Int) : Int = {
    val address = _address & 0x3FFF
    if (internal) {
      if (internalFunctionROM_high == null) return 0
      if (address >= internalFunctionROM_high.length) return 0
      internalFunctionROM_high(address)
    }
    else {
      if (externalFunctionROM_high == null) return 0
      if (address >= externalFunctionROM_high.length) return 0
      externalFunctionROM_high(address)
    }
  }
  // ============================================================================
  
  // C64 Management =============================================================
  @inline private[this] def read64(address: Int): Int = {
    if (ULTIMAX) {
      if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return lastByteRead
    }
    if (isForwardRead) forwardReadTo.read(address)
    if (address == 0) return _0
    if (address == 1) return read64_1 
    if (address < 0x8000) return ram.read(address)
    if (address < 0xA000) { // ROML or RAM
      if (c64MC.roml) return ROML.read(address) else return ram.read(address)
    }
    if (address < 0xC000) { // BASIC or RAM or ROMH
      if (c64MC.basic) return BASIC64_ROM.read(address)
      if (c64MC.romh) return ROMH.read(address)
      return ram.read(address)
    }
    if (address < 0xD000) return ram.read(address) // RAM
    if (address < 0xE000) { // I/O or RAM or CHAR
      if (c64MC.io) { // I/O               
        if (address < 0xD400) {
          val reg = address & 0x3F
          if (reg == VIC_XSCAN_REG) return vic_xscan_reg | 0xF8
          if (reg == VIC_CLKRATE_REG) return vic_clkrate_reg | 0xFC
          return vic.read(address)
        } 
        if (address >= 0xD500 && address < 0xD50C) {
          // MMU REGS ----------------------------------------------------------
          if (address == MMU_CR1) return cr_reg
          if (address == 0xD505) return MMU_D505_read
          if (address < 0xD50B) return D500_REGS(address & 0xF)
          if (address == 0xD50B) return MMU_D50B_read
          // -------------------------------------------------------------------
        }
        if (address >= 0xD600 && address < 0xD700) return vdc.read(address)
        if (address < 0xD800) return sid.read(address)
        if (address < 0xDC00) return COLOR_RAM.read(address) & 0x0F | lastByteRead & 0xF0
        if (address < 0xDD00) return cia_dc00.read(address)
        if (address < 0xDE00) return cia_dd00.read(address)
        
        return expansionPort.read(address)
      }
      if (c64MC.char) return CHARACTERS64_ROM.read(address)
      return ram.read(address)
    }
    // KERNAL or RAM or ROMHULTIMAX
    if (c64MC.kernal) return KERNAL64_ROM.read(address)
    if (c64MC.romhultimax) return ROMH_ULTIMAX.read(address)
    ram.read(address)
  }
  private[this] def write64(address: Int,value:Int) : Unit = {
    if (ULTIMAX) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return
      }
    if (isForwardWrite) forwardWriteTo.write(address,value)
    
    if (address < 2) {
      if (address == 0) _0 = value & 0xFF
      else _1 = value & 0x3F
      ram.write(address,memLastByteRead)
      check64_1
    }
    else if (address < 0x8000) ram.write(address,value)
    else if (address < 0xA000) { // ROML or RAM
      if (c64MC.roml) ROML.write(address,value) else ram.write(address,value)
    }
    else if (address < 0xC000) { // BASIC or RAM or ROMH
      if (c64MC.romh) ROMH.write(address,value) else ram.write(address,value)
    }
    else if (address < 0xD000) ram.write(address,value) // RAM
    else if (address < 0xE000) { // I/O or RAM or CHAR
      if (c64MC.io) { // I/O        
        if (address < 0xD400) {
          val reg = address & 0x3F
          if (reg == VIC_XSCAN_REG) {
            vic_xscan_reg = value
            keyboard.selectC128ExtendedRow(value)
          }
          else if (reg == VIC_CLKRATE_REG) mem_write_D030(reg,value)
          else vic.write(address,value)
        }
        // MMU REGS ----------------------------------------------------------
        else if (address == MMU_CR1) MMU_CR_write(value)
        else if (address > 0xD500 && address < 0xD50C) mem_write_D5xx(address,value)
        else if (address >= 0xD600 && address < 0xD700) vdc.write(address,value)
        // -------------------------------------------------------------------
        else if (address < 0xD800) sid.write(address,value)
        else if (address < 0xDC00) COLOR_RAM.write(address,value)
        else if (address < 0xDD00) cia_dc00.write(address,value)
        else if (address < 0xDE00) cia_dd00.write(address,value)
        else expansionPort.write(address,value)
        // TestCart
        if (TestCart.enabled) TestCart.write(address,value)
      }
      else ram.write(address,value)
    }
    // KERNAL or RAM or ROMH
    else if (c64MC.romhultimax) ROMH_ULTIMAX.write(address,value) else ram.write(address,value)
  }
 
  @inline private[this] def read64_1 : Int = {
    val playSense = if ((_0 & 0x10) > 0) _1 & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
    val capsLockSense =  if (keyboard.isCapsLockPressed) 0x0 else 0x1
    var one = _1 & 0x2F | capsLockSense << 6 | playSense | ((_0 & 0x7) ^ 0x7) // pull up resistors
    
    if ((_0 & 0x20) == 0) one &= 0xDF    // CASS MOTOR is output
    if ((_0 & 0x40) == 0x40) one &= 0xBF // CAPS-LOCK is an input
    if ((_0 & 0x10) == 0x10) one &= 0xEF // PLAY-SENSE is an input
    one
  }
  @inline private[this] def check64_1  : Unit = {
    // check tape motor
    datassette.setMotor((_0 & 0x20) > 0 && (_1 & 0x20) == 0)
    datassette.setWriteLine((_0 & 0x08) > 0 && (_1 & 0x08) > 0)
    val EXROM = expansionPort.EXROM
    val GAME = expansionPort.GAME
    expansionPortConfigurationChanged(GAME,EXROM)
  }
  // VIC memory ===========================================================================
  final def getBank = videoBank
  final def setVideoBank(bank: Int) : Unit = {
    videoBank = ~bank & 3
    vicBaseAddress = videoBank << 14
  }
  final def lastByteRead = memLastByteRead

  override def readPCOpcode = ram.read(cpu.getPC)
  
  @inline private def vicReadPhi1(address: Int) : Int = {
    val realAddress = vicBaseAddress | address
    if (!c128Mode) { // C64
      if ((realAddress & 0x7000) == 0x1000 && !ULTIMAX) CHARACTERS64_ROM.read(0xD000 | (address & 0x0FFF),ChipID.VIC)
      else {
        if (ULTIMAX) {
          val bank = address & 0xF000
          if (bank == 0x3000) return ROMH_ULTIMAX.read(0xF000 + address - 0x3000,ChipID.VIC)
          if (bank == 0x7000) return ROMH_ULTIMAX.read(0xF000 + address - 0x7000,ChipID.VIC)
          if (bank == 0xB000) return ROMH_ULTIMAX.read(0xF000 + address - 0xB000,ChipID.VIC)
          if (bank == 0xF000) return ROMH_ULTIMAX.read(0xF000 + address - 0xF000,ChipID.VIC)
        }
        ram.read(realAddress,ChipID.VIC)
      }
    }
    else { // 128 mode
      if ((realAddress & 0x3000) == 0x1000 && CHARACTERS128_ROM.isActive) CHARACTERS128_ROM.read(0xD000 | (address & 0x0FFF),ChipID.VIC)
      else ram.read(realAddress,ChipID.VIC)
    }    
  }
  @inline private[this] def vicRead(address:Int) : Int = {
    memLastByteRead = vicReadPhi1(address)    
    memLastByteRead
  }
  final def readPhi2(address:Int) : Int = vicReadPhi1(address)
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(c128Mode)
    out.writeBoolean(z80enabled)
    out.writeObject(D500_REGS)
    out.writeObject(FF00_REGS)
    out.writeInt(cr_reg)
    out.writeInt(ramBank)
    out.writeInt(vicBank)
    out.writeBoolean(ioacc)
    out.writeInt(vic_xscan_reg)
    out.writeInt(vic_clkrate_reg)
    out.writeInt(_0)
    out.writeInt(_1)
    out.writeInt(c64MemConfig)
    out.writeBoolean(ULTIMAX)
    out.writeInt(videoBank)
    out.writeInt(vicBaseAddress)
    out.writeInt(memLastByteRead)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    val old128Mode = c128Mode
    val oldZ80enabled = z80enabled
    c128Mode = in.readBoolean
    z80enabled = in.readBoolean    
    if (z80enabled != oldZ80enabled) mmuChangeListener.cpuChanged(!z80enabled)
    if (!c128Mode && old128Mode && !z80enabled) { 
      mmuChangeListener.c64Mode(true)
      check64_1
    }
    
    loadMemory[Int](D500_REGS,in)
    loadMemory[Int](FF00_REGS,in)
    cr_reg = in.readInt
    ramBank = in.readInt
    vicBank = in.readInt
    ioacc = in.readBoolean
    vic_xscan_reg = in.readInt
    keyboard.selectC128ExtendedRow(vic_xscan_reg)
    
    val old_clkrate = vic_clkrate_reg & 1
    vic_clkrate_reg = in.readInt    
    val clkrate = vic_clkrate_reg & 1
    if (clkrate != old_clkrate) mmuChangeListener.frequencyChanged(clkrate + 1)
    vic.c128TestBitEnabled((vic_clkrate_reg & 2) > 0)
    
    _0 = in.readInt
    _1 = in.readInt
    c64MemConfig = in.readInt
    if (c64MemConfig != -1) c64MC = C64_MEM_CONFIG(c64MemConfig)
    ULTIMAX = in.readBoolean
    videoBank = in.readInt
    vicBaseAddress = in.readInt
    memLastByteRead = in.readInt
    check128_1
    check64_1
  }
  protected def allowsStateRestoring : Boolean = true
}