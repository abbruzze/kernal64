package ucesoft.cbm.c128

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.cpu.ROM
import ucesoft.cbm.expansion.ExpansionPortConfigurationListener
import ucesoft.cbm.c64.ExtendedROM
import ucesoft.cbm.c64.C64MMU
import ucesoft.cbm.Log
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.c64.MemConfig
import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.peripheral.vdc.VDC

object C128MMU extends RAMComponent with ExpansionPortConfigurationListener {
  type CPUChangeListener = (Boolean) => Unit // true if 8502 selected, false if Z80 selected
  type FreqChangeListener = (Int) => Unit    // 1 or 2 Mhz
  
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
  final private[this] val COLOR_RAM_ADDR = 0xD800
  final private[this] val KERNAL_ADDR = 0xC000
  final private[this] val BASIC_LOW_ADDR = 0x4000
  final private[this] val BASIC_HI_ADDR = 0x8000
  final private[this] val IO_ADDR = 0xD000
  final private[this] val Z80_BIOS_ADDR = 0xD000
  final private[this] val CHARACTERS128_ADDR = 0xD000
  // RAM ---------------------------------------
  final private[this] val ram = new C128RAM(this)
  // Color & ROMS ------------------------------
  final private[this] val COLOR_RAM = new ColorRAM
  // 64
  final private[this] val IO64 = new C64MMU.IO(ram,COLOR_RAM)
  final private[this] val KERNAL64_ROM = new ROM(ram, "KERNAL64", KERNAL64_ADDR, 0x2000,"roms/kernal.rom")
  final private[this] val BASIC64_ROM = new ROM(ram, "BASIC64", BASIC64_ADDR, 0x2000, "roms/basic.rom")
  final private[this] val CHARACTERS64_ROM = new ROM(ram, "CHARACTERS64", CHARACTERS64_ADDR, 0x1000, "roms/128/characters.rom",0x0)
  final private[this] val ROML = new ExtendedROM(ram,"ROML",ROML64_ADDR)
  final private[this] val ROMH = new ExtendedROM(ram,"ROMH",BASIC64_ADDR)
  final private[this] val ROMH_ULTIMAX = new ExtendedROM(ram,"ROMH_ULTIMAX",KERNAL64_ADDR)
  final private[this] val banks64 = Array(KERNAL64_ROM,IO64,CHARACTERS64_ROM,BASIC64_ROM,ROML,ROMH,ROMH_ULTIMAX)
  private[this] val banksStart = banks64 map { _.startAddress }
  private[this] val banksEnd = banks64 map { _.endAddress }
  private[this] val minAddress = banksStart.min
  // 128  
  final private[this] val BASIC128_ROM = new ROM(ram,"BASIC128_LOW_HI",BASIC_LOW_ADDR,0x8000,"roms/128/basic.rom")
  final private[this] val KERNAL128_ROM = new ROM(ram, "KERNAL128", KERNAL_ADDR, 0x4000,"roms/128/kernal.rom")
  final private[this] val CHARACTERS128_ROM = new ROM(ram, "CHARACTERS128", CHARACTERS128_ADDR, 0x1000, "roms/128/characters.rom",0x1000)
  private[this] var c128Mode = true
  // MMU ======================================================================================
  final private[this] val MMU_VER_NUMBER = 0
  final private[this] val MMU_CR1 = 0xD500
  final private[this] val MMU_CR = 0xFF00
  final private[this] val D500_REGS = Array.ofDim[Int](0xB) // index 0 not used, see cr_reg, 0xB is not used
  final private[this] val FF00_REGS = Array.ofDim[Int](0x5) // index 0 not used, see cr_reg  
  private[this] var cr_reg = 0
  private[this] var z80enabled = true
  private[this] var cpuChangeListener : CPUChangeListener = _
  // IO =======================================================================================
  private[this] var cia_dc00,cia_dd00 : CIA = _
  private[this] var vic : VIC = _
  private[this] var sid : SID = _
  private[this] var vdc : VDC = _
  //private[this] var vcd : VDC = _
  // Extended VIC registers ===================================================================
  final private[this] val VIC_XSCAN_REG = 0x2F
  final private[this] val VIC_CLKRATE_REG = 0x30
  private[this] var vic_xscan_reg,vic_clkrate_reg = 0
  // 0/1 datassette lines =====================================================================
  private[this] var datassette : Datassette = _
  // ==========================================================================================
  private[this] var _0 = 0
  private[this] var _1 = 0
  private[this] var exrom,game = false
  // C64 memory configuration =================================================================
  private[this] var c64MemConfig = -1
  private[this] val C64_MEM_CONFIG = MemConfig.MEM_CONFIG
  private[this] var ULTIMAX = false  
  // 1Mhz/2Mhz change listener ================================================================
  private[this] var freqChangeListener : FreqChangeListener = _
  // ==========================================================================================
  
  def setFreqChangeListener(freqChangeListener:FreqChangeListener) {
    this.freqChangeListener = freqChangeListener
  }
  
  def setCPUChangeListener(cpuChangeListener:CPUChangeListener) {
    this.cpuChangeListener = cpuChangeListener
  }
    
  final def init {
    // TODO      
    if (cia_dc00 == null ||
        cia_dd00 == null ||
        vic == null ||
        sid == null ||
        vdc == null) throw new IllegalStateException("I/O components not configured properly")
    if (datassette == null) throw new IllegalStateException("Datassette not set")
    Log.info("Initializing 128 main memory ...")
    add(COLOR_RAM)
    // 64
    add(IO64)
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
  
  override def afterInitHook {
    check128_1
  }
  
  final def reset {
    Log.info("Resetting 128 main memory ...")
    // TODO
    _0 = 0
    _1 = read128_1
    c128Mode = true
    c64MemConfig = -1
    ULTIMAX = false
    z80enabled = true
    cpuChangeListener(!z80enabled)
    java.util.Arrays.fill(D500_REGS,0)
    java.util.Arrays.fill(FF00_REGS,0)
    cr_reg = 0
    vic_xscan_reg = 0
    vic_clkrate_reg = 0
    exrom = false
    game = false
    check128_1
  }
  
  final def setIO(cia_dc00:CIA,cia_dd00:CIA,vic:VIC,sid:SID,vdc:VDC) {
    this.cia_dc00 = cia_dc00
    this.cia_dd00 = cia_dd00
    this.vic = vic
    this.sid = sid
    this.vdc = vdc
  }
  
  final def setDatassette(datassette:Datassette) {
    this.datassette = datassette
  }
  
  /**
   * Used in C64 mode
   */
  final def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
    this.game = game
    this.exrom = exrom
    
    val newMemConfig = (~_0 | _1) & 0x7 | (if (game) 1 << 3 else 0) | (if (exrom) 1 << 4 else 0)
    if (c64MemConfig == newMemConfig) return
    
    c64MemConfig = newMemConfig
    val mc = C64_MEM_CONFIG(c64MemConfig)
    ULTIMAX = mc.romhultimax
    ram.setULTIMAXMode(ULTIMAX)
    BASIC64_ROM.setActive(mc.basic)
    ROML.setActive(mc.roml)
    ROMH.setActive(mc.romh)
    CHARACTERS64_ROM.setActive(mc.char)
    KERNAL64_ROM.setActive(mc.kernal)
    IO64.setActive(mc.io)
    ROMH_ULTIMAX.setActive(mc.romhultimax)
  }
    
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (c128Mode) read128(address)
    else read64(address)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    if (c128Mode) write128(address,value)
    else write128(address,value)
  }
  
  @inline private[this] def ioenabled : Boolean = (cr_reg & 1) == 0
  
  @inline private[this] def read128(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    // Z-80 Bios
    if (z80enabled && address < 0x1000) return KERNAL128_ROM.read(Z80_BIOS_ADDR | address)
    // 0/1 ports --------------------------------------------
    if (address == 0) return _0
    if (address == 1) return read128_1
    // FF00-FF04 --------------------------------------------
    if (address == 0xFF00) return cr_reg
    if (address > 0xFF00 && address < 0xFF05) return D500_REGS(address & 7) // read the preconfiguration registers
    // 0002-3FFF --------------------------------------------
    if (address < 0x4000) return ram.read(address)
    // 4000-7FFF --------------------------------------------
    if (address < 0x7FFF) return mem_read_0x4000_0x7FFF(address)
    // 8000-BFFF --------------------------------------------
    if (address < 0xBFFF) return mem_read_0x8000_0xBFFF(address)
    // D000-DFFF I/O ----------------------------------------
    if (address >= 0xD000 && address < 0xE000) {
      if (ioenabled) return mem_read_0xD000_0xDFFF(address)
      return mem_read_0xC000_0xFFFF(address,true)
    }
    // C000-FFFF --------------------------------------------
    mem_read_0xC000_0xFFFF(address,false)
  }
  @inline private[this] def write128(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {    
    // Z-80 Bios
    if (z80enabled && address < 0x1000) ram.write(Z80_BIOS_ADDR | address,value) // to be checked ??
    // 0/1 ports --------------------------------------------
    else if (address == 0) { _0 = value ; check128_1 }
    else if (address == 1) { _1 = value & 0x7F ; check128_1 }
    // FF00-FF04 --------------------------------------------
    else if (address == 0xFF00) MMU_CR_write(value)
    else if (address > 0xFF00 && address < 0xFF05) MMU_FF01_4_write(address & 7) // load cr from preconfiguration registers
    // 0002-3FFF --------------------------------------------
    else if (address < 0x4000) ram.write(address,value)
    // 4000-7FFF --------------------------------------------
    else if (address < 0x7FFF) ram.write(address,value)
    // 8000-BFFF --------------------------------------------
    else if (address < 0xBFFF) ram.write(address,value)
    // D000-DFFF I/O ----------------------------------------
    else if (ioenabled && address >= 0xD000 && address < 0xE000) mem_write_0xD000_0xDFFF(address,value)
    // C000-FFFF --------------------------------------------
    else ram.write(address,value)
  }
  
  @inline private[this] def read128_1 : Int = {
    val playSense = if ((_0 & 0x10) > 0) _1 & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
    val capsLockSense = 0x0 // TODO: must read caps lock sense from keyboard
    var one = _1 & 0x2F | capsLockSense | playSense | ((_0 & 0x7) ^ 0x7) // pull up resistors
    
    if ((_0 & 0x20) == 0) one &= 0xDF
    one
  }
  
  @inline private[this] def check128_1 {
    val pr = read64_1
    // check color bank
    COLOR_RAM.setProcessorAndVICColorBanks(pr)
    // check Charen
    CHARACTERS128_ROM.setActive((pr & 4) == 0)
    // check tape motor
    datassette.setMotor((_0 & 0x20) > 0 && (_1 & 0x20) == 0)
    datassette.setWriteLine((_0 & 0x08) > 0 && (_1 & 0x08) > 0)
  }
  
  // MMU regs handling ==========================================================
  @inline private[this] def MMU_CR_write(value:Int) {
    cr_reg = value
    Log.debug(s"MMU CR set to 0x${Integer.toHexString(cr_reg)} %${Integer.toBinaryString(cr_reg)}")
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D505_read : Int = {
    var d505 = D500_REGS(5) | 0x36 // (xx11x11x)
    // game & exrom
    val expansionPort = ExpansionPort.getExpansionPort
    if ((d505 & 0x10) > 0 && expansionPort.GAME) d505 &= 0xEF // clear game bit
    if ((d505 & 0x20) > 0 && expansionPort.EXROM) d505 &= 0xDF // clear exrom bit
    // 40/80 cols
    // TODO: read the 40/80 button
    if ((d505 & 0x80) > 0) d505 |= 0x80 else d505 &= 0x7F
    Log.debug(s"Reading D505 register: 0x${Integer.toHexString(d505)} %${Integer.toBinaryString(d505)}")
    d505
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D505_write(value:Int) {    
    D500_REGS(5) = value
    // Z80/8502 check
    val oldZ80enabled = z80enabled
    z80enabled = (value & 1) == 0
    if (z80enabled != oldZ80enabled) cpuChangeListener(!z80enabled)
    // TODO: FSDIR on bit 3
    // 64/128 mode
    c128Mode = (value & 0x40) == 0
    Log.debug(s"Writing D505 register: z80enabled=$z80enabled c128Mode=$c128Mode")
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D506_write(value:Int) {
    D500_REGS(6) = value
    // can be optimized in one call ...
    ram.setCommonAreaAndSize((value & 0xC) >> 2,value & 3)
    ram.setVICBank((value & 0xC0) >> 6)
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D507_write(value:Int) {
    D500_REGS(7) = value
    ram.setDivertedPage(0,value,D500_REGS(8))
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D509_write(value:Int) {
    D500_REGS(9) = value
    ram.setDivertedPage(1,value,D500_REGS(0xA))
  }
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_D50B_read : Int = MMU_VER_NUMBER | ram.getBanksNumber << 4
  // ----------------------------------------------------------------------------
  @inline private[this] def MMU_FF01_4_write(index:Int) {
    MMU_CR_write(D500_REGS(index))
  }
  // Memory handling ============================================================
  // basic low
  @inline private[this] def mem_read_0x4000_0x7FFF(address:Int) : Int = {
    if ((cr_reg & 1) == 0) BASIC128_ROM.read(address)
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
    if (address < 0xD50B) return D500_REGS(address & 0xF)
    if (address == 0xD50B) return MMU_D50B_read
    if (address < 0xD600) return 0xFF // unused MMU space
    // VDC ---------------------------------------------------------------
    if (address < 0xD700) return vdc.read(address) 
    // Unused I/O --------------------------------------------------------
    if (address < 0xD800) return 0
    // Color RAM ---------------------------------------------------------
    if (address < 0xDC00) return COLOR_RAM.read(address)
    // CIA 1 -------------------------------------------------------------
    if (address < 0xDD00) return cia_dc00.read(address)
    // CIA 2 -------------------------------------------------------------
    if (address < 0xDE00) return cia_dd00.read(address)
    // I/O expansion slots 1 & 2 -----------------------------------------
    0
  }
  @inline private[this] def mem_write_0xD000_0xDFFF(address:Int,value:Int) {
    // VIC ---------------------------------------------------------------
    if (address < 0xD400) {
      // additional VIC registers D02F & D030
      val reg = address & 0x3F
      if (reg == VIC_XSCAN_REG) vic_xscan_reg = value
      else if (reg == VIC_CLKRATE_REG) {
        val old_clkrate = vic_clkrate_reg & 1
        vic_clkrate_reg = value
        val clkrate = value & 1
        if (clkrate != old_clkrate) freqChangeListener(clkrate + 1)
        Log.debug(s"Clock frequency set to ${clkrate + 1} Mhz")
      }
      else vic.write(address,value)
    }
    // MMU REGS ----------------------------------------------------------
    else if (address == MMU_CR1) MMU_CR_write(value)
    else if (address < 0xD50C) {
      if (address == 0xD505) MMU_D505_write(value)
      else if (address == 0xD506) MMU_D506_write(value)
      else if (address == 0xD507) MMU_D507_write(value)
      else if (address == 0xD509) MMU_D509_write(value)
      else if (address != 0xD50B) {
        D500_REGS(address & 0xF) = value
        Log.debug(s"D505_REG(${address & 0xF})=${Integer.toHexString(value)}")
      }
    }
    else if (address < 0xD600) return // unused MMU space
    // VDC ---------------------------------------------------------------
    else if (address < 0xD700) vdc.write(address,value)
    // Unused I/O --------------------------------------------------------
    else if (address < 0xD800) return
    // Color RAM ---------------------------------------------------------
    else if (address < 0xDC00) COLOR_RAM.write(address,value)
     // CIA 1 -------------------------------------------------------------
    else if (address < 0xDD00) cia_dc00.write(address,value)
    // CIA 2 -------------------------------------------------------------
    else if (address < 0xDE00) cia_dd00.write(address,value)
    // I/O expansion slots 1 & 2 -----------------------------------------
  }
  // internal & external function ROM
  @inline private[this] def mem_read_function_rom_0x8000_0xBFFF(internal:Boolean,address:Int) : Int = {
    // TODO
    0
  }
  @inline private[this] def mem_read_function_rom_0xC000_0xFFFF(internal:Boolean,address:Int) : Int = {
    // TODO
    0
  }
  // ============================================================================
  
  // C64 Management =============================================================
  @inline private[this] def read64(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (isForwardRead) forwardReadTo.read(address)
    if (ULTIMAX) {
      if (chipID == ChipID.VIC) {
        val bank = address & 0xF000
        if (bank == 0x3000) return ROMH_ULTIMAX.read(0xF000 + address - 0x3000,chipID)
        if (bank == 0x7000) return ROMH_ULTIMAX.read(0xF000 + address - 0x7000,chipID)
        if (bank == 0xB000) return ROMH_ULTIMAX.read(0xF000 + address - 0xB000,chipID)
        if (bank == 0xF000) return ROMH_ULTIMAX.read(0xF000 + address - 0xF000,chipID)
      }
    }
    if (chipID == ChipID.VIC) ram.read(address, chipID)
    else {        
      var b = 0
      var found = false
      val length = banks64.length
      if (address >= minAddress)
      while (b < length && !found) {
        if (banks64(b).isActive && address >= banksStart(b) && address < banksEnd(b)) found = true
        else b += 1
      }
      if (found) {
        val r = banks64(b).read(address, chipID)
        //Log.debug("Reading from bank %s %4X = %2X".format(bank.name,address,r)) 
        r
      }
      else {
        val r = if (address == 0) _0 
      		  else
      		  if (address == 1) read64_1 
      		  else ram.read(address, chipID)
        //Log.debug("Reading from RAM %4X = %2X".format(address,r))
        r
      }
    }
  }
  @inline private[this] def write64(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {
    if (isForwardWrite) forwardWriteTo.write(address,value)      
      
    var b = 0
    var found = false
    val length = banks64.length
    if (address >= minAddress)
    while (b < length && !found) {
      if (banks64(b).isActive && address >= banksStart(b) && address < banksEnd(b)) found = true
      else b += 1
    }
    val bank = if (!found) ram else banks64(b)
    //Log.debug("Writing to %s %4X = %2X".format(bank.name,address,value))      
    if (!found && address < 2) {
      ram.write(address,ram.lastByteRead)
      if (address == 0) _0 = value
      else _1 = value & 0x3F
      
      check64_1
    }
    else bank.write(address,value,chipID)
  }
  @inline private[this] def read64_1 : Int = {
    val playSense = if ((_0 & 0x10) > 0) _1 & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
    val capsLockSense = 0x0 // TODO: must read caps lock sense from keyboard
    var one = _1 & 0x2F | capsLockSense | playSense | ((_0 & 0x7) ^ 0x7) // pull up resistors
    
    if ((_0 & 0x20) == 0) one &= 0xDF
    one
  }
  @inline private[this] def check64_1 {
    val pr = read64_1
    // check tape motor
    datassette.setMotor((_0 & 0x20) > 0 && (_1 & 0x20) == 0)
    datassette.setWriteLine((_0 & 0x08) > 0 && (_1 & 0x08) > 0)
    val expansionPort = ExpansionPort.getExpansionPort
    val EXROM = expansionPort.EXROM
    val GAME = expansionPort.GAME
    expansionPortConfigurationChanged(GAME,EXROM)
  }
  // ============================================================================
  // state
  protected def saveState(out:ObjectOutputStream) {
    // TODO
  }
  protected def loadState(in:ObjectInputStream) {
    // TODO
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}