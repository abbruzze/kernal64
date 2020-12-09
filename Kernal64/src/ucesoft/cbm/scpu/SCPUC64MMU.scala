package ucesoft.cbm.scpu

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util

import ucesoft.cbm.cpu._
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortConfigurationListener, LastByteReadMemory}
import ucesoft.cbm.misc.TestCart
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.{CBMComponentType, ChipID, Log}
import ucesoft.cbm.c64._

object SCPUC64MMU {
  import ROM._
  final val M_ROML = 0x8000
  final val M_BASIC = 0xA000
  final val M_KERNAL = 0xE000
  final val M_CHARACTERS, M_IO = 0xD000
  final val COLOR_RAM = 0xD800
  final val SID_RAM = 0xD400
  final val M_SCPU = 0xF80000
    
  // ------------------ ROMs ----------------------------------------------
  class CHARACTERS_ROM(ram: Memory) extends ROM(ram, "CHARACTERS", M_CHARACTERS, 4096, C64_CHAR_ROM_PROP)
  class SCPU_ROM(ram:Memory) extends ROM(ram,"SCPU_ROM",M_SCPU,-1,SCPU64_ROM_PROP,0,List(64*1024,128*1024,256*1024,512*1024))
  // ------------------ C64 1Mhz RAM --------------------------------------
  class RAM extends RAMComponent {
    val componentID = "RAM"
    val componentType = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "RAM"
    val startAddress = 0x0
    val length = 0x10000

    private[this] val mem = Array.fill(length)(0xFF)
    private[SCPUC64MMU] var ULTIMAX = false
    private[SCPUC64MMU] var lastByteReadMemory : LastByteReadMemory = _
    
    final val isActive = true
    def init : Unit = {
      Log.info("Initializing RAM memory ...")
      var m = 0
      var v0 = 0xFF
      var v2 = 0
      for(_ <- 0 to 255) {
        if (m == 0x4000) {
          v0 = 0
          v2 = 0xFF
        }
        else
        if (m == 0xC000) {
          v0 = 0xFF
          v2 = 0
        }
        for(j <- 0 to 127) {
          mem(m) = if (j == 0) ~v0 & 0xFF else v0
          m += 1
        }
        for(_ <- 0 to 127) {
          mem(m) = v2
          m += 1
        }
      }
    }
    def reset : Unit = {}
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (ULTIMAX && chipID == ChipID.CPU) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return lastByteReadMemory.lastByteRead
      }
      mem(address & 0xFFFF)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
      if (ULTIMAX && chipID == ChipID.CPU) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return
      }
      mem(address & 0xFFFF) = value & 0xff
    }
    
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeObject(mem)
      out.writeBoolean(ULTIMAX)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      loadMemory[Int](mem,in)
      ULTIMAX = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }

  /**
   * Wrapper for color ram stored in bank 1 of fastram
   */
  class COLOR_RAM(mem:Array[Int]) extends RAMComponent {
    val componentID = "COLOR RAM"
    val componentType = CBMComponentType.MEMORY

    val isRom = false
    val name = "COLOR_RAM"
    val startAddress = COLOR_RAM
    val length = 1024

    final val isActive = true
    private[this] var lastByteReadMemory : LastByteReadMemory = _

    def init : Unit = {}
    def reset : Unit = {}
    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory): Unit = {
      this.lastByteReadMemory = lastByteReadMemory
    }

    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(0x010000 | address)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(0x010000 | address) = value & 0x0f
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {}
    protected def loadState(in:ObjectInputStream) : Unit = {}
    protected def allowsStateRestoring : Boolean = true
  }

  class SCPU_MMU(cpuFastModeListener : Boolean => Unit = null,simmUsageListener: Float => Unit = null) extends RAMComponent with ExpansionPortConfigurationListener {
    val componentID = "Main RAM"
    val componentType = CBMComponentType.MEMORY
    
    private[this] val ram = new RAM

    val name = "MAIN-RAM"
    val isRom = false
    val startAddress = ram.startAddress
    val length = ram.length
    val CHAR_ROM = new CHARACTERS_ROM(ram)
    val SCPU_ROM = new SCPU_ROM(ram)
    val isActive = true
    
    private[this] val ROML = new ExtendedROM(ram,"ROML",M_ROML)
    private[this] val ROMH = new ExtendedROM(ram,"ROMH",M_BASIC)
    private[this] val ROMH_ULTIMAX = new ExtendedROM(ram,"ROMH_ULTIMAX",M_KERNAL)
    
    private[this] var ULTIMAX = false
    private[this] var ddr = 0
    private[this] var pr = 7
    private[this] var exrom,game = false
    private[this] var lastByteReadMemory : LastByteReadMemory = _
    private[this] var memConfig = -1
    private[this] val MEM_CONFIG = MemConfig.MEM_CONFIG
    private[this] var c64MemConfig : MemConfig = _
    private[this] val expansionPort = ExpansionPort.getExpansionPort

    private[this] var cia1,cia2 : CIA = _
    private[this] var sid : SID = _
    private[this] var vic : VIC = _

    private[this] var clockStretchingRequest : () => Unit = _
    private[this] var cacheWriteWaitListener : Boolean => Unit = _

    // -------------- SCPU staff ---------------------------------------
    private[this] val fastram = Array.ofDim[Int](2 * 65536) // 128K
    private[this] val simmram = Array.ofDim[Int](0xF6 * 65536)
    private[this] val simmuseMap = Array.ofDim[Boolean](0xF6)
    private[this] var simmuseBankCount = 0
    val COLOR_RAM = new COLOR_RAM(fastram)

    private[this] var bootMap = true
    private[this] var switch_1Mhz = false
    private[this] var switch_Jiffy = false
    private[this] var reg_sw_1Mhz = false
    private[this] var reg_sys_1Mhz = false
    private[this] var reg_hw_enabled = false
    private[this] var reg_dosext_enabled = false
    private[this] var reg_ramlink_enabled = false
    private[this] var reg_simm_config = 0
    private[this] var mem_conf_page_size,mem_conf_size,mem_simm_page_size,mem_simm_ram_mask = 0
    private[this] var simm_banks = 0
    private[this] var reg_opt_mode = 0
    private[this] var cpuEmulationMode = false
    private[this] var mirroringMode = 0
    private[this] var zeroPageAndStackMirroring = false
    private[this] var SCPU_ROM_MASK = 0
    private[this] var romlhWriting = false
    private[this] var baLow = false
    private[this] var fastMode = false

    /**
     * MIRRORING mode
     * ====================================
     * 00xxx1xx	VIC bank 0 0000-3FFF
     * 01xxx0xx	VIC bank 1 4000-7FFF
     * 00xxx0xx	VIC bank 2 8000-BFFF
     * 01xxx1xx	VIC bank 3 C000-FFFF
     * 10xxx0xx	BASIC      0400-07FF
     * 11xxx0xx	NO OPT	   0000-FFFF
     * 11xxx1xx	N/A
     * 10xxx1xx	FULL OPT   NO MIRRORING
     * ====================================
     */
    private[this] val MIRROR_AREAS : Array[Array[Int]] = Array(
      Array(0x8000,0xBFFF),    // 00xxx0xx	VIC bank 2 8000-BFFF
      Array(0x0000,0x3FFF),    // 00xxx1xx	VIC bank 0 0000-3FFF
      Array(0x4000,0x7FFF),    // 01xxx0xx	VIC bank 1 4000-7FFF
      Array(0xC000,0xFFFF),    // 01xxx1xx	VIC bank 3 C000-FFFF
      Array(0x0400,0x07FF),    // 10xxx0xx	BASIC      0400-07FF
      Array(0,-1),             // 10xxx1xx	FULL OPT   NO MIRRORING
      Array(0x0000,0xFFFF),    // 11xxx0xx	NO OPT	   0000-FFFF
      Array(0,0)               // 128 only
    )

    final private[this] val MIRROR_VIC_BANK_2 = 0
    final private[this] val MIRROR_VIC_BANK_0 = 1
    final private[this] val MIRROR_VIC_BANK_1 = 2
    final private[this] val MIRROR_VIC_BANK_3 = 3
    final private[this] val MIRROR_BASIC      = 4
    final private[this] val MIRROR_FULL_OPT   = 5
    final private[this] val MIRROR_NO_OPT     = 6

    private[this] var mirroringArea : Array[Int] = MIRROR_AREAS(MIRROR_NO_OPT)
    // -----------------------------------------------------------------

    def setClockStretchingRequestListener(l:() => Unit) : Unit = clockStretchingRequest = l
    def setCacheWriteWaitListener(l: Boolean => Unit) : Unit = cacheWriteWaitListener = l

    private def localCpuFastModeListener(fastMode:Boolean) : Unit = {
      this.fastMode = fastMode
      if (cpuFastModeListener != null) cpuFastModeListener(fastMode)
    }

    def setBALow(baLow:Boolean) : Unit = {
      this.baLow = baLow
      if (!baLow) cacheWriteWaitListener(false)
    }

    def setIO(cia1:CIA,cia2:CIA,sid:SID,vic:VIC): Unit = {
      this.cia1 = cia1
      this.cia2 = cia2
      this.sid = sid
      this.vic = vic
    }
    
    def getRAM : Memory = this

    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory) = {
      this.lastByteReadMemory = lastByteReadMemory
      ram.lastByteReadMemory = lastByteReadMemory
      COLOR_RAM.setLastByteReadMemory(lastByteReadMemory)
    }
    
    override def getProperties = {
      super.getProperties
      properties.setProperty("Mem config",MEM_CONFIG(memConfig).toString)
      properties.setProperty("$0",Integer.toHexString(ddr))
      properties.setProperty("$1",Integer.toHexString(pr))
      properties.setProperty("exrom",exrom.toString)
      properties.setProperty("game",game.toString)
      properties.setProperty("Mirroring range",mirroringArea.mkString("[",",","]"))
      properties.setProperty("reg_sw_1Mhz",reg_sw_1Mhz.toString)
      properties.setProperty("reg_sys_1Mhz",reg_sys_1Mhz.toString)
      properties.setProperty("reg_hw_enabled",reg_hw_enabled.toString)
      properties.setProperty("reg_dosext_enabled",reg_dosext_enabled.toString)
      properties.setProperty("reg_ramlink_enabled",reg_ramlink_enabled.toString)
      properties.setProperty("reg_simm_config",reg_simm_config.toString)
      properties.setProperty("Zero page and Stack mirroring",zeroPageAndStackMirroring.toString)
      properties.setProperty("Fast mode",fastMode.toString)
      properties
    }

    @inline private def check0001 : Unit = {
      val EXROM = expansionPort.EXROM
      val GAME = expansionPort.GAME
      expansionPortConfigurationChanged(GAME,EXROM)
    }
    
    def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) : Unit = {
      this.game = game
      this.exrom = exrom
      
      val newMemConfig = pr | (if (game) 1 << 3 else 0) | (if (exrom) 1 << 4 else 0)
      if (memConfig == newMemConfig) return
      
      memConfig = newMemConfig
      c64MemConfig = MEM_CONFIG(memConfig)
      ULTIMAX = c64MemConfig.romhultimax
      ram.ULTIMAX = ULTIMAX
      Log.debug(s"New memory configuration $c64MemConfig")
    }
    
    def init : Unit = {
      Log.info("Initializing main memory ...")
      add(ram)
      add(CHAR_ROM)
      add(ROML)
      add(ROMH)
      add(ROMH_ULTIMAX)
      add(cia1)
      add(cia2)
      add(sid)
      add(vic)
      add(COLOR_RAM)
      add(SCPU_ROM)

      Log.debug("Initializing static & SIMM ram ...")
      initSIMMMemory(fastram)
      initSIMMMemory(simmram)
      //setSIMMSize(16)
    }

    private def initSIMMMemory(ram:Array[Int]) : Unit = {
      val banks = ram.length / 65536

      var bank = 0x00
      while (bank < banks) {
        var m = 0x0002
        var v = 0
        var n = 0
        while (m < 0x10000) {
          if ((n % 4) == 0) v = ~v & 0xFF
          if (m == 0x4000) v = 0xFF

          ram(bank << 16 | m) = v
          m += 1
          n += 1
        }

        bank += 1
      }
    }
    
    override def afterInitHook : Unit = {
      check0001
      SCPU_ROM_MASK = (0xF7 + SCPU_ROM.getDynamicLength / 65536) << 16 | 0xFFFF
    }
    
    def reset : Unit = {
      Log.info("Resetting main memory...")
      ddr = 0
      pr = 7
      memConfig = -1
      ULTIMAX = false
      ram.ULTIMAX = false
      check0001
      // SCPU
      bootMap = true
      reg_sw_1Mhz = false
      reg_sys_1Mhz = false
      reg_hw_enabled = false
      reg_dosext_enabled = false
      reg_ramlink_enabled = false
      reg_simm_config = 0
      reg_opt_mode = 0
      mirroringMode = 0
      cpuEmulationMode = false
      mirroringArea = MIRROR_AREAS(MIRROR_NO_OPT)
      zeroPageAndStackMirroring = true

      util.Arrays.fill(simmuseMap,false)
      simmuseBankCount = 0
      romlhWriting = false
      baLow = false
    }

    @inline private def readVIC(address:Int) : Int = {
      if (ULTIMAX) {
        val bank = address & 0xF000
        if (bank == 0x3000) return ROMH_ULTIMAX.read(0xF000 + address - 0x3000, ChipID.VIC)
        if (bank == 0x7000) return ROMH_ULTIMAX.read(0xF000 + address - 0x7000, ChipID.VIC)
        if (bank == 0xB000) return ROMH_ULTIMAX.read(0xF000 + address - 0xB000, ChipID.VIC)
        if (bank == 0xF000) return ROMH_ULTIMAX.read(0xF000 + address - 0xF000, ChipID.VIC)
      }
      ram.read(address, ChipID.VIC)
    }

    final def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      val address = _address & 0xFFFFFF

      if (chipID == ChipID.VIC) return readVIC(address)

      val bank = address >> 16

      if (isForwardRead) forwardReadTo.read(address)

      if (bank == 0) readBank0(address)
      else if (bank == 1) fastram(address)
      else if (bank < 0xF8) {
        val offset = address & 0x00FFFF
        var address2 = if (bank == 0xF6) offset else if (bank == 0xF7) 0x010000 | offset else address
        if (mem_simm_page_size != mem_conf_page_size) address2 = ((address2 >> mem_conf_page_size) << mem_simm_page_size) | (address2 & ((1 << mem_simm_page_size) - 1))
        if ((mem_simm_ram_mask & address2) < mem_conf_size) simmram(address2 & mem_simm_ram_mask)
        else bank
      }
      else SCPU_ROM.read(address & SCPU_ROM_MASK)
    }

    /**
     * ===============================================================================================
     *      BANK 1 MAP         ||                           BANK 0 MIRROR
     * ===============================================================================================
     * ADDRESS      REFERENCE     MIRRORED AT           STATE               MIRRORED IF
     * ===============================================================================================
     * $E000-$FFFF  KERNAL        $E000-$FFFF         READ ONLY       CPU H/W REGISTERS DISABLED AND
     *                                                                KERNAL ROM ENABLED
     * $D400-$DFFF    -           NO MIRROR
     * $D200-$D3FF  EXTRA RAM     $D200-$D3FF         READ ALWAYS,    I/O ENABLED
     *                                                WRITE IF CPU HW
     *                                                REG. ENABLED
     * $C000-$D1FF    -           NO MIRROR
     * $A000-$BFFF  BASIC         $A000-$BFFF         READ ONLY       BASIC ROM ENABLED
     * $8000-$9FFF  RLDOS         $8000-$9FFF         READ ONLY       CPU DOS EXTENSIONS ENABLED
     * $6000-$7FFF  ALT.KERNAL    $E000-$FFFF         READ ONLY       CPU H/W REGISTERS ENABLED AND
     *                                                                KERNAL ROM ENABLED
     * $1000-$5FFF  RLDOS         $1000-$5FFF         READ ONLY       CPU DOS EXTENSIONS ENABLED
     * $0000-$0FFF    -           NO MIRROR
     * ===============================================================================================
     */
    @inline private def readBank0(address:Int) : Int = {
      if (address < 0x1000) {
        if (address == 0) ddr
        else if (address == 1) pr
        else fastram(address)
      }
      else if (address < 0x6000) {
        if (reg_dosext_enabled) fastram(0x010000 | address) // bank 1 mirroring of RLDOS
        else fastram(address)
      }
      else if (address < 0x8000) {
        if (reg_hw_enabled && c64MemConfig.kernal) fastram(0x010000 | address) // bank 1 mirroring of ALT.KERNAL
        else fastram(address)
      }
      // from 0x8000 we have to check bootmap mode
      else if (address < 0xA000) { // ROML or RAM
        if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else if (c64MemConfig.roml) { clockStretchingRequest() ; ROML.read(address) }
        else fastram(address)
      }
      else if (address < 0xC000) {
        if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else if (c64MemConfig.basic) fastram(0x010000 | address) // bank 1 mirroring of BASIC
        else if (c64MemConfig.romh) { clockStretchingRequest() ; ROMH.read(address) }
        else fastram(address)
      }
      else if (address < 0xD000) { // RAM
        if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else fastram(address)
      }
      else if (address < 0xE000) { // I/O or RAM or CHAR
        if (c64MemConfig.io) {
          if (address < 0xD200) {
            // not emulated: in this range 2 cycles are needed to access instead of 1
            if ((address >= 0xD071 && address < 0xD080) || (address >= 0xD0B0 && address < 0xD0C0)) readRegisters(address)
            else {
              clockStretchingRequest()
              vic.read(address)
            }
          }
          else if (address < 0xD400) fastram(0x010000 | address) // bank 1 mirroring of EXTRA-RAM I/O
          else if (address < 0xD800) { clockStretchingRequest() ; sid.read(address) }
          else if (address < 0xDC00) { clockStretchingRequest() ; COLOR_RAM.read(address) }
          else if (address < 0xDD00) { clockStretchingRequest(); cia1.read(address) }
          else if (address < 0xDE00) { clockStretchingRequest() ; cia2.read(address) }
          else {
            clockStretchingRequest()
            expansionPort.read(address)
          }
        }
        else if (c64MemConfig.char) { clockStretchingRequest() ; CHAR_ROM.read(address) }
        else if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else fastram(address)
      }
      else if (bootMap) SCPU_ROM.read(0xF80000 | address)
      else if (c64MemConfig.kernal && !reg_hw_enabled) fastram(0x010000 | address) // bank 1 mirroring of KERNAL
      else if (c64MemConfig.kernal && reg_hw_enabled) fastram(0x016000 + (address - 0xE000)) // bank 1 mirroring of ALT. KERNAL
      else if (c64MemConfig.romh) { clockStretchingRequest() ; ROMH.read(address) }
      else fastram(address)
    }

    final def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit  = {
      val address = _address & 0xFFFFFF
      val bank = address >> 16

      if (isForwardWrite) forwardWriteTo.write(address,value)

      if (bank == 0) writeBank0(address,value)
      else if (bank == 1) fastram(address) = value
      else if (bank < 0xF8) {
        val offset = address & 0x00FFFF
        var address2 = if (bank == 0xF6) offset else if (bank == 0xF7) 0x010000 | offset else address
        if (mem_simm_page_size != mem_conf_page_size) address2 = ((address2 >> mem_conf_page_size) << mem_simm_page_size) | (address2 & ((1 << mem_simm_page_size) - 1))
        if ((mem_simm_ram_mask & address2) < mem_conf_size) {
          simmram(address2 & mem_simm_ram_mask) = value
          val bank2 = (address2 & mem_simm_ram_mask) >> 16
          if (!simmuseMap(bank2)) {
            simmuseMap(bank2) = true
            simmuseBankCount += 1
            if (simmUsageListener != null) simmUsageListener(simmuseBankCount.toFloat / simm_banks)
          }
        }
      }
    }

    @inline private def writeBank0(address:Int,value:Int) : Unit = {
      if (address < 0x1000) {
        if (address == 0) {
          ddr = value
          //check0001
        }
        else if (address == 1) {
          pr = value & 0x07
          check0001
          clockStretchingRequest()
        }
        fastram(address) = value
        if (address < 0x200) {
          if (zeroPageAndStackMirroring) writeMirror(address,value)
        }
        else writeMirror(address,value)
      }
      else if (address < 0x8000) {
        fastram(address) = value
        writeMirror(address,value)
      }
      else if (address < 0xA000) { // ROML or RAM
        // bootmap ignored ??
        if (c64MemConfig.roml && !romlhWriting) {
          romlhWriting = true
          ROML.write(address, value)
          romlhWriting = false
        }
        else {
          fastram(address) = value
          writeMirror(address,value)
        }
      }
      else if (address < 0xC000) { // BASIC or RAM or ROMH
        if (c64MemConfig.romh && !romlhWriting) {
          romlhWriting = true
          ROMH.write(address,value)
          romlhWriting = false
        }
        else {
          fastram(address) = value
          writeMirror(address,value)
        }
      }
      else if (address < 0xD000) {
        fastram(address) = value
        writeMirror(address,value)
      } // RAM
      else if (address < 0xE000) { // I/O or RAM or CHAR
        if (c64MemConfig.io) {
          if (address < 0xD200) {
            // not emulated: in this range 2 cycles are needed to access instead of 1
            if ((address >= 0xD071 && address < 0xD080) || (address >= 0xD0B0 && address < 0xD0C0)) writeRegisters(address,value)
            else { clockStretchingRequest() ; vic.write(address,value) }
          }
          else if (address < 0xD400 && (reg_hw_enabled || address == 0xD27E)) fastram(0x010000 | address) = value // bank 1 mirroring of EXTRA-RAM I/O
          else if (address < 0xD800) { clockStretchingRequest() ; sid.write(address,value) }
          else if (address < 0xDC00) {
            COLOR_RAM.write(address,value)
            clockStretchingRequest()
          }
          else if (address < 0xDD00) { clockStretchingRequest(); cia1.write(address,value) }
          else if (address < 0xDE00) { clockStretchingRequest(); cia2.write(address,value) }
          else {
            expansionPort.write(address,value)
            clockStretchingRequest()
          }
        }
        else {
          fastram(address) = value
          writeMirror(address,value)
        }
      }
      else if (c64MemConfig.romh && !romlhWriting) {
        romlhWriting = true
        ROMH.write(address,value)
        romlhWriting = false
      }
      else if (!bootMap) {
        fastram(address) = value
        writeMirror(address,value)
      }

      // TestCart
      if (TestCart.enabled) TestCart.write(address,value)
    }

    @inline private def writeMirror(address:Int,value:Int) : Unit = {
      if (address >= mirroringArea(0) && address <= mirroringArea(1)) {
        // check BA
        if (baLow && !fastMode && !cpuEmulationMode) cacheWriteWaitListener(true) // not clear how the BA signal is managed
        // mirroring handling: cache full not emulated
        ram.write(address,value)
      }
    }

    @inline private def readRegisters(address:Int) : Int = {
      val value = address match {
        case 0xD0B0 => 0x40 // SCPU v2, 64 mode
        case 0xD0B1 => 0x00
        case 0xD0B2 => // bit 7: hwreg enabled, bit 6: system 1Mhz enabled
          (if (reg_hw_enabled) 0x80 else 0x00) | (if (reg_sys_1Mhz) 0x40 else 0x00)
        case 0xD0B3|
             0xD0B4 => reg_opt_mode & 0xC0 // TODO to be checked ??
        case 0xD0B5 => // bit 7: Jiffy DOS physical switch, bit 6: 1Mhz physical switch
          (if (switch_Jiffy) 0x80 else 0x00) | (if (switch_1Mhz) 0x40 else 0x00)
        case 0xD0B6 => // bit 7: emulation mode
          if (cpuEmulationMode) 0x80 else 0x00
        case 0xD0B7 => 0x00
        case 0xD0B8|
             0xD0B9 => // bit 7: software 1Mhz enabled, bit 6: master speed (switch+sw+system)
          val masterSpeed = reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled) || reg_sys_1Mhz
          (if (reg_sw_1Mhz) 0x80 else 0x00) | (if (masterSpeed) 0x40 else 0x00)
        case 0xD0BA|
             0xD0BB => 0x00
        case 0xD0BC|
             0xD0BD|
             0xD0BE|
             0xD0BF => // bit 7: DOS ext mode enabled, bit 6: ramlink enabled
          (if (reg_dosext_enabled) 0x80 else 0x00) | (if (reg_ramlink_enabled) 0x40 else 0x00)
        case _ => 0xFF
      }
      Log.debug(s"Reading SCPU REG ${address.toHexString}: ${value.toHexString}")
      value
    }
    @inline private def writeRegisters(address:Int,value:Int) : Unit = {
      Log.debug(s"Writing SCPU REG ${address.toHexString} <- ${value.toHexString}")
      address match {
        case 0xD071 => // do nothing
        case 0xD072 => // System 1Mhz enable
          if (!reg_sys_1Mhz) {
            reg_sys_1Mhz = true
            localCpuFastModeListener(false)
            Log.debug("Fast mode off")
          }
        case 0xD073 => // System 1Mhz disable
          if (reg_sys_1Mhz) {
            reg_sys_1Mhz = false
            localCpuFastModeListener(!(reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
            Log.debug("Slow mode off")
          }
        case 0xD074 => // set optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = 0x00 // VIC Bank 2
            updateMirroringMode
          }
        case 0xD075 => // set optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = 0x40 // VIC Bank 1
            updateMirroringMode
          }
        case 0xD076 => // set optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = 0x80 // BASIC
            updateMirroringMode
          }
        case 0xD077 => // set optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = 0xC0 // No optimization
            updateMirroringMode
          }
        case 0xD078 => // SIMM configuration
          if (reg_hw_enabled && reg_simm_config != value) {
            reg_simm_config = value
            updateSIMMConfig
          }
        case 0xD07A => // software 1Mhz enable
          if (!reg_sw_1Mhz) {
            reg_sw_1Mhz = true
            localCpuFastModeListener(false)
            Log.debug("Fast mode off")
          }
        case 0xD079|
             0xD07B => // software 1Mhz disable
          if (reg_sw_1Mhz) {
            reg_sw_1Mhz = false
            localCpuFastModeListener(!(reg_sys_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
            Log.debug("Slow mode off")
          }
        case 0xD07C => // do nothing
        case 0xD07E => // hw register enable
          if (!reg_hw_enabled) {
            reg_hw_enabled = true
            localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz))
            Log.debug("Hw reg enabled")
          }
        case 0xD07D|
             0xD07F => // hw register disable
          if (reg_hw_enabled) {
            reg_hw_enabled = false
            Log.debug("Hw reg disabled")
            localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || switch_1Mhz))
          }
        case 0xD0B0|
             0xD0B1 => // do nothing
        case 0xD0B2 => // bit 7: hw enable, bit 6: system 1Mhz
          if (reg_hw_enabled) {
            reg_sys_1Mhz = (value & 0x40) > 0
            if ((value & 0x80) == 0) {
              reg_hw_enabled = false
              Log.debug("Hw reg disabled")
            }
            localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
          }
        case 0xD0B3 => // optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = (reg_opt_mode & 0x38) | (value & 0xC7)
            updateMirroringMode
          }
        case 0xD0B4 => // optimization mode
          if (reg_hw_enabled) {
            reg_opt_mode = (reg_opt_mode & 0x3F) | (value & 0xC0)
            updateMirroringMode
          }
        case 0xD0B5 => // do nothing
        case 0xD0B6 => // disable bootmap
          if (reg_hw_enabled && bootMap) {
            bootMap = false
            Log.debug("Bootmap mode disabled")
          }
        case 0xD0B7 => // enable bootmap
          if (reg_hw_enabled && !bootMap) {
            bootMap = true
            Log.debug("Bootmap mode enabled")
          }
        case 0xD0B8 => // bit 7: software 1Mhz
          if (reg_hw_enabled) {
            reg_sw_1Mhz = (value & 0x80) > 0
            localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
          }
        case 0xD0B9|
             0xD0BA|
             0xD0BB => // do nothing
        case 0xD0BC => // bit 7: dos extension
          val newDosExt = (value & 0x80) > 0
          if (reg_hw_enabled && reg_dosext_enabled != newDosExt) {
            reg_dosext_enabled = newDosExt
            Log.debug(s"DOS ext ${if (newDosExt) "enabled" else "disabled"}")
          }
        case 0xD0BE => // dos extension enable
          if (reg_hw_enabled && !reg_dosext_enabled) {
            reg_dosext_enabled = true
            Log.debug("DOS ext enabled")
          }
        case 0xD0BD|
             0xD0BF => // dos extension disable
          if (reg_hw_enabled && reg_dosext_enabled) {
            reg_dosext_enabled = false
            Log.debug("DOS ext disabled")
          }
        case _ =>
      }
    }

    /**
     * ================================================
     * VECTOR ADDRESS	          CONDITION
     * ================================================
     * $00FFE4 - $00FFFF	  KERNAL ROM SWITCHED OUT
     * CPU ROM	            65816 IN NATIVE MODE
     *                      CPU H/W REGISTERS ENABLED
     *                      RAMLINK H/W ENABLED
     *                      CPU DOS EXTENSIONS ENABLED
     *                      SYSTEM 1 MHZ MODE ENABLED
     * $017FE4 - $017FFF	  ALL OTHER CONDITIONS
     * ================================================
     **/
    final def interruptVectorMapper(address:Int) : Int = {
      if ((c64MemConfig.kernal) &&
        (!cpuEmulationMode ||
          reg_hw_enabled ||
          reg_ramlink_enabled ||
          reg_dosext_enabled ||
          reg_sys_1Mhz))
        (address & 0x00FFFF) | 0xF80000
      else address
    }

    final def cpuOnNativeMode(native:Boolean) : Unit = {
      cpuEmulationMode = !native
    }

    def setSIMMSize(mb:Int) : Unit = {
      val size = mb << 20
      mem_simm_ram_mask = if (size == 0) 0 else size - 1
      mb match {
        case 1 =>
          mem_simm_page_size = 9 + 2
          simm_banks = 16
        case 4 =>
          mem_simm_page_size = 10 + 2
          simm_banks = 16 * 4
        case 8 =>
          mem_simm_page_size = 10 + 2
          simm_banks = 16 * 8
        case _ =>
          mem_simm_page_size = 11 + 2
          simm_banks = 0xF6
      }
      mem_conf_page_size = mem_simm_page_size
      mem_conf_size = mb * 1024 * 1024

      Log.debug(s"Set ${mb}M of memory. mem_simm_ram_mask=${mem_simm_ram_mask.toHexString} mem_simm_page_size=$mem_simm_page_size")
    }

    private def updateSIMMConfig : Unit = {
      reg_simm_config match {
        case 0 =>
          mem_conf_page_size = 9 + 2
          mem_conf_size = 1 * 1024 * 1024
        case 1 =>
          mem_conf_page_size = 10 + 2
          mem_conf_size = 4 * 1024 * 1024
        case 2 =>
          mem_conf_page_size = 10 + 2
          mem_conf_size = 8 * 1024 * 1024
        case 3 =>
          mem_conf_page_size = 10 + 2
          mem_conf_size = 16 * 1024 * 1024
        case _ =>
          mem_conf_page_size = 11 + 2
          mem_conf_size = 16 * 1024 * 1024
      }
      Log.debug(s"Updating SIMM configuration: $reg_simm_config mem_conf_page_size=$mem_conf_page_size mem_conf_size=${mem_conf_size.toHexString}")
    }

    private def updateMirroringMode : Unit = {
      mirroringMode = (reg_opt_mode & 0xC0) >> 5 | (reg_opt_mode & 0x04) >> 2
      zeroPageAndStackMirroring = (reg_opt_mode & 1) > 0
      mirroringArea = MIRROR_AREAS(mirroringMode)
      Log.debug(s"Updating mirroring mode to: reg_opt_mode=$reg_opt_mode mirroring of ${MIRROR_AREAS(mirroringMode).mkString("(",",",")")} zeroPageAndStackMirroring=$zeroPageAndStackMirroring")
    }

    def setJiffyDOS(enabled:Boolean) : Unit = switch_Jiffy = enabled
    def isJiffyDOSEnabled : Boolean = switch_Jiffy
    def setSystem1Mhz(enabled:Boolean) : Unit = {
      switch_1Mhz = enabled
      localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
    }

    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeBoolean(bootMap)
      out.writeBoolean(ULTIMAX)
      out.writeInt(ddr)
      out.writeInt(pr)
      out.writeBoolean(exrom)
      out.writeBoolean(game)
      out.writeInt(memConfig)
      out.writeObject(fastram)
      out.writeObject(simmram)
      out.writeObject(simmuseMap)
      out.writeInt(simmuseBankCount)
      out.writeBoolean(reg_sw_1Mhz)
      out.writeBoolean(reg_sys_1Mhz)
      out.writeBoolean(reg_hw_enabled)
      out.writeBoolean(reg_dosext_enabled)
      out.writeBoolean(reg_ramlink_enabled)
      out.writeInt(reg_simm_config)
      out.writeInt(mem_conf_page_size)
      out.writeInt(mem_conf_size)
      out.writeInt(mem_simm_page_size)
      out.writeInt(mem_simm_ram_mask)
      out.writeInt(simm_banks)
      out.writeInt(reg_opt_mode)
      out.writeBoolean(cpuEmulationMode)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      bootMap = in.readBoolean
      ULTIMAX = in.readBoolean
      ddr = in.readInt
      pr = in.readInt
      exrom = in.readBoolean
      game = in.readBoolean
      memConfig = in.readInt
      c64MemConfig = MEM_CONFIG(memConfig)
      check0001
      loadMemory[Int](fastram,in)
      loadMemory[Int](simmram,in)
      loadMemory[Boolean](simmuseMap,in)
      simmuseBankCount = in.readInt
      if (simmUsageListener != null) simmUsageListener(simmuseBankCount.toFloat / simm_banks)
      reg_sw_1Mhz = in.readBoolean
      reg_sys_1Mhz = in.readBoolean
      reg_hw_enabled = in.readBoolean
      reg_dosext_enabled = in.readBoolean
      reg_ramlink_enabled = in.readBoolean
      reg_simm_config = in.readInt
      mem_conf_page_size = in.readInt
      mem_conf_size = in.readInt
      mem_simm_page_size = in.readInt
      mem_simm_ram_mask = in.readInt
      simm_banks = in.readInt
      reg_opt_mode = in.readInt
      cpuEmulationMode = in.readBoolean
      updateMirroringMode
      localCpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
    }

    protected def allowsStateRestoring : Boolean = true
  }
}