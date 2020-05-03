package ucesoft.cbm.scpu

import java.io.{ObjectInputStream, ObjectOutputStream}
import ucesoft.cbm.cpu._
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortConfigurationListener, LastByteReadMemory}
import ucesoft.cbm.misc.TestCart
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.{CBMComponentType, ChipID, Clock, Log}
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
    def init {
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
    def reset {}
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (ULTIMAX && chipID == ChipID.CPU) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return lastByteReadMemory.lastByteRead
      }
      mem(address & 0xFFFF)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (ULTIMAX && chipID == ChipID.CPU) {
        if ((address >= 0x1000 && address < 0x8000) || (address >= 0xA000 && address < 0xD000)) return
      }
      mem(address & 0xFFFF) = value & 0xff
    }
    
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeObject(mem)
      out.writeBoolean(ULTIMAX)
    }
    protected def loadState(in:ObjectInputStream) {
      loadMemory[Int](mem,in)
      ULTIMAX = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }

  class COLOR_RAM extends RAMComponent {
    val componentID = "COLOR RAM"
    val componentType = CBMComponentType.MEMORY

    val isRom = false
    val name = "COLOR_RAM"
    val startAddress = COLOR_RAM
    val length = 1024

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    private[this] var lastByteReadMemory : LastByteReadMemory = _

    def init {}
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0xFF
    }
    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory): Unit = {
      this.lastByteReadMemory = lastByteReadMemory
    }

    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = (lastByteReadMemory.lastByteRead & 0xF0) | (mem(address & 0x3FF) & 0x0F)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address & 0x3FF) = value & 0xff
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeObject(mem)
    }
    protected def loadState(in:ObjectInputStream) {
      loadMemory[Int](mem,in)
    }
    protected def allowsStateRestoring : Boolean = true
  }

  class SCPU_MMU(cpuFastModeListener : Boolean => Unit = null) extends RAMComponent with ExpansionPortConfigurationListener {
    val componentID = "Main RAM"
    val componentType = CBMComponentType.MEMORY
    
    private[this] val ram = new RAM

    val name = "MAIN-RAM"
    val isRom = false
    val startAddress = ram.startAddress
    val length = ram.length
    val CHAR_ROM = new CHARACTERS_ROM(ram)
    val COLOR_RAM = new COLOR_RAM
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

    // -------------- SCPU staff ---------------------------------------
    private[this] val fastram = Array.ofDim[Int](0xF8 * 65536) // about 16M

    private[this] var bootMap = true
    private[this] var switch_1Mhz = false
    private[this] var switch_Jiffy = false
    private[this] var reg_sw_1Mhz = false
    private[this] var reg_sys_1Mhz = false
    private[this] var reg_hw_enabled = false
    private[this] var reg_dosext_enabled = false
    private[this] var reg_ramlink_enabled = false
    private[this] var reg_simm_config = 0
    private[this] var reg_opt_mode = 0
    private[this] var cpuEmulationMode = false
    private[this] var mirroringMode = 0

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

    def setIO(cia1:CIA,cia2:CIA,sid:SID,vic:VIC): Unit = {
      this.cia1 = cia1
      this.cia2 = cia2
      this.sid = sid
      this.vic = vic
    }
    
    def getRAM : Memory = ram

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
      properties
    }

    @inline private def check0001 {
      val EXROM = expansionPort.EXROM
      val GAME = expansionPort.GAME
      expansionPortConfigurationChanged(GAME,EXROM)
    }
    
    def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
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
    
    def init {
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
    }
    
    override def afterInitHook {
      check0001
    }
    
    def reset {
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

    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (chipID == ChipID.VIC) return readVIC(address)

      val bank = address >> 16

      if (isForwardRead) forwardReadTo.read(address)

      if (bank == 0) readBank0(address)
      else if (bank < 0xF8) fastram(address)
      else if (bank < 0xF9) SCPU_ROM.read(address)
      else {
        println(s"Reading from ${address.toHexString}")
        0xFF
      }
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
        else if (c64MemConfig.roml) ROML.read(address)
        else fastram(address)
      }
      else if (address < 0xC000) {
        if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else if (c64MemConfig.basic) fastram(0x010000 | address) // bank 1 mirroring of BASIC
        else if (c64MemConfig.roml) ROML.read(address)
        else fastram(address)
      }
      else if (address < 0xD000) { // RAM
        if (bootMap) SCPU_ROM.read(0xF80000 | address)
        else fastram(address)
      }
      else if (address < 0xE000) { // I/O or RAM or CHAR
        if (c64MemConfig.io) {
          if (address < 0xD200) {
            if ((address >= 0xD071 && address < 0xD080) || (address >= 0xD0B0 && address < 0xD0C0)) readRegisters(address)
            else vic.read(address)
          }
          else if (address < 0xD400) fastram(0x010000 | address) // bank 1 mirroring of EXTRA-RAM I/O
          else if (address < 0xD800) sid.read(address)
          else if (address < 0xDC00) COLOR_RAM.read(address)
          else if (address < 0xDD00) cia1.read(address)
          else if (address < 0xDE00) cia2.read(address)
          else expansionPort.read(address)
        }
        else if (c64MemConfig.char) CHAR_ROM.read(address)
        else if (bootMap) SCPU_ROM.read(0xF80000 | address) // TODO to be checked
        else fastram(address)
      }
      else if (bootMap) SCPU_ROM.read(0xF80000 | address)
      else if (c64MemConfig.kernal && !reg_hw_enabled) fastram(0x010000 | address) // bank 1 mirroring of KERNAL
      else if (c64MemConfig.kernal && reg_hw_enabled) fastram(0x016000 + (address - 0xE000)) // bank 1 mirroring of ALT. KERNAL
      else if (c64MemConfig.romh) ROMH.read(address)
      else fastram(address)
    }

    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit  = {
      val bank = address >> 16

      if (isForwardWrite) forwardWriteTo.write(address,value)

      if (bank == 0) writeBank0(address,value)
      else if (bank == 1) {
        if (address < 0x011000) fastram(address) = value
        if (address < 0x01C000) {
          /*if (bootMap)*/ fastram(address) = value
        }
        if (address < 0x01E000) fastram(address) = value
        else /*if (bootMap)*/ fastram(address) = value
      }
      else if (bank < 0xF8) fastram(address) = value

      // mirroring handling
      if (address >= mirroringArea(0) && address <= mirroringArea(1)) ram.write(address,value)
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
        }
        fastram(address) = value
      }
      else if (address < 0x6000) {
        // ?? don't know if with dosext enabled the writes go to ram
        if (!reg_dosext_enabled) fastram(address) = value
      }
      else if (address < 0x8000) {
        // ?? don't know if with alt. kernal enabled the writes go to ram
        if (!(reg_hw_enabled && c64MemConfig.kernal)) fastram(address) = value
      }
      if (address < 0xA000) { // ROML or RAM
        // bootmap ignored ??
        if (c64MemConfig.roml) ROML.write(address, value)
        else fastram(address) = value
      }
      else if (address < 0xC000) { // BASIC or RAM or ROMH
        if (c64MemConfig.romh) ROMH.write(address,value)
        else fastram(address) = value
      }
      else if (address < 0xD000) fastram(address) = value // RAM
      else if (address < 0xE000) { // I/O or RAM or CHAR
        if (c64MemConfig.io) {
          if (address < 0xD200) {
            if ((address >= 0xD071 && address < 0xD080) || (address >= 0xD0B0 && address < 0xD0C0)) writeRegisters(address,value)
            else vic.write(address,value)
          }
          else if (reg_hw_enabled && address < 0xD400) fastram(0x010000 | address) = value // bank 1 mirroring of EXTRA-RAM I/O
          else if (address < 0xD800) sid.write(address,value)
          else if (address < 0xDC00) {
            COLOR_RAM.write(address,value)
            fastram(0x010000 | address) = value // color ram mirroring ??
          }
          else if (address < 0xDD00) cia1.write(address,value)
          else if (address < 0xDE00) cia2.write(address,value)
          else expansionPort.write(address,value)
        }
        else fastram(address) = value
      }
      else if (c64MemConfig.romh) ROMH.write(address,value)
      else if (!bootMap) fastram(address) = value
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
            if (cpuFastModeListener != null) cpuFastModeListener(false)
            Log.debug("Fast mode off")
          }
        case 0xD073 => // System 1Mhz disable
          if (reg_sys_1Mhz) {
            reg_sys_1Mhz = false
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
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
            if (cpuFastModeListener != null) cpuFastModeListener(false)
            Log.debug("Fast mode off")
          }
        case 0xD079|
             0xD07B => // software 1Mhz disable
          if (reg_sw_1Mhz) {
            reg_sw_1Mhz = false
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sys_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
            Log.debug("Slow mode off")
          }
        case 0xD07C => // do nothing
        case 0xD07E => // hw register enable
          if (!reg_hw_enabled) {
            reg_hw_enabled = true
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz))
            Log.debug("Hw reg enabled")
            plaConfigChanged
          }
        case 0xD07D|
             0xD07F => // hw register disable
          if (reg_hw_enabled) {
            reg_hw_enabled = false
            Log.debug("Hw reg disabled")
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || switch_1Mhz))
            plaConfigChanged
          }
        case 0xD0B0|
             0xD0B1 => // do nothing
        case 0xD0B2 => // bit 7: hw enable, bit 6: system 1Mhz
          if (reg_hw_enabled) {
            reg_sys_1Mhz = (value & 0x40) > 0
            if ((value & 0x80) == 0) {
              reg_hw_enabled = false
              Log.debug("Hw reg disabled")
              plaConfigChanged
            }
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
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
            // plaConfigChanged ??
          }
        case 0xD0B7 => // enable bootmap
          if (reg_hw_enabled && !bootMap) {
            bootMap = true
            Log.debug("Bootmap mode enabled")
            // plaConfigChanged ??
          }
        case 0xD0B8 => // bit 7: software 1Mhz
          if (reg_hw_enabled) {
            reg_sw_1Mhz = (value & 0x80) > 0
            if (cpuFastModeListener != null)
              cpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
          }
        case 0xD0B9|
             0xD0BA|
             0xD0BB => // do nothing
        case 0xD0BC => // bit 7: dos extension
          val newDosExt = (value & 0x80) > 0
          if (reg_hw_enabled && reg_dosext_enabled != newDosExt) {
            reg_dosext_enabled = newDosExt
            Log.debug(s"DOS ext ${if (newDosExt) "enabled" else "disabled"}")
            plaConfigChanged
          }
        case 0xD0BE => // dos extension enable
          if (reg_hw_enabled && !reg_dosext_enabled) {
            reg_dosext_enabled = true
            Log.debug("DOS ext enabled")
            plaConfigChanged
          }
        case 0xD0BD|
             0xD0BF => // dos extension disable
          if (reg_hw_enabled && reg_dosext_enabled) {
            reg_dosext_enabled = false
            Log.debug("DOS ext disabled")
            plaConfigChanged
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

    private def plaConfigChanged : Unit = {
      Log.debug("Changing PLA config ...")
    }

    private def updateSIMMConfig : Unit = {
      println(s"Updating SIMM configuration: $reg_simm_config")
      Log.debug(s"Updating SIMM configuration: $reg_simm_config")
    }

    private def updateMirroringMode : Unit = {
      mirroringMode = (reg_opt_mode & 0xC0) >> 5 | (reg_opt_mode & 0x04) >> 2
      mirroringArea = MIRROR_AREAS(mirroringMode)
      Log.debug(s"Updating mirroring mode to: reg_opt_mode=$reg_opt_mode mirroring of ${MIRROR_AREAS(mirroringMode).mkString("(",",",")")}")
    }

    def setJiffyDOS(enabled:Boolean) : Unit = switch_Jiffy = enabled
    def setSystem1Mhz(enabled:Boolean) : Unit = {
      switch_1Mhz = enabled
      cpuFastModeListener(!(reg_sys_1Mhz || reg_sw_1Mhz || (switch_1Mhz && !reg_hw_enabled)))
    }

    override def toString = ram.toString
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(ULTIMAX)
      out.writeInt(ddr)
      out.writeInt(pr)
      out.writeBoolean(exrom)
      out.writeBoolean(game)
      out.writeInt(memConfig)
    }
    protected def loadState(in:ObjectInputStream) {
      ULTIMAX = in.readBoolean
      ddr = in.readInt
      pr = in.readInt
      exrom = in.readBoolean
      game = in.readBoolean
      memConfig = in.readInt
      check0001
    }
    protected def allowsStateRestoring : Boolean = true
  }
}