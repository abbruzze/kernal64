package ucesoft.cbm.c64

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu._
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortConfigurationListener, LastByteReadMemory}
import ucesoft.cbm.misc.TestCart
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.{CBMComponentType, ChipID, Clock, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

object C64MMU {
  import ROM._
  final val M_ROML = 0x8000
  final val M_BASIC = 0xA000
  final val M_KERNAL = 0xE000
  final val M_CHARACTERS, M_IO = 0xD000
  final val COLOR_RAM = 0xD800
  final val SID_RAM = 0xD400
    
  // --------------------------------------
  class BASIC_ROM(ram: Memory) extends ROM(ram, "BASIC", M_BASIC, 8192, C64_BASIC_ROM_PROP)

  class KERNAL_ROM(ram: Memory) extends ROM(ram, "KERNAL", M_KERNAL, 8192,C64_KERNAL_ROM_PROP) {
    override def getProperties: Properties = {
      super.getProperties
      properties.setProperty("Version",read(0xFF80).toString)
      properties
    }
  }
  
  class CHARACTERS_ROM(ram: Memory) extends ROM(ram, "CHARACTERS", M_CHARACTERS, 4096, C64_CHAR_ROM_PROP)

  class RAM extends RAMComponent {
    val componentID = "RAM"
    val componentType: Type = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "RAM"
    val startAddress = 0x0
    val length = 0x10000

    private[this] val mem = Array.fill(length)(0xFF)
    private[C64MMU] var ULTIMAX = false
    private[C64MMU] var lastByteReadMemory : LastByteReadMemory = _
    
    final val isActive = true
    def init  : Unit = {
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
    def reset  : Unit = {}
    override def hardReset : Unit = init
    
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
  
  class COLOR_RAM extends RAMComponent {
    val componentID = "COLOR RAM"
    val componentType: Type = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "COLOR_RAM"
    val startAddress: Int = COLOR_RAM
    val length = 1024

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    private[this] var lastByteReadMemory : LastByteReadMemory = _

    def init  : Unit = {}
    def reset  : Unit = {
      for(i <- 0 until mem.length) mem(i) = 0xFF
    }
    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory): Unit = {
      this.lastByteReadMemory = lastByteReadMemory
    }
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = (lastByteReadMemory.lastByteRead & 0xF0) | (mem(address & 0x3FF) & 0x0F)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = mem(address & 0x3FF) = value & 0xff
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeObject(mem)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      loadMemory[Int](mem,in)
    }
    protected def allowsStateRestoring : Boolean = true
  }

  class MAIN_MEMORY extends RAMComponent with ExpansionPortConfigurationListener {
    val componentID = "Main RAM"
    val componentType: Type = CBMComponentType.MEMORY
    
    private[this] val ram = new RAM
    val name = "MAIN-RAM"
    val isRom = false
    val startAddress: Int = ram.startAddress
    val length: Int = ram.length
    val CHAR_ROM = new CHARACTERS_ROM(ram)
    val COLOR_RAM = new COLOR_RAM
    val isActive = true
    
    private[this] val BASIC_ROM = new BASIC_ROM(ram)
    private[this] val KERNAL_ROM = new KERNAL_ROM(ram)
    private[this] val ROML = new ExtendedROM(ram,"ROML",M_ROML)
    private[this] val ROMH = new ExtendedROM(ram,"ROMH",M_BASIC)
    private[this] val ROMH_ULTIMAX = new ExtendedROM(ram,"ROMH_ULTIMAX",M_KERNAL)
    
    private[this] var ULTIMAX = false
    private[this] var ddr = 0
    private[this] var data,data_out = 0x3F
    private[this] var exrom,game = false
    private[this] var data_falloff_bit6, data_falloff_bit7 = false
    private[this] var data_set_clk_bit6,data_set_clk_bit7 = 0L
    private[this] var data_set_bit6, data_set_bit7 = 0
    private[this] val CAPACITOR_FADE_CYCLES = 350000
    private[this] var datassette : Datassette = _
    private[this] var lastByteReadMemory : LastByteReadMemory = _
    private[this] var memConfig = -1
    private[this] val MEM_CONFIG = MemConfig.MEM_CONFIG
    private[this] val expansionPort = ExpansionPort.getExpansionPort

    private[this] var cia1,cia2 : CIA = _
    private[this] var sid : SID = _
    private[this] var vic : VIC = _

    def setIO(cia1:CIA,cia2:CIA,sid:SID,vic:VIC): Unit = {
      this.cia1 = cia1
      this.cia2 = cia2
      this.sid = sid
      this.vic = vic
    }
    
    def getRAM : Memory = ram
    
    def setDatassette(datassette:Datassette): Unit = this.datassette = datassette
    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory): Unit = {
      this.lastByteReadMemory = lastByteReadMemory
      ram.lastByteReadMemory = lastByteReadMemory
      COLOR_RAM.setLastByteReadMemory(lastByteReadMemory)
    }
    
    override def getProperties: Properties = {
      super.getProperties
      properties.setProperty("Mem config",MEM_CONFIG(memConfig).toString)
      properties.setProperty("$0",Integer.toHexString(ddr))
      properties.setProperty("$1",Integer.toHexString(read0001))
      properties.setProperty("exrom",exrom.toString)
      properties.setProperty("game",game.toString)
      properties.setProperty("capacitor 6",data_set_clk_bit6.toString)
      properties.setProperty("capacitor 7",data_set_clk_bit7.toString)
      properties
    }
    
    @inline private def read0001 = {
      data_out = (data_out & ~ddr) | (data & ddr)
      var data_read = (data | ~ddr) & (data_out | 0x7)
      if ((ddr & 0x20) == 0) data_read &= 0xDF
      data_read &= 0xef

      val playSense = if ((ddr & 0x10) > 0) data & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
      data_read |= playSense

      val clk = Clock.systemClock.currentCycles
      if (data_falloff_bit6 && (data_set_clk_bit6 < clk)) {
        data_falloff_bit6 = false;
        data_set_bit6 = 0;
      }
      if (data_falloff_bit7 && (data_set_clk_bit7 < clk)) {
        data_falloff_bit7 = false;
        data_set_bit7 = 0;
      }

      if ((ddr & 0x40) == 0) {
        data_read &= ~0x40
        data_read |= data_set_bit6
      }
      if ((ddr & 0x80) == 0) {
        data_read &= ~0x80
        data_read |= data_set_bit7
      }

      data_read
    }

    @inline private def check0001()  : Unit = {
      val pr = read0001
      // check tape motor
      datassette.setMotor((ddr & 0x20) > 0 && (pr & 0x20) == 0)
      datassette.setWriteLine((ddr & 0x08) > 0 && (pr & 0x08) > 0)
      val EXROM = expansionPort.EXROM
      val GAME = expansionPort.GAME
      expansionPortConfigurationChanged(GAME,EXROM)
    }
    
    def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) : Unit = {
      this.game = game
      this.exrom = exrom
      
      val newMemConfig = (data | ~ddr) & 0x7 | (if (game) 1 << 3 else 0) | (if (exrom) 1 << 4 else 0)
      if (memConfig == newMemConfig) return
      
      memConfig = newMemConfig
      val mc = MEM_CONFIG(memConfig)
      ULTIMAX = mc.romhultimax
      ram.ULTIMAX = ULTIMAX
    }
    
    def init  : Unit = {
      Log.info("Initializing main memory ...")
      
      add(ram)
      add(BASIC_ROM)
      add(KERNAL_ROM)      
      add(CHAR_ROM)
      add(ROML)
      add(ROMH)
      add(ROMH_ULTIMAX)
      add(cia1)
      add(cia2)
      add(sid)
      add(vic)
      add(COLOR_RAM)
    }
    
    override def afterInitHook  : Unit = {
      check0001
    }
    
    def reset  : Unit = {
      Log.info("Resetting main memory...")
      ddr = 0
      data = 0
      data_out = 0
      data_set_bit6 = 0
      data_set_bit7 = 0
      data_falloff_bit6 = false
      data_falloff_bit7 = false
      memConfig = -1
      ULTIMAX = false
      ram.ULTIMAX = false
      check0001
    }

    final def read(_address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      val address = _address & 0xFFFF
      if (isForwardRead) {
        val read = forwardReadTo.read(address)
        if (read >= 0) return read
      }
      if (ULTIMAX) {
        if (chipID == ChipID.VIC) {
          val bank = address & 0xF000
          if (bank == 0x3000) return ROMH_ULTIMAX.read(0xF000 + address - 0x3000, chipID)
          if (bank == 0x7000) return ROMH_ULTIMAX.read(0xF000 + address - 0x7000, chipID)
          if (bank == 0xB000) return ROMH_ULTIMAX.read(0xF000 + address - 0xB000, chipID)
          if (bank == 0xF000) return ROMH_ULTIMAX.read(0xF000 + address - 0xF000, chipID)
        }
      }
      if (chipID == ChipID.VIC) return ram.read(address, chipID)
      if (address == 0) return ddr
      if (address == 1) return read0001
      if (address < 0x8000) return ram.read(address)
      val c64MC = MEM_CONFIG(memConfig)
      if (address < 0xA000) { // ROML or RAM
        if (c64MC.roml) return ROML.read(address) else return ram.read(address)
      }
      if (address < 0xC000) { // BASIC or RAM or ROMH
        if (c64MC.basic) return BASIC_ROM.read(address)
        if (c64MC.romh) return ROMH.read(address)
        return ram.read(address)
      }
      if (address < 0xD000) return ram.read(address) // RAM
      if (address < 0xE000) { // I/O or RAM or CHAR
        if (c64MC.io) { // I/O
          if (address < 0xD400) return vic.read(address)
          if (address < 0xD800) return sid.read(address)
          if (address < 0xDC00) return COLOR_RAM.read(address)
          if (address < 0xDD00) return cia1.read(address)
          if (address < 0xDE00) return cia2.read(address)

          return expansionPort.read(address)
        }
        if (c64MC.char) return CHAR_ROM.read(address)
        return ram.read(address)
      }
      // KERNAL or RAM or ROMHULTIMAX
      if (c64MC.kernal) return KERNAL_ROM.read(address)
      if (c64MC.romhultimax) return ROMH_ULTIMAX.read(address)
      ram.read(address)
    }
    final def write(_address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit  = {
      val address = _address & 0xFFFF
      if (isForwardWrite) forwardWriteTo.write(address,value)
      val c64MC = MEM_CONFIG(memConfig)

      if (address < 2) {
        ram.write(address,lastByteReadMemory.lastByteRead)
        if (address == 0) ddr = value else data = value

        val clk = Clock.systemClock.currentCycles

        if ((ddr & 0x40) > 0) {
          data_set_clk_bit6 = clk + CAPACITOR_FADE_CYCLES
          data_set_bit6 = data & 0x40
          data_falloff_bit6 = true
        }
        if ((ddr & 0x80) > 0) {
          data_set_clk_bit7 = clk + CAPACITOR_FADE_CYCLES
          data_set_bit7 = data & 0x80
          data_falloff_bit7 = true
        }

        check0001
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
          if (address < 0xD400) vic.write(address,value)
          else if (address < 0xD800) sid.write(address,value)
          else if (address < 0xDC00) COLOR_RAM.write(address,value)
          else if (address < 0xDD00) cia1.write(address,value)
          else if (address < 0xDE00) cia2.write(address,value)
          else expansionPort.write(address,value)
          if (TestCart.enabled) TestCart.write(address,value)
        }
        else ram.write(address,value)
      }
      // KERNAL or RAM or ROMH
      else if (c64MC.romhultimax) ROMH_ULTIMAX.write(address,value) else ram.write(address,value)
    }
    
    override def toString: String = ram.toString
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeBoolean(ULTIMAX)
      out.writeInt(ddr)
      out.writeInt(data)
      out.writeInt(data_out)
      out.writeBoolean(exrom)
      out.writeBoolean(game)
      out.writeInt(memConfig)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      ULTIMAX = in.readBoolean
      ddr = in.readInt
      data = in.readInt
      data_out = in.readInt
      exrom = in.readBoolean
      game = in.readBoolean
      memConfig = in.readInt
      check0001
    }
    protected def allowsStateRestoring : Boolean = true
  }
}