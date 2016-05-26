package ucesoft.c64.cpu

import java.io.IOException
import ucesoft.c64.ChipID
import ucesoft.c64.Log
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.expansion.ExpansionPortConfigurationListener
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.peripheral.c2n.Datassette
import ucesoft.c64.Clock
import ucesoft.c64.expansion.LastByteReadMemory

object CPU6510Mems {
  final val M_ROML = 0x8000
  final val M_BASIC = 0xA000
  final val M_KERNAL = 0xE000
  final val M_CHARACTERS, M_IO = 0xD000
  final val COLOR_RAM = 0xD800
  final val SID_RAM = 0xD400
    
  abstract class ROM(ram: Memory, val name: String, val startAddress: Int, val length: Int, val resourceName: String) extends RAMComponent {
    val componentID = "ROM " + name
    val componentType = C64ComponentType.MEMORY 
    
    val isRom = true
    private[this] var mem : Array[Int] = _
    private[this] var active = false
    
    final def isActive = active
    def setActive(active:Boolean) = this.active = active

    def init {
      mem = Array.fill(length)(0)
      Log.info(s"Initialaizing ${name} memory ...")
      Option(ClassLoader.getSystemClassLoader.getResourceAsStream(resourceName)) match {
        case None => throw new IOException(s"Can't find resource ${resourceName} for ROM ${name}")
        case Some(in) =>
          val buffer = Array.ofDim[Byte](length)
          var read = in.read(buffer)
          var offset = 0
          while (read > 0) {
            offset += read
            read = in.read(buffer, offset, length - offset)
          }
          in.close
          for (i <- 0 until length) mem(i) = buffer(i) & 0xff
      }
    }
    
    def reset {}

    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)    
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = ram.write(address,value,chipID)
    final def patch(address:Int,value:Int) = mem(address - startAddress) = value
  }

  // --------------------------------------
  
  private[this] val KERNAL_ROM = System.getProperty("kernal")

  class BASIC_ROM(ram: Memory) extends ROM(ram, "BASIC", M_BASIC, 8192, "roms/basic.rom")

  class KERNAL_ROM(ram: Memory) extends ROM(ram, "KERNAL", M_KERNAL, 8192, if (KERNAL_ROM != null) KERNAL_ROM else "roms/kernal.rom") {
    override def getProperties = {
      super.getProperties
      properties.setProperty("Version",read(0xFF80).toString)
      properties
    }
  }
  
  class CHARACTERS_ROM(ram: Memory) extends ROM(ram, "CHARACTERS", M_CHARACTERS, 4096, "roms/chargen.rom")

  class RAM extends RAMComponent {
    val componentID = "RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "RAM"
    val startAddress = 0x0
    val length = 0x10000

    private[this] val mem = Array.fill(length)(0xFF)
    private[CPU6510Mems] var ULTIMAX = false
    private[CPU6510Mems] var lastByteReadMemory : LastByteReadMemory = _
    
    final val isActive = true
    def init {
      Log.info("Initialaizing RAM memory ...")
    }
    def reset {
      var i = 0
      while (i < mem.length) {
        for(j <- 1 to 64) {
          mem(i) = 0
          i += 1
        }
        for(j <- 1 to 64) {
          mem(i) = 0xFF
          i += 1
        }
      }
    }
    
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
  }
  
  class COLOR_RAM extends RAMComponent {
    val componentID = "COLOR RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "COLOR_RAM"
    val startAddress = COLOR_RAM
    val length = 1024

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    def init {}
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0xFF
    }
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address - startAddress) = value & 0xff    
  }

  class IO(ram: Memory,colorRam:Memory) extends BridgeMemory {
    val componentID = "IO RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "I/O"
    val startAddress = M_IO
    val length = 4096
    private[this] var active = false
    
    final def isActive = active
    def setActive(active:Boolean) = this.active = active
    
    def reset {}
    
    def init {
      Log.info("Initialaizing IO memory ...")
      //addBridge(ram,COLOR_RAM,1024 + 24) // + 24 unused bytes
      addBridge(colorRam)
      //addBridge(ram,SID_RAM,1024)
      addBridge(ExpansionPort.getExpansionPort)
    }
  }
  
  private class ExtendedROM(ram: Memory,val name:String,val startAddress:Int) extends RAMComponent {    
    import ExpansionPort._
    val componentID = "Extended " + name
    val componentType = C64ComponentType.MEMORY
    val length = 8192
    val isRom = true
    
    private[this] var active = false
    final private[this] val isROML = name == "ROML"
      
    final def isActive = active
    def setActive(active:Boolean) = this.active = active
    def init {}
    def reset = active = false
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
      if (selectedROM != null) selectedROM.read(address,chipID)
      else ram.read(address,chipID)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
      if (selectedROM == null) ram.write(address,value,chipID)
      else selectedROM.write(address,value)
    }
  }
  
  private case class MemConfig(basic:Boolean,roml:Boolean,romh:Boolean,char:Boolean,kernal:Boolean,io:Boolean,romhultimax:Boolean) {
    override def toString = s"basic=${b2i(basic)} roml=${b2i(roml)} romh=${b2i(romh)} char=${b2i(char)} kernal=${b2i(kernal)} io=${b2i(io)} rom_ultimax=${b2i(romhultimax)}"
    @inline private def b2i(b:Boolean) = if (b) "1" else "0"
  }

  class MAIN_MEMORY extends RAMComponent with ExpansionPortConfigurationListener {
    val componentID = "Main RAM"
    val componentType = C64ComponentType.MEMORY
    
    private[this] val ram = new RAM
    val name = "MAIN-RAM"
    val isRom = false
    val startAddress = ram.startAddress
    val length = ram.length
    val CHAR_ROM = new CHARACTERS_ROM(ram)
    val COLOR_RAM = new COLOR_RAM
    val IO = new IO(ram,COLOR_RAM)    
    val isActive = true
    
    private[this] val BASIC_ROM = new BASIC_ROM(ram)
    private[this] val KERNAL_ROM = new KERNAL_ROM(ram)
    private[this] val ROML = new ExtendedROM(ram,"ROML",M_ROML)
    private[this] val ROMH = new ExtendedROM(ram,"ROMH",M_BASIC)
    private[this] val ROMH_ULTIMAX = new ExtendedROM(ram,"ROMH_ULTIMAX",M_KERNAL)
    private[this] val banks = Array(KERNAL_ROM,IO,CHAR_ROM,BASIC_ROM,ROML,ROMH,ROMH_ULTIMAX)
    // cache
    private[this] val banksStart = banks map { _.startAddress }
    private[this] val banksEnd = banks map { _.endAddress }
    private[this] val minAddress = banksStart.min

    
    private[this] var ULTIMAX = false
    private[this] var ddr = 0
    private[this] var pr = 0
    private[this] var exrom,game = false
    private[this] var lastCycle1Written6,lastCycle1Written7 = 0L
    private[this] var capacitor6,capacitor7 = false
    private[this] val CAPACITOR_FADE_CYCLES = 350000
    private[this] var datassette : Datassette = _
    private[this] var lastByteReadMemory : LastByteReadMemory = _
    private[this] var memConfig = -1
    private[this] val MEM_CONFIG = Array.ofDim[MemConfig](32)
    
    def getRAM : Memory = ram
    
    def setDatassette(datassette:Datassette) = this.datassette = datassette
    def setLastByteReadMemory(lastByteReadMemory:LastByteReadMemory) = {
      this.lastByteReadMemory = lastByteReadMemory
      ram.lastByteReadMemory = lastByteReadMemory
    }
    
    override def getProperties = {
      super.getProperties
      properties.setProperty("Mem config",MEM_CONFIG(memConfig).toString)
      properties.setProperty("$0",Integer.toHexString(ddr))
      properties.setProperty("$1",Integer.toHexString(read0001))
      properties.setProperty("pr",Integer.toHexString(pr))
      properties.setProperty("exrom",exrom.toString)
      properties.setProperty("game",game.toString)
      properties.setProperty("capacitor 6",capacitor6.toString)
      properties.setProperty("capacitor 7",capacitor7.toString)
      properties
    }
    
    @inline private def read0001 = {
      val playSense = if ((ddr & 0x10) > 0) pr & 0x10 else if (datassette.isPlayPressed) 0x00 else 0x10
      val clk = Clock.systemClock.currentCycles
      // check bit 6 & 7
      // bit 6
      if (capacitor6) {
        if (clk - lastCycle1Written6 > CAPACITOR_FADE_CYCLES) {
          pr &= 0xBF
          capacitor6 = false
        }
        //else pr |= 0x40
      }
      // bit 7
      if (capacitor7) {
        if (clk - lastCycle1Written7 > CAPACITOR_FADE_CYCLES) {
          pr &= 0x7F
          capacitor7 = false
        }
        //else pr |= 0x80
      }
      var one = pr & 0xEF | playSense | ((ddr & 0x7) ^ 0x7) // pull up resistors
      if ((ddr & 0x20) == 0) one &= 0xDF
      one
    }

    @inline private def check0001 {
      val pr = read0001
      // check tape motor
      datassette.setMotor((ddr & 0x20) > 0 && (pr & 0x20) == 0)
      datassette.setWriteLine((ddr & 0x08) > 0 && (pr & 0x08) > 0)
      val expansionPort = ExpansionPort.getExpansionPort
      val EXROM = expansionPort.EXROM
      val GAME = expansionPort.GAME
      expansionPortConfigurationChanged(GAME,EXROM)
    }
    
    def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) {
      this.game = game
      this.exrom = exrom
      
      val newMemConfig = (~ddr | pr) & 0x7 | (if (game) 1 << 3 else 0) | (if (exrom) 1 << 4 else 0)
      if (memConfig == newMemConfig) return
      
      memConfig = newMemConfig
      val mc = MEM_CONFIG(memConfig)
      ULTIMAX = mc.romhultimax
      ram.ULTIMAX = ULTIMAX
      BASIC_ROM.setActive(mc.basic)
      ROML.setActive(mc.roml)
      ROMH.setActive(mc.romh)
      CHAR_ROM.setActive(mc.char)
      KERNAL_ROM.setActive(mc.kernal)
      IO.setActive(mc.io)
      ROMH_ULTIMAX.setActive(mc.romhultimax)
    }
    
    private def initMemConfigs {
      Log.info("Initializing main memory configurations ...")
      for(m <- 0 to 31) {
        val mc = m match {
          case 31 => 
            MemConfig(basic=true,roml=false,romh=false,char=false,kernal=true,io=true,romhultimax=false)
          case 30|14 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=true,io=true,romhultimax=false)
          case 29|13 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=true,romhultimax=false)
          case 28|24 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
          case 27 =>
            MemConfig(basic=true,roml=false,romh=false,char=true,kernal=true,io=false,romhultimax=false)
          case 26|10 =>
            MemConfig(basic=false,roml=false,romh=false,char=true,kernal=true,io=false,romhultimax=false)
          case 25|9 =>
            MemConfig(basic=false,roml=false,romh=false,char=true,kernal=false,io=false,romhultimax=false)
          case 23|22|21|20|19|18|17|16 =>
            MemConfig(basic=false,roml=true,romh=false,char=false,kernal=false,io=true,romhultimax=true)
          case 15 =>
            MemConfig(basic=true,roml=true,romh=false,char=false,kernal=true,io=true,romhultimax=false)
          case 12|8|4|0 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
          case 11 =>
            MemConfig(basic=true,roml=true,romh=false,char=true,kernal=true,io=false,romhultimax=false)
          case 7 =>
            MemConfig(basic=false,roml=true,romh=true,char=false,kernal=true,io=true,romhultimax=false)
          case 6 =>
            MemConfig(basic=false,roml=false,romh=true,char=false,kernal=true,io=true,romhultimax=false)
          case 5 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=true,romhultimax=false)
          case 3 =>
            MemConfig(basic=false,roml=true,romh=true,char=true,kernal=true,io=false,romhultimax=false)
          case 2 =>
            MemConfig(basic=false,roml=false,romh=true,char=true,kernal=true,io=false,romhultimax=false)
          case 1 =>
            MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
        }
        MEM_CONFIG(m) = mc
      }      
    }

    def init {
      Log.info("Initializing main memory ...")
      
      add(ram)
      add(IO)
      add(BASIC_ROM)
      add(KERNAL_ROM)      
      add(CHAR_ROM)
      add(ROML)
      add(ROMH)
      add(ROMH_ULTIMAX)
      
      initMemConfigs
    }
    
    override def afterInitHook {
      check0001
    }
    
    def reset {
      Log.info("Resetting main memory...")
      ddr = 0
      pr = read0001
      memConfig = -1
      ULTIMAX = false
      ram.ULTIMAX = false
      check0001
    }
    
    @inline final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
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
        val length = banks.length
        if (address >= minAddress)
        while (b < length && !found) {
          if (banks(b).isActive && address >= banksStart(b) && address < banksEnd(b)) found = true
          else b += 1
        }
        if (found) {
          val r = banks(b).read(address, chipID)
          //Log.debug("Reading from bank %s %4X = %2X".format(bank.name,address,r)) 
          r
        }
        else {
          val r = if (address == 0) ddr 
        		  else
        		  if (address == 1) read0001 
        		  else ram.read(address, chipID)
          //Log.debug("Reading from RAM %4X = %2X".format(address,r))
          r
        }
      }
    }
    
    @inline final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (isForwardWrite) forwardWriteTo.write(address,value)      
      
      var b = 0
      var found = false
      val length = banks.length
      if (address >= minAddress)
      while (b < length && !found) {
        if (banks(b).isActive && address >= banksStart(b) && address < banksEnd(b)) found = true
        else b += 1
      }
      val bank = if (!found) ram else banks(b)
      //Log.debug("Writing to %s %4X = %2X".format(bank.name,address,value))      
      if (!found && address < 2) {
        ram.write(address,lastByteReadMemory.lastByteRead)
        if (address == 0) { // $00
          val clk = Clock.systemClock.currentCycles
          if ((ddr & 0x80) > 0 && (value & 0x80) == 0 && !capacitor7) {
            lastCycle1Written7 = clk
            capacitor7 = true
          }
          //else capacitor7 = false
          if ((value & 0x80) > 0 && capacitor7) capacitor7 = false
          if ((value & 0x40) > 0 && capacitor6) capacitor6 = false
          if ((ddr & 0x40) > 0 && (value & 0x40) == 0 && !capacitor6) {
            lastCycle1Written6 = clk
            capacitor6 = true
          }
          //else capacitor6 = false
          ddr = value
        }
        else { // $01
          pr = value & 0x3F | (pr & 0xC0)
          if ((ddr & 0x80) > 0) {
            if ((value & 0x80) > 0) pr |= 0x80 else pr &= 0x7F
          }
          if ((ddr & 0x40) > 0) {
            if ((value & 0x40) > 0) pr |= 0x40 else pr &= 0xBF
          }
        }
        check0001
      }
      else bank.write(address,value,chipID)           
    }
    
    override def toString = ram.toString + banks.map(_.toString).mkString("[",",","]")
  }
}