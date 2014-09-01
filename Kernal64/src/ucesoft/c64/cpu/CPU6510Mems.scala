package ucesoft.c64.cpu

import java.io.IOException
import ucesoft.c64.ChipID
import ucesoft.c64.Log
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.expansion.ExpansionPortConfigurationListener
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.peripheral.c2n.Datassette

object CPU6510Mems {
  val M_ROML = 0x8000
  val M_BASIC = 0xA000
  val M_KERNAL = 0xE000
  val M_CHARACTERS, M_IO = 0xD000
  val COLOR_RAM = 0xD800
  val SID_RAM = 0xD400
    
  abstract class ROM(ram: Memory, val name: String, val startAddress: Int, val length: Int, resourceName: String) extends RAMComponent {
    val componentID = "ROM " + name
    val componentType = C64ComponentType.MEMORY 
    
    val isRom = true
    private[this] val mem = Array.fill(length)(0)
    private[this] var active = false
    
    final def isActive = active
    def setActive(active:Boolean) = this.active = active

    def init {
      Log.info(s"Initialaizing ${name} memory ...")
      Option(getClass.getClassLoader.getResourceAsStream(resourceName)) match {
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

  class BASIC_ROM(ram: Memory) extends ROM(ram, "BASIC", M_BASIC, 8192, "roms/basic.rom")

  class KERNAL_ROM(ram: Memory) extends ROM(ram, "KERNAL", M_KERNAL, 8192, "roms/kernal.rom")
  
  class CHARACTERS_ROM(ram: Memory) extends ROM(ram, "CHARACTERS", M_CHARACTERS, 4096, "roms/chargen.rom")

  class RAM extends RAMComponent {
    val componentID = "RAM"
    val componentType = C64ComponentType.MEMORY
    
    val isRom = false
    val name = "RAM"
    val startAddress = 0x0
    val length = 0x10000

    private[this] val mem = Array.fill(length)(0xFF)
    final val isActive = true
    def init {
      Log.info("Initialaizing RAM memory ...")
    }
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0xFF
    }
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address & 0xFFFF)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = mem(address & 0xFFFF) = value & 0xff    
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
    private[this] val isROML = name == "ROML"
      
    final def isActive = active
    def setActive(active:Boolean) = this.active = active
    def init {}
    def reset = active = false
    
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      val selectedROM = if (isROML) getExpansionPort.ROML else getExpansionPort.ROMH
      if (selectedROM != null) selectedROM.read(address,chipID) else ram.read(address,chipID)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (!getExpansionPort.isUltimax) ram.write(address,value,chipID)
    }
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
    private[this] val banks = Array(IO,CHAR_ROM,KERNAL_ROM,BASIC_ROM,ROML,ROMH,ROMH_ULTIMAX)
    
    private[this] var LORAM = true
    private[this] var HIRAM = true
    private[this] var CHAREN = true
    private[this] var ULTIMAX = false
    private[this] var ddr,pr = 0
    private[this] var datassette : Datassette = _
    
    def setDatassette(datassette:Datassette) = this.datassette = datassette
    
    override def getProperties = {
      super.getProperties
      properties.setProperty("LORAM",LORAM.toString)
      properties.setProperty("HIRAM",HIRAM.toString)
      properties.setProperty("CHAREN",CHAREN.toString)
      properties.setProperty("$0",Integer.toHexString(ddr))
      properties.setProperty("$1",Integer.toHexString(read0001))
      properties
    }
    
    @inline private def read0001 = pr & 0xEF | (if (datassette.isPlayPressed) 0x00 else 0x10)  | ((ddr & 7) ^ 0x07) // pull up resistors

    @inline private def check0001 {
      // check tape motor
      if ((ddr & 0x20) > 0) datassette.setMotor((pr & 0x20) == 0)
      val _LORAM = LORAM
      val _HIRAM = HIRAM
      val _CHAREN = CHAREN
      //Log.debug(s"ddr0=${Integer.toBinaryString(ddr0)} 01=${Integer.toBinaryString(one)} p=${Integer.toBinaryString(p)}")
      LORAM = if ((ddr & 1) > 0) (pr & 1) == 1 else true
      HIRAM = if ((ddr & 2) > 0) (pr & 2) == 2 else true
      CHAREN = if ((ddr & 4) > 0) (pr & 4) == 4 else true
      if (_LORAM != LORAM || _HIRAM != HIRAM || _CHAREN != CHAREN) {
        Log.debug(s"Memory DDR=${Integer.toBinaryString(ddr)} PR=${Integer.toBinaryString(pr)}. LORAM=${LORAM} HIRAM=${HIRAM} CHAREN=${CHAREN}")
        expansionPortConfigurationChanged
      }
    }
    
    def expansionPortConfigurationChanged {
      val expansionPort = ExpansionPort.getExpansionPort
      val EXROM = expansionPort.EXROM
      val GAME = expansionPort.GAME
      Log.debug(s"Reconfiguring memory map: LORAM=${LORAM} HIRAM=${HIRAM} CHAREN=${CHAREN} EXROM=${EXROM} GAME=${GAME}")
      
      // PLA implementation
      val BASIC = LORAM && HIRAM && GAME	// p0
      val KERNAL = (HIRAM && GAME) || (HIRAM && !EXROM && !GAME) // p1 || p2
      val CHARROM = (HIRAM && !CHAREN && GAME) 				||	// p3 
      				(LORAM && !CHAREN && GAME) 				||	// p4
      				(HIRAM && !CHAREN && !EXROM && !GAME)	 	// p5. p6, p7 are omitted
      val IO = (HIRAM && CHAREN && GAME)			||	// p10
      		   (LORAM && CHAREN && GAME)			||  // p12
      		   (HIRAM && CHAREN && !EXROM && !GAME)	||  // p14
      		   (LORAM && CHAREN && !EXROM && !GAME)	|| 	// p16
      		   (EXROM && !GAME)							// p18
      val ROML = (LORAM && HIRAM && !EXROM) || (EXROM && !GAME)	// p19, p20
      val ROMH = (HIRAM && !EXROM && !GAME) || (EXROM && !GAME) // p21,p22
      // enabling/disabling ROMs
      BASIC_ROM.setActive(BASIC)
      KERNAL_ROM.setActive(KERNAL)
      CHAR_ROM.setActive(CHARROM)
      this.IO.setActive(IO)
      this.ROML.setActive(ROML)
      if (ROMH) {
        if (EXROM && !GAME) {
          this.ROMH.setActive(false)
          ROMH_ULTIMAX.setActive(true)
        }
        else {
          this.ROMH.setActive(true)
          ROMH_ULTIMAX.setActive(false)
        }
      }
      ULTIMAX = !GAME && EXROM
      Log.debug(s"Memory configuration is: BASIC=${BASIC} KERNAL=${KERNAL} CHARROM=${CHARROM} IO=${IO} ROML=${ROML} ROMH=${ROMH} GAME=${GAME} EXROM=${EXROM}")
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
    }
    
    override def afterInitHook {
      expansionPortConfigurationChanged
    }
    
    def reset {
      Log.info("Resetting main memory...")
      LORAM = true
      HIRAM = true
      CHAREN = true
      ULTIMAX = false
      expansionPortConfigurationChanged
    }
    
    @inline final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
      if (ULTIMAX) {
        if (chipID == ChipID.VIC) {
          if (address >= 0x3000 && address < 0x4000) return ROMH.read(0xF000 + address - 0x3000,chipID)
          if (address >= 0x7000 && address < 0x8000) return ROMH.read(0xF000 + address - 0x7000,chipID)
          if (address >= 0xB000 && address < 0xC000) return ROMH.read(0xF000 + address - 0xB000,chipID)
          if (address >= 0xF000 && address < 0x10000) return ROMH.read(0xF000 + address - 0xF000,chipID)
        }
      }
      if (chipID == ChipID.VIC) ram.read(address, chipID)
      else {        
        var b = 0
        var found = false
        var bank : Memory = null
        while (b < banks.length && !found) {
          bank = banks(b)
          if (bank.isActive && address >= bank.startAddress && address < bank.endAddress) found = true
          else b += 1
        }
        if (found) {
          val r = bank.read(address, chipID)
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
      var b = 0
      var found = false
      var bank: Memory = null
      while (b < banks.length && !found) {
        bank = banks(b)
        if (bank.isActive && address >= bank.startAddress && address < bank.endAddress) found = true
        else b += 1
      }
      if (!found) bank = ram
      //Log.debug("Writing to %s %4X = %2X".format(bank.name,address,value))      
      if (!found && address < 2) {
        if (address == 0) ddr = value
        else pr = value & 0x3F // bits 6 & 7 not used
        check0001
      }
      else bank.write(address,value,chipID)
    }
    
    override def toString = ram.toString + banks.map(_.toString).mkString("[",",","]")
  }
}