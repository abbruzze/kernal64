package ucesoft.cbm.formats

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.misc.M93C86
import java.util.Properties
import ucesoft.cbm.Log

object ExpansionPortFactory {
  private class CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends ExpansionPort {
    class ROM(val name: String, val startAddress: Int, val length: Int, val data: Array[Int]) extends Memory {
      val isRom = true
      def isActive = true
      def init {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
        if (!game && exrom) {
          if (startAddress == 0xA000) data(address - 0xE000)
          else data(address - startAddress)
        } else data(address - startAddress)
      }
      def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
        ram.write(address,value,chipID)
      }
      override def toString = s"ROM(${name})[startAddress=${Integer.toHexString(startAddress)} length=${length}]"
    }
    val name = crt.name
    protected val romlBanks = crt.chips filter { c => c.startingLoadAddress >= 0x8000 && c.startingLoadAddress < 0xA000 } sortBy { c => convertBankNumber(c.bankNumber) } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      Array.copy(c.romData, 0, data, 0, size)
      new ROM(s"${crt.name}-roml-${convertBankNumber(c.bankNumber)}", c.startingLoadAddress, size, data): Memory
    }
    protected val romhBanks = crt.chips filter { c =>
      (c.startingLoadAddress >= 0xA000 && c.startingLoadAddress < 0xC000) ||
        c.startingLoadAddress >= 0xE000 ||
        c.romSize > 8192
    } sortBy { c => convertBankNumber(c.bankNumber) } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      if (c.romSize > 8192) Array.copy(c.romData, 8192, data, 0, size) else Array.copy(c.romData, 0, data, 0, size)
      val startAddress = if (c.romSize > 8192) {
        if (!crt.GAME && crt.EXROM) 0xE000 else 0xA000
      } else c.startingLoadAddress
      new ROM(s"${crt.name}-romh-${convertBankNumber(c.bankNumber)}", startAddress, size, data): Memory
    }
    protected var romlBankIndex = 0
    protected var romhBankIndex = 0
    protected var game = /*if (crt.GAME && crt.EXROM) false else*/ crt.GAME
    protected var exrom = /*if (crt.GAME && crt.EXROM) false else*/ crt.EXROM

    protected def convertBankNumber(bank: Int): Int = bank

    def GAME = game
    def EXROM = exrom
    def ROML = if (romlBanks.length > 0) romlBanks(romlBankIndex) else null
    def ROMH = if (romhBanks.length > 0) romhBanks(romhBankIndex) else null

    override def toString = s"ExpansionPort{crt=${crt} game=${game} exrom=${exrom} romlBanks=${romlBanks.mkString("<", ",", ">")} romhBanks=${romhBanks.mkString("<", ",", ">")}}"
  }
  // ================================= CARTRIDGE IMPL ===================================================
  private class GMOD2CartridgeExpansionPort(crt: Cartridge,ram:Memory,config:Properties) extends CartridgeExpansionPort(crt,ram) {
    private[this] val CONFIGURATION_GMOD2_FILE = "gmod2.file"
    private[this] var reg = 0
    private[this] val m93c86 = new M93C86(x16 = true)
    
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (address == 0xDE00) m93c86.output << 7 else 0      
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address == 0xDE00) {
        reg = value
        if ((value & 0x40) == 0) {
          m93c86.chipSelect(false)
          val bank = value & 0x3F
          romlBankIndex = bank
          romhBankIndex = bank
        }
        else {
          m93c86.chipSelect(true)
          m93c86.clock((value & 0x20) > 0)
          m93c86.input((value >> 4) & 1)
        }
      }
    }
    override def reset {
      romlBankIndex = 0
      romhBankIndex = 0
    }
    override def eject = saveEeprom
    override def shutdown = saveEeprom
    override def init {
      Option(config.getProperty(CONFIGURATION_GMOD2_FILE)) match {
        case None =>
        case Some(eeprom) =>
          val file = new java.io.File(eeprom)
          if (file.exists) {
            m93c86.load(file)
            Log.info(s"EEPROM loaded from $file")
          }
      }
    }
    private def saveEeprom {
      Option(config.getProperty(CONFIGURATION_GMOD2_FILE)) match {
        case None =>
        case Some(eeprom) =>
          m93c86.save(new java.io.File(eeprom))
          Log.info(s"EEPROM saved to $eeprom")
      }
    }
  }
  private class SimonsBasicCartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val target = address - startAddress
      game = true
      notifyMemoryConfigurationChange
      0
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      val target = address - startAddress
      if (target == 0) {
        game = false
        notifyMemoryConfigurationChange
      }
    }
  }
  private class Comal80CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if ((value & 0x80) > 0) {
        val bank = value & 0x3
        romlBankIndex = bank
        romhBankIndex = bank
      }
    }
  }

  private class Type7CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override protected def convertBankNumber(bank: Int): Int = ((bank >> 3) & 7) | ((bank & 1) << 3)

    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address == startAddress) {
        romlBankIndex = ((value >> 3) & 7) | ((value & 1) << 3)
        if ((value & 0xc6) == 0x00) {
          exrom = false
          game = true
          notifyMemoryConfigurationChange
        } else if ((value & 0xc6) == 0x86) {
          exrom = true
          game = true
          notifyMemoryConfigurationChange
        }
      }
    }
  }

  private class Type10CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (address == 0xDF18) {
        game = true
        exrom = false
        notifyMemoryConfigurationChange
      } else if (address == 0xdf38) {
        game = true
        exrom = true
        notifyMemoryConfigurationChange
      }
      romlBanks(0).read((address & 0x1fff) + 0x8000)
    }
    override def reset {
      game = true
      exrom = false
      notifyMemoryConfigurationChange
    }
  }

  private class Type13CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (address >= 0xDF00) { // IO2
        game = false
        exrom = false
      } else { // IO1
        game = true
        exrom = true
      }
      notifyMemoryConfigurationChange
      romlBanks(0).read((address & 0x1fff) + 0x8000)
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address >= 0xDF00) { // IO2
        game = false
        exrom = false
      } else { // IO1
        game = true
        exrom = true
      }
      notifyMemoryConfigurationChange
    }
  }

  private class Type8CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address == 0xDF00) {
        val bank = value & 0x03
        romlBankIndex = bank
        romhBankIndex = bank
        val invGame = (bank & 4) > 0
        val invExrom = (bank & 8) > 0
        game = invGame
        exrom = invExrom
        notifyMemoryConfigurationChange
      }
    }
  }

  private class Type19CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    private[this] var reg = 0
    
    exrom = false
    game = true
    
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      reg = value
      romlBankIndex = value & 0x3F
      exrom = (value & 0x80) > 0
      notifyMemoryConfigurationChange
    }
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = reg    
  }
  private class Type5CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      val target = address - startAddress
      if (target == 0) {
        val bank = value & 0x3F        
        crt.kbSize match {          
          case 256 =>
            if (bank < 16) romlBankIndex = bank else romhBankIndex = bank - 16
          case _ =>
            romlBankIndex = bank
        }
      }
    }
  }
  private class Type16CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val offs = address - startAddress
      romlBanks(0).read(0x9E00 + offs)
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address >= 0xDF00) {
        game = true
        exrom = true        
      }
      else {
        game = false
        exrom = false 
      }
      notifyMemoryConfigurationChange
    }
    
    override def reset = {
      game = false
      exrom = false
      notifyMemoryConfigurationChange
    }
  }
  
  private class Type17CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val bank = address - startAddress
      romlBankIndex = bank
      0
    }
  }
  private class Type15CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (address < 0xDF00) {
        romlBankIndex = 0
      }
      0
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address < 0xDF00) {
        romlBankIndex = (address - 0xDE00) & 0x3F
      }
    }
  }
  
  private class Type32CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    private[this] val io2mem = Array.ofDim[Int](256)
    
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      if (address >= 0xDF00) io2mem(address & 0xFF) else 0
    }
    
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address >= 0xDF00) io2mem(address & 0xFF) = value
      else {
        if ((address & 2) == 0) {//(address == 0xDE00) {
          val bank = value & 0x3F
          //println(s"Selecting bank $bank")
          romlBankIndex = bank
          romhBankIndex = bank
        }
        else 
        /*if (address == 0xDE02)*/ {
          //println(s"EasyFlash Control = $value (${address.toHexString})")
          val gameControlledViaBit0 = (value & 4) == 4
          exrom = (value & 2) == 0
          game = if (gameControlledViaBit0) (value & 1) == 0 else false
          notifyMemoryConfigurationChange
        }
      }
    }
    
    override def reset {
      game = false//crt.GAME
      exrom = true//crt.EXROM
      romlBankIndex = 0
      romhBankIndex = 0
      notifyMemoryConfigurationChange
    }
  }

  private class Type3CartridgeExpansionPort(crt: Cartridge, nmiAction: (Boolean) => Unit,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    private[this] var controlRegister = true
    
    game = false
    exrom = false
    
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      //romlBanks(romlBankIndex).read((address & 0x1fff) + 0x8000)
      //romlBanks(romlBankIndex).read(0x8000 + 0x1E00 + (address & 0x1FF))
      romlBanks(romlBankIndex).asInstanceOf[ROM].data(0x1E00 + (address & 0x1FF))
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (controlRegister && address == 0xdfff) {
        romlBankIndex = value & 3
        romhBankIndex = value & 3
        game = (value & 0x20) != 0
        exrom = (value & 0x10) != 0
        notifyMemoryConfigurationChange
        nmiAction((value & 0x40) == 0)
        if ((value & 0x80) != 0) controlRegister = false
      }
    }

    override def isFreezeButtonSupported = true

    override def freezeButton {      
      val clk = Clock.systemClock
      clk.pause
      clk.schedule(new ClockEvent("Freeze", clk.currentCycles + 3, cycles => {
        //exrom = true
        game = false
        notifyMemoryConfigurationChange
        nmiAction(true)
        controlRegister = true
      }))
      clk.play
    }

    override def reset {
      controlRegister = true
      romlBankIndex = 0
      romhBankIndex = 0
      game = false//crt.GAME 
      exrom = false//crt.EXROM
    }
  }
  
  private class Type1CartridgeExpansionPort(crt: Cartridge, nmiAction: (Boolean) => Unit,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
    private[this] var isActive = true
    private[this] var exportRAM = false
    private[this] val crtRAM = Array.ofDim[Int](0x2000)
    private[this] val romh = Array.ofDim[Memory](4)
    
    for(i <- 0 to 3) {
      romh(i) = new ROM("ROMH-" + i,0xE000,0x2000,romlBanks(i).asInstanceOf[ROM].data) {
        override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
          if (!game && exrom) super.read(address,chipID)
          else data(address - 0xA000)
        }
      }
    }

    private object CRTRAM extends Memory {
      val name = "ActionReplay RAM"
      val startAddress = 0x8000
      val length = 0x2000
      val isActive = true
      val isRom = false
      def init {}
      def reset {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = crtRAM(address & 0x1FFF)     
      def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = crtRAM(address & 0x1FFF) = value      
    }
    
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = if (address < 0xDF00) 0 else readIO2(address)    
    
    @inline private def readIO2(address:Int) : Int = {
      if (!isActive) 0
      else
      if (exportRAM) crtRAM(0x1F00 + (address & 0xFF))
      else romlBanks(romlBankIndex).asInstanceOf[ROM].data(address & 0x1FFF)//romlBanks(romlBankIndex).read(address - 0xDF00 + 0x8000)
    }
    
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = if (address < 0xDF00) writeIO1(address,value) else writeIO2(address,value)    
    
    @inline private def writeIO1(address: Int, value: Int) {      
      if (isActive) {
        game = (value & 1) == 0
        exrom = (value & 2) > 0          
        romlBankIndex = (value >> 3) & 3
        exportRAM = (value & 0x20) != 0
        if ((value & 0x40) != 0) nmiAction(false)
        if ((value & 0x4) != 0) isActive = false
        notifyMemoryConfigurationChange
        //println(s"bank=$romlBankIndex game=$game exrom=$exrom ram=$exportRAM active=$isActive")
      }
    }
    
    @inline private def writeIO2(address: Int, value: Int) {
      if (isActive && exportRAM) crtRAM(0x1F00 + (address & 0xFF)) = value 
    }
    
    override def ROML = if (exportRAM) CRTRAM else super.ROML
    override def ROMH = romh(romlBankIndex)

    override def isFreezeButtonSupported = true

    override def freezeButton {
      nmiAction(true)
      //nmiAction(false)
      val clk = Clock.systemClock
      clk.pause
      clk.schedule(new ClockEvent("Freeze", clk.currentCycles + 3, cycles => {
        isActive = true
        write(0xDE00,0x23)
      }))
      clk.play      
    }
    
    override def reset {
      game = crt.GAME
      exrom = crt.EXROM
      isActive = true
      exportRAM = false
    }
  }
  // ====================================================================================================
  def loadExpansionPort(crtName: String, irqAction: (Boolean) => Unit, nmiAction: (Boolean) => Unit, ram: Memory,config:Properties): ExpansionPort = {
    val crt = new Cartridge(crtName)
    crt.ctrType match {
      case 1 => new Type1CartridgeExpansionPort(crt,nmiAction,ram)
      case 3 => new Type3CartridgeExpansionPort(crt, nmiAction,ram)
      case 0 => new CartridgeExpansionPort(crt,ram)
      case 4 => new SimonsBasicCartridgeExpansionPort(crt,ram)
      case 21 => new Comal80CartridgeExpansionPort(crt,ram)
      case 19 => new Type19CartridgeExpansionPort(crt,ram)
      case 17 => new Type17CartridgeExpansionPort(crt,ram)
      case 15 => new Type15CartridgeExpansionPort(crt,ram)
      case 16 => new Type16CartridgeExpansionPort(crt,ram)
      case 32 => new Type32CartridgeExpansionPort(crt,ram)
      case 5 => new Type5CartridgeExpansionPort(crt,ram)
      case 7 => new Type7CartridgeExpansionPort(crt,ram)
      case 8 => new Type8CartridgeExpansionPort(crt,ram)
      case 10 => new Type10CartridgeExpansionPort(crt,ram)
      case 13 => new Type13CartridgeExpansionPort(crt,ram)
      case 60 => new GMOD2CartridgeExpansionPort(crt,ram,config)
      case _ =>        
        throw new IllegalArgumentException(s"Unsupported cartridge type ${crt.ctrType} for ${crt.name}")
    }
  }

  def main(args: Array[String]) {
    val crt = new Cartridge(args(0))
    println(crt.ctrType + " -> " + new CartridgeExpansionPort(crt,null))
  }

}