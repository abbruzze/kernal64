package ucesoft.c64.formats

import ucesoft.c64.ChipID
import ucesoft.c64.cpu.Memory
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.Log
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent

object ExpansionPortFactory {
  private class CartridgeExpansionPort(crt:Cartridge) extends ExpansionPort {
    private class ROM(val name: String, val startAddress: Int, val length: Int, data: Array[Int]) extends Memory {
      val isRom = true
      def isActive = true
      def init {}
      final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
        if (!game && exrom) {
          if (startAddress == 0xA000) data(address - 0xE000) 
          else data(address - startAddress)
        }
        else data(address - startAddress)
      }
      final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
      override def toString = s"ROM(${name})[startAddress=${Integer.toHexString(startAddress)} length=${length}]"
    }
    val name = crt.name
    protected val romlBanks = crt.chips filter { c => c.startingLoadAddress >= 0x8000 && c.startingLoadAddress < 0xA000 } sortBy { c => convertBankNumber(c.bankNumber) } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      Array.copy(c.romData,0,data,0,size)
      new ROM(s"${crt.name}-roml-${convertBankNumber(c.bankNumber)}",c.startingLoadAddress,size,data) : Memory
    }
    protected val romhBanks = crt.chips filter { c => (c.startingLoadAddress >= 0xA000 && c.startingLoadAddress < 0xC000) || 
    												 c.startingLoadAddress >= 0xE000 ||
    												 c.romSize > 8192 } sortBy { c =>  convertBankNumber(c.bankNumber) } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      if (c.romSize > 8192) Array.copy(c.romData,8192,data,0,size) else Array.copy(c.romData,0,data,0,size)
      val startAddress = if (c.romSize > 8192) {
        if (!crt.GAME && crt.EXROM) 0xE000 else 0xA000
      }
      else c.startingLoadAddress
      new ROM(s"${crt.name}-romh-${convertBankNumber(c.bankNumber)}",startAddress,size,data) : Memory
    }
    protected var romlBankIndex = 0
    protected var romhBankIndex = 0
    protected var game = if (crt.GAME && crt.EXROM) false else crt.GAME
    protected var exrom = if (crt.GAME && crt.EXROM) false else crt.EXROM
    
    protected def convertBankNumber(bank:Int) : Int = bank
        
    def GAME = game
    def EXROM = exrom
    def ROML = if (romlBanks.length > 0) romlBanks(romlBankIndex) else null
    def ROMH = if (romhBanks.length > 0) romhBanks(romhBankIndex) else null
    
    override def toString = s"ExpansionPort{crt=${crt} game=${game} exrom=${exrom} romlBanks=${romlBanks.mkString("<",",",">")} romhBanks=${romhBanks.mkString("<",",",">")}}"
  }
  // ================================= CARTRIDGE IMPL ===================================================
  private class SimonsBasicCartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
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
  private class Comal80CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
	override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
	  if ((value & 0x80) > 0) {
	      val bank = value & 0x3
	      romlBankIndex = bank
	      romhBankIndex = bank
	  }
    }
  } 
  
  private class Type7CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
    override protected def convertBankNumber(bank:Int) : Int = ((bank >> 3) & 7) | ((bank & 1) << 3)
    
	override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address == startAddress) {
        romlBankIndex = ((value >> 3) & 7) | ((value & 1) << 3)
        if ((value & 0xc6) == 0x00) {
          exrom = false
          game = true
          notifyMemoryConfigurationChange
        }
        else
        if ((value & 0xc6) == 0x86) {
          exrom = true
          game = true
          notifyMemoryConfigurationChange
        }
	  }	  
    }
  } 
  
  private class Type10CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {    
	override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
	  if (address == 0xDF18) {
	    game = true
	    exrom = false
	    notifyMemoryConfigurationChange
	  }
	  else
	  if (address == 0xdf38) {
	    game = true
	    exrom = true
	    notifyMemoryConfigurationChange
	  }
	  romlBanks(0).read((address & 0x1fff) + 0x8000)
	}
  } 
  
  private class Type13CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {    
	override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
	  if (address >= 0xDF00) { // IO2
	    game = false
	    exrom = false	    
	  }
	  else { // IO1
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
	  }
	  else { // IO1
	    game = true
	    exrom = true
	  }
	  notifyMemoryConfigurationChange
	}
  } 
  
  private class Type8CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
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
  
  private class Type19CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
	override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      val target = address - startAddress
      if (target == 0) {
        romlBankIndex = value & 0x7F
      }
    }
  }   
  private class Type5CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
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
  private class Type17CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val bank = address - startAddress
      romlBankIndex = bank
      0
    }
  }
  private class Type15CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val bank = address - startAddress
      romlBankIndex = bank
      0
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      val bank = address - startAddress
      romlBankIndex = bank
    }
  }
  private class Type32CartridgeExpansionPort(crt:Cartridge) extends CartridgeExpansionPort(crt) {
    private[this] val io2mem = Array.ofDim[Int](256)
    override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
      val target = startAddress - address
      if (target == 0) {
        (if (exrom) 2 else 0) | (if (game) 1 else 0)
      }
      else
      if (target >= 0x100) io2mem(target - 0x100)
      else 0
    }
    override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      val target = startAddress - address
      if (target == 0) {
        exrom = (value & 2) == 2
        game = (value & 1) == 1
        println("Changed exrom & game: " + value)
        notifyMemoryConfigurationChange
      }
      else
      if (target == 2) {
        romlBankIndex = value
        romhBankIndex = value
      }
      else
      if (target >= 0x100) io2mem(target - 0x100) = value
    }
  }
  
  private class Type3CartridgeExpansionPort(crt:Cartridge,nmiAction:(Boolean) => Unit) extends CartridgeExpansionPort(crt) {
    private[this] var controlRegister = true
    
	override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = {
	  romlBanks(romlBankIndex).read((address & 0x1fff) + 0x8000)
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
	  nmiAction(true)
	  controlRegister = true
	  val clk = Clock.systemClock
	  clk.pause
	  clk.schedule(new ClockEvent("Freeze",clk.currentCycles + 3,cycles => {
	    //exrom = true
	    game = false	
	    notifyMemoryConfigurationChange 
	  }))
	  clk.play
	}
		
	override def reset {
	  controlRegister = true
	  romlBankIndex = 0
	  romhBankIndex = 0
//	  game = if (crt.GAME && crt.EXROM) false else crt.GAME 
//	  exrom = if (crt.GAME && crt.EXROM) false else crt.EXROM
	}
  }
  // ====================================================================================================
  def loadExpansionPort(crtName:String,irqAction:(Boolean) => Unit,nmiAction:(Boolean) => Unit) : ExpansionPort = {
    val crt = new Cartridge(crtName)
    crt.ctrType match {
      case 3 => new Type3CartridgeExpansionPort(crt,nmiAction)
      case 0 => new CartridgeExpansionPort(crt)
      case 4 => new SimonsBasicCartridgeExpansionPort(crt)
      case 21 => new Comal80CartridgeExpansionPort(crt)
      case 19 => new Type19CartridgeExpansionPort(crt)
      case 17 => new Type17CartridgeExpansionPort(crt)
      case 15 => new Type15CartridgeExpansionPort(crt)
      case 32 => new Type32CartridgeExpansionPort(crt)
      case 5 => new Type5CartridgeExpansionPort(crt)
      case 7 => new Type7CartridgeExpansionPort(crt)
      case 8 => new Type8CartridgeExpansionPort(crt)
      case 10 => new Type10CartridgeExpansionPort(crt)
      case 13 => new Type13CartridgeExpansionPort(crt)      
      case _ => throw new IllegalArgumentException(s"Unsupported cartridge type ${crt.ctrType} for ${crt.name}")
    }
  }
  
  def main(args:Array[String]) {
    val crt = new Cartridge(args(0))
    println(crt.ctrType + " -> " + new CartridgeExpansionPort(crt))
  }
  
}