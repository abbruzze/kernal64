package ucesoft.c64.formats

import ucesoft.c64.ChipID
import ucesoft.c64.cpu.Memory
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.Log

object ExpansionPortFactory {
  private class ROM(val name: String, val startAddress: Int, val length: Int, data: Array[Int]) extends Memory {
    val isRom = true
    def isActive = true
    def init {}
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) = data(address - startAddress)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
    override def toString = s"ROM(${name})[startAddress=${Integer.toHexString(startAddress)} length=${length}]"
  }
  
  private class CartridgeExpansionPort(crt:Cartridge) extends ExpansionPort {
    val name = crt.name
    protected val romlBanks = crt.chips filter { c => c.startingLoadAddress >= 0x8000 && c.startingLoadAddress < 0xA000 } sortBy { _.bankNumber } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      Array.copy(c.romData,0,data,0,size)
      new ROM(s"${crt.name}-roml-${c.bankNumber}",c.startingLoadAddress,size,data)
    }
    protected val romhBanks = crt.chips filter { c => (c.startingLoadAddress >= 0xA000 && c.startingLoadAddress < 0xC000) || 
    												 c.startingLoadAddress >= 0xE000 ||
    												 c.romSize > 8192 } sortBy { _.bankNumber } map { c =>
      val data = Array.ofDim[Int](8192)
      val size = if (c.romSize > 8192) 8192 else c.romSize
      if (c.romSize > 8192) Array.copy(c.romData,8192,data,0,size) else Array.copy(c.romData,0,data,0,size)
      val startAddress = if (c.romSize > 8192) {
        if (!crt.GAME && crt.EXROM) 0xE000 else 0xA000
      }
      else c.startingLoadAddress
      new ROM(s"${crt.name}-romh-${c.bankNumber}",startAddress,size,data)
    }
    protected var romlBankIndex = 0
    protected var romhBankIndex = 0
    protected var game = if (crt.GAME && crt.EXROM) false else crt.GAME
    protected var exrom = if (crt.GAME && crt.EXROM) false else crt.EXROM
        
    def GAME = game
    def EXROM = exrom
    def ROML = if (romlBanks.length > 0) romlBanks(romlBankIndex) else null
    def ROMH = if (romhBanks.length > 0) romhBanks(romhBankIndex) else null
    
    override def toString = s"ExpansionPort{type=${crt.ctrType} game=${game} exrom=${exrom} romlBanks=${romlBanks.mkString("<",",",">")} romhBanks=${romhBanks.mkString("<",",",">")}}"
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
	override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (address == startAddress) {
	    var bank = 0
	    if ((value & 0x08) > 0) bank |= 1
	    if ((value & 0x10) > 0) bank |= 2
	    if ((value & 0x20) > 0) bank |= 4
	    if ((value & 0x01) > 0) bank |= 8
	    romlBankIndex = bank
	  }
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
        println("Selected bank " + value)
      }
      else
      if (target >= 0x100) io2mem(target - 0x100) = value
    }
  }
  // ====================================================================================================
  def loadExpansionPort(crtName:String) : ExpansionPort = {
    val crt = new Cartridge(crtName)
    crt.ctrType match {
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
      case _ => throw new IllegalArgumentException(s"Unsupported cartridge type ${crt.ctrType} for ${crt.name}")
    }
  }
  
  def main(args:Array[String]) {
    val crt = new Cartridge(args(0))
    println(crt.ctrType + " -> " + new CartridgeExpansionPort(crt))
  }
  
}