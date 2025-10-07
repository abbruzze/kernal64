package ucesoft.cbm.formats

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortType}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties
import scala.collection.mutable

object ExpansionPortFactory {
  class CartridgeExpansionPort(crt: Cartridge,ram:Memory) extends ExpansionPort {
    val TYPE : ExpansionPortType.Value = ExpansionPortType.CRT

    abstract class ROMMemory extends Memory {
      final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
        if (!(!game && exrom)) ram.write(address,value,chipID) // ultimax: Internal memory does not respond to write accesses to these areas
        writeROM(address,value,chipID)
      }
      protected def writeROM(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
    }

    class ROM(val name: String, val startAddress: Int, val length: Int, val data: Array[Int]) extends ROMMemory {
      val isRom = true
      def isActive = true
      def init(): Unit = {}
      def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = data(address & 0x1FFF)

      override def toString = s"ROM($name)[startAddress=${Integer.toHexString(startAddress)} length=$length]"
    }
    val name: String = crt.name
    val crtType: Int = crt.ctrType
    // ROML Banks
    protected val romlBanks : mutable.LinkedHashMap[Int,Memory] = {
      val banks = crt.chips filter { c => c.startingLoadAddress >= 0x8000 && c.startingLoadAddress < 0xA000 } map { c =>
        val data = Array.ofDim[Int](8192)
        val size = if (c.romSize > 8192) 8192 else c.romSize
        Array.copy(c.romData, 0, data, 0, size)
        convertBankNumber(c.bankNumber) -> (new ROM(s"${crt.name}-roml-${convertBankNumber(c.bankNumber)}", c.startingLoadAddress, size, data): Memory)
      }
      val map = new mutable.LinkedHashMap[Int,Memory]()
      map.addAll(banks)
    }
    // ROMH Banks
    protected val romhBanks : mutable.LinkedHashMap[Int,Memory] = {
      val banks = crt.chips filter { c =>
        (c.startingLoadAddress >= 0xA000 && c.startingLoadAddress < 0xC000) ||
          c.startingLoadAddress >= 0xE000 ||
          c.romSize > 8192
      } map { c =>
        val data = Array.ofDim[Int](8192)
        val size = if (c.romSize > 8192) 8192 else c.romSize
        if (c.romSize > 8192) Array.copy(c.romData, 8192, data, 0, size) else Array.copy(c.romData, 0, data, 0, size)
        val startAddress = if (c.romSize > 8192) {
          if (!crt.GAME && crt.EXROM) 0xE000 else 0xA000
        } else c.startingLoadAddress
        convertBankNumber(c.bankNumber) -> (new ROM(s"${crt.name}-romh-${convertBankNumber(c.bankNumber)}", startAddress, size, data): Memory)
      }
      val map = new mutable.LinkedHashMap[Int,Memory]()
      map.addAll(banks)
      map
    }

    override def getCRT: Option[Cartridge] = Some(crt)

    private var _romlBankIndex = 0
    private var _romhBankIndex = 0
    protected var romLBank : Memory = null
    protected var romHBank : Memory = null
    protected var game: Boolean = crt.GAME
    protected var exrom: Boolean = crt.EXROM

    romlBankIndex = 0
    romhBankIndex = 0

    protected def romlBankIndex : Int = _romlBankIndex
    protected def romlBankIndex_=(index:Int) : Unit = {
      _romlBankIndex = index
      if (romlBanks.contains(index)) romLBank = romlBanks(index)
    }
    protected def romhBankIndex_=(index:Int) : Unit = {
      _romhBankIndex = index
      if (romhBanks.contains(index)) romHBank = romhBanks(index)
    }
    protected def romhBankIndex : Int = _romhBankIndex

    protected def convertBankNumber(bank: Int): Int = bank

    def GAME: Boolean = game
    def EXROM: Boolean = exrom
    def ROML: Memory = romLBank
    def ROMH: Memory = romHBank

    override def toString = s"ExpansionPort{crt=$crt game=$game exrom=$exrom\n\tromlBanks=${romlBanks.mkString("<", "\n\t\t", ">")}\n\tromhBanks=${romhBanks.mkString("<", "\n\t\t", ">")}}"

    override def saveState(out: ObjectOutputStream): Unit = {
      crt.saveState(out)
      super.saveState(out)
      out.writeInt(romlBankIndex)
      out.writeInt(romhBankIndex)
      out.writeBoolean(game)
      out.writeBoolean(exrom)
    }

    override def loadState(in: ObjectInputStream): Unit = {
      // the crt state is handled by state handler
      super.loadState(in)
      romlBankIndex = in.readInt
      romhBankIndex = in.readInt
      game = in.readBoolean
      exrom = in.readBoolean
      notifyMemoryConfigurationChange()
    }
  }

  // ====================================================================================================
  def loadExpansionPort(crtName: String,
                        irqAction: Boolean => Unit,
                        nmiAction: Boolean => Unit,
                        ram: Memory,forwardRam:Memory,
                        reset: () => Unit,
                        config:Properties): ExpansionPort = {
    import cart._
    val crt = new Cartridge(crtName)
    if (crt.cbmType != Cartridge.CBMType.C64) throw new IllegalArgumentException(s"Unsupported cartridge signature '${crt.cbmType}'")
    crt.ctrType match {
      case 1 => new ActionReplay(crt,nmiAction,ram)
      case 2 => new KCS(crt,ram,nmiAction)
      case 3 => new FinalCartridgeIII(crt, nmiAction,ram)
      case 0 => new CartridgeExpansionPort(crt,ram)
      case 4 => new SimonsBasic(crt,ram)
      case 21 => new Comal80(crt,ram)
      case 19 => new MagicDesk(crt,ram)
      case 17 => new Dinamic(crt,ram)
      case 15 => new GameSystem(crt,ram)
      case 16 => new WarpSpeed(crt,ram)
      case 32 => new EasyFlash(crt,ram)
      case 5 => new Ocean(crt,ram)
      case 7 => new FunPlay(crt,ram)
      case 8 => new SuperGames(crt,ram)
      case 10 => new EpyxFastload(crt,ram)
      case 13 => new FinalCartridgeI(crt,ram)
      case 20 => new SuperSnapshot5(crt,nmiAction,ram)
      case 60 => new GMOD2(crt,ram,config)
      case 62 => new GMOD3(crt,ram,forwardRam)
      case 18 => new Zaxxon(crt,ram)
      case 51 => new Mach5(crt,ram)
      case 53 => new PageFox(crt,ram)
      case 48 => new SuperExplode(crt,ram,reset)
      case 85 => new MagicDesk16K(crt,ram)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported cartridge type ${crt.ctrType} for ${crt.name}")
    }
  }

  def main(args: Array[String]) : Unit = {
    val crt = new Cartridge(args(0))
    println(s"${crt.ctrType} -> " + new CartridgeExpansionPort(crt,null))
  }

}
