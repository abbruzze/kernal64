package ucesoft.cbm.formats.cart

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Cartridge
import ucesoft.cbm.formats.ExpansionPortFactory.CartridgeExpansionPort
import ucesoft.cbm.{ChipID, Clock, ClockEvent}

import java.io.{ObjectInputStream, ObjectOutputStream}

class ActionReplay(crt: Cartridge, nmiAction: (Boolean) => Unit,ram:Memory) extends CartridgeExpansionPort(crt,ram) {
  private[this] var isActive = true
  private[this] var exportRAM = false
  private[this] val crtRAM = Array.ofDim[Int](0x2000)
  private[this] val romh = Array.ofDim[Memory](4)

  for(i <- 0 to 3) {
    romh(i) = new ROM("ROMH-" + i,0xE000,0x2000,romlBanks(i).asInstanceOf[ROM].data) {
      override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
        if (!game && exrom) super.read(address,chipID)
        else data(address - 0xA000)
      }
    }
  }

  private object CRTRAM extends ROMMemory {
    val name = "ActionReplay RAM"
    val startAddress = 0x8000
    val length = 0x2000
    val isActive = true
    val isRom = false
    def init  : Unit = {}
    def reset()  : Unit = {}
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = crtRAM(address & 0x1FFF)
    override def writeROM(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = {
      crtRAM(address & 0x1FFF) = value
    }
  }

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = if (address < 0xDF00) 0 else readIO2(address)

  @inline private def readIO2(address:Int) : Int = {
    if (!isActive) 0
    else
      if (exportRAM) crtRAM(0x1F00 + (address & 0xFF))
      else romlBanks(romlBankIndex).asInstanceOf[ROM].data(address & 0x1FFF)//romlBanks(romlBankIndex).read(address - 0xDF00 + 0x8000)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = if (address < 0xDF00) writeIO1(address,value) else writeIO2(address,value)

  @inline private def writeIO1(address: Int, value: Int) : Unit = {
    if (isActive) {
      game = (value & 1) == 0
      exrom = (value & 2) > 0
      romlBankIndex = (value >> 3) & 3
      exportRAM = (value & 0x20) != 0
      if ((value & 0x40) != 0) nmiAction(false)
      if ((value & 0x4) != 0) isActive = false
      notifyMemoryConfigurationChange
      //println(s"value=${value.toHexString} bank=$romlBankIndex game=$game exrom=$exrom ram=$exportRAM active=$isActive")
    }
  }

  @inline private def writeIO2(address: Int, value: Int) : Unit = {
    if (isActive && exportRAM) crtRAM(0x1F00 + (address & 0xFF)) = value
  }

  override def ROML: Memory = if (exportRAM) CRTRAM else super.ROML
  override def ROMH: Memory = romh(romlBankIndex)

  override def isFreezeButtonSupported = true

  override def freezeButton  : Unit = {
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

  override def reset  : Unit = {
    game = crt.GAME
    exrom = crt.EXROM
    isActive = true
    exportRAM = false
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(isActive)
    out.writeBoolean(exportRAM)
    out.writeObject(crtRAM)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    isActive = in.readBoolean
    exportRAM = in.readBoolean
    loadMemory[Int](crtRAM,in)
  }
}
