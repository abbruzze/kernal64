package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.ROM
import ucesoft.cbm.ChipID
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.BridgeMemory
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.cpu.RAMComponent
import java.io.{FileNotFoundException, ObjectInputStream, ObjectOutputStream}

import javax.swing.JFrame

object C1541Mems {
  import ROM._
  val KERNEL_M = 0xC000

  private class DISK_KERNEL extends ROM(null,"C1541_KERNEL",KERNEL_M,16384,D1541_DOS_ROM_PROP) {
    private[this] val startAndLen = {
      try {
        val in = ROM.getROMInputStream(resourceName)
        val al = (0x10000 - in.available,in.available)
        in.close
        al
      }
      catch {
        case _: FileNotFoundException => (0,0)
      }
    }
    override val startAddress = startAndLen._1
    override val length = startAndLen._2
     
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {}
  }
  
  private class RAM extends RAMComponent {
    val componentID = "DISK RAM"
    val componentType = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_RAM"
    val startAddress = 0x0
    val length = 0x0800

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    private[this] var channelActive = 0
    
    def isChannelActive = channelActive != 0
    def getChannelsState = channelActive
    
    def init {
      Log.info("Initialaizing C1541 RAM memory ...")
    }
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0
    }
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address & 0xFFFF)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      mem(address & 0xFFFF) = value & 0xff
      if (address >= 0x22B && address <= 0x239) {
        val channel = address - 0x22B
        if (value != 0xFF) channelActive |= 1 << channel else channelActive &= ~(1 << channel)
      }
    }
    protected def saveState(out:ObjectOutputStream) {
      out.writeObject(mem)
      out.writeInt(channelActive)
    }
    protected def loadState(in:ObjectInputStream) {
      loadMemory[Int](mem,in)
      channelActive = in.readInt
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
  
  class EXP_RAM(baseAddress:Int) extends RAMComponent {
    val componentID = "Extended RAM " + Integer.toHexString(baseAddress)
    val componentType = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_RAM"
    val startAddress = baseAddress
    val length = 0x2000

    private[this] val mem = Array.fill(length)(0)
    var isActive = false
    
    def init {
      Log.info(s"Initialaizing C1541 Extended RAM ${Integer.toHexString(baseAddress)} memory ...")
    }
    def reset {
      for(i <- 0 until mem.length) mem(i) = 0xFF
    }
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = if (isActive) mem(address - baseAddress) else {
      if (baseAddress < KERNEL.startAddress) 0 else KERNEL.read(address)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
      if (isActive) mem(address - baseAddress) = value & 0xff
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeObject(mem)
      out.writeBoolean(isActive)
    }
    protected def loadState(in:ObjectInputStream) {
      loadMemory[Int](mem,in)
      isActive = in.readBoolean
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
  
  private[this] val KERNEL = new DISK_KERNEL
  val RAM_EXP_2000 = new EXP_RAM(0x2000)
  val RAM_EXP_4000 = new EXP_RAM(0x4000)
  val RAM_EXP_6000 = new EXP_RAM(0x6000)
  val RAM_EXP_8000 = new EXP_RAM(0x8000)
  val RAM_EXP_A000 = new EXP_RAM(0xA000)
  
  class C1541_RAM extends BridgeMemory {
    val componentID = "MAIN DISK RAM"
    val componentType = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_MAIN_RAM"
    val startAddress = 0x0
    val length = 0xFFFF
    final val isActive = true
        
    private[this] val RAM = new RAM
    
    def init {
      addBridge(KERNEL)
      addBridge(RAM)
      addBridge(RAM_EXP_2000)
      addBridge(RAM_EXP_4000)
      addBridge(RAM_EXP_6000)
      addBridge(RAM_EXP_8000)
      addBridge(RAM_EXP_A000)
    }
    
    def reset {}
    
    def isChannelActive = RAM.isChannelActive
    def getChannelsState = RAM.getChannelsState
    
    override def defaultValue(address:Int) = Some(address >> 8)
    // state
    protected def saveState(out:ObjectOutputStream) {}
    protected def loadState(in:ObjectInputStream) {}
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
}