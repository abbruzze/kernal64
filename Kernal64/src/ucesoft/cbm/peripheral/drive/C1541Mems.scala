package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.{BridgeMemory, RAMComponent, ROM}
import ucesoft.cbm.{CBMComponentType, ChipID, Log}

import java.io.{FileNotFoundException, ObjectInputStream, ObjectOutputStream}

object C1541Mems {
  import ROM._
  val KERNEL_M = 0xC000

  private class DISK_KERNEL extends ROM(null,"C1541_KERNEL",KERNEL_M,16384,D1541_DOS_ROM_PROP) {
    private[this] val startAndLen = {
      try {
        val in = ROM.getROMInputStream(this,resourceName)
        val al = (0x10000 - in.available,in.available)
        in.close()
        al
      }
      catch {
        case _: FileNotFoundException => (0,0)
      }
    }
    override val startAddress: Int = startAndLen._1
    override val length: Int = startAndLen._2
     
    final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {}
  }
  
  private class RAM extends RAMComponent {
    val componentID = "DISK RAM"
    val componentType: Type = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_RAM"
    val startAddress = 0x0
    val length = 0x0800

    private[this] val mem = Array.fill(length)(0)
    final val isActive = true
    private[this] var channelActive = 0
    
    def isChannelActive: Boolean = channelActive != 0
    def getChannelsState: Int = channelActive
    
    def init() : Unit = {
      Log.info("Initialaizing C1541 RAM memory ...")
      java.util.Arrays.fill(mem,0)
    }
    def reset() : Unit = {}
    override def hardReset(): Unit = init()
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address & 0xFFFF)
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
      mem(address & 0xFFFF) = value & 0xff
      if (address >= 0x22B && address <= 0x239) {
        val channel = address - 0x22B
        if (value != 0xFF) channelActive |= 1 << channel else channelActive &= ~(1 << channel)
      }
    }
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeObject(mem)
      out.writeInt(channelActive)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      loadMemory[Int](mem,in)
      channelActive = in.readInt
    }
    protected def allowsStateRestoring : Boolean = true
  }
  
  class EXP_RAM(baseAddress:Int) extends RAMComponent {
    val componentID: String = "Extended RAM " + Integer.toHexString(baseAddress)
    val componentType: Type = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_RAM"
    val startAddress: Int = baseAddress
    val length = 0x2000

    private[this] val mem = Array.fill(length)(0)
    var isActive = false
    
    def init() : Unit = {
      Log.info(s"Initialaizing C1541 Extended RAM ${Integer.toHexString(baseAddress)} memory ...")
      java.util.Arrays.fill(mem,0)
    }
    def reset() : Unit = {}
    override def hardReset(): Unit = init()
    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = if (isActive) mem(address - baseAddress) else {
      if (baseAddress < KERNEL.startAddress) 0 else KERNEL.read(address)
    }
    final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
      if (isActive) mem(address - baseAddress) = value & 0xff
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeObject(mem)
      out.writeBoolean(isActive)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      loadMemory[Int](mem,in)
      isActive = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }
  
  private[this] val KERNEL = new DISK_KERNEL
  val RAM_EXP_2000 = new EXP_RAM(0x2000)
  val RAM_EXP_4000 = new EXP_RAM(0x4000)
  val RAM_EXP_6000 = new EXP_RAM(0x6000)
  val RAM_EXP_8000 = new EXP_RAM(0x8000)
  val RAM_EXP_A000 = new EXP_RAM(0xA000)
  
  class C1541_RAM extends BridgeMemory {
    val componentID = "MAIN DISK RAM"
    val componentType: Type = CBMComponentType.MEMORY
    
    val isRom = false
    val name = "C1541_MAIN_RAM"
    val startAddress = 0x0
    val length = 0xFFFF
    final val isActive = true
        
    private[this] val RAM = new RAM
    
    def init() : Unit = {
      addBridge(KERNEL)
      addBridge(RAM)
      addBridge(RAM_EXP_2000)
      addBridge(RAM_EXP_4000)
      addBridge(RAM_EXP_6000)
      addBridge(RAM_EXP_8000)
      addBridge(RAM_EXP_A000)
    }
    
    def reset() : Unit = {}
    
    def isChannelActive: Boolean = RAM.isChannelActive
    def getChannelsState: Int = RAM.getChannelsState
    
    override def defaultValue(address:Int): Option[Int] = Some(address >> 8)
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {}
    protected def loadState(in:ObjectInputStream) : Unit = {}
    protected def allowsStateRestoring : Boolean = true
  }
}