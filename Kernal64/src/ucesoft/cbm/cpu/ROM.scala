package ucesoft.cbm.cpu

import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Log
import java.io._
import java.util.Properties

import ucesoft.cbm.ChipID
import javax.swing.JFrame

class ROM(ram: Memory,
          val name: String,
          val startAddress: Int,
          val length: Int,
          val resourceName: String,
          initialOffset:Int = 0) extends RAMComponent {
  val componentID = "ROM " + name
  val componentType = CBMComponentType.MEMORY

  val isRom = true
  private[this] var mem : Array[Int] = _
  private[this] var active = false

  final def isActive = active
  def setActive(active:Boolean) = this.active = active

  def init {
    mem = Array.fill(length)(0)
    Log.info(s"Initialaizing ${name} memory ...")
    val in = new DataInputStream(ROM.getROMInputStream(resourceName))
    in.skip(initialOffset)
    val buffer = Array.ofDim[Byte](length)
    in.readFully(buffer)
    in.close
    for (i <- 0 until length) mem(i) = buffer(i) & 0xff
  }

  def reset {}

  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = ram.write(address,value,chipID)
  final def patch(address:Int,value:Int) = mem(address - startAddress) = value
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeBoolean(active)
    out.writeObject(mem)
  }
  protected def loadState(in:ObjectInputStream) {
    active = in.readBoolean
    loadMemory[Int](mem,in)
  }
  protected def allowsStateRestoring : Boolean = true
}

object ROM {
  val C64_KERNAL_ROM_PROP = "kernal64.rom.file"
  val C64_BASIC_ROM_PROP = "basic64.rom.file"
  val C64_CHAR_ROM_PROP = "char64.rom.file"

  val C128_KERNAL_ROM_PROP = "kernal128.rom.file"
  val C128_BASIC_ROM_PROP = "basic128.rom.file"
  val C128_CHAR_ROM_PROP = "char128.rom.file"
  val C128_INTERNAL_ROM_PROP = "internal128.function.rom.file"
  val C128_EXTERNAL_ROM_PROP = "external128.function.rom.file"

  val D1541_DOS_ROM_PROP = "drive1541.rom.file"
  val D1571_DOS_ROM_PROP = "drive1571.rom.file"
  val D1581_DOS_ROM_PROP = "drive1581.rom.file"

  private val ROM_DEFAULT_MAP : Map[String,String] = Map(C64_KERNAL_ROM_PROP -> "roms/kernal.rom",
                                                         C64_BASIC_ROM_PROP -> "roms/basic.rom",
                                                         C64_CHAR_ROM_PROP -> "roms/chargen.rom",
                                                         C128_KERNAL_ROM_PROP -> "roms/128/kernal.rom",
                                                         C128_BASIC_ROM_PROP -> "roms/128/basic.rom",
                                                         C128_CHAR_ROM_PROP -> "roms/128/characters.rom",
                                                         D1541_DOS_ROM_PROP -> "roms/c1541II.rom",
                                                         D1571_DOS_ROM_PROP -> "roms/c1571.rom",
                                                         D1581_DOS_ROM_PROP -> "roms/1581.rom")

  var props : Properties = _

  def getROMInputStream(resource:String) : InputStream = {
    val prop = props.getProperty(resource)
    if (prop != null && prop != "") {
      if (!new File(prop).exists) throw new FileNotFoundException(s"ROM '$prop' not found")
      Log.info(s"Loading ROM '$prop' ...")
      new FileInputStream(prop)
    }
    else {
      val defROM = ROM_DEFAULT_MAP(resource)
      val in = ClassLoader.getSystemClassLoader.getResourceAsStream(defROM)
      if (in == null) throw new FileNotFoundException(s"Default ROM '$defROM' not found")
      Log.info(s"Loading default ROM '$defROM' ...")
      in
    }
  }
}
