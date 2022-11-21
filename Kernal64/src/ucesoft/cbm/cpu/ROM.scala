package ucesoft.cbm.cpu

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.{CBMComponentType, ChipID, Log}

import java.io._
import java.util.Properties

class ROM(ram: Memory,
          val name: String,
          val startAddress: Int,
          val length: Int,
          var resourceName: String,
          initialOffset:Int = 0,
          validLengths:List[Int] = Nil) extends RAMComponent {
  val componentID: String = "ROM " + name
  val componentType: Type = CBMComponentType.MEMORY

  val isRom = true
  private[this] var mem : Array[Int] = _
  private[this] var active = false

  def getDynamicLength : Int = mem.length

  final def isActive: Boolean = active
  def setActive(active:Boolean): Unit = this.active = active

  def init  : Unit = {
    mem = Array.fill(length)(0)
    Log.info(s"Initialaizing $name memory ...")
    reload
  }

  def reload() : Unit = {
    val in = new DataInputStream(ROM.getROMInputStream(this,resourceName))
    if (length > 0) {
      in.skip(initialOffset)
      val buffer = Array.ofDim[Byte](length)
      in.readFully(buffer)
      in.close()
      if (mem != null) for (i <- 0 until length) mem(i) = buffer(i) & 0xff
    }
    else {
      // Loading variable length ROM ...
      val buffer = new ByteArrayOutputStream()
      val bin = new BufferedInputStream(in)
      var read = bin.read
      while (read != -1) {
        buffer.write(read)
        read = bin.read
      }
      bin.close()
      if (!validLengths.contains(buffer.size)) throw new IllegalArgumentException(s"Bad ROM size: ${buffer.size}. Valid sizes: ${validLengths.mkString(",")}")
      val bufferArray = buffer.toByteArray
      mem = Array.fill(buffer.size)(0)
      for (i <- 0 until buffer.size) mem(i) = bufferArray(i) & 0xff
      Log.info(s"Loaded $name as a variable ROM: size is ${bufferArray.length}")
    }
    mem = transform(mem)
  }

  final def getROMBytes(): Array[Int] = mem

  protected def transform(buffer:Array[Int]): Array[Int] = buffer

  def reset  : Unit = {}

  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = if (ram != null) ram.write(address,value,chipID)
  final def patch(address:Int,value:Int): Unit = mem(address - startAddress) = value
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(active)
    out.writeObject(mem)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    active = in.readBoolean
    loadMemory[Int](mem,in)
  }
  protected def allowsStateRestoring : Boolean = true
}

object ROM {
  val C64_KERNAL_ROM_PROP = "kernal64.rom.file"
  val C64_BASIC_ROM_PROP = "basic64.rom.file"
  val C64_CHAR_ROM_PROP = "char64.rom.file"
  val SCPU64_ROM_PROP = "scpu.rom.file"

  val C128_KERNAL_ROM_PROP = "kernal128.rom.file"
  val C128_BASIC_ROM_PROP = "basic128.rom.file"
  val C128_CHAR_ROM_PROP = "char128.rom.file"
  val C128_INTERNAL_ROM_PROP = "internal128.function.rom.file"
  val C128_EXTERNAL_ROM_PROP = "external128.function.rom.file"

  val VIC20_KERNAL_PAL_ROM_PROP = "kernal_pal20.rom.file"
  val VIC20_KERNAL_NTSC_ROM_PROP = "kernal_ntsc20.rom.file"
  val VIC20_BASIC_ROM_PROP = "basic20.rom.file"
  val VIC20_CHAR_ROM_PROP = "char20.rom.file"

  val CBM2_KERNAL_ROM_PROP = "cbm2.kernal.rom.file"
  val CBM2_BASIC128_ROM_PROP = "cbm2.basic128.rom.file"
  val CBM2_BASIC256_ROM_PROP = "cbm2.basic256.rom.file"
  val CBM2_CHAR600_ROM_PROP = "cbm2.char600.rom.file"
  val CBM2_CHAR700_ROM_PROP = "cbm2.char700.rom.file"
  val CBM2_ROMAT1000_PROP = "cbm2.1000.rom.file"
  val CBM2_ROMAT2000_PROP = "cbm2.2000.rom.file"
  val CBM2_ROMAT4000_PROP = "cbm2.4000.rom.file"
  val CBM2_ROMAT6000_PROP = "cbm2.6000.rom.file"

  val D1541_DOS_ROM_PROP = "drive1541.rom.file"
  val D1571_DOS_ROM_PROP = "drive1571.rom.file"
  val D1581_DOS_ROM_PROP = "drive1581.rom.file"

  private val ROM_DEFAULT_MAP : Map[String,String] = Map(
    // C64
    C64_KERNAL_ROM_PROP -> "roms/kernal.rom",
    C64_BASIC_ROM_PROP -> "roms/basic.rom",
    C64_CHAR_ROM_PROP -> "roms/chargen.rom",
    // VIC20
    VIC20_KERNAL_PAL_ROM_PROP -> "roms/vic20/kernal",
    VIC20_KERNAL_NTSC_ROM_PROP -> "roms/vic20/kernal_ntsc",
    VIC20_BASIC_ROM_PROP -> "roms/vic20/basic",
    VIC20_CHAR_ROM_PROP -> "roms/vic20/chargen",
    // SCPU
    SCPU64_ROM_PROP -> "roms/scpu/scpu64.rom",
    // C128
    C128_KERNAL_ROM_PROP -> "roms/128/kernal.rom",
    C128_BASIC_ROM_PROP -> "roms/128/basic.rom",
    C128_CHAR_ROM_PROP -> "roms/128/characters.rom",
    // DRIVES
    D1541_DOS_ROM_PROP -> "roms/c1541II.rom",
    D1571_DOS_ROM_PROP -> "roms/c1571.rom",
    D1581_DOS_ROM_PROP -> "roms/1581.rom",
    // CBM2
    CBM2_KERNAL_ROM_PROP -> "roms/cbm2/kernal",
    CBM2_BASIC128_ROM_PROP -> "roms/cbm2/basic.128",
    CBM2_BASIC256_ROM_PROP -> "roms/cbm2/basic.256",
    CBM2_CHAR600_ROM_PROP -> "roms/cbm2/chargen.600",
    CBM2_CHAR700_ROM_PROP -> "roms/cbm2/chargen.700"
  )

  var props : Properties = new Properties()
  private val registeredROMMap = new collection.mutable.HashMap[String,ROM]
  private var reloadListeners : List[String => Unit] = Nil

  def addReloadListener(l: String => Unit): Unit = reloadListeners ::= l

  def reload(resource:String) : Unit = {
    registeredROMMap get resource match {
      case Some(rom) =>
        rom.reload
      case None =>
    }
    reloadListeners.foreach(_(resource))
  }

  def getROMInputStream(rom:ROM,resource:String) : InputStream = {
    if (!registeredROMMap.contains(resource)) registeredROMMap += resource -> rom
    val prop = props.getProperty(resource)
    if (prop != null && prop != "") {
      val file = new File(prop)
      //if (!new File(prop).exists) throw new FileNotFoundException(s"ROM '$prop' not found")
      if (!file.exists()) println(s"Warning: '$prop' not found. Using default one.")
      else {
        Log.info(s"Loading ROM '$prop' ...")
        return new FileInputStream(prop)
      }
    }

    val defROM = ROM_DEFAULT_MAP(resource)
    val in = ClassLoader.getSystemClassLoader.getResourceAsStream(defROM)
    if (in == null) throw new FileNotFoundException(s"Default ROM '$defROM' not found")
    Log.info(s"Loading default ROM '$defROM' ...")
    in
  }
}
