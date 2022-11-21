package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID
import ucesoft.cbm.cpu.Memory

import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}

class DigiMaxCart(digiAddress:Int) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.DIGIMAX
  val name = "DigiMAX"
  val EXROM = true
  val GAME = true
  val ROML: Memory = null
  val ROMH: Memory = null
  
  DigiMAX.enabled(true,false)
  
  private[this] val soundData = Array(0,0,0,0)
  
  @inline private def checkAddress(address:Int) : Boolean = (address & 0xFFFC) == digiAddress
    
  final override def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = {
    if (checkAddress(address)) soundData(address & 3) else 0
  }
  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (checkAddress(address)) {
      val channel = address & 3
      DigiMAX.selectChannel(channel)
      DigiMAX.write(value)      
    }
  }
  override def eject  : Unit = {
    DigiMAX.enabled(false,false)
  }  
}

object DigiMAX {
  private[this] var _enabled,enabledOnUserPort = false   
  private[this] final val DEFAULT_SAMPLE_RATE = 44100
  private[this] var sampleRate = DEFAULT_SAMPLE_RATE
  
  private[this] var lines : Array[SourceDataLine] = _
  private[this] lazy val buffers = Array.ofDim[Byte](4,256)
  private[this] lazy val pos = Array.ofDim[Int](4)
  private[this] var channel = 0
  
  private def createLines(fHz:Int) = {
    (for(i <- 0 to 3) yield {
      val af = new AudioFormat(fHz,8,1,false, false)
      val dli = new DataLine.Info(classOf[SourceDataLine], af, fHz * 2)
      val dataLine = try {
        AudioSystem.getLine(dli).asInstanceOf[SourceDataLine] 
      }
      catch {
        case t:Throwable =>
          null
      }
    
    if (dataLine != null) dataLine.open(dataLine.getFormat,fHz * 2)
    dataLine
    }).toArray
  }
  
  def getSampleRate : Int = sampleRate
  
  def setSampleRate(fHz:Int) : Unit = {
    sampleRate = fHz
    if (lines != null) {
      for(l <- lines) if (l != null) l.close()
    }
    lines = createLines(fHz)
    if (_enabled) for(l <- lines) if (l != null) l.start()
  }
  
  def selectChannel(channel:Int) : Unit = {
    this.channel = channel
  }
  def enabled: Boolean = _enabled
  def isEnabledOnUserPort: Boolean = _enabled && enabledOnUserPort
  
  def enabled(on:Boolean,enabledOnUserPort:Boolean = false) : Unit = {
    _enabled = on
    if (on && lines == null) setSampleRate(DEFAULT_SAMPLE_RATE)
    this.enabledOnUserPort = enabledOnUserPort
    if (lines != null) {
      for (dl <- lines) {
        if (dl != null) if (on) {
          dl.start()
        } else {
          dl.stop()
        }
      }
    }
  }
  
  def write(value:Int) : Unit = {
    val buffer = buffers(channel)
    buffer(pos(channel)) = value.asInstanceOf[Byte]
    pos(channel) = pos(channel) + 1
    if (pos(channel) == buffer.length) {
      pos(channel) = 0
      val dataLine = lines(channel)
      if (dataLine != null) dataLine.write(buffer,0,buffer.length)
    }
  }
}