package ucesoft.c64.peripheral.sid

import resid.{SID => RESID}
import ucesoft.c64.Chip
import ucesoft.c64.cpu.Memory
import ucesoft.c64.ChipID
import ucesoft.c64.peripheral.sid.resid.ISIDDefs
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent
import ucesoft.c64.Log
import ucesoft.c64.cpu.RAMComponent

class SID extends Chip with SIDDevice {
  override lazy val componentID = "SID"
  private[this] val RESID_6581 = 1
  private[this] val SAMPLE_RATE = 44100
  private[this] val BUFFER_SIZE = 256
  private[this] val CPU_FREQ = 985248
  private[this] val CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
  private[this] val CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
  
  val id = ChipID.SID
  val name = "SID"
  val startAddress = 0xd400
  val length = 1024
  val isRom = false
  
  val isActive = true
  
  private[this] val buffer = Array.fill[Byte](BUFFER_SIZE * 2)(0)
  private[this] val sid = {
    val sid = new RESID
    sid.set_chip_model(ISIDDefs.chip_model.MOS6581)
    sid.set_sampling_parameters(CPU_FREQ,ISIDDefs.sampling_method.SAMPLE_FAST, SAMPLE_RATE,-1, 0.97)
    sid
  }
  private[this] val POTX_OFS = 0x19
  private[this] val POTY_OFS = 0x1A
  private[this] var potx = 0
  private[this] var poty = 0
  
  private[this] var lastCycles = Clock.systemClock.currentCycles
  private[this] var nextRest = 0
  private[this] var pos = 0
  private[this] var removeSample = false
  private[this] val driver = new DefaultAudioDriver(SAMPLE_RATE, SAMPLE_RATE)
  
  def getDriver = driver
  def init = start
  def reset = {
    sid.reset
    Clock.systemClock.cancel("SID")
    start
  }
  
  final def read(address: Int, chipID: ChipID.ID) = address - startAddress match {
    case POTX_OFS => potx
    case POTY_OFS => poty
    case ofs => sid.read(ofs)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID) = {
    address - startAddress match {
      case POTX_OFS =>
        potx = value
      case POTY_OFS =>
        poty = value
      case ofs =>
        sid.write(ofs,value)
    }    
  }
  
  private def sidEvent(cycles:Long) {
    var nextSample = cycles + CLOCKS_PER_SAMPLE
    nextRest += CLOCKS_PER_SAMPLE_REST
    if (nextRest > 1000) {
      nextRest -= 1000
      nextSample += 1
    }
    
    val delta = cycles - lastCycles
    lastCycles = cycles
    sid.clock(delta.toInt)
    
    val sample = sid.output
    buffer(pos) = (sample & 0xff).toByte ; pos += 1
    buffer(pos) = ((sample >> 8)).toByte ; pos += 1
    if (pos == buffer.length) {
      driver.write(buffer)
      pos = 0
    }
    
    if (!removeSample) Clock.systemClock.schedule(new ClockEvent("SID",nextSample,sidEvent _))
  }
  
  def stop = removeSample = true
  def start {
    Clock.systemClock.schedule(new ClockEvent("SID",Clock.systemClock.currentCycles + 5,sidEvent _))
    lastCycles = Clock.systemClock.currentCycles
    nextRest = 0
    pos = 0
    removeSample = false
  }
  def setFullSpeed(full:Boolean) = {
    //driver.setFullSpeed(full)
    if (full) stop else start
  }
}