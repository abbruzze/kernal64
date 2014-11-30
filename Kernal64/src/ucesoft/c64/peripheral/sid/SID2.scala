package ucesoft.c64.peripheral.sid

import resid2.{SID => RESID}
import ucesoft.c64.Chip
import ucesoft.c64.cpu.Memory
import ucesoft.c64.ChipID
import ucesoft.c64.Clock
import ucesoft.c64.ClockEvent
import ucesoft.c64.Log
import ucesoft.c64.cpu.RAMComponent

class SID2 extends Chip with RAMComponent with SIDDevice {
  override lazy val componentID = "SID"
  private[this] val RESID_6581 = 1
  private[this] val SAMPLE_RATE = 44100
  private[this] val BUFFER_SIZE = 10
  private[this] val CPU_FREQ = 985248
  private[this] val CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
  private[this] val CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
  
  val id = ChipID.SID
  val name = "SID"
  val startAddress = 0xd400
  val length = 1024
  val isRom = false
  
  val isActive = true
  
  private[this] val buffer = Array.ofDim[Int](BUFFER_SIZE)
  private[this] val byteBuffer = Array.ofDim[Byte](BUFFER_SIZE)
  private[this] val sid = {
    val sid = new RESID
    sid.setChipModel(resid2.ChipModel.MOS6581)
    sid.setSamplingParameters(CPU_FREQ,resid2.SamplingMethod.DECIMATE,SAMPLE_RATE,20000)
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
  private[this] val driver = new AudioDriver(SAMPLE_RATE, SAMPLE_RATE)
  private[this] var firstSample = true
  private[this] var resetCycles = 0L
  
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
        sid.write(ofs,value.asInstanceOf[Byte])
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
    var samples = sid.clock(delta.toInt,buffer,0)
    var pos = 0
    var i = 0
    while (i < samples) {
      val l = (buffer(i) & 0xFF).asInstanceOf[Byte]
      val h = (buffer(i) >> 8).asInstanceOf[Byte]
      byteBuffer(pos) = l
      byteBuffer(pos + 1) = h
      pos += 2
      i += 1
    }
    if (firstSample) {
      if (cycles - resetCycles > 1000000) {
        firstSample = false
        driver.write(byteBuffer,samples * 2)
      }
    }
    else driver.write(byteBuffer,samples * 2)
        
    if (!removeSample) Clock.systemClock.schedule(new ClockEvent("SID",nextSample,sidEvent _))
  }
  
  def stop = {
    driver.setSoundOn(false)
    removeSample = true
  }
  def start {
    driver.setSoundOn(true)
    write(0xD418,0)
    firstSample = true
    Clock.systemClock.schedule(new ClockEvent("SID",Clock.systemClock.currentCycles + 5,sidEvent _))
    lastCycles = Clock.systemClock.currentCycles + 5
    resetCycles = Clock.systemClock.currentCycles
    nextRest = 0
    pos = 0
    removeSample = false
  }
  def setFullSpeed(full:Boolean) = {
    //driver.setFullSpeed(full)
    if (full) stop else start
  }
}