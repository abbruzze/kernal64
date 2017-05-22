package ucesoft.cbm.peripheral.sid

import resid.{SID => RESID}
import ucesoft.cbm.Chip
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.ChipID
import ucesoft.cbm.peripheral.sid.resid.ISIDDefs
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.RAMComponent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class SID(override val startAddress:Int = 0xd400,sidID:Int = 1,externalDriver:Option[AudioDriverDevice] = None) extends Chip with SIDDevice {
  override lazy val componentID = "SID_" + sidID
  private[this] val SAMPLE_RATE = 44100
  private[this] val CPU_FREQ = 985248
  private[this] val CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
  private[this] val CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
  
  val id = ChipID.SID
  val name = "SID"
  //val startAddress = 0xd400
  val length = 1024
  val isRom = false
  
  val isActive = true
  
  private[this] val sid = {
    val sid = new RESID
    sid.set_chip_model(ISIDDefs.chip_model.MOS6581)
    sid.set_sampling_parameters(CPU_FREQ,ISIDDefs.sampling_method.SAMPLE_FAST, SAMPLE_RATE,-1, 0.97)
    sid
  }
  private[this] val POTX_OFS = 0x19
  private[this] val POTY_OFS = 0x1A
  private[this] var mouseEnabled = false
  
  private[this] var lastCycles = Clock.systemClock.currentCycles
  private[this] var nextRest = 0
  private[this] var removeSample = false
  private[this] var driver = externalDriver.getOrElse(new DefaultAudioDriver(SAMPLE_RATE, SAMPLE_RATE * 2))
  private[this] val driverProxy : AudioDriverDevice = new AudioDriverDevice {
    def getMasterVolume : Int = driver.getMasterVolume
    def setMasterVolume(v:Int) = driver.setMasterVolume(v)
    def setSoundOn(on:Boolean) = driver.setSoundOn(on)
    def addSample(sample:Int) = driver.addSample(sample)
    def reset = driver.reset
    def discard = driver.discard
  }
  
  def setStereo(isStereo:Boolean) {
    externalDriver match {
      case None =>
        driver.discard
        driver = new DefaultAudioDriver(SAMPLE_RATE, SAMPLE_RATE * 2,isStereo)
      case Some(_) =>
    }
  }
  
  def getDriver = driverProxy
  def init = start
  def reset = {
    driver.reset
    sid.reset
    Clock.systemClock.cancel(componentID)
    start
  }
  
  def setModel(is6581:Boolean) {
    if (is6581) sid.set_chip_model(ISIDDefs.chip_model.MOS6581)
    else sid.set_chip_model(ISIDDefs.chip_model.MOS8580)
  }
  
  @inline private def decode(address:Int) = address & 0x1F
  
  def setMouseEnabled(enabled:Boolean) = mouseEnabled = enabled
  
  final def read(address: Int, chipID: ChipID.ID) = decode(address) match {
    case POTX_OFS =>
      if (mouseEnabled) (java.awt.MouseInfo.getPointerInfo.getLocation.x & 0x7F) << 1 else 0
    case POTY_OFS =>
      if (mouseEnabled) (0x7F - (java.awt.MouseInfo.getPointerInfo.getLocation.y & 0x7F)) << 1 else 0
    case ofs => sid.read(ofs)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID) = sid.write(decode(address),value)
  
  private[this] val sidEventCallBack = sidEvent _
  
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
    
    driver.addSample(sid.output)
    
    if (!removeSample) Clock.systemClock.schedule(new ClockEvent(componentID,nextSample,sidEventCallBack))
  }
  
  def stop = {
    removeSample = true
    driver.setSoundOn(false)
  }
  def start {
    driver.setSoundOn(true)
    Clock.systemClock.schedule(new ClockEvent(componentID,Clock.systemClock.currentCycles + 5,sidEventCallBack))
    lastCycles = Clock.systemClock.currentCycles
    nextRest = 0
    driver.reset
    removeSample = false
  }
  def setFullSpeed(full:Boolean) = {
    if (full) stop else start
  }
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeObject(sid.read_state)
  }
  protected def loadState(in:ObjectInputStream) {
    sid.write_state(in.readObject.asInstanceOf[ucesoft.cbm.peripheral.sid.resid.SID.State])
    Clock.systemClock.cancel(componentID)
    start
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}