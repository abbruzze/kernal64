package ucesoft.cbm.peripheral.sid

import resid.{SID => RESID}
import ucesoft.cbm.Chip
import ucesoft.cbm.ChipID
import ucesoft.cbm.peripheral.sid.resid.ISIDDefs
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import javax.swing.JFrame
import ucesoft.cbm.misc.MouseCage

class SID(override val startAddress:Int = 0xd400,sidID:Int = 1,externalDriver:Option[AudioDriverDevice] = None) extends Chip with SIDDevice {
  override lazy val componentID = "SID_" + sidID
  private[this] final val endAddress = startAddress + 0x20
  private[this] final val SAMPLE_RATE = 44100
  private[this] final val CPU_FREQ = 985248
  private[this] final val CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
  private[this] final val CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
  
  val id = ChipID.SID
  val name = "SID"
  val length = 1024
  val isRom = false
  
  val isActive = true
  
  private[this] val sid = {
    val sid = new RESID
    sid.set_chip_model(ISIDDefs.chip_model.MOS6581)
    sid.set_sampling_parameters(CPU_FREQ,ISIDDefs.sampling_method.SAMPLE_FAST, SAMPLE_RATE,-1, 0.97)
    sid.enable_filter(true)
    sid
  }
  private[this] val POTX_OFS = 0x19
  private[this] val POTY_OFS = 0x1A
  private[this] var mouseEnabled = false
  private[this] var sid2 : SID = null

  private[this] var lastCycles = Clock.systemClock.currentCycles
  private[this] var nextRest = 0
  private[this] var removeSample = false
  private[this] var nextSample = 0
  private[this] var cycleExact = false
  private[this] var fullSpeed = false
  private[this] var driver = externalDriver.getOrElse(new DefaultAudioDriver(SAMPLE_RATE, SAMPLE_RATE * 2))
  private[this] val driverProxy : AudioDriverDevice = new AudioDriverDevice {
    def getMasterVolume : Int = driver.getMasterVolume
    def setMasterVolume(v:Int) = driver.setMasterVolume(v)
    def setSoundOn(on:Boolean) = driver.setSoundOn(on)
    def addSample(sample:Int) = driver.addSample(sample)
    def reset = driver.reset
    def discard = driver.discard
  }

  def setCycleExact(ce:Boolean): Unit = {
    Clock.systemClock.cancel(componentID)

    cycleExact = ce
    if (sid2 != null) sid2.setCycleExact(ce)
    if (!fullSpeed) start
  }

  def setStereo(isStereo:Boolean,sid2:Option[SID] = None) {
    if (this.sid2 != null) this.sid2.stop

    this.sid2 = sid2.getOrElse(null)
    if (this.sid2 != null) this.sid2.setCycleExact(cycleExact)
    externalDriver match {
      case None =>
        driver.discard
        driver = new DefaultAudioDriver(SAMPLE_RATE, SAMPLE_RATE * 2,isStereo)
      case Some(_) =>
    }

    Clock.systemClock.cancel(componentID)
    start
  }
  
  def getDriver = driverProxy
  def init = start
  def reset = {
    if (sid2 != null) sid2.reset

    externalDriver match {
      case None =>
        driver.reset
      case _ =>
    }
    sid.reset
    Clock.systemClock.cancel(componentID)
    start
  }
  
  def setModel(is6581:Boolean) {
    if (is6581) {
      sid.set_chip_model(ISIDDefs.chip_model.MOS6581)
      if (sid2 != null) sid2.setModel(true)
    }
    else {
      sid.set_chip_model(ISIDDefs.chip_model.MOS8580)
      if (sid2 != null) sid2.setModel(false)
    }
  }

  /**
    * Decode address.
    *
    * @param address
    * @return -1 if address refers to 2nd sid
    */
  @inline private def decode(address:Int) = {
    if (sid2 == null) address & 0x1F
    else
    if (address >= startAddress && address < endAddress) address & 0x1F
    else -1
  }
  
  def setMouseEnabled(enabled:Boolean) = mouseEnabled = enabled
  
  final def read(address: Int, chipID: ChipID.ID) : Int = decode(address) match {
    case -1 => sid2.read(address)
    case POTX_OFS =>
      if (mouseEnabled) (MouseCage.x & 0x7F) << 1 else 0
    case POTY_OFS =>
      if (mouseEnabled) (0x7F - (MouseCage.y & 0x7F)) << 1 else 0
    case ofs => sid.read(ofs)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID) = {
    decode(address) match {
      case -1 =>
        sid2.write(address,value)
      case ofs =>
        sid.write(ofs,value)
    }
  }

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

  def clock: Unit = {
    sid.clock
    if (sid2 != null) sid2.clock

    if (!removeSample) {
      nextSample += 1
      if (nextSample == CLOCKS_PER_SAMPLE) {
        nextRest += CLOCKS_PER_SAMPLE_REST
        if (nextRest > 1000) {
          nextRest -= 1000
          nextSample = -1
        }
        else nextSample = 0

        driver.addSample(sid.output)
      }
    }
  }
  
  def stop = {
    removeSample = true
    externalDriver match {
      case None =>
        driver.setSoundOn(false)
      case _ =>
    }
  }
  def start {
    driver.setSoundOn(true)
    nextRest = 0
    nextSample = 0
    removeSample = false
    driver.reset
    if (!cycleExact) Clock.systemClock.schedule(new ClockEvent(componentID,Clock.systemClock.currentCycles + 5,sidEventCallBack))
    lastCycles = Clock.systemClock.currentCycles
  }
  def setFullSpeed(full:Boolean) = {
    fullSpeed = full
    if (full) stop else start
  }
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeObject(sid.read_state)
    out.writeBoolean(cycleExact)
  }
  protected def loadState(in:ObjectInputStream) {
    sid.write_state(in.readObject.asInstanceOf[ucesoft.cbm.peripheral.sid.resid.SID.State])
    cycleExact = in.readBoolean
    Clock.systemClock.cancel(componentID)
    start
  }
  protected def allowsStateRestoring : Boolean = true
}