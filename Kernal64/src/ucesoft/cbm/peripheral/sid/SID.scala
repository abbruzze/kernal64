package ucesoft.cbm.peripheral.sid

import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.misc.MouseCage
import ucesoft.cbm.peripheral.sid.resid2.{SID => RESID}
import ucesoft.cbm.{Chip, ChipID, Clock, ClockEvent}

import java.io.{ObjectInputStream, ObjectOutputStream}

class SID(override val startAddress:Int = 0xd400,sidID:Int = 1,externalDriver:Option[AudioDriverDevice] = None) extends Chip with SIDDevice {
  override lazy val componentID: String = "SID_" + sidID
  private[this] final val endAddress = startAddress + 0x20
  private[this] final val SAMPLE_RATE = 44100
  private[this] var CPU_FREQ = 985248
  private[this] var CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
  private[this] var CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
  
  val id: ID = ChipID.SID
  val name = "SID"
  val length = 1024
  val isRom = false
  
  val isActive = true
  
  private[this] val sid : SIDChip = {
    val sid = new RESID(Clock.systemClock)
    sid.setModel(0)
    sid
  }

  setCPUFrequency(Clock.systemClock.getClockHz)
  Clock.systemClock.addChangeFrequencyListener(setCPUFrequency _)

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
  private[this] var driver = externalDriver.getOrElse(new DefaultAudioDriver(SAMPLE_RATE, 100))
  private[this] val driverProxy : AudioDriverDevice = new AudioDriverDevice {
    override val sampleRate: Int = driver.sampleRate
    def getMasterVolume : Int = driver.getMasterVolume
    def setMasterVolume(v:Int): Unit = driver.setMasterVolume(v)
    def setSoundOn(on:Boolean): Unit = driver.setSoundOn(on)
    def addSample(sample:Int): Unit = driver.addSample(sample)
    def reset: Unit = driver.reset
    def discard: Unit = driver.discard
    def setMuted(muted: Boolean): Unit = driver.setMuted(muted)
    def isMuted : Boolean = driver.isMuted

    override def isSoundOn: Boolean = driver.isSoundOn
  }

  def setCPUFrequency(f:Double) : Unit = {
    CPU_FREQ = f.toInt
    CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE
    CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / SAMPLE_RATE).toInt - CLOCKS_PER_SAMPLE * 1000
    if (sid2 != null) sid2.setCPUFrequency(f)
  }

  def setCycleExact(ce:Boolean): Unit = {
    Clock.systemClock.cancel(componentID)

    cycleExact = ce
    if (sid2 != null) sid2.setCycleExact(ce)
    if (!fullSpeed) start
  }

  def setStereo(isStereo:Boolean,sid2:Option[SID] = None) : Unit = {
    if (this.sid2 != null) this.sid2.stop

    this.sid2 = sid2.getOrElse(null)
    if (this.sid2 != null) this.sid2.setCycleExact(cycleExact)
    externalDriver match {
      case None =>
        driver.discard
        driver = new DefaultAudioDriver(SAMPLE_RATE, 100,isStereo)
      case Some(_) =>
    }

    Clock.systemClock.cancel(componentID)
    start
  }
  
  def getDriver: AudioDriverDevice = driverProxy
  def init: Unit = start
  def reset: Unit = {
    if (sid2 != null) sid2.reset

    externalDriver match {
      case None =>
        driver.reset
      case _ =>
    }
    sid.reset()
    Clock.systemClock.cancel(componentID)
    start
  }
  
  def setModel(is6581:Boolean) : Unit = {
    if (is6581) {
      sid.setModel(0)
      if (sid2 != null) sid2.setModel(true)
    }
    else {
      sid.setModel(1)
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
  
  def setMouseEnabled(enabled:Boolean): Unit = mouseEnabled = enabled
  
  final def read(address: Int, chipID: ChipID.ID) : Int = decode(address) match {
    case -1 => sid2.read(address)
    case POTX_OFS =>
      val value = if (mouseEnabled) (MouseCage.x & 0x7F) << 1 else 0xFF
      sid.updateBusValue(value)
      value
    case POTY_OFS =>
      val value = if (mouseEnabled) (0x7F - (MouseCage.y & 0x7F)) << 1 else 0xFF
      sid.updateBusValue(value)
      value
    case ofs => sid.read(ofs)
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID): Unit = {
    decode(address) match {
      case -1 =>
        sid2.write(address,value)
      case ofs =>
        sid.write(ofs,value)
    }
  }

  private[this] val sidEventCallBack = sidEvent _

  private def sidEvent(cycles:Long) : Unit = {
    var nextSample = cycles + CLOCKS_PER_SAMPLE
    nextRest += CLOCKS_PER_SAMPLE_REST
    if (nextRest > 1000) {
      nextRest -= 1000
      nextSample += 1
    }

    val delta = cycles - lastCycles
    lastCycles = cycles
    var c = delta.toInt
    while (c > 0) {
      sid.clock()
      c -= 1
    }

    driver.addSample(sid.output)

    if (!removeSample) Clock.systemClock.schedule(new ClockEvent(componentID,nextSample,sidEventCallBack))
  }

  def clock: Unit = {
    sid.clock()
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
  
  def stop: Unit = {
    removeSample = true
    externalDriver match {
      case None =>
        driver.setSoundOn(false)
      case _ =>
    }
  }
  def start : Unit = {
    if (!driver.isMuted) {
      driver.setSoundOn(true)
      nextRest = 0
      nextSample = 0
      removeSample = false
      driver.reset
    }
    if (!cycleExact) Clock.systemClock.schedule(new ClockEvent(componentID,Clock.systemClock.currentCycles + 5,sidEventCallBack))
    lastCycles = Clock.systemClock.currentCycles
  }
  def setFullSpeed(full:Boolean) : Unit ={
    fullSpeed = full
    if (full) stop else start
  }
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    sid.saveState(out)
    out.writeBoolean(cycleExact)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    sid.loadState(in)
    cycleExact = in.readBoolean
    Clock.systemClock.cancel(componentID)
    start
  }
  protected def allowsStateRestoring : Boolean = true
}