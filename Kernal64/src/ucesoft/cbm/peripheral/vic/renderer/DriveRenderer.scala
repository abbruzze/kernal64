package ucesoft.cbm.peripheral.vic.renderer

import ucesoft.cbm.Clock
import ucesoft.cbm.misc.VolumeSettingsPanel
import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.peripheral.sid.DefaultAudioDriver
import ucesoft.cbm.peripheral.vic.{Display, ExternalRenderer, Palette, VIC_I_Audio}

class DriveRenderer(bus: IECBus, display: Display, volumeSetting: VolumeSettingsPanel.VolumeSetting) extends ExternalRenderer {
  private final val HBORDER = 88 + 24
  private final val mem = display.displayMem
  private var pos = 0
  private final val BLACK = Palette.VIC_RGB(0)
  private final val WHITE = Palette.VIC_RGB(1)
  private final val clock = Clock.systemClock
  private var syncON = false
  private val clip = display.removeClipArea()
  private var syncStart, cyclesDetected, hSyncSize = 0
  private var stableSyncCount, stableSyncEnd = 0
  private var vsyncPending = false
  private var skipDrawCycles = 0
  private var warpOn = false
  private var lastVSyncCycle = 0L
  private var outOfVSyncCount = 0
  private val outOfVSyncThreshold = 312 * 64

  private class DriveAudio {
    private final val SAMPLE_RATE = 44100
    private final val CPU_FREQ = Clock.systemClock.getClockHz.toInt
    private final val CLOCKS_PER_SAMPLE = CPU_FREQ / SAMPLE_RATE

    private val driver = new DefaultAudioDriver(44100, 1)
    private var acc = 0
    private val volumeValues = Array(0, 42, 403, 420)
    private var volume = volumeValues(1)
    private val table = VIC_I_Audio.voltageFunction
    private var clocks = 0
    var isOn = false

    def setVolume(v: Int): Unit = volume = volumeValues(v & 3)

    volumeSetting.addDriver(driver)

    def clock(): Unit = {
      if (isOn) acc += volume

      clocks += 1

      if (clocks == CLOCKS_PER_SAMPLE) {
        val v = table(acc / CLOCKS_PER_SAMPLE).toInt
        driver.addSample(v)
        acc = 0
        clocks = 0
      }
    }

    def soundOn(on: Boolean): Unit = driver.setSoundOn(on)

    def stop(): Unit = {
      driver.discard()
      volumeSetting.removeDriver(driver)
    }
  }

  private val audio = new DriveAudio

  java.util.Arrays.fill(mem, BLACK)
  vsync()
  display.setClipArea(0, 0, 64 * 8 - HBORDER, 312)

  override def stop(): Unit = {
    clip match {
      case Some((p1, p2)) =>
        display.setClipArea(p1.x, p1.y, p2.x, p2.y)
    }
    audio.stop()
  }

  override def renderCycle(): Unit = {
    val warp = clock.maximumSpeed
    if (warp ^ warpOn) {
      warpOn = warp
      audio.soundOn(!warpOn)
    }
    audio.isOn = bus.freeSpinStepperOn
    audio.clock()

    val data = bus.data == IECBus.GROUND
    val clk = bus.clk == IECBus.GROUND
    var color = BLACK

    if (clk) {
      if (!syncON) {
        syncON = true
        cyclesDetected = (clock.currentCycles - syncStart).toInt
        syncStart = clock.currentCycles.toInt
        if (cyclesDetected > 64) {
          if (vsyncPending) {
            vsync()
            vsyncPending = false
            outOfVSyncCount = 0
          }
        } else skipDrawCycles = 1
      }
    }
    else {
      if (!data) color = WHITE
      if (syncON) syncOFF()
    }

    if (clock.currentCycles - lastVSyncCycle > outOfVSyncThreshold) {
      if (outOfVSyncCount > 10) vsync()
      else {
        outOfVSyncCount += 1
        lastVSyncCycle = clock.currentCycles
      }
    }

    draw(color)
  }

  private def syncOFF(): Unit = {
    syncON = false
    val newSize = (clock.currentCycles - syncStart).toInt
    if (newSize != hSyncSize) {
      hSyncSize = newSize
      if (stableSyncCount == 5) vsyncPending = true
      stableSyncCount = 0
    }
    else if (stableSyncCount < 5) {
      stableSyncCount += 1
      if (stableSyncCount == 5) stableSyncEnd = hSyncSize
    }
  }

  @inline private def vsync(): Unit = {
    display.showFrame(-1, 0, 0, 0)
    pos = 0
    skipDrawCycles = stableSyncEnd
    lastVSyncCycle = clock.currentCycles
  }

  @inline private def draw(color: Int): Unit = {
    if (skipDrawCycles > 0) {
      skipDrawCycles -= 1
      return
    }
    var i = 0
    while (i < 8 && pos < mem.length) {
      mem(pos) = color
      pos += 1
      i += 1
    }
  }
}
