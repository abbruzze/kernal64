package ucesoft.cbm.peripheral.sid

import javax.sound.sampled._

class DefaultAudioDriver(override val sampleRate:Int,bufferSizeInMillis:Int,isStereo:Boolean = false) extends AudioDriverDevice {
  private[this] val dataLine = {
    val af = new AudioFormat(sampleRate.toFloat, 16,if (isStereo) 2 else 1, true, false)
    val dli = new DataLine.Info(classOf[SourceDataLine], af)
    val dataLine = try {
      AudioSystem.getLine(dli).asInstanceOf[SourceDataLine]
    }
    catch {
      case t:Throwable =>
        println("Warning: no audio available. Cause: " + t)
        null
    }

    if (dataLine != null) dataLine.open(dataLine.getFormat)
    dataLine
  }
  private[this] val volume : FloatControl = if (dataLine != null) dataLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl] else null
  private[this] var vol = 0
  private[this] val buffer = Array.ofDim[Byte]((if (isStereo) 2 else 1) * 2 * (sampleRate * bufferSizeInMillis / 1000.0).toInt)
  private[this] var pos = 0
  private[this] var muted = false
  private[this] var soundOn = true
  
  setMasterVolume(100)
  if (dataLine != null) dataLine.start()
  
  def getMasterVolume: Int = vol
  def setMasterVolume(v:Int) : Unit = {
    if (volume != null) {
      val max = volume.getMaximum
      val min = volume.getMinimum / 2f
      volume.setValue((v / 100.0f) * (max - min) + min)
      vol = v
    }
  }
  final def addSample(sample:Int) : Unit = {
    if (dataLine == null || !soundOn) return

    buffer(pos) = (sample & 0xff).toByte ; pos += 1
    buffer(pos) = (sample >> 8).toByte ; pos += 1
    if (pos == buffer.length) {      
      pos = 0
      val av = dataLine.available()
      val bsize = if (av < buffer.length) av else buffer.length
      dataLine.write(buffer, 0, bsize)
    }
  }
  final def reset() : Unit = {
    pos = 0
    if (dataLine != null) dataLine.flush()
    setSoundOn(true)
  }
  def discard() : Unit = {
    if (dataLine != null) {
      dataLine.stop()
      dataLine.flush()
    }
  }
  def setSoundOn(on:Boolean) : Unit = {
    soundOn = on
    updateLine()
  }

  private def updateLine() : Unit = {
    if (dataLine != null) {
      if (soundOn && !muted) dataLine.start()
      else {
        dataLine.stop()
        dataLine.flush()
        pos = 0
      }
    }
  }

  override def setMuted(muted: Boolean): Unit = {
    this.muted = muted
    updateLine()
  }

  override def isMuted: Boolean = muted

  override def isSoundOn: Boolean = soundOn
}