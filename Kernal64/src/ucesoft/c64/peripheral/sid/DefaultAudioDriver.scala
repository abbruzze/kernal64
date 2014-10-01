package ucesoft.c64.peripheral.sid

import javax.sound.sampled._

class DefaultAudioDriver(sampleRate:Int,bufferSize:Int) {
  private[this] val dataLine = {
    val af = new AudioFormat(sampleRate, 16, 1, true, false)
    val dli = new DataLine.Info(classOf[SourceDataLine], af, bufferSize)
    val dataLine = AudioSystem.getLine(dli).asInstanceOf[SourceDataLine]
    if (dataLine != null) dataLine.open(dataLine.getFormat,bufferSize)
    dataLine    
  }
  private[this] val volume : FloatControl = if (dataLine != null) dataLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl] else null
  private[this] var vol = 0
  private[this] var soundOn = true
  private[this] var isFullSpeed = false
  
  setMasterVolume(100)
  dataLine.start()
  
  def available = dataLine.available
  def getMasterVolume = vol
  def getMicros = dataLine.getMicrosecondPosition
  def hasSound = dataLine != null
  def setMasterVolume(v:Int) {
    if (volume != null) volume.setValue(-10.0f + 0.1f * v)
    vol = v
  }
  def shutdown = if (dataLine != null) dataLine.close
  final def write(buffer:Array[Byte]) {
    val bsize = buffer.length
    if (!isFullSpeed) while (dataLine.available < bsize) Thread.sleep(0,500)
    else 
    if (dataLine.available < bsize) return
    
    if (!soundOn) {
      var i = 0
      while (i < buffer.length) {
        buffer(i) = 0
        i += 1
      }
    }
    dataLine.write(buffer, 0, bsize)
  }
  def setSoundOn(on:Boolean) = soundOn = on
  def setFullSpeed(full:Boolean) = isFullSpeed = full
  def fullSpeed = isFullSpeed
}