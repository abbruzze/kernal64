package ucesoft.c64.peripheral.sid

import javax.sound.sampled._

class DefaultAudioDriver(sampleRate:Int,bufferSize:Int) extends AudioDriverDevice {
  private[this] val dataLine = {
    val af = new AudioFormat(sampleRate, 16, 1, true, false)
    val dli = new DataLine.Info(classOf[SourceDataLine], af, bufferSize)
    val dataLine = try {
      AudioSystem.getLine(dli).asInstanceOf[SourceDataLine] 
    }
    catch {
      case t:Throwable =>
        println("Warning: no audio available. Cause: " + t)
        null
    }
    
    if (dataLine != null) dataLine.open(dataLine.getFormat,bufferSize)
    dataLine    
  }
  private[this] val volume : FloatControl = if (dataLine != null) dataLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl] else null
  private[this] var vol = 0
  private[this] var soundOn = true
  
  setMasterVolume(100)
  if (dataLine != null) dataLine.start()
  
  def getMasterVolume = vol
  def setMasterVolume(v:Int) {
    if (volume != null) {
      val max = volume.getMaximum
      val min = volume.getMinimum / 2f
      volume.setValue((v / 100.0f) * (max - min) + min)
      vol = v
    }
  }
  final def write(buffer:Array[Byte]) {
    val bsize = buffer.length
    if (dataLine == null || dataLine.available < bsize) return
    
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
}