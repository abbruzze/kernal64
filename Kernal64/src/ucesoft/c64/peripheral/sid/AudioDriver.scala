package ucesoft.c64.peripheral.sid

import javax.sound.sampled._

class AudioDriver(sampleRate:Int,bufferSize:Int) extends AudioDriverDevice {
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
  
  setMasterVolume(100)
  dataLine.start()
  
  def getMasterVolume = vol
  def setMasterVolume(v:Int) {
    val max = volume.getMaximum
    val min = volume.getMinimum / 2f
    if (volume != null) volume.setValue((v / 100.0f) * (max - min) + min)//-10.0f + 0.1f * v)
    vol = v
  }
  def shutdown = if (dataLine != null) dataLine.close
  final def write(buffer:Array[Byte],bsize:Int) {    
    if (!soundOn) {
      var i = 0
      while (i < bsize) {
        buffer(i) = 0
        i += 1
      }
    }
    dataLine.write(buffer, 0, bsize)
  }
  def setSoundOn(on:Boolean) = soundOn = on
}