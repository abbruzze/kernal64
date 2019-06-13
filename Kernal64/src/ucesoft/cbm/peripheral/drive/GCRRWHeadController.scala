package ucesoft.cbm.peripheral.drive

import java.io.ObjectOutputStream
import java.io.ObjectInputStream

class GCRRWHeadController(val name:String,_floppy:Floppy,ledListener:DriveLedListener) extends RWHeadController(_floppy,ledListener) {
  val componentID = name + "GCR rw head controller"
  
  private[this] var trackSteps = track << 1      
  private[this] var last10Bits = 0
  
  override def reset {
    super.reset
    trackSteps = 2
    last10Bits = 0
  }
  
  final def changeSide(side:Int) {
    if (!floppy.singleSide) {
      floppy.side = side
      track = floppy.currentTrack
      val oldTrackSteps = trackSteps
      trackSteps = (track << 1) | (oldTrackSteps & 1)
    }
  }
  
  final override def setFloppy(newFloppy:Floppy) {
    super.setFloppy(newFloppy)
    floppy.changeTrack(trackSteps)
  }
  final def isSync : Boolean = !isWriting && motorOn && last10Bits == 0x3FF
  final def moveHead(moveOut: Boolean) {
    var trackMoved = false
    if (moveOut) {
      if (track > floppy.minTrack) {
        trackSteps -= 1
        trackMoved = true
      }
    }
    else {
      if (track < floppy.maxTrack) {
        trackSteps += 1
        trackMoved = true
      }
    }
    if (trackMoved) {
      floppy.changeTrack(trackSteps)
      track = floppy.currentTrack
    }
  }
  final protected def readNextBit {
    val bit = floppy.nextBit
    last10Bits = ((last10Bits << 1) | bit) & 0x3FF
    if (last10Bits == 0x3FF) {
      bitCounter = 0
      byteReadySignal = 1
    }
    if (bitCounter == 8) {
      bitCounter = 0
      lastWrite = lastRead
      byteReady = true
      lastRead = last10Bits & 0xFF    
      byteReadySignal = 0
    }
  }
  final protected def writeNextBit {
    floppy.writeNextBit((lastWrite & 0x80) > 0)
    lastWrite <<= 1        
    if (bitCounter == 8) {
      bitCounter = 0
      lastWrite = nextWrite
      byteReady = true
      byteReadySignal = 0
    }
  }
  // ======================================================================
  
  override final protected def saveState(out:ObjectOutputStream) {
    super.saveState(out)
    out.writeInt(trackSteps)
    out.writeInt(last10Bits)
  }
  override final protected def loadState(in:ObjectInputStream) {
    super.loadState(in)
    trackSteps = in.readInt
    last10Bits = in.readInt
    
  }
}