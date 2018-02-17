package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.formats.MFM

class MFMRWHeadController(val name:String,_floppy:Floppy,ledListener:DriveLedListener) extends RWHeadController(_floppy,ledListener) {
  val componentID = name + "MFM rw head controller"
  
  canSetByteReady = true
  
  final def isSync : Boolean = floppy.nextByte == MFM.SYNC_MARK
  final def moveHead(moveOut: Boolean) {
    var direction = 0
    if (moveOut) {
      if (track > floppy.minTrack) direction = -1
    }
    else {
      if (track < floppy.maxTrack) direction = 1
    }
    if (direction != 0) {
      floppy.changeTrack(direction)
      track = floppy.currentTrack
    }
  }
  
  final def changeSide(side:Int) {
    floppy.side = side
    track = floppy.currentTrack
  }
  
  final protected def readNextBit {
    if (bitCounter == 8) {
      bitCounter = 0
      byteReady = true
      lastRead = floppy.nextByte    
    }
  }
  
  final protected def writeNextBit {
    if (bitCounter == 8) {
      bitCounter = 0
      floppy.writeNextByte(nextWrite)
      lastWrite = nextWrite
      byteReady = true
    }
  }
}