package ucesoft.cbm.formats

import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.RandomAccessFile

private[formats] class D81(val file: String) extends Diskette {
  import Diskette._
  val canBeEmulated = false
  val isReadOnly = false
  val totalTracks = 80
  override lazy val singleSide = true
  override protected final val DIR_TRACK = 40
  override protected final val DIR_SECTOR = 3
  override protected final val BAM_SECTOR = 1
  private[this] final val HEADER_SECTOR = 0
  
  override final val minTrack = 1
  override final val maxTrack = totalTracks
  
  // D81
  private[this] final val D81_DISK_SIZE_80_TRACKS = 819200
  private[this] final val D81_DISK_SIZE_80_TRACKS_WITH_ERRORS = 822400
  
  override protected def absoluteSector(t: Int, s: Int) = (t - 1) * 40 + s
  
  def bam : BamInfo = {
    disk.seek(absoluteSector(DIR_TRACK,HEADER_SECTOR) * BYTES_PER_SECTOR + 4)
    val diskName = new StringBuilder
    var i = 0
    while (i < 16) {
      val c = disk.readByte.toInt & 0xFF
      if (c != 0xA0) diskName.append(c.toChar)
      i += 1
    }
    disk.skipBytes(2)
    val diskID = "" + disk.readByte.toChar + disk.readByte.toChar
    disk.skipBytes(1)
    val dosType = "" + disk.readByte.toChar + disk.readByte.toChar
    disk.seek(absoluteSector(DIR_TRACK,BAM_SECTOR) * BYTES_PER_SECTOR + 4)

    var free = 0
    for(i <- 1 to 2) {
      disk.seek(absoluteSector(DIR_TRACK,i) * BYTES_PER_SECTOR + 16)
      for(t <- 1 to 40) {
        val f = disk.read & 0xFF
        free += (if (t == 40 && i == 1) 0 else f)
        disk.skipBytes(5)
      }
    }
    BamInfo(diskName.toString, diskID, dosType,true,free)
  }
    
  private[this] var _side = 0
  private[this] var track = 1
  private[this] var trackOffset = 0
  private[this] var trackChangeListener : TrackListener = null
  protected val disk = new RandomAccessFile(file, "rw")
  private[this] val physicalTracks : Array[Array[Array[Int]]] = {
    val sides = Array.ofDim[Array[Array[Int]]](2)
    for(h <- 0 to 1) {
      sides(h) = Array.ofDim[Array[Int]](80) // 80 tracks per side
      for(t <- 0 to 79) {
        sides(h)(t) = Array.fill(MFM.TRACK_SIZE)(MFM.FILL_MARK)
      }
    }
    sides
  }
  
  // ================================= CONSTRUCTOR =======================================
  if (disk.length != D81_DISK_SIZE_80_TRACKS && disk.length != D81_DISK_SIZE_80_TRACKS_WITH_ERRORS) {
    close
    throw new IllegalArgumentException("Invalid D81 format")
  }
  init
  // ======================================================================================
  
  /**
   * file offset       | CBM logical  |  drive physical   |   specials
     decimal sedecimal | track/sector | cyl head sec offs |
     ------------------+--------------+-------------------+--------------
           0 0x000000  |    01;00     |   00;01;01        | first block
         256 0x000100  |    01;01     |   00;01;01   +256 |
           .     .     |      .       |     .  .          |
        4864 0x001300  |    01;19     |   00;01;10   +256 |
        5120 0x001400  |    01;20     |   00;00;01        |
           .     .     |      .       |     .  .          |
        9984 0x002700  |    01;39     |   00;00;10   +256 |
       10240 0x002800  |    02;00     |   01;01;01        |
           .     .     |      .       |     .  .          |
       15360 0x003C00  |    02;20     |   01;00;01        |
           .     .     |      .       |     .  .          |
       20480 0x005000  |    03;00     |   02;01;01        |
           .     .     |      .       |     .  .          |
           .     .     |      .       |     .  .          |
       30729 0x007800  |    04;00     |   03;01;01        |
           .     .     |      .       |     .  .          |
           .     .     |      .       |     .  .          |
           .     .     |      .       |     .  .          |
      399360 0x061800  |    40;00     |   39;01;01        | disk header
      399616 0x061900  |    40;01     |   39;01;01   +256 | 1st BAM block
      399872 0x061A00  |    40;02     |   39;01;02        | 2nd BAM block
      400128 0x061B00  |    40;03     |   39;01;02   +256 | 1st dir block
           .     .     |      .       |     .  .          |
      409600 0x064000  |    41;00     |   40;01;01        |
           .     .     |      .       |     .  .          |
           .     .     |      .       |     .  .          |
           .     .     |      .       |     .  .          |
      808960 0x0C5800  |    80;00     |   79;01;01        |
           .     .     |      .       |     .  .          |
      813824 0x0C6B00  |    80;19     |   79;01;10   +256 |
      814080 0x0C6C00  |    80;20     |   79;00;01        |
           .     .     |      .       |     .  .          |
      818688 0x0C7E00  |    80;38     |   79;00;10        |
      818944 0x0C7F00  |    80;39     |   79;00;10   +256 | last block
   * 
   */
  /**
   * 0 <= t <= 79
   * 1 <= s <= 10
   * 1 <= h <= 0
   */
  @inline private def p2l(h:Int,t:Int,s:Int) : Array[Int] = {
    var offset = t * 0x2800
    if (h == 0) offset += 20 * 256
    offset += (s - 1) * 2 * 256
    val buffer = Array.ofDim[Byte](512)
    disk.seek(offset)
    disk.readFully(buffer)    
    buffer map { _.toInt & 0xFF }
  }
    
  private def init {
    for(side <- 0 to 1;
        track <- 0 to 79) {
      MFM.buildPhysicalTrack(1 - side,2,track,physicalTracks(1 - side)(track),p2l _)
    }
  }
  
  override def isOnIndexHole = trackOffset < 5
  
  final override def side = _side
  final override def side_=(newSide:Int) =_side = newSide
  
  final def nextByte : Int = {
    val byte = physicalTracks(_side)(track - 1)(trackOffset)
    trackOffset = (trackOffset + 1) % physicalTracks(_side)(track).length
    byte
  }
  final def writeNextByte(b:Int) {
    physicalTracks(_side)(track - 1)(trackOffset) = b
    trackOffset = (trackOffset + 1) % physicalTracks(_side)(track).length
  }
  
  final def nextBit : Int = ???
  final def writeNextBit(bit:Boolean) = ???
  
  final def currentTrack = track
  final def currentSector = None
  /**
   * trackSteps > 0 inc track
   * trackSteps < 0 dec track
   */
  final def changeTrack(trackSteps:Int) {
    if (trackSteps > 0) {
      if (track < maxTrack) track += 1 
    }
    else {
      if (track > minTrack) track -= 1
    }
    notifyTrackSectorChangeListener
  }
  final def setTrackChangeListener(l:TrackListener) = trackChangeListener = l
  final def notifyTrackSectorChangeListener = if (trackChangeListener != null) trackChangeListener(track,false,None)
  
  override def defaultZoneFor(track:Int) = 4 // 250.0000 bit/sec
  
  override def flush {
    // TODO
  }
  
  final def close {
    flush
    disk.close
  }
  final def reset {
    _side = 0
    track = 0
    trackOffset = 0
  }
  // state
  final def save(out:ObjectOutputStream) {
    
  }
  final def load(in:ObjectInputStream) {
    
  }
}

//object D81 extends App {
//  val d81 = new D81("/Users/ealeame/Desktop/C64/C128/graphicbooster_c_128.d81")
//  d81.init
//  for((b,i) <- d81.physicalTracks(0)(39).zipWithIndex) {
//    println("#" + i + " " + b.toHexString)
//    io.StdIn.readLine
//  }
//}