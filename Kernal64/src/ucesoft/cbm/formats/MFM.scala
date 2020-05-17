package ucesoft.cbm.formats

object MFM {
  /**
   * MFM Encoding
   * Hex 4E written as a gap, with 10 sectors of data, with full gaps written for motor speed variation.
      12 Bytes of 00
      3 Bytes of Hex A 1 (Data Hex A 1, Clock Hex OA)
      1 Byte of FE (ID Address Mark)
      1 Byte (Track Number)
      1 Byte (Side Number)
      1 Byte (Sector Number)
      1 Byte (Sector Length, 02 for 512 Byte Sectors)
      2 Bytes CRC (Cyclic Redundancy Check)
      22 Bytes of Hex 4E
      12 Bytes of 00
      3 Bytes of Hex A 1 (Data Hex A 1, Clock Hex OA)
      1 Byte of Hex FB (Data Address Mark)
      512 Bytes of Data
      2 Bytes of CRC (Cyclic Redundancy Check)
      38 Bytes of Hex 4E
      ----------------------------------------
      612 bytes per sector
   */
  private[this] final val INITIAL_TRACK_GAP_LEN = 80 + 12 + 4 + 50 // 80 x 4E,12 x 0,3 x C2,1 x FC,50 x 4E
  final val MAX_SECTORS = 10
  final val TRACK_SIZE = (100 + 1024 + INITIAL_TRACK_GAP_LEN) * MAX_SECTORS // max track size when sector size is 1024
  final val SYNC_MARK = 0x1A1
  final val SYNC_MARK_HEADER_NEXT = 0xFE
  final val SYNC_MARK_DATA_NEXT = 0xFB
  final val SYNC_INDEX_MARK = 0x1C2
  final val SYNC_INDEX_MARK_NEXT = 0xFC
  final val FILL_MARK = 0x4E
  
  /*
   * Sector sizes
   * 
    00 128 bytes per sector
    01 256 bytes per sector
    02 512 bytes per sector
    03 1024 bytes per sector
   */
  final val SECTOR_SIZE = Array(128,256,512,1024)
  
  private[this] val crcCache = {
    val cache = Array.ofDim[Int](256)
    for(i <- 0 to 255) {
      var w = i << 8
      for(_ <- 0 to 7) {
        if ((w & 0x8000) == 0x8000) {
          w <<= 1
          w ^= 0x1021
        }
        else w <<= 1
      }
      cache(i) = w & 0xFFFF
    }
    cache
  }
  
  final def crc(b:Int,crc:Int = 0xFFFF) : Int = {
    val _crc = crc & 0xFFFF
    val _b = b & 0xFF
    (crcCache(_crc >> 8 ^ _b) ^ _crc << 8) & 0xFFFF
  }
  
  private def fill(track:Array[Int],offset:Int,value:Int,size:Int) : Int = {
    var newOffset = offset
    while (newOffset < offset + size) {
      track(newOffset) = value
      newOffset += 1
    }
    newOffset
  }
  
  /**
   * head,track,sector
   */
  def physicalTrackToLogical(head:Int,track:Array[Int],writeLogical:(Int,Int,Int,Array[Int]) => Unit) : Unit = {
    var offset = 0
    var sectorsEncountered = 0
    var round = 0
    var index = 0
    val header = Array(0,0,0) // t,h,s
    val data = Array.ofDim[Int](512)
    var headerFound = false
    var dataFound = false
    var lastTwo = 0
    val HEADER = SYNC_MARK << 8 | SYNC_MARK_HEADER_NEXT
    val DATA = SYNC_MARK << 8 | SYNC_MARK_DATA_NEXT
    
    while (sectorsEncountered < 10) {
      val byte = track(offset)
      lastTwo <<= 8
      lastTwo |= byte
      if (dataFound) {
        data(index) = byte
        index += 1
        if (index == 512) {
          dataFound = false
          writeLogical(head,header(0),header(2),data)
          sectorsEncountered += 1
        }
      }
      else
      if (headerFound) {
        header(index) = byte                
        index += 1        
        if (index == 3) headerFound = false
      }
      else {        
        lastTwo & 0x1FFFF match {
          case HEADER =>
            index = 0
            headerFound = true
            dataFound = false
          case DATA => 
            index = 0
            dataFound = true
            headerFound = false
          case _ =>
        }
      }
      offset += 1
      if (offset == track.length) {
        offset = 0
        round += 1
        if (round > 1) throw new IllegalArgumentException("Invalid MFM disk format")
      }
    }
  }
  
  def buildPhysicalTrack(side:Int,sectorSize:Int,trackID:Int,track:Array[Int],readLogical512:(Int,Int,Int) => Array[Int]) : Unit = {
    var offset = fill(track,0,FILL_MARK,80) // initial track filler
    offset = fill(track,offset,0x00,12)
    offset = fill(track,offset,SYNC_INDEX_MARK,3)
    offset = fill(track,offset,SYNC_INDEX_MARK_NEXT,1)
    offset = fill(track,offset,FILL_MARK,50)
    for(sector <- 1 to 10) {
      offset = fill(track,offset,0x00,12)
      // ================ ID FIELD =====================================
      offset = fill(track,offset,SYNC_MARK,3)
      offset = fill(track,offset,SYNC_MARK_HEADER_NEXT,1)
      offset = fill(track,offset,trackID,1)
      offset = fill(track,offset,side,1)
      offset = fill(track,offset,sector,1)
      offset = fill(track,offset,sectorSize,1)
      var _crc = crc(trackID,45616) // 45616 = crc of A1 x 3, FE x 1
      _crc = crc(side,_crc)
      _crc = crc(sector,_crc)
      _crc = crc(sectorSize,_crc)
      offset = fill(track,offset,_crc >> 8,1)
      offset = fill(track,offset,_crc & 0xFF,1)
      // ===============================================================
      offset = fill(track,offset,FILL_MARK,22)
      offset = fill(track,offset,0x00,12)
      // ================ DATA FIELD ===================================
      offset = fill(track,offset,SYNC_MARK,3)
      offset = fill(track,offset,SYNC_MARK_DATA_NEXT,1)
      // ==================== DATA =====================================
      _crc = 58005 // 58005 crc of A1 x 3, FB x 1
      val logicalSector = readLogical512(side,trackID,sector)
      //println(s"H${side}T${trackID}S${sector}")
      for(d <- logicalSector) {        
        //print(s"'${d.toChar}' ")
        offset = fill(track,offset,d,1)
        _crc = crc(d,_crc)
      }
      //println
      offset = fill(track,offset,_crc >> 8,1)
      offset = fill(track,offset,_crc & 0xFF,1)
      offset = fill(track,offset,FILL_MARK,38)
      // ===============================================================
    }
  }  
}