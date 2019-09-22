package ucesoft.cbm.formats

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[formats] object GCR {
  final private[this] val SYNC = 0xFF
  final private[this] val SYNC_SIZE = 5
  final private[this] val GAP = 0x55
  final private[this] val HEADER_DATA_GAP_SIZE = 9  
  final private[this] val LAST_SECTOR_GAP = 180
  final private[this] val INTER_SECTOR_GAPS_PER_ZONE = Array(9, 12, 17, 8)
  
  final private[this] val DOS_20_HEADER_DESC_BYTE_NOT_FOUND = 2
  final private[this] val DOS_21_NO_SYNC_SEQ_FOUND = 3
  final private[this] val DOS_27_CHECKSUM_ERROR_IN_HEADER_BLOCK = 9
  final private[this] val DOS_29_DISK_SECTOR_ID_MISMATCH = 0x0B
  final private[this] val DOS_22_DATA_DESC_BYTE_NOT_FOUND = 4
  final private[this] val DOS_23_CHECKSUM_ERROR_IN_DATA_BLOCK = 5
  
  final val EMPTY_GCR_SECTOR = Array.ofDim[Int](400)
  
  @inline private def getZoneFrom(track:Int) = {
    if (track <= 17) 0
    else if (track <= 24) 1
    else if (track <= 30) 2
    else 3
  } 
  
  /**
   * Convert a gcr track back to a disk format sectors.
   */
  def GCR2track(gcrTrack:Array[Int],_sectorSize:Int,writeSector : (Int,Int,Array[Int]) => Unit) = {
    val ungcr = new UNGCR
    var i = 0
    var readData = 0
    var sectorsFound = 0
    var dataBuffer = Array.ofDim[Int](256)
    var sync = false
    var dataFound = false
    var headerFound = false
    var dataBufferIndex = 0 
    var bits = 0
    var track,sector = 0
    var round = 0
    val sectorSize = if (_sectorSize == -1) Int.MaxValue else _sectorSize
    val sectorsSet = new mutable.HashSet[Int]()
    var finished = false
    
    while (sectorsFound < sectorSize && !finished) {
      var data = gcrTrack(i)
      var b = 0
      while (b < 8) {
        readData <<= 1
        if ((data & 0x80) == 0x80) readData |= 0x1
        
        if (headerFound) {
          bits += 1
          if (bits == 10) {
            readData &= 0x3FF
            dataBuffer(dataBufferIndex) = ungcr.gcr2Byte(readData)
            dataBufferIndex += 1
            bits = 0
            if (dataBufferIndex == 5) {
              headerFound = false
              sector = dataBuffer(1)
              track = dataBuffer(2)
              if (_sectorSize == -1) {
                if (!sectorsSet.add(sector))
                  finished = true
              }
              //println(s"Found header sector=${sector} track=${track}")              
            }
          }
        }
        else
        if (dataFound) {
          bits += 1
          if (bits == 10) {
            readData &= 0x3FF
            dataBuffer(dataBufferIndex) = ungcr.gcr2Byte(readData)
            dataBufferIndex += 1
            bits = 0
            if (dataBufferIndex == 256) {
              writeSector(track,sector,dataBuffer)
              sectorsFound += 1
              dataFound = false
              dataBufferIndex = 0
              dataBuffer = Array.ofDim[Int](256)
              readData = 0
              bits = 0
            }
          }
        }
        else
        if (sync) {
          readData &= 0xFFFFF
          if (readData == 1047895) { // 10 1's 01010 10111 = 7
            dataFound = true
            dataBufferIndex = 0
            bits = 0
            sync = false
          }
          else
          if (readData == 1047881) { // 10 1's 01010 01001 = 8
            sync = false 
            headerFound = true
            dataBufferIndex = 0
            bits = 0
          }
        }
        else {
          readData &= 0x3FF // 10 1's
          if (readData == 0x3FF) sync = true          
        }
        
        b += 1
        data <<= 1
      }
      i += 1
      if (i == gcrTrack.length) {
        i = 0
        round += 1
        if (round == 2) throw new IllegalArgumentException("Cannot write changes on disk: invalid format")
      }
    }
  }
  
  /**
   * Convert a whole sector to a GCR sector.
   */
  def sector2GCR(sector:Int,track:Int,sectorData:Array[Byte],diskID:String,sectorError:Option[Int],isLastSector:Boolean) = {
    val sectorErrorCode = sectorError.getOrElse(0)
    val gcr = new GCR
    // SYNC
    for(_ <- 1 to SYNC_SIZE) gcr.add(SYNC)
    // HEADER
    val diskIDHi = if (sectorErrorCode == DOS_29_DISK_SECTOR_ID_MISMATCH) 0xFF else diskID.charAt(0).toInt
    val diskIDLo = diskID.charAt(1).toInt
    var headerChecksum = sector ^ track ^ diskIDHi ^ diskIDLo
    if (sectorErrorCode == DOS_27_CHECKSUM_ERROR_IN_HEADER_BLOCK) headerChecksum ^= 0xFF
    gcr.addAndConvertToGCR(if (sectorErrorCode == DOS_20_HEADER_DESC_BYTE_NOT_FOUND) 0x00 else 0x08)
    gcr.addAndConvertToGCR(headerChecksum)
    gcr.addAndConvertToGCR(sector)
    gcr.addAndConvertToGCR(track)
    gcr.addAndConvertToGCR(diskIDLo)
    gcr.addAndConvertToGCR(diskIDHi)
    gcr.addAndConvertToGCR(0x0F,0x0F)
    // HEADER-DATA GAP
    for(_ <- 1 to HEADER_DATA_GAP_SIZE) gcr.add(GAP)
    // DATA
    for(_ <- 1 to SYNC_SIZE) gcr.add(SYNC)
    gcr.addAndConvertToGCR(if (sectorErrorCode == DOS_22_DATA_DESC_BYTE_NOT_FOUND) 0x00 else 0x07)
    var dataChecksum = 0
    for(i <- 0 until sectorData.length) {
      val b = if (sectorErrorCode == DOS_21_NO_SYNC_SEQ_FOUND) 0 else sectorData(i).toInt & 0xFF
      if (dataChecksum == 0) dataChecksum = b else dataChecksum ^= b
      gcr.addAndConvertToGCR(b)
    }
    if (sectorErrorCode == DOS_23_CHECKSUM_ERROR_IN_DATA_BLOCK) dataChecksum ^= 0xFF
    gcr.addAndConvertToGCR(dataChecksum)
    gcr.addAndConvertToGCR(0x00,0x00)
    // INTER-SECTOR GAP
    if (isLastSector) for(_ <- 1 to LAST_SECTOR_GAP) gcr.add(GAP)
    else for(_ <- 1 to INTER_SECTOR_GAPS_PER_ZONE(getZoneFrom(track))) gcr.add(GAP)
    gcr.getGCRBytes
  }  
}

private[formats]class UNGCR {
  private[this] val tmpGCRBuffer = Array.ofDim[Int](5)
  private[this] val buffer = new ListBuffer[Int]
  private[this] var index = 0
  
  private[this] val GCR_TO_NIBBLE = Array(
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0x08, 0x00, 0x01, 0, 0x0c, 0x04, 0x05,
      0, 0, 0x02, 0x03, 0, 0x0f, 0x06, 0x07,
      0, 0x09, 0x0a, 0x0b, 0, 0x0d, 0x0e, 0
  )
  
  def gcr2Byte(b:Int) = GCR_TO_NIBBLE((b >> 5) & 0x1F) << 4 | GCR_TO_NIBBLE(b & 0x1F)
  
  def add(b:Int) {
    tmpGCRBuffer(index) = b
    index += 1
    if (index == 5) {
      index = 0
      val b1 = tmpGCRBuffer(0) << 2 | tmpGCRBuffer(1) >> 6			// first 10 bits  (8,2)
      val b2 = (tmpGCRBuffer(1) & 0x3F) << 4 | tmpGCRBuffer(2) >> 4	// second 10 bits (6,4)
      val b3 = (tmpGCRBuffer(2) & 0x0F) << 6 | tmpGCRBuffer(3) >> 2	// third 10 bits  (4,6)
      val b4 = (tmpGCRBuffer(3) & 0x03) << 8 | tmpGCRBuffer(4)		// fourth 10 bits (2,8)
      
      buffer += gcr2Byte(b1)
      buffer += gcr2Byte(b2)
      buffer += gcr2Byte(b3)
      buffer += gcr2Byte(b4)
    }
  }
  
  def getBytes = buffer.toArray
}

private[formats]class GCR private {
  private[this] val tmpGCRBuffer = Array.ofDim[Int](4)
  private[this] val GCRBuffer = new ListBuffer[Int]
  private[this] var tmpGCRIndex = 0
  /*   
    GCR Coding Table
	Nybble		GCR
	0  0	0000	01010	 a 10
	1  1	0001	01011	 b 11
	2  2	0010	10010	12 18
	3  3	0011	10011	13 19
	4  4	0100	01110	 e 14
	5  5	0101	01111	 f 15
	6  6	0110	10110	16 22
	7  7	0111	10111	17 23
	8  8	1000	01001	 9  9
	9  9	1001	11001	19 25
	a 10	1010	11010	1a 26
	b 11	1011	11011	1b 27
	c 12	1100	01101	 d 13
	d 13	1101	11101	1d 29
	e 14	1110	11110	1e 30
	f 15	1111	10101	15 21
   */
  private[this] val NIBBLE_TO_GCR = Array(
    0x0a, 0x0b, 0x12, 0x13,
    0x0e, 0x0f, 0x16, 0x17,
    0x09, 0x19, 0x1a, 0x1b,
    0x0d, 0x1d, 0x1e, 0x15
  )
  /*
   * Convert a byte into a 10 gcr bits.
   */
  private def byte2GCR(value:Int) = NIBBLE_TO_GCR((value >> 4) & 0x0F) << 5 | NIBBLE_TO_GCR(value & 0x0F)
  
  /**
   * Add a byte without conversion to GCR
   */
  def add(bs:Int*) = for(b <- bs) GCRBuffer += b
  
  def addAndConvertToGCR(bs:Int*) {
    for(b <- bs) {
      tmpGCRBuffer(tmpGCRIndex) = byte2GCR(b)
      tmpGCRIndex += 1
      if (tmpGCRIndex == 4) {
        tmpGCRIndex = 0
        GCRBuffer += (tmpGCRBuffer(0) >> 2) & 0xFF // high 8 bits of 0
        GCRBuffer += (tmpGCRBuffer(0) & 0x3) << 6 | ((tmpGCRBuffer(1) >> 4) & 0x3F) // lo 2 bits of 0 + high 6 bits of 1 
        GCRBuffer += (tmpGCRBuffer(1) & 0x0F) << 4 | ((tmpGCRBuffer(2) >> 6) & 0x0F) // lo 4 bits of 1 + high 4 bits of 2
        GCRBuffer += (tmpGCRBuffer(2) & 0x3F) << 2 | ((tmpGCRBuffer(3) >> 8) & 0x3) // lo 6 bits of 2 + high 2 bits of 3
        GCRBuffer += tmpGCRBuffer(3) & 0xFF // lo 8 bits of 3
      }
    }
  }
  /**
   * Get the final GCR buffer.
   */
  def getGCRBytes = GCRBuffer.toArray// else throw new IllegalStateException("Buffer is not full.Current length is " + GCRBuffer.size)
}