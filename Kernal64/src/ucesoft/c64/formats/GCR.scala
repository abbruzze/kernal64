package ucesoft.c64.formats

import scala.collection.mutable.ListBuffer

private[formats] object GCR {
  private val SYNC = 0xFF
  private val SYNC_SIZE = 5
  private val HEADER_SIZE = 10
  private val GAP = 0x55
  private val HEADER_DATA_GAP_SIZE = 9  
  private val DATA_SIZE = 325
  private val GAP_AFTER_DATA_SIZE = 8
  private val GCR_SECTOR_SIZE = SYNC_SIZE + HEADER_SIZE + HEADER_DATA_GAP_SIZE + SYNC_SIZE + DATA_SIZE + GAP_AFTER_DATA_SIZE
  
  /**
   * Convert a gcr sector back to a disk format sector.
   */
  def GCR2sector(gcrSector:Array[Int]) = {
    val ungcr = new UNGCR
    val base = SYNC_SIZE + HEADER_SIZE + HEADER_DATA_GAP_SIZE + SYNC_SIZE - 1
    for(i <- 0 until DATA_SIZE) {
      ungcr.add(gcrSector(base + i))
    }
    val bytes = ungcr.getBytes
    val sector = Array.ofDim[Int](256)
    Array.copy(bytes,1,sector,0,256)
    sector
  }
  
  /**
   * Convert a whole sector to a GCR sector.
   */
  def sector2GCR(sector:Int,track:Int,sectorData:Array[Byte],diskID:String) = {
    val gcr = new GCR
    // SYNC
    for(_ <- 1 to SYNC_SIZE) gcr.add(SYNC)
    // HEADER
    val diskIDHi = diskID.charAt(0).toInt
    val diskIDLo = diskID.charAt(1).toInt
    val headerChecksum = sector ^ track ^ diskIDHi ^ diskIDLo
    gcr.addAndConvertToGCR(0x08)
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
    gcr.addAndConvertToGCR(0x07)
    var dataChecksum = -1
    for(i <- 0 until sectorData.length) {
      val b = sectorData(i).toInt & 0xFF
      if (dataChecksum == -1) dataChecksum = b else dataChecksum ^= b
      gcr.addAndConvertToGCR(b)
    }
    gcr.addAndConvertToGCR(dataChecksum)
    gcr.addAndConvertToGCR(0x00,0x00)
    // SECTOR-HEADER GAP
    for(_ <- 1 to GAP_AFTER_DATA_SIZE) gcr.add(GAP)
    gcr.getGCRBytes
  }  
}

private[formats]class UNGCR {
  import GCR._
  private[this] val tmpGCRBuffer = Array.ofDim[Int](5)
  private[this] val buffer = new ListBuffer[Int]
  private[this] var index = 0
  
  private[this] val GCR_TO_NIBBLE = Array(
      -1, -1, -1, -1, -1, -1, -1, -1,
      -1, 0x08, 0x00, 0x01, -1, 0x0c, 0x04, 0x05,
      -1, -1, 0x02, 0x03, -1, 0x0f, 0x06, 0x07,
      -1, 0x09, 0x0a, 0x0b, -1, 0x0d, 0x0e, -1
  )
  
  private def gcr2Byte(b:Int) = GCR_TO_NIBBLE((b >> 5) & 0x1F) << 4 | GCR_TO_NIBBLE(b & 0x1F)
  
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
  import GCR._
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
  def getGCRBytes = if (GCRBuffer.size == GCR_SECTOR_SIZE) GCRBuffer.toArray else throw new IllegalStateException("Buffer is not full.Current length is " + GCRBuffer.size)
}