package ucesoft.cbm.peripheral.vic

object VICType extends Enumeration {
  val PAL: VICType.Value = Value
  val NTSC: VICType.Value = Value
}

sealed trait VICModel {
  val VIC_TYPE: VICType.Value
  val CPU_FREQ: Double
  val RASTER_LINES: Int
  val RASTER_CYCLES: Int
  val BLANK_TOP_LINE: Int
  val BLANK_BOTTOM_LINE: Int
  val BLANK_LEFT_CYCLE: Int
  val BLANK_RIGHT_CYCLE: Int
  val RASTER_OFFSET = 0
  val MAX_COLUMNS = 0
  val MAX_ROWS = 0
}

object VIC_I_PAL extends VICModel {
  override final val VIC_TYPE = VICType.PAL
  override final val CPU_FREQ = 1_108_405.0d
  override final val RASTER_LINES = 312
  override final val RASTER_CYCLES = 71
  override final val BLANK_TOP_LINE = 28
  override final val BLANK_BOTTOM_LINE = 312
  override final val BLANK_LEFT_CYCLE = 6
  override final val BLANK_RIGHT_CYCLE = 62//64
  override final val MAX_COLUMNS = 32
  override final val MAX_ROWS = 37
}

object VIC_I_NTSC extends VICModel { // TODO to check if these values are correct
  override final val VIC_TYPE = VICType.NTSC
  override final val CPU_FREQ = 1_022_727.0d
  override final val RASTER_LINES = 261
  override final val RASTER_CYCLES = 65
  override final val BLANK_TOP_LINE = 14
  override final val BLANK_BOTTOM_LINE = RASTER_LINES
  override final val BLANK_LEFT_CYCLE = 1
  override final val BLANK_RIGHT_CYCLE = 51
  override final val MAX_COLUMNS = 32
  override final val MAX_ROWS = 31
  override final val RASTER_OFFSET = 0
}

sealed trait VIC_II_Model extends VICModel {
  val XCOORD : Array[Int]

  /* ================================= SPRITE INFO =====================================
     +-------+---------------------------------------------------+
     |   17  |     16     |      15 - 8       |     7 - 0        |
     +-----------------------------------------------------------+
     | Ignore| Sprite fb  |    Sprite 1-8     |     BA Info      |
     +-----------------------------------------------------------+

     Ignore    = 1 => ignore
     Sprite fb = first byte => 1 first byte, 2 = second byte
     Sprite 1-8= 0 => no sprite, 1 - 8 sprite 0 - 7
     BA Info   = sprite mask, must be anded with spriteDMAon
     ===================================================================================
   */
  val SPRITE_BA_INFO : Array[Int]
}

object VIC_II_NTSC extends VIC_II_Model {
  override final val VIC_TYPE           = VICType.NTSC
  override final val CPU_FREQ           = 1022727.0d
  override final val RASTER_LINES       = 263
  override final val RASTER_CYCLES      = 65
  override final val BLANK_TOP_LINE     = 0
  override final val BLANK_BOTTOM_LINE  = RASTER_LINES
  override final val BLANK_LEFT_CYCLE   = 11
  override final val BLANK_RIGHT_CYCLE  = 59
  override final val RASTER_OFFSET = 27

  override final val XCOORD = Array(-1,0x1a0,0x1a8,0x1b0,0x1b8,0x1c0,0x1c8,0x1d0,0x1d8,0x1e0,0x1e8,0x1f0,0x1f8,0x000,0x008,0x010,0x018,0x020,0x028,0x030,0x038,0x040,0x048,0x050,0x058,0x060,0x068,0x070,0x078,0x080,0x088,0x090,0x098,0x0a0,0x0a8,0x0b0,0x0b8,0x0c0,0x0c8,0x0d0,0x0d8,0x0e0,0x0e8,0x0f0,0x0f8,0x100,0x108,0x110,0x118,0x120,0x128,0x130,0x138,0x140,0x148,0x150,0x158,0x160,0x168,0x170,0x178,0x180,0x18C,0x18C,0x190,0x19C)

  override final val SPRITE_BA_INFO: Array[Int] = Array(
    -1,                             // N/A
    (0 << 16) | 4 << 8 | 0x38,      // 1
    (1 << 16) | 5 << 8 | 0x30,      // 2
    (0 << 16) | 5 << 8 | 0x70,      // 3
    (1 << 16) | 6 << 8 | 0x60,      // 4
    (0 << 16) | 6 << 8 | 0xE0,      // 5
    (1 << 16) | 7 << 8 | 0xC0,      // 6
    (0 << 16) | 7 << 8 | 0xC0,      // 7
    (1 << 16) | 8 << 8 | 0x80,      // 8
    (0 << 16) | 8 << 8 | 0x80,      // 9
    (0 << 16) | 0 << 8 | 0x00,      // 10
    (0 << 16) | 0 << 8 | 0x00,      // 11
    1 << 17,                      // 12
    1 << 17,                      // 13
    1 << 17,                      // 14
    1 << 17,                      // 15
    1 << 17,                      // 16
    1 << 17,                      // 17
    1 << 17,                      // 18
    1 << 17,                      // 19
    1 << 17,                      // 20
    1 << 17,                      // 21
    1 << 17,                      // 22
    1 << 17,                      // 23
    1 << 17,                      // 24
    1 << 17,                      // 25
    1 << 17,                      // 26
    1 << 17,                      // 27
    1 << 17,                      // 28
    1 << 17,                      // 29
    1 << 17,                      // 30
    1 << 17,                      // 31
    1 << 17,                      // 32
    1 << 17,                      // 33
    1 << 17,                      // 34
    1 << 17,                      // 35
    1 << 17,                      // 36
    1 << 17,                      // 37
    1 << 17,                      // 38
    1 << 17,                      // 39
    1 << 17,                      // 40
    1 << 17,                      // 41
    1 << 17,                      // 42
    1 << 17,                      // 43
    1 << 17,                      // 44
    1 << 17,                      // 45
    1 << 17,                      // 46
    1 << 17,                      // 47
    1 << 17,                      // 48
    1 << 17,                      // 49
    1 << 17,                      // 50
    1 << 17,                      // 51
    1 << 17,                      // 52
    1 << 17,                      // 53
    1 << 17,                      // 54
    (0 << 16) | 0 << 8 | 0x00,      // 55
    (0 << 16) | 0 << 8 | 0x01,      // 56
    (0 << 16) | 0 << 8 | 0x01,      // 57
    (0 << 16) | 0 << 8 | 0x03,      // 58
    (1 << 16) | 1 << 8 | 0x03,      // 59
    (0 << 16) | 1 << 8 | 0x07,      // 60
    (1 << 16) | 2 << 8 | 0x06,      // 61
    (0 << 16) | 2 << 8 | 0x0E,      // 62
    (1 << 16) | 3 << 8 | 0x0C,      // 63
    (0 << 16) | 3 << 8 | 0x1C,      // 64
    (1 << 16) | 4 << 8 | 0x18       // 65
  )
}

object VIC_II_PAL extends VIC_II_Model {
  override final val VIC_TYPE           = VICType.PAL
  override final val CPU_FREQ           = 985248.0d
  override final val RASTER_LINES       = 312
  override final val RASTER_CYCLES      = 63
  override final val BLANK_TOP_LINE     = 15
  override final val BLANK_BOTTOM_LINE  = 288
  override final val BLANK_LEFT_CYCLE   = 11
  override final val BLANK_RIGHT_CYCLE  = 59

  override final val XCOORD = Array(-1,0x198,0x1a0,0x1a8,0x1b0,0x1b8,0x1c0,0x1c8,0x1d0,0x1d8,0x1e0,0x1e8,0x1f0,0x000,0x008,0x010,0x018,0x020,0x028,0x030,0x038,0x040,0x048,0x050,0x058,0x060,0x068,0x070,0x078,0x080,0x088,0x090,0x098,0x0a0,0x0a8,0x0b0,0x0b8,0x0c0,0x0c8,0x0d0,0x0d8,0x0e0,0x0e8,0x0f0,0x0f8,0x100,0x108,0x110,0x118,0x120,0x128,0x130,0x138,0x140,0x148,0x150,0x158,0x160,0x168,0x170,0x178,0x180,0x188,0x190)

  override final val SPRITE_BA_INFO: Array[Int] = Array(
    -1,                             // N/A
    (1 << 16) | 4 << 8 | 0x18,      // 1
    (0 << 16) | 4 << 8 | 0x38,      // 2
    (1 << 16) | 5 << 8 | 0x30,      // 3
    (0 << 16) | 5 << 8 | 0x70,      // 4
    (1 << 16) | 6 << 8 | 0x60,      // 5
    (0 << 16) | 6 << 8 | 0xE0,      // 6
    (1 << 16) | 7 << 8 | 0xC0,      // 7
    (0 << 16) | 7 << 8 | 0xC0,      // 8
    (1 << 16) | 8 << 8 | 0x80,      // 9
    (0 << 16) | 8 << 8 | 0x80,      // 10
    (0 << 16) | 0 << 8 | 0x00,      // 11
    1 << 17,                      // 12
    1 << 17,                      // 13
    1 << 17,                      // 14
    1 << 17,                      // 15
    1 << 17,                      // 16
    1 << 17,                      // 17
    1 << 17,                      // 18
    1 << 17,                      // 19
    1 << 17,                      // 20
    1 << 17,                      // 21
    1 << 17,                      // 22
    1 << 17,                      // 23
    1 << 17,                      // 24
    1 << 17,                      // 25
    1 << 17,                      // 26
    1 << 17,                      // 27
    1 << 17,                      // 28
    1 << 17,                      // 29
    1 << 17,                      // 30
    1 << 17,                      // 31
    1 << 17,                      // 32
    1 << 17,                      // 33
    1 << 17,                      // 34
    1 << 17,                      // 35
    1 << 17,                      // 36
    1 << 17,                      // 37
    1 << 17,                      // 38
    1 << 17,                      // 39
    1 << 17,                      // 40
    1 << 17,                      // 41
    1 << 17,                      // 42
    1 << 17,                      // 43
    1 << 17,                      // 44
    1 << 17,                      // 45
    1 << 17,                      // 46
    1 << 17,                      // 47
    1 << 17,                      // 48
    1 << 17,                      // 49
    1 << 17,                      // 50
    1 << 17,                      // 51
    1 << 17,                      // 52
    1 << 17,                      // 53
    1 << 17,                      // 54
    (0 << 16) | 0 << 8 | 0x01,      // 55
    (0 << 16) | 0 << 8 | 0x01,      // 56
    (0 << 16) | 0 << 8 | 0x03,      // 57
    (1 << 16) | 1 << 8 | 0x03,      // 58
    (0 << 16) | 1 << 8 | 0x07,      // 59
    (1 << 16) | 2 << 8 | 0x06,      // 60
    (0 << 16) | 2 << 8 | 0x0E,      // 61
    (1 << 16) | 3 << 8 | 0x0C,      // 62
    (0 << 16) | 3 << 8 | 0x1C       // 63
  )
}


