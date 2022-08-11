package ucesoft.cbm.cbm2

import ucesoft.cbm.cpu.ROM

/*object CBM2Model extends Enumeration {
  val _600 = Value
  val _620 = Value
  val _700 = Value
  val _720 = Value
}*/

sealed trait CBM2Model {
  val name : String
  val memoryK : Int
  val isPAL : Boolean
  val lowProfile : Boolean
  val basicROMPropName : String
  val charROMPropName : String
  val crtClip : (Int,Int,Int,Int)
}

case object _610PAL extends CBM2Model {
  override val name = "610 PAL"
  override val memoryK = 128
  override val isPAL = true
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (25,13,28,0)
}

case object _610NTSC extends CBM2Model {
  override val name = "610 NTSC"
  override val memoryK = 128
  override val isPAL = false
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (21,13,14,0)
}

case object _620PAL extends CBM2Model {
  override val name = "610 PAL"
  override val memoryK = 256
  override val isPAL  = true
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (25,13,28,0)
}

case object _620NTSC extends CBM2Model {
  override val name = "610 NTSC"
  override val memoryK = 256
  override val isPAL  = false
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (21,13,14,0)
}

case object _710NTSC extends CBM2Model {
  override val name = "710 NTSC"
  override val memoryK = 128
  override val isPAL  = false
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
}

case object _710PAL extends CBM2Model {
  override val name = "710 PAL"
  override val memoryK = 128
  override val isPAL  = true
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
}

case object _720NTSC extends CBM2Model {
  override val name = "720 NTSC"
  override val memoryK = 256
  override val isPAL  = false
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
}