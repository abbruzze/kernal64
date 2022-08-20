package ucesoft.cbm.cbm2

import ucesoft.cbm.cpu.ROM

import java.awt.Dimension

sealed trait CBM2Model {
  val name : String
  val memoryK : Int
  val isPAL : Boolean
  val lowProfile : Boolean
  val basicROMPropName : String
  val charROMPropName : String
  val crtClip : (Int,Int,Int,Int)
  val preferredFrameSize : Dimension
}

case object _610PAL extends CBM2Model {
  override val name = "610 PAL"
  override val memoryK = 128
  override val isPAL = true
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (25,13,28,0)
  //override val preferredFrameSize = new Dimension(720,624)
  override val preferredFrameSize = new Dimension(720,1 + (312 - crtClip._3 - crtClip._4) * 2)
}

case object _610NTSC extends CBM2Model {
  override val name = "610 NTSC"
  override val memoryK = 128
  override val isPAL = false
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (21,13,14,0)
  //override val preferredFrameSize = new Dimension(742,525)
  override val preferredFrameSize = new Dimension(744,1 + (262 - crtClip._3 - crtClip._4) * 2)
}

case object _620PAL extends CBM2Model {
  override val name = "620 PAL"
  override val memoryK = 256
  override val isPAL  = true
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (25,13,28,0)
  override val preferredFrameSize = new Dimension(720,1 + (312 - crtClip._3 - crtClip._4) * 2)
}

case object _620NTSC extends CBM2Model {
  override val name = "620 NTSC"
  override val memoryK = 256
  override val isPAL  = false
  override val lowProfile = true
  override val charROMPropName = ROM.CBM2_CHAR600_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (21,13,14,0)
  override val preferredFrameSize = new Dimension(744,1 + (262 - crtClip._3 - crtClip._4) * 2)
}

case object _710NTSC extends CBM2Model {
  override val name = "710 NTSC"
  override val memoryK = 128
  override val isPAL  = false
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
  override val preferredFrameSize = new Dimension(742,732)
}

case object _710PAL extends CBM2Model {
  override val name = "710 PAL"
  override val memoryK = 128
  override val isPAL  = true
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC128_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
  override val preferredFrameSize = new Dimension(720,732)
}

case object _720NTSC extends CBM2Model {
  override val name = "720 NTSC"
  override val memoryK = 256
  override val isPAL  = false
  override val lowProfile = false
  override val charROMPropName = ROM.CBM2_CHAR700_ROM_PROP
  override val basicROMPropName = ROM.CBM2_BASIC256_ROM_PROP
  override val crtClip: (Int, Int, Int, Int) = (18,0,0,0)
  override val preferredFrameSize = new Dimension(742,732)
}