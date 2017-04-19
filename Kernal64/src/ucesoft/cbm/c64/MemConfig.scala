package ucesoft.cbm.c64

import ucesoft.cbm.Log

case class MemConfig(basic:Boolean,roml:Boolean,romh:Boolean,char:Boolean,kernal:Boolean,io:Boolean,romhultimax:Boolean) {
  override def toString = s"basic=${b2i(basic)} roml=${b2i(roml)} romh=${b2i(romh)} char=${b2i(char)} kernal=${b2i(kernal)} io=${b2i(io)} rom_ultimax=${b2i(romhultimax)}"
  @inline private def b2i(b:Boolean) = if (b) "1" else "0"
}

object MemConfig {
  val MEM_CONFIG : Array[MemConfig] = {
    val mem = Array.ofDim[MemConfig](32)
    Log.info("Initializing main memory configurations ...")
    for(m <- 0 to 31) {
      val mc = m match {
        case 31 => 
          MemConfig(basic=true,roml=false,romh=false,char=false,kernal=true,io=true,romhultimax=false)
        case 30|14 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=true,io=true,romhultimax=false)
        case 29|13 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=true,romhultimax=false)
        case 28|24 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
        case 27 =>
          MemConfig(basic=true,roml=false,romh=false,char=true,kernal=true,io=false,romhultimax=false)
        case 26|10 =>
          MemConfig(basic=false,roml=false,romh=false,char=true,kernal=true,io=false,romhultimax=false)
        case 25|9 =>
          MemConfig(basic=false,roml=false,romh=false,char=true,kernal=false,io=false,romhultimax=false)
        case 23|22|21|20|19|18|17|16 =>
          MemConfig(basic=false,roml=true,romh=false,char=false,kernal=false,io=true,romhultimax=true)
        case 15 =>
          MemConfig(basic=true,roml=true,romh=false,char=false,kernal=true,io=true,romhultimax=false)
        case 12|8|4|0 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
        case 11 =>
          MemConfig(basic=true,roml=true,romh=false,char=true,kernal=true,io=false,romhultimax=false)
        case 7 =>
          MemConfig(basic=false,roml=true,romh=true,char=false,kernal=true,io=true,romhultimax=false)
        case 6 =>
          MemConfig(basic=false,roml=false,romh=true,char=false,kernal=true,io=true,romhultimax=false)
        case 5 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=true,romhultimax=false)
        case 3 =>
          MemConfig(basic=false,roml=true,romh=true,char=true,kernal=true,io=false,romhultimax=false)
        case 2 =>
          MemConfig(basic=false,roml=false,romh=true,char=true,kernal=true,io=false,romhultimax=false)
        case 1 =>
          MemConfig(basic=false,roml=false,romh=false,char=false,kernal=false,io=false,romhultimax=false)
       }
      mem(m) = mc
    }
    mem
  }
}