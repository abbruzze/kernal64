package ucesoft.cbm.misc

object SettingsKey {
  final val TOTAL_DRIVES          = 4
  final val RUN_FILE              = "RUNFILE"
  final val WARP_ON_LOAD          = "WARPLOAD"
  final val LOAD_PRG_AS_D64       = "LOAD_PRG_AS_D64"
  final val DRIVE_X_ENABLED       = (for(d <- 0 until TOTAL_DRIVES) yield s"DRIVE_${8 + d}_ENABLED").toArray
  final val DRIVE_12_PATH         = "DRIVE_12_PATH"
  final val WRITE_ON_DISK         = "WRITE_ON_DISK"
  final val ATTACH_CTR            = "ATTACH_CTR"
  final val PALETTE               = "PALETTE"
  final val RENDERING_TYPE        = "RENDERING_TYPE"
  final val PRINTER_ENABLED       = "PRINTER_ENABLED"
  final val SID_8580              = "SID_8580"
  final val DUAL_SID              = "DUAL_SID"
  final val SID_CYCLE_EXACT       = "SID_CYCLE_EXACT"
  final val DRIVE_X_TYPE          = (for(d <- 0 until TOTAL_DRIVES) yield s"DRIVE_${8 + d}_TYPE").toArray
  final val DRIVE_X_FILE          = (for(d <- 0 until TOTAL_DRIVES) yield s"DRIVE_${8 + d}_FILE").toArray
  final val REU_TYPE              = "REU_TYPE"
  final val GEO_RAM               = "GEO_RAM"
  final val CPM64                 = "CPM64"
  final val BEAMRACER             = "BEAMRACER"
  final val VIDEO_NTSC            = "VIDEO_NTSC"
  final val VIC_BORDER_MODE       = "VIC_BORDER_MODE"
  // SCPU
  final val SCPU_MEM_SIZE         = "SCPU_MEM_SIZE"
  final val SCPU_JIFFYDOS_ENABLED = "SCPU_JIFFYDOS_ENABLED"
}
