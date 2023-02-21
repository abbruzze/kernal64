package ucesoft.cbm.peripheral.vic

import scala.io.BufferedSource

object Palette {
  object PaletteType extends Enumeration {
    val VICE = Value
    val BRIGHT = Value
    val PEPTO = Value
    val COLORDORE = Value
    val VIC20_VICE = Value
    val VIC20_COLODORE = Value
    val VIC20_MIKE_PAL = Value
  }

  // ========================= C64/C128 VICII =============================
  private[this] val WINVICE_RGB = Array(
    0xFF000000, // 0 Black
    0xFFFFFFFF, // 1 White
    0xFF894036, // 2 Red
    0xFF7abfc7, // 3 Cyan
    0xFF8a46ae, // 4 Purple
    0xFF68a941, // 5 Green
    0xFF3e31a2, // 6 Blue
    0xFFd0dc71, // 7 Yellow
    0xFF905f25, // 8 Orange
    0xFF5c4700, // 9 Brown
    0xFFbb776d, // 10 Lt.Red
    0xFF555555, // 11 Dk.Gra
    0xFF808080, // 12 Gray
    0xFFaeea88, // 13 Lt.Gre
    0xFF7c70da, // 14 Lt.Blu
    0xFFababab  // 15 Lt.Gray
  )
  private[this] val BRIGHT_RGB = Array(
    0xff000000, // 0 Black
    0xffffffff, // 1 White
    0xffe04040, // 2 Red
    0xff60ffff, // 3 Cyan
    0xffe060e0, // 4 Purple
    0xff40e040, // 5 Green
    0xff4040e0, // 6 Blue
    0xffffff40, // 7 Yellow
    0xffe0a040, // 8 Orange
    0xff9c7448, // 9 Brown
    0xffffa0a0, // 10 Lt.Red
    0xff545454, // 11 Dk.Gray
    0xff888888, // 12 Gray
    0xffa0ffa0, // 13 Lt.Green
    0xffa0a0ff, // 14 Lt.Blue
    0xffc0c0c0  // 15 Lt.Gray
  )
  private[this] val PEPTO_RGB = Array(
    0xFF000000, // 0 Black
    0xFFFFFFFF, // 1 White
    0xFF68372B, // 2 Red
    0xFF70A4B2, // 3 Cyan
    0xFF6F3D86, // 4 Purple
    0xFF588D43, // 5 Green
    0xFF352879, // 6 Blue
    0xFFB8C76F, // 7 Yellow
    0xFF6F4F25, // 8 Orange
    0xFF433900, // 9 Brown
    0xFF9A6759, // 10 Lt.Red
    0xFF444444, // 11 Dk.Gray
    0xFF6C6C6C, // 12 Gray
    0xFF9AD284, // 13 Lt.Green
    0xFF6C5EB5, // 14 Lt.Blue
    0xFF959595  // 15 Lt.Gray
  )

  private[this] val COLORDORE_RGB = Array(
    0xFF000000, // 0 Black
    0xFFFFFFFF, // 1 White
    0xFF96282e, // 2 Red
    0xFF5bd6ce, // 3 Cyan
    0xFF9f2dad, // 4 Purple
    0xFF41b936, // 5 Green
    0xFF2724c4, // 6 Blue
    0xFFeff347, // 7 Yellow
    0xFF9f4815, // 8 Orange
    0xFF5e3500, // 9 Brown
    0xFFda5f66, // 10 Lt.Red
    0xFF474747, // 11 Dk.Gray
    0xFF787878, // 12 Gray
    0xFF91ff84, // 13 Lt.Green
    0xFF6864ff, // 14 Lt.Blue
    0xFFaeaeae  // 15 Lt.Gray
  )
  // ========================= VIC20 ======================================
  private[this] val VIC20_WINVICE_RGB = Array(
    0xFF000000, // 0 Black
    0xFFFFFFFF, // 1 White
    0xFFF00000, // 2 Red
    0xFF00F0F0, // 3 Cyan
    0xFF600060, // 4 Purple
    0xFF00A000, // 5 Green
    0xFF0000F0, // 6 Blue
    0xFFD0D000, // 7 Yellow
    0xFFC0A000, // 8 Orange
    0xFFFFA000, // 9 Lt. Orange
    0xFFF08080, // 10 Pink
    0xFF00FFFF, // 11 Lt. Cyan
    0xFFFF00FF, // 12 Lt. Purple
    0xFF00FF00, // 13 Lt.Green
    0xFF00A0FF, // 14 Lt.Blue
    0xFFFFFF00  // 15 Lt.Yellow
  )

  private[this] val VIC20_COLODORE_RGB = Array(
    0xFF000000,
    0xFFFFFFFF,
    0xFF6D2327,
    0xFFA0FEF8,
    0xFF8E3C97,
    0xFF7EDA75,
    0xFF252390,
    0xFFFFFF86,
    0xFFA4643B,
    0xFFFFC8A1,
    0xFFF2A7AB,
    0xFFDBFFFF,
    0xFFFFB4FF,
    0xFFD7FFCE,
    0xFF9D9AFF,
    0xFFFFFFC9
  )

  private[this] val VIC20_MIKE_PAL_RGB = Array(
    0xFF000000,
    0xFFFFFFFF,
    0xFFB61F21,
    0xFF4DF0FF,
    0xFFB43FFF,
    0xFF44E237,
    0xFF1A34FF,
    0xFFDCD71B,
    0xFFCA5400,
    0xFFE9B072,
    0xFFE79293,
    0xFF9AF7FD,
    0xFFE09FFF,
    0xFF8FE493,
    0xFF8290FF,
    0xFFE5DE85
  )

  // ==================== VDC =====================================
  private[this] val VDC_DEFAULT_RGB = Array(
    0xFF000000, // 00 Black
    0xFF555555, // 01 Medium Gray
    0xFF0000AA, // 02 Blue
    0xFF5555FF, // 03 Light blue
    0xFF00AA00, // 04 Green
    0xFF55FF55, // 05 Light green
    0xFF00AAAA, // 06 Dark cyan
    0xFF55FFFF, // 07 Light cyan
    0xFFAA0000, // 08 Dark red
    0xFFFF5555, // 09 Light red
    0xFFAA00AA, // 10 Dark purple
    0xFFFF55FF, // 11 Light purple
    0xFFAA5500, // 12 Brown
    0xFFFFFF55, // 13 Yellow
    0xFFAAAAAA, // 14 Light Gray
    0xFFFFFFFF // 15 White
  )

  private def toRGB(c:Int) : java.awt.Color = {
    val b = c & 0xFF
    val g = (c >> 8) & 0xFF
    val r = (c >> 16) & 0xFF
    val alpha = (c >> 24) & 0xFF
    new java.awt.Color(r,g,b,alpha)
  }

  private[this] val PALETTE_RGB : Map[PaletteType.Value,Array[java.awt.Color]] = Map(
    PaletteType.VICE -> (WINVICE_RGB map toRGB),
    PaletteType.BRIGHT -> (BRIGHT_RGB map toRGB),
    PaletteType.PEPTO -> (PEPTO_RGB map toRGB),
    PaletteType.COLORDORE -> (COLORDORE_RGB map toRGB),
    PaletteType.VIC20_VICE -> (VIC20_WINVICE_RGB map toRGB),
    PaletteType.VIC20_COLODORE -> (VIC20_COLODORE_RGB map toRGB),
    PaletteType.VIC20_MIKE_PAL -> (VIC20_MIKE_PAL_RGB map toRGB)
  )
	private[this] val PALETTE_COLORS : Map[PaletteType.Value,Array[Int]] = Map(
    PaletteType.VICE -> WINVICE_RGB,
    PaletteType.BRIGHT -> BRIGHT_RGB,
    PaletteType.PEPTO -> PEPTO_RGB,
    PaletteType.COLORDORE -> COLORDORE_RGB,
    PaletteType.VIC20_VICE -> VIC20_WINVICE_RGB,
    PaletteType.VIC20_COLODORE -> VIC20_COLODORE_RGB,
    PaletteType.VIC20_MIKE_PAL -> VIC20_MIKE_PAL_RGB
  )

	final val VIC_COLORS = Array.ofDim[java.awt.Color](16)
  final val VIC_RGB = Array.ofDim[Int](16)
  final val VDC_DEFAULT = VDC_DEFAULT_RGB

  setVICPalette(PaletteType.BRIGHT)

  def setVICPalette(pal:PaletteType.Value): Unit = {
    val colors = PALETTE_RGB(pal)
    val rgb = PALETTE_COLORS(pal)
    System.arraycopy(colors,0,VIC_COLORS,0,colors.length)
    System.arraycopy(rgb,0,VIC_RGB,0,rgb.length)
  }

  private def parseVPLFile(file:String): Option[Array[(Int,java.awt.Color)]] = {
    var f: BufferedSource = null
    try {
      f = io.Source.fromFile(file)
      val rgbs = f.getLines().filterNot(l => l.trim().startsWith("#") || l.trim().isEmpty).toArray.map { l =>
        l.split(" ") match {
          case rgb@Array(_, _, _) =>
            val rgbn = rgb.map(Integer.parseInt(_, 16))
            (0xFF << 24 | rgbn(0) << 16 | rgbn(1) << 8 | rgbn(2), new java.awt.Color(rgbn(0), rgbn(1), rgbn(2), 0xFF))
          case _ =>
            return None
        }
      }
      if (rgbs.length != 16) return None
      Some(rgbs)
    }
    catch {
      case _: Throwable =>
        None
    }
    finally {
      if (f != null) f.close()
    }
  }

  def setVICPaletteFromFile(file:String): Boolean = {
    parseVPLFile(file) match {
      case None => false
      case Some(rgbs) =>
        System.arraycopy(rgbs.map(_._1), 0, VIC_RGB, 0, 16)
        System.arraycopy(rgbs.map(_._2), 0, VIC_COLORS, 0, 16)
        true
    }
  }

  def setVDCPaletteFromFile(file: String): Boolean = {
    parseVPLFile(file) match {
      case None => false
      case Some(rgbs) =>
        System.arraycopy(rgbs.map(_._1), 0, VDC_DEFAULT, 0, 16)
        true
    }
  }
}