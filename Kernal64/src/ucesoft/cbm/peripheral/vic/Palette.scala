package ucesoft.cbm.peripheral.vic

object Palette {
  object PaletteType extends Enumeration {
    val VICE: PaletteType.Value = Value
    val BRIGHT: PaletteType.Value = Value
    val PEPTO: PaletteType.Value = Value
    val COLORDORE: PaletteType.Value = Value
  }

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
    0xffc0c0c0 // 15 Lt.Gray
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
    0xFF959595 // 15 Lt.Gray
  )

  private[this] val COLORDORE_RGB = Array(
    0xFF000000, // 0 Black
    0xFFFFFFFF, // 1 White
    0xFF813338, // 2 Red
    0xFF75CEC8, // 3 Cyan
    0xFF8E3C97, // 4 Purple
    0xFF56AC4D, // 5 Green
    0xFF2E2C9B, // 6 Blue
    0xFFEDF171, // 7 Yellow
    0xFF8E5029, // 8 Orange
    0xFF553800, // 9 Brown
    0xFFC46C71, // 10 Lt.Red
    0xFF4A4A4A, // 11 Dk.Gray
    0xFF7B7B7B, // 12 Gray
    0xFFA9FF9F, // 13 Lt.Green
    0xFF706DEB, // 14 Lt.Blue
    0xFFB2B2B2 // 15 Lt.Gray
  )

  private def toRGB(c:Int) : java.awt.Color = {
    val b = c & 0xFF
    val g = (c >> 8) & 0xFF
    val r = (c >> 16) & 0xFF
    val alpha = (c >> 24) & 0xFF
    new java.awt.Color(r,g,b,alpha)
  }

  private[this] val PALETTE_RGB : Map[PaletteType.Value,Array[java.awt.Color]] = Map(PaletteType.VICE -> (WINVICE_RGB map toRGB),PaletteType.BRIGHT -> (BRIGHT_RGB map toRGB),PaletteType.PEPTO -> (PEPTO_RGB map toRGB),PaletteType.COLORDORE -> (COLORDORE_RGB map toRGB))
	private[this] val PALETTE_COLORS : Map[PaletteType.Value,Array[Int]] = Map(PaletteType.VICE -> WINVICE_RGB,PaletteType.BRIGHT -> BRIGHT_RGB,PaletteType.PEPTO -> PEPTO_RGB,PaletteType.COLORDORE -> COLORDORE_RGB)

	final val VIC_COLORS = Array.ofDim[java.awt.Color](16)
  final val VIC_RGB = Array.ofDim[Int](16)

  setPalette(PaletteType.BRIGHT)

  def setPalette(pal:PaletteType.Value): Unit = {
    val colors = PALETTE_RGB(pal)
    val rgb = PALETTE_COLORS(pal)
    System.arraycopy(colors,0,VIC_COLORS,0,colors.length)
    System.arraycopy(rgb,0,VIC_RGB,0,rgb.length)
  }
}