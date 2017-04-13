package ucesoft.cbm.peripheral.vic

object Palette {
	val VIC_RGB = Array(
	  /* WINVice
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
      */
	  // these are more brightness
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
	
	val VIC_COLORS = VIC_RGB map { c =>
	  val b = c & 0xFF
	  val g = (c >> 8) & 0xFF
	  val r = (c >> 16) & 0xFF
	  val alpha = (c >> 24) & 0xFF
	  new java.awt.Color(r,g,b,alpha)
	}
}