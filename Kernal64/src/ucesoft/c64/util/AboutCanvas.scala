package ucesoft.c64.util

import ucesoft.c64.cpu.Memory

class AboutCanvas(charRom:Memory,version:String) extends CBMCanvas(charRom) {
  private val WIDTH = 50
  
  for(_ <- 1 to 4) newLine
  red
  enhanceWidth
  add(center("KERNAL64",WIDTH)).newLine.newLine
  standardWidth
  add(center("VER " + version,WIDTH)).newLine
  newLine
  white
  add(center("A COMMODORE 64 EMULATOR WRITTEN IN SCALA",WIDTH)).newLine
  newLine
  add(center("BY ALESSANDRO ABBRUZZETTI",WIDTH)).newLine
  newLine
  add(center("2013-2014",WIDTH)).newLine
  newLine
  yellow
  add(center("VISIT",WIDTH)).newLine
  add(center("HTTPS://GITHUB.COM/ABBRUZZE/KERNAL64",WIDTH)).newLine
  for(_ <- 1 to 4) newLine
  
  end
}