package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory

import java.io.IOException
import java.util.Properties

class AboutCanvas(charRom:Memory,version:String,romCharBytes:Int = 8,romCharHeight:Int = 8) extends CBMCanvas(charRom,romCharBytes,romCharHeight) {
  private val WIDTH = 50
  private val scalaVersion = {
    val p = new Properties
    Option(ClassLoader.getSystemResourceAsStream("library.properties")) match {
      case None => "N/A"
      case Some(in) =>
        try {
          p.load(in)
          in.close()
          p.getProperty("version.number")
        }
        catch {
          case _:IOException => "N/A"
        }
    }
  }
  
  for(_ <- 1 to 4) newLine
  red
  enhanceWidth
  add(center("KERNAL64",WIDTH)).newLine.newLine
  standardWidth
  add(center("VER " + version,WIDTH)).newLine
  add(center("JVM VERSION " + scala.util.Properties.javaVersion.replaceAll("_","-"),WIDTH)).newLine
  add(center("SCALA VERSION " + scalaVersion,WIDTH)).newLine
  newLine
  white
  add(center(" A COMMODORE 64/128/CBMII/VIC20 EMULATOR ",WIDTH)).newLine
  newLine
  add(center("WRITTEN IN SCALA BY ALESSANDRO ABBRUZZETTI",WIDTH)).newLine
  newLine
  add(center("2013-2023",WIDTH)).newLine
  newLine
  yellow
  add(center("VISIT",WIDTH)).newLine
  add(center("HTTPS://GITHUB.COM/ABBRUZZE/KERNAL64",WIDTH)).newLine
  for(_ <- 1 to 4) newLine
  
  end
}