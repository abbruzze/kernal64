package ucesoft.cbm.util

import ucesoft.cbm.cpu.Memory
import java.util.Properties
import java.io.IOException

class AboutCanvas(charRom:Memory,version:String) extends CBMCanvas(charRom) {
  private val WIDTH = 50
  private val scalaVersion = {
    val p = new Properties
    Option(ClassLoader.getSystemResourceAsStream("library.properties")) match {
      case None => "N/A"
      case Some(in) =>
        try {
          p.load(in)
          in.close
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
  add(center("A COMMODORE 64/128 EMULATOR WRITTEN IN SCALA",WIDTH)).newLine
  newLine
  add(center("BY ALESSANDRO ABBRUZZETTI",WIDTH)).newLine
  newLine
  add(center("2013-2018",WIDTH)).newLine
  newLine
  yellow
  add(center("VISIT",WIDTH)).newLine
  add(center("HTTPS://GITHUB.COM/ABBRUZZE/KERNAL64",WIDTH)).newLine
  for(_ <- 1 to 4) newLine
  
  end
}