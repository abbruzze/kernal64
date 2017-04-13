package ucesoft.cbm.util

import ucesoft.cbm.cpu.Memory
import javax.swing.JFileChooser
import java.io.File

class T64Canvas(fc:JFileChooser,charRom:Memory) extends D64Canvas(fc,charRom) {
  override def readDir(file:File) {
    val t64 = new ucesoft.cbm.formats.T64(file.toString)
    t64.close
    
    clear    
    black
    add("0 ")
    rvsOn
    add("\"")
    for(i <- 0 until 16) {
      if (i < t64.tapeName.length) add(t64.tapeName.charAt(i)) else add(0x20)
    }
    add("\" ")
    rep(0x20,5)
    rvsOff
    newLine
    for(e <- t64.entries) {
      val blanks = if (e.blocks < 10) 4 
      	else
        if (e.blocks < 100) 3
        else 2
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      add(e.blocks.toString)
      rep(0x20,blanks)
      add("\"")
      add(e.fileName)
      add("\"")
      for(i <- 1 to 16 - e.fileName.length) add(0x20)
      add(0x20)
      add("PRG")
      rep(0x20,endBlanks)
      newLine
    }
  }
}