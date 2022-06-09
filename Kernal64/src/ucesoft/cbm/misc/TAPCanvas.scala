package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.TAP

import java.io.File
import javax.swing.JFileChooser

class TAPCanvas(fc:JFileChooser, charRom:Memory, c64Mode:Boolean) extends D64Canvas(fc,charRom,c64Mode) {
  private final val TAPE_NAME = "C64-TAPE-RAW"

  override protected def isFileExtOK(fileName:String) : Boolean = fileName.toUpperCase.endsWith(".TAP")

  override def readDir(file:File) : Unit = {
    val entries = TAP.anaylize(file).header

    validSelectableIndexes = entries.zipWithIndex filter { case (d,_) => d.fileType == "PRG" } map { d => (d._2 + 1,d._1) } toMap

    clear
    enhanceHeight
    //black
    add("0 ")
    rvsOn
    add("\"")
    for(i <- 0 until 16) {
      if (i < TAPE_NAME.length) add(TAPE_NAME.charAt(i)) else add(0x20)
    }
    add("\" ")
    rep(0x20,5)
    rvsOff
    newLine
    for(e <- entries) {
      val blanks = 2
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      add("%03d".format(e.counter))
      rep(0x20,blanks)
      add("\"")
      add(e.fileName)
      add("\"")
      for(i <- 1 to 16 - e.fileName.length) add(0x20)
      add(0x20)
      add(e.fileType)
      rep(0x20,endBlanks)
      newLine
    }
    end
  }
}