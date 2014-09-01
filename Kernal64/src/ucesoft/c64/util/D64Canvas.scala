package ucesoft.c64.util

import ucesoft.c64.cpu.Memory
import java.io.File
import javax.swing.JFrame
import ucesoft.c64.cpu.CPU6510Mems
import javax.swing.JFileChooser
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeEvent
import java.awt.Dimension

class D64Canvas(fc:JFileChooser,charRom:Memory) extends CBMCanvas(charRom) with PropertyChangeListener {
  fc.addPropertyChangeListener(this)
  setPreferredSize(new Dimension(32 * 8,25 * 8))
  bgColor(1)
  
  def propertyChange(e:PropertyChangeEvent) {
    val prop = e.getPropertyName
    if (JFileChooser.SELECTED_FILE_CHANGED_PROPERTY.equals(prop)) {
      val file = e.getNewValue.asInstanceOf[File]
      if (file != null) readDir(file)        
      else clear
      repaint()
    }
  }
  
  protected def readDir(file:File) {
    val d64 = new ucesoft.c64.formats.D64(file.toString)   
    val dirs = d64.directories
    val bam = d64.bam
    d64.close
    
    clear    
    black
    add("0 ")
    rvsOn
    add("\"")
    for(i <- 0 until 16) {
      if (i < bam.diskName.length) add(bam.diskName.charAt(i)) else add(0x20)
    }
    add("\" ")
    add(bam.diskID(0))
    add(bam.diskID(1))
    add(0x20)
    add(bam.dosType(0))
    add(bam.dosType(1))
    rvsOff
    newLine
    for(dir <- dirs) {
      val blanks = if (dir.sizeInSectors < 10) 4 
      	else
        if (dir.sizeInSectors < 100) 3
        else 2
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      add(dir.sizeInSectors.toString)
      rep(0x20,blanks)
      add("\"")
      add(dir.fileName)
      add("\"")
      for(i <- 1 to 16 - dir.fileName.length) add(0x20)
      add(0x20)
      add(dir.fileType.toString)
      rep(0x20,endBlanks)
      newLine
    }
    val blocksFree = ucesoft.c64.formats.D64.TOTAL_AVAILABLE_SECTORS - (dirs map { _.sizeInSectors } sum)
    add(if (blocksFree < 0) "0" else blocksFree.toString)
    add(" BLOCKS FREE.")
    end
  }
}