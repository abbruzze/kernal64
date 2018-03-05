package ucesoft.cbm.util

import ucesoft.cbm.cpu.Memory
import java.io.File
import javax.swing.JFileChooser
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeEvent
import java.awt.Dimension

import ucesoft.cbm.formats.Diskette

class D64Canvas(fc:JFileChooser,charRom:Memory) extends CBMCanvas(charRom) with PropertyChangeListener {
  fc.addPropertyChangeListener(this)
  setPreferredSize(new Dimension(32 * 8,25 * 8))
  bgColor(1)
  
  def propertyChange(e:PropertyChangeEvent) {
    val prop = e.getPropertyName
    if (JFileChooser.SELECTED_FILE_CHANGED_PROPERTY.equals(prop)) {
      val file = e.getNewValue.asInstanceOf[File]
      if (file != null && (file.getName.toUpperCase.endsWith(".D64") || file.getName.toUpperCase.endsWith(".D71") || file.getName.toUpperCase.endsWith(".D81"))) readDir(file)        
      else clear
      repaint()
    }
  }
  
  protected def readDir(file:File) {
    clear
    try {
    	val d64 = Diskette(file.toString)   
	    val dirs = d64.directories
	    val bam = d64.bam	    
	    
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
	    val blocksFree = bam.freeSectors
	    add(if (blocksFree < 0) "0" else blocksFree.toString)
	    add(" BLOCKS FREE.")
	    d64.close
    }
    catch {
      case t:Throwable =>
        add("FORMAT NOT SUPPORTED.")
    }
    end
  }
}