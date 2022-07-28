package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.Diskette

import java.awt.Dimension
import java.awt.event.{MouseAdapter, MouseEvent}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.File
import javax.swing.{JFileChooser, JScrollPane}

class D64Canvas(fc:JFileChooser,charRom:Memory,c64Mode:Boolean) extends CBMCanvas(charRom) with PropertyChangeListener {
	var sp : JScrollPane = _
	protected var validSelectableIndexes : Map[Int,AnyRef] = Map.empty

	fc.addPropertyChangeListener(this)
	setPreferredSize(new Dimension(32 * 8,25 * 8))
	if (c64Mode) {
		bgColor(6)
		lightBlue
	}
	else {
		bgColor(11)
		lightGreen
	}

	setToolTipText("Select a PRG file to run")

	addMouseListener(new MouseAdapter {
		override def mouseClicked(e: MouseEvent): Unit = {
			val rowHeight = if (isDoubleHeight) 16 else 8
			val rowSel = e.getY / rowHeight
			if (validSelectableIndexes.contains(rowSel)) {
				if (e.isControlDown && rowSel == selectedRow) selectRow(-1) else selectRow(rowSel)
				repaint()
			}
		}
	})

	def selectedFile : Option[String] = validSelectableIndexes get selectedRow map { _.toString }

	def selectedObject : Option[AnyRef] = validSelectableIndexes get selectedRow

	protected def isFileExtOK(fileName:String) : Boolean = fileName.toUpperCase.endsWith(".D64") ||
		fileName.toUpperCase.endsWith(".D71") ||
		fileName.toUpperCase.endsWith(".D81") ||
		fileName.toUpperCase.endsWith(".G64") ||
		fileName.toUpperCase.endsWith(".G71")

	def propertyChange(e:PropertyChangeEvent) : Unit = {
		val prop = e.getPropertyName
		if (JFileChooser.SELECTED_FILE_CHANGED_PROPERTY.equals(prop)) {
			val file = e.getNewValue.asInstanceOf[File]
			if (file != null && isFileExtOK(file.toString)) readDir(file)
			else clear
			revalidate()
			if (sp != null) sp.repaint()
		}
	}

	protected def readDir(file:File) : Unit = {
		clear
		try {
			val d64 = Diskette(file.toString)
			val dirs = d64.directories
			val bam = d64.bam

			validSelectableIndexes = dirs.zipWithIndex filter { case (d,_) => d.fileType == Diskette.FileType.PRG } map { d => (d._2 + 1,d._1.fileName) } toMap

			enhanceHeight
			//enhanceWidth
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