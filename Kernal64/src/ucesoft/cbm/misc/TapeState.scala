package ucesoft.cbm.misc

import javax.swing.JComponent
import ucesoft.cbm.peripheral.c2n.DatassetteListener
import ucesoft.cbm.peripheral.c2n.DatassetteState
import javax.swing.JProgressBar
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom.Path2D

class TapeState extends JComponent with DatassetteListener {
    private[this] var state = DatassetteState.STOPPED
    val progressBar = new JProgressBar
    
    setPreferredSize(new Dimension(10,10))
    progressBar.setPreferredSize(new Dimension(100,15))
    progressBar.setVisible(false)
    setVisible(false)
    
    override def paint(g:Graphics) {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      import DatassetteState._
      state match {
        case PLAYING =>
          g2.setColor(Color.GREEN.darker)
          val path = new Path2D.Float
          path.moveTo(0,0)
          path.lineTo(size.width,size.height / 2)
          path.lineTo(0,size.height)
          path.closePath
          g2.fill(path)
        case STOPPED =>
          g2.setColor(Color.BLACK)
          g2.fillRect(0,0,size.width,size.height)
        case RECORDING =>
          g2.setColor(Color.RED)
          g2.fillOval(0,0,size.width,size.height)
      }
    }
    
    def datassetteStateChanged(newState:DatassetteState.Value) {
      setVisible(true)
      progressBar.setVisible(true)
      state = newState
      repaint()
    }
    def datassetteUpdatePosition(perc:Int) {
      progressBar.setValue(perc)
    }
  }