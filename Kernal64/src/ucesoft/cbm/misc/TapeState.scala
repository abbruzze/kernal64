package ucesoft.cbm.misc

import javax.swing.{JComponent, JMenuItem, JPopupMenu, JProgressBar}
import ucesoft.cbm.peripheral.c2n.{Datassette, DatassetteListener, DatassetteState}
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.geom.Path2D

class TapeState(datassette:Datassette) extends JComponent with DatassetteListener {
  private[this] var state = DatassetteState.STOPPED
  val progressBar = new JProgressBar
  private[this] val pop = new JPopupMenu()

  setPreferredSize(new Dimension(10, 10))
  progressBar.setPreferredSize(new Dimension(100, 15))
  progressBar.setVisible(false)
  progressBar.setStringPainted(true)
  setVisible(false)
  setToolTipText("Right click to show controls")
  initPopup

  override def paint(g: Graphics): Unit = {
    val size = getSize()
    val g2 = g.asInstanceOf[Graphics2D]
    import DatassetteState._
    state match {
      case PLAYING =>
        g2.setColor(Color.GREEN.darker)
        val path = new Path2D.Float
        path.moveTo(0, 0)
        path.lineTo(size.width, size.height / 2)
        path.lineTo(0, size.height)
        path.closePath
        g2.fill(path)
      case STOPPED =>
        g2.setColor(Color.BLACK)
        g2.fillRect(0, 0, size.width, size.height)
      case RECORDING =>
        g2.setColor(Color.RED)
        g2.fillOval(0, 0, size.width, size.height)
      case FORWARD =>
        g2.setColor(Color.GREEN.darker)
        var path = new Path2D.Float
        path.moveTo(0, 0)
        path.lineTo(size.width / 2, size.height / 2)
        path.lineTo(0, size.height)
        path.closePath
        g2.fill(path)
        path = new Path2D.Float
        path.moveTo(size.width / 2, 0)
        path.lineTo(size.width, size.height / 2)
        path.lineTo(size.width / 2, size.height)
        path.closePath
        g2.fill(path)
      case REWIND =>
        g2.setColor(Color.GREEN.darker)
        var path = new Path2D.Float
        path.moveTo(size.width, 0)
        path.lineTo(size.width / 2, size.height / 2)
        path.lineTo(size.width, size.height)
        path.closePath
        g2.fill(path)
        path = new Path2D.Float
        path.moveTo(size.width / 2, 0)
        path.lineTo(0, size.height / 2)
        path.lineTo(size.width / 2, size.height)
        path.closePath
        g2.fill(path)
    }
  }

  private def initPopup : Unit = {
    val tapePlayItem = new JMenuItem("Press play")
    tapePlayItem.addActionListener(_ => datassette.pressPlay)
    pop.add(tapePlayItem)

    val tapeStopItem = new JMenuItem("Press stop")
    tapeStopItem.addActionListener(_ => datassette.pressStop)
    pop.add(tapeStopItem)

    val tapeRecordItem = new JMenuItem("Press record & play")
    tapeRecordItem.addActionListener(_ => datassette.pressRecordAndPlay)
    pop.add(tapeRecordItem)

    val tapeRewindItem = new JMenuItem("Press rewind")
    tapeRewindItem.addActionListener(_ => datassette.pressRewind)
    pop.add(tapeRewindItem)

    val tapeForwardItem = new JMenuItem("Press forward")
    tapeForwardItem.addActionListener(_ => datassette.pressForward)
    pop.add(tapeForwardItem)

    val tapeResetItem = new JMenuItem("Reset")
    tapeResetItem.addActionListener(_ => datassette.resetToStart)
    pop.add(tapeResetItem)

    val tapeResetCounterItem = new JMenuItem("Reset counter")
    tapeResetCounterItem.addActionListener(_ => datassette.resetCounter)
    pop.add(tapeResetCounterItem)

    addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        if (e.isPopupTrigger) pop.show(e.getComponent,e.getX,e.getY)
      }
      override def mouseReleased(e: MouseEvent): Unit = {
        if (e.isPopupTrigger) pop.show(e.getComponent,e.getX,e.getY)
      }
    })
  }

  def datassetteStateChanged(newState: DatassetteState.Value): Unit = {
    setVisible(true)
    progressBar.setVisible(true)
    state = newState
    repaint()
  }

  def datassetteUpdatePosition(perc: Int, counter: Int): Unit = {
    progressBar.setValue(perc)
    progressBar.setString("%03d".format(counter))
  }

  def datassetteUpdateCounter(counter: Int): Unit = {
    progressBar.setString("%03d".format(counter))
  }
}