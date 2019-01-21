package ucesoft.cbm.misc

import javax.swing._
import ucesoft.cbm.Clock
import java.awt.Point

trait FloppyFlushListener {
  def flushing(file:String,f: => Unit)
  def update(p:Int)
}

class FloppyFlushUI(parentWindow:JFrame) extends FloppyFlushListener {
  private val progress = new JProgressBar
  def flushing(file:String,f: => Unit) {
    val dialog = new JDialog(parentWindow,"Flushing...",true)
    progress.setValue(0)
    progress.setStringPainted(true)
    val p1 = new JPanel
    p1.add(new JLabel(s"Saving back file $file ..."))
    dialog.getContentPane.add("North",p1)
    val p2 = new JPanel
    p2.add(progress)
    dialog.getContentPane.add("Center",p2)
    dialog.pack
    val parentPos = parentWindow.getLocationOnScreen
    val parentSize = parentWindow.getSize
    val size = dialog.getSize
    val pos = new Point(parentPos.x + (parentSize.width - size.width) / 2,parentPos.y + (parentSize.height - size.height) / 2)
    dialog.setLocation(pos)
    val t = new Thread {
      override def run {        
        Clock.systemClock.pause        
        try {
          f
        }
        catch {
          case _:IllegalArgumentException =>
            JOptionPane.showMessageDialog(dialog,"Can't write back on disk due to format error","Disk write error",JOptionPane.ERROR_MESSAGE)
        }
        while (!dialog.isVisible) Thread.sleep(10)
        dialog.setVisible(false)
        dialog.dispose
        Clock.systemClock.play
      }
    }
    t.start
    dialog.setVisible(true)
  }
  def update(p:Int) {
    progress.setValue(p)
  }
}