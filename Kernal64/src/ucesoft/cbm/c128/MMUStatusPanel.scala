package ucesoft.cbm.c128

import javax.swing._
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Font
import java.awt.FlowLayout
import java.awt.Component

class LabelledLed(label:String,labelUp:Boolean,colorON : Color = Color.GREEN,colorOFF : Color = Color.DARK_GRAY) extends JPanel {
  var on : Boolean = false
  
  private[this] val led = new JComponent {
    setAlignmentX(Component.CENTER_ALIGNMENT)
    override def paint(g:Graphics) : Unit = {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      val color = if (on) colorON else colorOFF
      g2.setColor(color)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    }
  }
  private[this] val labelComponent = {
    val l = new JLabel(label)
    l.setFont(new Font("Monospaced",Font.BOLD,10))
    l.setAlignmentX(Component.CENTER_ALIGNMENT)
    l.setForeground(Color.RED)
    l
  }
  
  led.setPreferredSize(new Dimension(10,10))
  led.setMaximumSize(new Dimension(10,10))
  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))
  if (labelUp) {
    add(labelComponent)
    add(led)
  }
  else {
    add(led)
    add(labelComponent)
  }
}

class MMUStatusPanel extends JPanel with MMUChangeListener {
  private val z = new LabelledLed("Z80",true)
  private val m2 = new LabelledLed("2Mhz",true)
  private val c64 = new LabelledLed("64",true)
  private val _1571 = new LabelledLed("1571",true)
  
  setLayout(new FlowLayout(FlowLayout.LEFT,2,0))
  add(z)
  add(m2)
  add(c64)
  add(_1571)
  
  def frequencyChanged(f:Int) : Unit = { m2.on = f == 2 ; m2.repaint() }
  def cpuChanged(is8502:Boolean) : Unit = { z.on = !is8502 ; z.repaint() }
  def c64Mode(c64Mode:Boolean) : Unit = { c64.on = c64Mode ; c64.repaint() }
  def fastSerialDirection(input:Boolean) : Unit = {}
  def _1571mode(_1571Mode:Boolean) : Unit = { _1571.on = _1571Mode ; _1571.repaint() }
}