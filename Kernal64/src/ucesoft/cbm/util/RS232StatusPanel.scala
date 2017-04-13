package ucesoft.cbm.util

import javax.swing._
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Font
import ucesoft.cbm.peripheral.rs232.RS232StatusListener
import ucesoft.cbm.peripheral.rs232.RS232._

class RS232StatusPanel extends JPanel with RS232StatusListener {
  private[this] val FONT = new Font("Monospaced",Font.BOLD,10)
  
  private[this] class SignalComponent extends JComponent {
    private[this] var value = 0
    
    setPreferredSize(new Dimension(10,10))
    
    override def paint(g:Graphics) {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      g2.setColor(if (value > 0) Color.GREEN else Color.DARK_GRAY)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    } 
    
    def setValue(value:Int) {
      this.value = value
      repaint()
    }
  }
  
  private[this] class SignalPanel(signalName:String) extends JPanel {
    private[this] val signal = new SignalComponent
    private[this] val label = new JLabel(signalName + ":",SwingConstants.RIGHT)
    
    label.setFont(FONT)
    add(label)
    add(signal)
    
    def setValue(value:Int) = signal.setValue(value)
  }
    
  private[this] val rtsSignal = new SignalPanel("RTS")
  private[this] val dtrSignal = new SignalPanel("DTR")
  private[this] val rxdSignal = new SignalPanel("RXD")
  private[this] val txdSignal = new SignalPanel("TXD")
  private[this] val enabledSignal = new SignalPanel("RS-232")
  
  add(enabledSignal)
  add(rtsSignal)
  add(dtrSignal)
  add(rxdSignal)
  add(txdSignal)
  setVisible(false)
  
  def update(signal:Int,value:Int) {
    signal match {
      case RTS => rtsSignal.setValue(value)
      case DTR => dtrSignal.setValue(value)
      case RXD => rxdSignal.setValue(value)
      case TXD => txdSignal.setValue(value)
      case _ =>
    }
  }
  
  def setRS232Enabled(enabled:Boolean) {
    SwingUtilities.invokeLater(new Runnable {
      def run {
        if (enabled) enabledSignal.setValue(1) else enabledSignal.setValue(0)
      }
    })    
  }
}