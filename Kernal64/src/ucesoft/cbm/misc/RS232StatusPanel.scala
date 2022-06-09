package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.rs232.RS232._
import ucesoft.cbm.peripheral.rs232.RS232StatusListener

import java.awt._
import javax.swing._

class RS232StatusPanel extends JPanel with RS232StatusListener {
  private[this] val FONT = new Font("Monospaced",Font.BOLD,10)
  private[this] var connectionTS = -1L
  private[this] var connected : Option[String] = None
  
  private[this] class SignalComponent extends JComponent {
    private[this] var value = 0
    
    setPreferredSize(new Dimension(10,10))
    
    override def paint(g:Graphics) : Unit = {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      g2.setColor(if (value > 0) Color.GREEN else Color.DARK_GRAY)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    } 
    
    def setValue(value:Int) : Unit = {
      this.value = value
      repaint()
    }
  }
  
  private[this] class SignalPanel(signalName:String,showConnectionTime:Boolean = false) extends JPanel {
    private[this] val signal = new SignalComponent
    private[this] val label = new JLabel(signalName + ":",SwingConstants.RIGHT)
    
    label.setFont(FONT)
    add(label)
    add(signal)
    
    def setValue(value:Int): Unit = signal.setValue(value)
    def setColor(c:Color): Unit = label.setForeground(c)

    override def getToolTipText: String = {
      if (!showConnectionTime || connectionTS == -1) super.getToolTipText
      else {
        val deltaSec = (System.currentTimeMillis() - connectionTS) / 1000
        val min = deltaSec / 60
        val sec = deltaSec % 60
        connected.getOrElse("") + " " + super.getToolTipText + s" ($min:$sec)"
      }
    }
  }
    
  private[this] val rtsSignal = new SignalPanel("RTS")
  private[this] val dtrSignal = new SignalPanel("DTR")
  private[this] val rxdSignal = new SignalPanel("RXD")
  private[this] val txdSignal = new SignalPanel("TXD")
  private[this] val enabledSignal = new SignalPanel("Internet",true)
  
  add(enabledSignal)
  add(rtsSignal)
  add(dtrSignal)
  add(rxdSignal)
  add(txdSignal)
  setVisible(false)

  def connectedTo(address:String): Unit = {
    connected = Some(address)
  }
  def disconnected: Unit = {
    connected =  None
  }
  
  def update(signal:Int,value:Int) : Unit = {
    signal match {
      case RTS => rtsSignal.setValue(value)
      case DTR => dtrSignal.setValue(value)
      case RXD => rxdSignal.setValue(value)
      case TXD => txdSignal.setValue(value)
      case _ =>
    }
  }
  
  def setRS232Enabled(enabled:Boolean) : Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run()  : Unit = {
        if (enabled) {
          enabledSignal.setValue(1)
          enabledSignal.setToolTipText("Connected")
          enabledSignal.setColor(Color.BLUE)
          connectionTS = System.currentTimeMillis()
        }
        else {
          enabledSignal.setValue(0)
          enabledSignal.setToolTipText("NOT connected")
          enabledSignal.setColor(Color.RED)
          connectionTS = -1
        }
      }
    })    
  }
}