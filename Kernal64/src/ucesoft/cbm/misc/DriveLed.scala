package ucesoft.cbm.misc

import javax.swing.JComponent
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class DriveLed extends JComponent with CBMComponent {
    val componentID = "Drive led"
    val componentType = CBMComponentType.INTERNAL
    var driveLedOn,driveWriteMode = false
    
    private[this] val LED_OFF = Color.DARK_GRAY
    private[this] val LED_READ_ON = Color.RED
    private[this] val LED_WRITE_ON = Color.ORANGE
    
    setPreferredSize(new Dimension(15,15))
    override def paint(g:Graphics) {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      val color = if (driveLedOn) {
        if (driveWriteMode) LED_WRITE_ON else LED_READ_ON
      }
      else Color.DARK_GRAY
      g2.setColor(color)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    }   
    
    override def getProperties = {
      properties.setProperty("Led on",driveLedOn.toString)
      properties
    }
    
    def init {}
    def reset {}
    // state
    protected def saveState(out:ObjectOutputStream) {}
    protected def loadState(in:ObjectInputStream) {}
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }