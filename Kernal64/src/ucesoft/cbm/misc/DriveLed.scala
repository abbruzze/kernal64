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
    var powerOn,driveLedOn,driveWriteMode = false
    
    private[this] final val LED_OFF = Color.DARK_GRAY
    private[this] final val LED_READ_ON = Color.RED
    private[this] final val LED_WRITE_ON = Color.ORANGE
    private[this] final val POWER_LED_ON = Color.RED
    private[this] final val POWER_ACCESS_LED_ON = Color.GREEN
    private[this] var powerLedMode = false
    
    def setPowerLedMode(on:Boolean) = powerLedMode = on
    
    setPreferredSize(new Dimension(15,15))
    override def paint(g:Graphics) {
      if (powerLedMode) {
        val size = getSize()
        g.setColor(Color.GRAY)
        g.fillRect(1,1,size.width - 1,size.height - 1)        
        val height = size.height
        size.height = (size.height * 0.5).toInt
        val midH = height - size.height        
        drawLed(powerOn,0,0,g,size,true,POWER_LED_ON)
        drawLed(driveLedOn,0,midH,g,size,false,POWER_ACCESS_LED_ON)
      }
      else {
        drawLed(driveLedOn,0,0,g,getSize,false,LED_READ_ON)
      }
    }   
    
    private def drawLed(on:Boolean,x:Int,y:Int,g:Graphics,size:Dimension,isPowerLed:Boolean,ledColor:Color) {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(x,y,size.width - 1,size.height - 1)
      val color = if (on) {
        if (isPowerLed) POWER_LED_ON
        else
        if (driveWriteMode) LED_WRITE_ON 
        else ledColor
      }
      else Color.DARK_GRAY
      g2.setColor(color)
      g2.fillRect(x + 1,y + 1,size.width - 1,size.height - 1)
    }
    
    override def getProperties = {
      properties.setProperty("Led on",driveLedOn.toString)
      properties
    }
    
    def init {}
    def reset {}
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(powerOn)
      out.writeBoolean(driveLedOn)
      out.writeBoolean(driveWriteMode)
      out.writeBoolean(powerLedMode)
    }
    protected def loadState(in:ObjectInputStream) {
      powerOn = in.readBoolean
      driveLedOn = in.readBoolean
      driveWriteMode = in.readBoolean
      powerLedMode = in.readBoolean
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }