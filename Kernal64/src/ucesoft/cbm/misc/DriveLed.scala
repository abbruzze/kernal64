package ucesoft.cbm.misc

import ucesoft.cbm.{CBMComponent, CBMComponentType}
import ucesoft.cbm.CBMComponentType.Type

import java.awt._
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties
import javax.swing.{JComponent, JLabel}

class DriveLed(id:Int) extends JComponent with CBMComponent {
  val componentID = "Drive led"
  val componentType: Type = CBMComponentType.INTERNAL
  var powerOn,driveLedOn,driveWriteMode = false

  private[this] final val LED_OFF = Color.DARK_GRAY
  private[this] final val LED_READ_ON = Color.RED
  private[this] final val LED_WRITE_ON = Color.ORANGE
  private[this] final val POWER_LED_ON = Color.RED
  private[this] final val POWER_ACCESS_LED_ON = Color.GREEN
  private[this] var powerLedMode = false
  private[this] val label = new JLabel("")

  setLayout(new FlowLayout(FlowLayout.LEFT))
  add(new JLabel("%2d:".format(id)))
  add(new Led)
  add(label)

  def setPowerLedMode(on:Boolean): Unit = {
    powerLedMode = on
    repaint()
  }

  def showLedInfo(info:String) : Unit = {
    label.setText("%s".format(info))
  }

  private class Led extends JComponent {
    setPreferredSize(new Dimension(15,15))
    override def paint(g:Graphics) : Unit = {
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

    private def drawLed(on:Boolean,x:Int,y:Int,g:Graphics,size:Dimension,isPowerLed:Boolean,ledColor:Color) : Unit = {
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
  }

  override def getProperties: Properties = {
    properties.setProperty("Led on",driveLedOn.toString)
    properties
  }

  def init(): Unit = {}
  def reset(): Unit = {}
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(powerOn)
    out.writeBoolean(driveLedOn)
    out.writeBoolean(driveWriteMode)
    out.writeBoolean(powerLedMode)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    powerOn = in.readBoolean
    driveLedOn = in.readBoolean
    driveWriteMode = in.readBoolean
    powerLedMode = in.readBoolean
  }
  protected def allowsStateRestoring : Boolean = true
}