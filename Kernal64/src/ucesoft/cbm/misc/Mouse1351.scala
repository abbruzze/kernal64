package ucesoft.cbm.misc

import java.awt.event.MouseAdapter
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.CBMComponentType
import java.awt.event.MouseEvent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.peripheral.sid.SID

class Mouse1351(display:Display,mouseEnabled: => Boolean,sid:SID) extends MouseAdapter with CBMComponent {
    val componentID = "Mouse Commodore 1351"
    val componentType = CBMComponentType.INPUT_DEVICE
    private[this] var x,y = 0
    
    override def mouseMoved(e:MouseEvent) = if (mouseEnabled) {
      x = display.getMouseX & 0x7F
      y = 0x7F - display.getMouseY & 0x7F
      sid.write(0xD419,x << 1)
      sid.write(0xD41A,y << 1)
    }
    
    override def mouseDragged(e:MouseEvent) = if (mouseEnabled) {
      x = display.getMouseX & 0x7F
      y = 0x7F - display.getMouseY & 0x7F
      sid.write(0xD419,x << 1)
      sid.write(0xD41A,y << 1)
    }
    
    override def getProperties = {
      properties.setProperty("X",x.toString)
      properties.setProperty("Y",y.toString)
      properties
    }
    
    def init {}
    def reset {}
    // state
    protected def saveState(out:ObjectOutputStream) {}
    protected def loadState(in:ObjectInputStream) {}
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }