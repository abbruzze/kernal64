package ucesoft.cbm.misc

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class IRQSwitcher(irqHandler: (Boolean) => Unit) extends CBMComponent {
    val componentID = "IRQ Switcher (CIA,VIC)"
    val componentType = CBMComponentType.INTERNAL 
    
    private[this] var ciaIRQLow = false
    private[this] var vicIRQLow = false
    private[this] var expPortIRQLow = false
    
    @inline private def handleIRQ = {
      //Log.debug(s"Handling IRQ ciaIRQ=${ciaIRQLow} vicIRQ=${vicIRQLow}")
      irqHandler(ciaIRQLow || vicIRQLow || expPortIRQLow)
    }
    
    final def ciaIRQ(low:Boolean) {     
      //Log.debug("CIA setting IRQ as " + low)
      ciaIRQLow = low
      handleIRQ
    }
    final def vicIRQ(low:Boolean) {  
      //Log.debug("VIC setting IRQ as " + low)
      vicIRQLow = low
      handleIRQ
    }
    
    final def expPortIRQ(low:Boolean) {
      expPortIRQLow = low
      handleIRQ
    }
    
    override def getProperties = {
      properties.setProperty("CIA1 IRQ",ciaIRQLow.toString)
      properties.setProperty("VIC IRQ",vicIRQLow.toString)
      properties.setProperty("Expansion port IRQ",expPortIRQLow.toString)
      properties
    }
    
    def init {}
    
    def reset {
      ciaIRQLow = false
      vicIRQLow = false
      expPortIRQLow = false
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(ciaIRQLow)
      out.writeBoolean(vicIRQLow)
      out.writeBoolean(expPortIRQLow)
    }
    protected def loadState(in:ObjectInputStream) {
      ciaIRQLow = in.readBoolean
      vicIRQLow = in.readBoolean
      expPortIRQLow = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }