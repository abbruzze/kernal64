package ucesoft.cbm.misc

import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class NMISwitcher(nmiHandler:(Boolean) => Unit) extends CBMComponent {
    val componentID = "NMI Switcher (CIA2)"
    val componentType = CBMComponentType.INTERNAL

    private var keyboardNMILow = false
    private var cia2NMILow = false
    private var expPortNMILow = false

    def keyboardNMIAction(low: Boolean) : Unit = {
      keyboardNMILow = low
      handleNMI
    }
    def cia2NMIAction(low: Boolean) : Unit = {
      cia2NMILow = low
      handleNMI
    }
    def expansionPortNMI(low: Boolean) : Unit = {
      expPortNMILow = low
      handleNMI
    }

    @inline private def handleNMI = nmiHandler(keyboardNMILow || cia2NMILow || expPortNMILow)

    override def getProperties = {
      properties.setProperty("CIA2 NMI", cia2NMILow.toString)
      properties.setProperty("Keyboard restore NMI", keyboardNMILow.toString)
      properties.setProperty("Expansion port NMI", expPortNMILow.toString)
      properties
    }

    def init  : Unit = {}

    def reset  : Unit = {
      keyboardNMILow = false
      cia2NMILow = false
      expPortNMILow = false
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      out.writeBoolean(keyboardNMILow)
      out.writeBoolean(cia2NMILow)
      out.writeBoolean(expPortNMILow)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      keyboardNMILow = in.readBoolean
      cia2NMILow = in.readBoolean
      expPortNMILow = in.readBoolean
    }
    protected def allowsStateRestoring : Boolean = true
  }