package ucesoft.cbm.misc

import javax.swing.JProgressBar
import java.awt.Dimension

class DriveLoadProgressPanel extends JProgressBar {
    setPreferredSize(new Dimension(150,15))
    setStringPainted(true)
    setString("")
    setVisible(false)
    
    def beginLoading(msg:String) : Unit = {
      setVisible(true)
      setValue(0)
      setString(msg)
    }
    
    def updateValue(perc:Int) : Unit = {
      setValue(perc)
    }
    
    def endLoading  : Unit = {
      setVisible(false)
    }
  }