package ucesoft.cbm

import javax.swing.{SwingUtilities, UIManager}
import ucesoft.cbm.misc.Settings

object CBMComputer {
  def turnOn(computer : => CBMComputer,args:Array[String]) : Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    val cbm = computer
    try {
      cbm.turnOn(args)
    }
    catch {
      case i:Settings.SettingIllegalArgumentException =>
        println(s"Bad command line argument: ${i.getMessage}")
        sys.exit(1)
      case t:Throwable =>
        cbm.errorHandler(t)
        if (cbm.isHeadless) sys.exit(1)
    }
  }
}

trait CBMComputer extends CBMComponent {
  def turnOn(args:Array[String]) : Unit
  def turnOff : Unit
  def errorHandler(t:Throwable) : Unit

  protected def swing(f: => Unit) : Unit = SwingUtilities.invokeAndWait(() => f)
}
