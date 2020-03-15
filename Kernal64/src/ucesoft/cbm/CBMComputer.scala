package ucesoft.cbm

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.Properties

import javax.swing.filechooser.FileFilter
import javax.swing.{JFileChooser, SwingUtilities, UIManager}
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
  protected val CONFIGURATION_FILENAME : String
  protected val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  protected val CONFIGURATION_FRAME_XY = "frame.xy"
  protected val CONFIGURATION_FRAME_DIM = "frame.dim"
  protected val CONFIGURATION_KEYB_MAP_FILE = "keyb.map.file"
  protected val CONFIGURATION_GMOD2_FILE = "gmod2.file"
  protected val CONFIGURATION_AUTOSAVE = "autosave"

  protected val clock : Clock
  protected val configuration : Properties

  def turnOn(args:Array[String]) : Unit
  def turnOff : Unit
  def errorHandler(t:Throwable) : Unit

  protected def swing(f: => Unit) : Unit = SwingUtilities.invokeAndWait(() => f)

  protected def reset(play:Boolean) : Unit

  protected def loadState(fileName:Option[String]) : Unit = {
    clock.pause
    var in : ObjectInputStream = null
    try {
      val canLoad = allowsState
      if (!canLoad) {

        showError("State saving error","Can't load state")
        return
      }
      val fn = fileName match {
        case None =>
          val fc = new JFileChooser
          fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
          fc.setFileFilter(new FileFilter {
            def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
            def getDescription = "Kernal64 state files"
          })
          fc.setDialogTitle("Choose a state file to load")
          fc.showOpenDialog(getActiveFrame.get) match {
            case JFileChooser.APPROVE_OPTION =>
              Some(fc.getSelectedFile)
            case _ =>
              None
          }
        case Some(fn) =>
          Some(new File(fn))
      }

      fn foreach { fn =>
        in = new ObjectInputStream(new FileInputStream(fn))
        reset(false)
        load(in)
      }
    }
    catch {
      case t:Throwable =>
        showError("State loading error","Can't load state. Unexpected error occurred: " + t)
        t.printStackTrace
        reset(false)
    }
    finally {
      if (in != null) in.close
      clock.play
    }
  }

  protected def saveState() : Unit = {
    clock.pause
    var out : ObjectOutputStream = null
    try {
      val canSave = allowsState
      if (!canSave) {

        showError("State saving error","Can't save state")
        return
      }
      val fc = new JFileChooser
      fc.setDialogTitle("Choose where to save current state")
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
      fc.setFileFilter(new FileFilter {
        def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
        def getDescription = "Kernal64 state files"
      })
      val fn = fc.showSaveDialog(getActiveFrame.get) match {
        case JFileChooser.APPROVE_OPTION =>
          if (fc.getSelectedFile.getName.toUpperCase.endsWith(".K64")) fc.getSelectedFile.toString else fc.getSelectedFile.toString + ".k64"
        case _ =>
          return
      }
      out = new ObjectOutputStream(new FileOutputStream(fn))
      save(out)
      out.close
    }
    catch {
      case t:Throwable =>

        showError("State saving error","Can't save state. Unexpected error occurred: " + t)
        t.printStackTrace
    }
    finally {
      if (out != null) out.close
      clock.play
    }
  }
}
