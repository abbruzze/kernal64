package ucesoft.cbm.cbm2

import ucesoft.cbm.{CBMComponentType, CBMComputer, CBMComputerModel, CBMIIModel, ClockEvent, Log}
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.formats.{Diskette, ProgramLoader}
import ucesoft.cbm.misc.{BasicListExplorer, Preferences, VolumeSettingsPanel}
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.drive.{DriveType, IEEE488Drive}
import ucesoft.cbm.peripheral.keyboard.{BKeyboard, Keyboard}
import ucesoft.cbm.peripheral.printer.{IEEE488MPS803, MPS803GFXDriver, MPS803ROM, Printer}
import ucesoft.cbm.peripheral.sid.SID

import java.awt.datatransfer.DataFlavor
import java.awt.{Dimension, Toolkit}
import java.io.{File, FileNotFoundException, ObjectInputStream, ObjectOutputStream}
import javax.swing.{JDialog, JMenu}

object CBMII extends App {
  CBMComputer.turnOn(new CBMII,args)
}

class CBMII extends CBMComputer {
  override val componentID: String = "CBMII"
  override val componentType = CBMComponentType.INTERNAL
  override protected val cbmModel: CBMComputerModel = CBMIIModel
  override protected val APPLICATION_NAME: String = "CBMII"
  override protected val CONFIGURATION_FILENAME: String = "CBMII.config"
  override protected val keyb: Keyboard = new BKeyboard(BKeyboard.DEF_CBM2_KEYMAPPER)
  protected val cbmmmu = new CBM2MMU
  override protected val mmu: Memory = cbmmmu
  protected val sid : SID = new SID()
  override protected lazy val volumeDialog: JDialog = VolumeSettingsPanel.getDialog(displayFrame,sid.getDriver)
  protected val bus = new IEEE488Bus
  override protected val printer: Printer = new IEEE488MPS803("MPS803",4,bus,new MPS803GFXDriver(new MPS803ROM))

  override protected def PRG_LOAD_ADDRESS() = 0x3
  override protected def PRG_RUN_DELAY_CYCLES = 2200000 // TODO

  def turnOn(args: Array[String]): Unit = {
    swing {
      setMenu(false,false)
    }
    // check help
    if (preferences.checkForHelp(args)) {
      println(s"CBMII, CBMII (models 610,620,710,720) emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      preferences.printUsage("file to attach")
      sys.exit(0)
    }
    // --headless handling to disable logging & debugging
    if (args.exists(_ == "--headless")) headless = true
    swing {
      initComponent
    }
    // CRT
    swing {
      displayFrame.pack()
    }
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    // screen's dimension and size restoration
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing {
        updateScreenDimension(new Dimension(dim(0), dim(1)))
      }
    }
    else crtZoom(2)
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map {_.toInt }
      swing {
        displayFrame.setLocation(xy(0), xy(1))
      }
    }
    else displayFrame.setLocationByPlatform(true)
    // SETTINGS
    loadSettings(args)
    // VIEW
    swing {
      displayFrame.setVisible(!headless)
      if (fullScreenAtBoot) setCRTFullScreen()
    }
    // PLAY
    clock.play
  }

  protected def updateScreenDimension(dim:Dimension): Unit = {
    // TODO
  }

  protected def crtZoom(f:Int): Unit = {
    // TODO
  }

  protected def setCRTFullScreen(): Unit = {
    // TODO
  }

  override protected def listBASIC(): Unit = {
    clock.pause()
    BasicListExplorer.listCBM2(cbmmmu, 3)
    clock.play()
  }

  override protected def delayedAutorun(fn: String): Unit = {
    val cmd = s"""DLOAD"$fn"""" + 13.toChar + "RUN" + 13.toChar
    clock.schedule(new ClockEvent("Loading", clock.currentCycles + PRG_RUN_DELAY_CYCLES, _ => BKeyboard.insertTextIntoKeyboardBuffer(cmd, cbmmmu)))
  }

  override protected def attachDevice(file: File, autorun: Boolean, fileToLoad: Option[String] = None, emulateInserting: Boolean = true): Unit = {
    val name = file.getName.toUpperCase

    if (name.endsWith(".PRG")) {
      loadPRGFile(file, autorun)
      lastLoadedPrg = Some(file)
    }
    else if (name.endsWith(".D80")) attachDiskFile(0, file, autorun, fileToLoad, emulateInserting)
    //TODO: else if (tapeAllowed && name.endsWith(".TAP")) attachTapeFile(file, None, autorun)
  }

  override protected def attachDiskFile(driveID: Int, file: File, autorun: Boolean, fileToLoad: Option[String], emulateInserting: Boolean = true): Unit = {
    try {
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      if (!file.isDirectory) {
        val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
        if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      }

      val disk = Diskette(file.toString)
      disk.canWriteOnDisk = canWriteOnDisk
      disk.flushListener = diskFlusher
      drives(driveID).getFloppy.close
      if (traceDialog != null && !traceDialog.isTracing) clock.pause
      drives(driveID).setDriveReader(disk, emulateInserting)
      preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_FILE(driveID), file.toString)
      clock.play

      loadFileItems(driveID).setEnabled(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      val drive = driveID + 8
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""DLOAD"$fn",u$drive""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          BKeyboard.insertTextIntoKeyboardBuffer(cmd, cbmmmu)
        case None if autorun =>
          val cmd = s"""DLOAD"*",u$drive""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          BKeyboard.insertTextIntoKeyboardBuffer(cmd,cbmmmu)
        case _ =>
      }
      driveLeds(driveID).setToolTipText(disk.toString)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()

        showError("Disk attaching error", t.toString)
    }
  }

  override protected def setFileMenu(menu: JMenu): Unit = ???

  override protected def getCharROM: Memory = ???

  override protected def initComputer(): Unit = ???

  override protected def handleDND(file: File, _reset: Boolean, autorun: Boolean): Unit = {
    if (_reset) reset(false)
    if (autorun) {
      clock.schedule(new ClockEvent("Loading", clock.currentCycles + PRG_RUN_DELAY_CYCLES, _ => attachDevice(file, true, None, false)))
      clock.play
    }
    else {
      attachDevice(file, false)
    }
  }

  override protected def loadPRGFile(file: File, autorun: Boolean): Unit = {
    val (start,end) = ProgramLoader.loadCBMIIPRG(cbmmmu,file)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      BKeyboard.insertTextIntoKeyboardBuffer("RUN" + 13.toChar,cbmmmu)
    }
  }

  override protected def saveSettings(save: Boolean): Unit = {
    if (!ignoreConfig) {
      if (!zoomOverride) {
        val dimension = display.getSize()
        configuration.setProperty(CONFIGURATION_FRAME_DIM, dimension.width + "," + dimension.height)
      }
      configuration.setProperty(CONFIGURATION_FRAME_XY, displayFrame.getX + "," + displayFrame.getY)
      if (save) {
        preferences.save(configuration)
        println("Settings saved")
      }
      saveConfigurationFile
    }
  }

  override protected def setSettingsMenu(optionsMenu: JMenu): Unit = ???

  override protected def paste(): Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      BKeyboard.insertTextIntoKeyboardBuffer(str, cbmmmu)
    }
  }

  override protected def mainLoop(cycles: Long): Unit = ???

  override protected def initDrive(id: Int, driveType: DriveType.Value): Unit = {
    val old = Option(drives(id))
    old match {
      case Some(od) if od.driveType == driveType => return
      case _ =>
    }
    drives(id) = driveType match {
      case DriveType._8050 =>
        driveLedListeners(id).setPowerLedMode(false)
        new IEEE488Drive(s"8050_${id + 8}",id + 8,bus,driveLedListeners(id))
    }

    old match {
      case None =>
        add(drives(id))
      case Some(c) =>
        floppyComponents(id).drive = drives(id)
        c.getFloppy.close
        c.disconnect
        drivesRunning(id) = true
        drives(id).initComponent
        change(c, drives(id))
        inspectDialog.updateRoot
        if (id == 0) {
          diskTraceDialog.mem = drives(id).getMem
          diskTraceDialog.traceListener = drives(id)
        }
    }

    drives(id).runningListener = running => {
      drivesRunning(id) = running
    }
  }

  override def reset(): Unit = ???

  override def init(): Unit = ???

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = ???
}
