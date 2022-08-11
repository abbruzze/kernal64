package ucesoft.cbm.cbm2

import ucesoft.cbm.cbm2.IEEE488Connectors.{CIAIEEE488ConnectorA, CIAIEEE488ConnectorB, IEEE488InterfaceA, IEEE488InterfaceB}
import ucesoft.cbm.{CBMComponentType, CBMComputer, CBMComputerModel, CBMIIModel, ClockEvent, Log}
import ucesoft.cbm.cpu.{CPU6510_CE, Memory}
import ucesoft.cbm.formats.{Diskette, ProgramLoader}
import ucesoft.cbm.misc.{BasicListExplorer, Preferences, Switcher, TestCart, VolumeSettingsPanel}
import ucesoft.cbm.peripheral.EmptyConnector
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.crtc.CRTC6845
import ucesoft.cbm.peripheral.drive.{DriveType, IEEE488Drive}
import ucesoft.cbm.peripheral.keyboard.{BKeyboard, Keyboard}
import ucesoft.cbm.peripheral.mos6525.MOS6525
import ucesoft.cbm.peripheral.mos6551.ACIA6551
import ucesoft.cbm.peripheral.printer.{IEEE488MPS803, MPS803GFXDriver, MPS803ROM, Printer}
import ucesoft.cbm.peripheral.sid.SID

import java.awt.datatransfer.DataFlavor
import java.awt.{Dimension, Toolkit}
import java.io.{File, FileNotFoundException, ObjectInputStream, ObjectOutputStream, PrintWriter, StringWriter}
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
  override protected val keyb = new BKeyboard(BKeyboard.DEF_CBM2_KEYMAPPER)

  override protected val mmu = new CBM2MMU
  protected val sid : SID = new SID()
  override protected lazy val volumeDialog: JDialog = VolumeSettingsPanel.getDialog(displayFrame,sid.getDriver)
  protected val bus = new IEEE488Bus
  override protected val printer: Printer = new IEEE488MPS803("MPS803",4,bus,new MPS803GFXDriver(new MPS803ROM))

  protected val _50_60_CYCLES = 2000000 / 50 // 50Hz
  protected var ciaieee, ciaip: CIA = _
  protected var tpiIeee: MOS6525 = _
  protected var tpiKb: MOS6525 = _
  protected var crt: CRTC6845 = _
  protected var acia: ACIA6551 = _

  protected var model : CBM2Model = _610PAL

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
    BasicListExplorer.listCBM2(mmu, 3)
    clock.play()
  }

  override protected def delayedAutorun(fn: String): Unit = {
    val cmd = s"""DLOAD"$fn"""" + 13.toChar + "RUN" + 13.toChar
    clock.schedule(new ClockEvent("Loading", clock.currentCycles + PRG_RUN_DELAY_CYCLES, _ => BKeyboard.insertTextIntoKeyboardBuffer(cmd, mmu)))
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
          BKeyboard.insertTextIntoKeyboardBuffer(cmd, mmu)
        case None if autorun =>
          val cmd = s"""DLOAD"*",u$drive""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          BKeyboard.insertTextIntoKeyboardBuffer(cmd,mmu)
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
    val (start,end) = ProgramLoader.loadCBMIIPRG(mmu,file)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      BKeyboard.insertTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu)
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
      BKeyboard.insertTextIntoKeyboardBuffer(str, mmu)
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

  override def init(): Unit = {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo

    Log.info("Building the system ...")

    mmu.setCPU(cpu.asInstanceOf[CPU6510_CE])
    mmu.setModel(model)

    val irq = new Switcher("IRQ",low => cpu.irqRequest(low))

    // frequency 2Mhz
    sid.setCPUFrequency(2000000)
    clock.setClockHz(2000000)

    // drive
    initializedDrives(DriveType._8050)
    // chips
    // ============== CIA-IEEE ==============================================
    val ciaConnectorA = new CIAIEEE488ConnectorA(bus)
    ciaieee = new CIA("CIAIEEE",
      0xDC00,
      ciaConnectorA,
      new CIAIEEE488ConnectorB,
      low => tpiIeee.setInterruptPin(MOS6525.INT_I2, if (low) 0 else 1),
      _ => {}
    )
    datassette = new Datassette(ciaieee.setFlagLow _)
    // ============== CIA-IP ================================================
    ciaip = new CIA("CIAIP",
      0xDB00,
      EmptyConnector,
      EmptyConnector,
      low => tpiIeee.setInterruptPin(MOS6525.INT_I3, if (low) 0 else 1),
      _ => {}
    )
    // ============== TPI-IEEE ==============================================
    tpiIeee = new MOS6525(
      "tpiIeee",
      new IEEE488InterfaceA(bus, ciaConnectorA),
      //new IECInterfaceB(iec,ciaieee.setFlagLow _),
      new IEEE488InterfaceB(bus, datassette),
      new MOS6525.PortC {
        override def read(): Int = {
          (tpiIeee.regs(MOS6525.PRC) & tpiIeee.regs(MOS6525.DDRC)) | (0xFF & ~tpiIeee.regs(MOS6525.DDRC))
        }
        override def write(value: Int): Unit = {}
        override def setCA(bit: Int): Unit = crt.setCharAddressMask(bit << 12)
        override def setCB(bit: Int): Unit = {}
      },
      _irq => irq.setLine(0x20, _irq) // MASTER IRQ
    )
    tpiKb = new MOS6525(
      "tpiKb",
      new MOS6525.PortAB {
        override def read(): Int = (tpiKb.regs(MOS6525.PRA) & tpiKb.regs(MOS6525.DDRA)) | (0xFF & ~tpiKb.regs(MOS6525.DDRA))
        override def write(value: Int): Unit = keyb.selectHighColAddress(value)
      },
      new MOS6525.PortAB {
        override def read(): Int = (tpiKb.regs(MOS6525.PRB) & tpiKb.regs(MOS6525.DDRB)) | (0xFF & ~tpiKb.regs(MOS6525.DDRB))
        override def write(value: Int): Unit = keyb.selectLowColAddress(value)
      },
      new MOS6525.PortC {
        override def read(): Int = (keyb.read() & 0x3F) | (if (model.isPAL) 0x00 else 0x40) | (if (model.lowProfile) 0x00 else 0x80)
        override def write(value: Int): Unit = {}
        override def setCA(bit: Int): Unit = {}
        override def setCB(bit: Int): Unit = {}
      },
      _ => {}
    )
    // ============== ACIA ==================================================
    acia = new ACIA6551(irq => tpiIeee.setInterruptPin(MOS6525.INT_I4,if (irq) 0 else 1))

    mmu.setIO(crt,ciaieee,ciaip,tpiKb,tpiIeee,sid,acia)

    // 50/60 Hz source
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))

    TestCart.setCartLocation(0xFDAFF)
    //TestCart.enabled = true

    // components
    add(clock)
    add(mmu)
    add(cpu)
    add(keyb)
    add(bus)
    add(sid)
    add(ciaieee)
    add(ciaip)
    add(tpiIeee)
  }

  private def _50_60_Hz(cycles: Long): Unit = {
    tpiIeee.setInterruptPin(MOS6525.INT_I0, 1)
    tpiIeee.setInterruptPin(MOS6525.INT_I0, 0)
    clock.schedule(new ClockEvent("50_60Hz", cycles + _50_60_CYCLES, _50_60_Hz _))
  }

  override def afterInitHook(): Unit = {
    mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
    mmu.setKernalROM(CBM2MMU.KERNAL_ROM.getROMBytes())

    crt = new CRTC6845(mmu.getCRTCRam,CBM2MMU.CHAR_ROM.getROMBytes(),16)
    crt.setDisplay(display)
    crt.setClipping(model.crtClip._1,model.crtClip._2,model.crtClip._3,model.crtClip._4)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = ???
}
