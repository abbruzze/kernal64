package ucesoft.cbm.cbm2

import ucesoft.cbm.cbm2.IEEE488Connectors.{CIAIEEE488ConnectorA, CIAIEEE488ConnectorB, IEEE488InterfaceA, IEEE488InterfaceB}
import ucesoft.cbm.cpu.{CPU6510_CE, Memory}
import ucesoft.cbm.formats.{Diskette, ProgramLoader, TAP}
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral.EmptyConnector
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.crtc.CRTC6845
import ucesoft.cbm.peripheral.drive.{DriveType, IEEE488Drive}
import ucesoft.cbm.peripheral.keyboard.{BKeyboard, KeyboardMapper, KeyboardMapperStore}
import ucesoft.cbm.peripheral.mos6525.MOS6525
import ucesoft.cbm.peripheral.mos6551.ACIA6551
import ucesoft.cbm.peripheral.printer.{IEEE488MPS803, MPS803GFXDriver, MPS803ROM, Printer}
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.trace.TraceDialog
import ucesoft.cbm._

import java.awt.datatransfer.DataFlavor
import java.awt.{Dimension, Toolkit}
import java.io._
import javax.swing.filechooser.FileFilter
import javax.swing._

object CBMII extends App {
  CBMComputer.turnOn(new CBMII,args)
}

class CBMII extends CBMComputer {
  override protected val ALLOWED_DRIVE_TYPES = DrivesConfigPanel.ALL_IEEE488_DRIVES_ALLOWED
  override val componentID: String = "CBMII"
  override val componentType = CBMComponentType.INTERNAL
  override protected val cbmModel: CBMComputerModel = CBMIIModel
  override protected val APPLICATION_NAME: String = "CBMII"
  override protected val CONFIGURATION_FILENAME: String = "CBMII.config"
  protected val keybMapper : KeyboardMapper = KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_cbm2",CBMIIModel)
  override protected val keyb = new BKeyboard(keybMapper)

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
    crt.play()
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

  override protected def attachDisk(driveID: Int, autorun: Boolean, c64Mode: Boolean): Unit = {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc, getCharROM, false,false,16,16)
    canvas.bgColor(0)
    canvas.fgColor(5)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setDialogTitle(s"Attach disk to drive ${driveID + 8}")
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File): Boolean = f.isDirectory || drives(driveID).formatExtList.exists { ext =>
        try {
          f.toString.toUpperCase.endsWith(ext)
        } catch {
          case _: Throwable => false
        }
      }

      def getDescription = s"${drives(driveID).formatExtList.mkString(",")} files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID, fc.getSelectedFile, autorun, canvas.selectedFile)
      case _ =>
    }
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

  override protected def attachTapeFile(file: File, tapFile: Option[TAP.TAPHeader], autorun: Boolean): Unit = {
    val tap = new TAP(file.toString)
    datassette.setTAP(Some(tap), tapFile.map(_.tapOffset.toInt))
    tapeMenu.setEnabled(true)
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      datassette.pressPlay
      BKeyboard.insertTextIntoKeyboardBuffer("LOAD" + 13.toChar + "RUN" + 13.toChar, mmu)
    }
  }

  protected def loadPrg(): Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")

      def getDescription = "PRG files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        loadPRGFile(fc.getSelectedFile, false)
        lastLoadedPrg = Some(fc.getSelectedFile)

      case _ =>
    }
  }

  override protected def attachTape(): Unit = {
    val fc = new JFileChooser
    val canvas = new TAPCanvas(fc, getCharROM, false,false,16,16)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP")

      def getDescription = "TAP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val tapFile = canvas.selectedObject.asInstanceOf[Option[TAP.TAPHeader]]
        attachTapeFile(fc.getSelectedFile, tapFile, tapFile.isDefined)
      case _ =>
    }
  }

  override protected def setFileMenu(fileMenu: JMenu): Unit = {
    import Preferences._

    if (tapeAllowed) {
      val attachTapeItem = new JMenuItem("Attach tape ...")
      attachTapeItem.addActionListener(_ => attachTape)
      fileMenu.add(attachTapeItem)

      tapeMenu.setEnabled(false)
      fileMenu.add(tapeMenu)

      val tapePlayItem = new JMenuItem("Press play")
      tapePlayItem.addActionListener(_ => datassette.pressPlay)
      tapeMenu.add(tapePlayItem)

      val tapeStopItem = new JMenuItem("Press stop")
      tapeStopItem.addActionListener(_ => datassette.pressStop)
      tapeMenu.add(tapeStopItem)

      val tapeRecordItem = new JMenuItem("Press record & play")
      tapeRecordItem.addActionListener(_ => datassette.pressRecordAndPlay)
      tapeMenu.add(tapeRecordItem)

      val tapeRewindItem = new JMenuItem("Press rewind")
      tapeRewindItem.addActionListener(_ => datassette.pressRewind)
      tapeMenu.add(tapeRewindItem)

      val tapeForwardItem = new JMenuItem("Press forward")
      tapeForwardItem.addActionListener(_ => datassette.pressForward)
      tapeMenu.add(tapeForwardItem)

      val tapeResetItem = new JMenuItem("Reset")
      tapeResetItem.addActionListener(_ => datassette.resetToStart)
      tapeMenu.add(tapeResetItem)

      val tapeResetCounterItem = new JMenuItem("Reset counter")
      tapeResetCounterItem.addActionListener(_ => datassette.resetCounter)
      tapeMenu.add(tapeResetCounterItem)
    }

    fileMenu.addSeparator()

    val makeDiskItem = new JMenuItem("Make empty disk ...")
    makeDiskItem.addActionListener(_ => makeDisk)
    fileMenu.add(makeDiskItem)

    val autorunDiskItem = new JMenuItem("Autorun disk ...")
    autorunDiskItem.addActionListener(_ => attachDisk(0, true, true))
    fileMenu.add(autorunDiskItem)

    val attackDiskItem = new JMenu("Attach disk ...")
    fileMenu.add(attackDiskItem)
    for (d <- 0 until TOTAL_DRIVES) {
      val attachDisk0Item = new JMenuItem(s"Attach disk ${d + 8}...")
      val key = if (d > 1) java.awt.event.KeyEvent.VK_0 + (d - 2) else java.awt.event.KeyEvent.VK_8 + d
      attachDisk0Item.setAccelerator(KeyStroke.getKeyStroke(key, java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
      attachDisk0Item.addActionListener(_ => attachDisk(d, false, isC64Mode))
      attackDiskItem.add(attachDisk0Item)
    }

    // For settings see below, after drive type

    val ejectMenu = new JMenu("Eject disk")
    fileMenu.add(ejectMenu)
    for (d <- 0 until TOTAL_DRIVES) {
      val ejectDisk0Item = new JMenuItem(s"Eject disk ${d + 8} ...")
      ejectDisk0Item.addActionListener(_ => ejectDisk(d))
      ejectMenu.add(ejectDisk0Item)
    }

    fileMenu.addSeparator()

    // ====================================================================================================
    val loadPrgItem = new JMenuItem("Load PRG file from local disk ...")
    loadPrgItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.ALT_DOWN_MASK))
    loadPrgItem.addActionListener(_ => loadPrg)
    fileMenu.add(loadPrgItem)

    // DRIVEX-ENABLED =====================================================================================
    for (d <- 1 until TOTAL_DRIVES) {
      preferences.add(PREF_DRIVE_X_ENABLED(d), s"Enabled/disable driver ${8 + d}", false) {
        enableDrive(d, _, false)
      }
    }
    // ====================================================================================================

    // WRITE-ON-DISK ======================================================================================
    val writeOnDiskCheckItem = new JCheckBoxMenuItem("Write changes on disk")
    writeOnDiskCheckItem.setSelected(true)
    writeOnDiskCheckItem.addActionListener(_ => preferences(PREF_WRITEONDISK) = writeOnDiskCheckItem.isSelected)
    fileMenu.add(writeOnDiskCheckItem)
    preferences.add(PREF_WRITEONDISK, "Tells if the modifications made on disks must be written on file", true) { wod =>
      writeOnDiskCheckItem.setSelected(wod)
      writeOnDiskSetting(wod)
    }
    // ====================================================================================================

    fileMenu.addSeparator()

    val resetMenu = new JMenu("Reset")
    fileMenu.add(resetMenu)
    val softResetItem = new JMenuItem("Soft Reset")
    softResetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.ALT_DOWN_MASK))
    softResetItem.addActionListener(_ => reset(true))
    resetMenu.add(softResetItem)
    val hardResetItem = new JMenuItem("Hard Reset")
    hardResetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    hardResetItem.addActionListener(_ => hardReset(true))
    resetMenu.add(hardResetItem)
    val reset2Item = new JMenuItem("Reset and run last PRG")
    reset2Item.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.CTRL_DOWN_MASK))
    reset2Item.addActionListener(_ => reset(true, true))
    resetMenu.add(reset2Item)

    fileMenu.addSeparator()
    // PREF-AUTO-SAVE ======================================================================================
    val autoSaveCheckItem = new JCheckBoxMenuItem("Autosave settings on exit")
    autoSaveCheckItem.addActionListener(e => preferences(PREF_PREFAUTOSAVE) = autoSaveCheckItem.isSelected)
    fileMenu.add(autoSaveCheckItem)
    preferences.add(PREF_PREFAUTOSAVE, "Auto save settings on exit", false) { auto =>
      autoSaveCheckItem.setSelected(auto)
    }
    // ====================================================================================================

    val saveSettingsCheckItem = new JMenuItem("Save settings")
    saveSettingsCheckItem.addActionListener(_ => saveSettings(true))
    fileMenu.add(saveSettingsCheckItem)

    fileMenu.addSeparator()

    val exitItem = new JMenuItem("Exit")
    exitItem.addActionListener(_ => turnOff)
    fileMenu.add(exitItem)
  }

  override protected def getCharROM: Memory = CBM2MMU.CHAR_ROM

  override protected def initComputer(): Unit = {}

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

  override protected def setSettingsMenu(optionMenu: JMenu): Unit = {
    setDriveMenu(optionMenu)

    optionMenu.addSeparator()

    val keybMenu = new JMenu("Keyboard")
    optionMenu.add(keybMenu)

    val keybEditorItem = new JMenuItem("Keyboard editor ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor(true))
    keybMenu.add(keybEditorItem)
    val loadKeybItem = new JMenuItem("Set keyboard layout ...")
    loadKeybItem.addActionListener(_ => loadKeyboard)
    keybMenu.add(loadKeybItem)

    optionMenu.addSeparator()

    setVolumeSettings(optionMenu)

    optionMenu.addSeparator()

    setWarpModeSettings(optionMenu)

    optionMenu.addSeparator()

    val modelMenu = new JMenu("Model")
    optionMenu.add(modelMenu)
    for(m <- Array(_610PAL,_610NTSC,_620PAL,_620NTSC,_710NTSC,_720NTSC)) {
      val item = new JMenuItem(m.name)
      modelMenu.add(item)
      item.addActionListener(_ => {
        clock.pause()
        setModel(m)
        hardReset(true)
      })
    }
  }

  override protected def paste(): Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      BKeyboard.insertTextIntoKeyboardBuffer(str, mmu)
    }
  }

  override protected def mainLoop(cycles: Long): Unit = {
    cpu.fetchAndExecute(1)
    ciaieee.clock(false)
    ciaip.clock(false)
  }

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

  override protected def showAbout(): Unit = {
    val about = new AboutCanvas(getCharROM, ucesoft.cbm.Version.VERSION.toUpperCase + " (" + ucesoft.cbm.Version.BUILD_DATE.toUpperCase + ")",16,16)
    JOptionPane.showMessageDialog(displayFrame, about, "About", JOptionPane.INFORMATION_MESSAGE, new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  override def reset(): Unit = {
    println("CBMII RESET!")
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))
    // TODO
  }

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

    // 50/60 Hz source
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))

    TestCart.setCartLocation(0xFDAFF)
    //TestCart.enabled = true

    // ============== DISPLAY ===============================================
    display = new Display(CRTC6845.SCREEN_WIDTH, CRTC6845.SCREEN_HEIGHT, displayFrame.getTitle, displayFrame)
    display.setRenderingHints(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    display.setPreferredSize(model.preferredFrameSize)
    displayFrame.setFocusTraversalKeysEnabled(false)

    // Keyboard
    displayFrame.addKeyListener(keyb)

    // components
    add(clock)
    add(CBM2MMU.KERNAL_ROM)
    add(CBM2MMU.BASIC_ROM)
    add(CBM2MMU.CHAR_ROM)
    add(irq)
    add(mmu)
    add(cpu)
    add(keyb)
    add(bus)
    add(sid)
    add(ciaieee)
    add(ciaip)
    add(tpiIeee)
    add(display)
    add(datassette)
    add(printer)

    // GUI
    displayFrame.getContentPane.add("Center",display)
    displayFrame.getContentPane.add("South",makeInfoPanel(true))
    // tracing
    if (!headless) {
      traceDialog = TraceDialog.getTraceDialog("CPU Debugger", displayFrame, mmu, cpu)
      Log.setOutput(traceDialog.logPanel.writer)
    }
    else Log.setOutput(null)

    displayFrame.setTransferHandler(DNDHandler)
    Log.info(sw.toString)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame, Array(display), Array("CRT"))
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
    //crt.setOwnThread(2000000)
    //crt.play()
    add(crt)
    crt.initComponent()
    mmu.setIO(crt,ciaieee,ciaip,tpiKb,tpiIeee,sid,acia)
  }

  protected def setModel(newModel:CBM2Model): Unit = {
    if (model != newModel) {
      model = newModel
      crt.setClipping(model.crtClip._1, model.crtClip._2, model.crtClip._3, model.crtClip._4)
      display.setPreferredSize(model.preferredFrameSize)
      displayFrame.invalidate()
      displayFrame.pack()
      CBM2MMU.CHAR_ROM.resourceName = model.charROMPropName
      CBM2MMU.BASIC_ROM.resourceName = model.basicROMPropName
      CBM2MMU.CHAR_ROM.reload()
      crt.charRom = CBM2MMU.CHAR_ROM.getROMBytes()
      CBM2MMU.BASIC_ROM.reload()
      mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
      mmu.setModel(model)
    }
  }

  override protected def saveState(out: ObjectOutputStream): Unit = ???

  override protected def loadState(in: ObjectInputStream): Unit = ???

  override protected def allowsStateRestoring: Boolean = ???
}
