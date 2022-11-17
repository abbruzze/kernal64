package ucesoft.cbm.cbm2

import ucesoft.cbm._
import ucesoft.cbm.cbm2.IEEE488Connectors.{CIAIEEE488ConnectorA, CIAIEEE488ConnectorB, IEEE488InterfaceA, IEEE488InterfaceB}
import ucesoft.cbm.cpu.{CPU6510_CE, Memory, ROM}
import ucesoft.cbm.formats.{Diskette, ProgramLoader, TAP}
import ucesoft.cbm.misc.Preferences._
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
import ucesoft.cbm.peripheral.printer.{IEEE488MPS803, Printer}
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.trace.Tracer.TracedDisplay
import ucesoft.cbm.trace.{InspectPanel, Tracer}

import java.awt.datatransfer.DataFlavor
import java.awt.event.MouseAdapter
import java.awt.{Dimension, Toolkit}
import java.io._
import javax.swing._
import javax.swing.filechooser.FileFilter

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
  override protected val printer: Printer = new IEEE488MPS803("MPS803",4,bus,printerGraphicsDriver)

  protected val crtOwnThreadMenu = new JCheckBoxMenuItem("CRT runs on separate thread")

  protected val _50_60_CYCLES = 2000000 / 50 // 50Hz
  protected var ciaieee, ciaip: CIA = _
  protected var tpiIeee: MOS6525 = _
  protected var tpiKb: MOS6525 = _
  protected var crt: CRTC6845 = _
  protected var acia: ACIA6551 = _

  protected var model : CBM2Model = _610PAL

  override protected def PRG_LOAD_ADDRESS() = 0x3
  override protected def PRG_RUN_DELAY_CYCLES = mmu.getModel().prgDelayCycles

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
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    swing {
      initComponent
    }
    // CRT
    swing {
      displayFrame.pack()
    }
    // screen's dimension and size restoration
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing {
        updateScreenDimension(new Dimension(dim(0), dim(1)))
      }
    }

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

  override protected def setGlobalCommandLineOptions: Unit = {
    import Preferences._
    // non-saveable settings
    preferences.add(PREF_WARP, "Run warp mode", false) { w =>
      val isAdjusting = preferences.get(PREF_WARP).get.isAdjusting
      warpMode(w, !isAdjusting)
    }
    preferences.add(PREF_HEADLESS, "Activate headless mode", false, Set(), false) {
      headless = _
    }
    preferences.add(PREF_TESTCART, "Activate testcart mode", false, Set(), false) {
      TestCart.enabled = _
    }
    preferences.add(PREF_LIMITCYCLES, "Run at most the number of cycles specified", "", Set(), false) { cycles =>
      if (cycles != "" && cycles.toLong > 0) clock.limitCyclesTo(cycles.toLong)
    }
    preferences.add(PREF_RUNFILE, "Run the given file taken from the attached disk", null: String, Set(), false) { file => }
    preferences.add(PREF_SCREENSHOT, "Take a screenshot of VIC screen and save it on the given file path. Used with --testcart only.", "") { file =>
      if (file != "") {
        TestCart.screenshotFile = Some(file)
        TestCart.screeshotHandler = display.waitFrameSaveSnapshot _
      }
    }
    preferences.add(PREF_CPUJAMCONTINUE, "On cpu jam continue execution", false, Set(), false) {
      cpujamContinue = _
    }
    preferences.add(PREF_LOADSTATE, "Load a previous saved state.", "", Set(), false) { file =>
      if (file != "") {
        try {
          loadStateFromOptions = true
          loadState(Some(file))
        }
        finally loadStateFromOptions = false
      }
    }
    preferences.add(PREF_FULLSCREEN, "Starts the emulator in full screen mode", false, Set(), false) {
      fullScreenAtBoot = _
    }
    preferences.add(PREF_IGNORE_CONFIG_FILE, "Ignore configuration file and starts emulator with default configuration", false, Set(), false) {
      ignoreConfig = _
    }
    preferences.add(PREF_TRACE, "Starts the emulator in trace mode", false, Set(), false) { trace =>
      traceOption = trace
      tracer.enableTracing(trace)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_KERNAL_ROM_PROP) mmu.setKernalROM(CBM2MMU.KERNAL_ROM.getROMBytes())
    }
    preferences.add(PREF_KERNEL, "Set kernel rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.CBM2_KERNAL_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_BASIC128_ROM_PROP && mmu.getModel().memoryK == 128) mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
    }
    preferences.add(PREF_CBM2_BASIC128, "Set basic 128 rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.CBM2_BASIC128_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_BASIC256_ROM_PROP && mmu.getModel().memoryK == 256) mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
    }
    preferences.add(PREF_CBM2_BASIC256, "Set basic 256 rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.CBM2_BASIC256_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_CHAR600_ROM_PROP && mmu.getModel().lowProfile) crt.charRom = CBM2MMU.CHAR_ROM.getROMBytes()
    }
    preferences.add(PREF_CBM2_CHAR600, "Set 600 char rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.CBM2_CHAR600_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_CHAR700_ROM_PROP && !mmu.getModel().lowProfile) crt.charRom = CBM2MMU.CHAR_ROM.getROMBytes()
    }
    preferences.add(PREF_CBM2_CHAR700, "Set 700 char rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.CBM2_CHAR700_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_ROMAT1000_PROP) {
        val rom = configuration.getProperty(ROM.CBM2_ROMAT1000_PROP)
        if (rom.isEmpty) mmu.setROM1000(null) else mmu.setROM1000(loadInternalROM(rom))
      }
    }
    preferences.add(PREF_CBM2_ROM1000, "Set rom at 1000 path", "", Set.empty, false) { file =>
      if (file != "") mmu.setROM1000(loadInternalROM(file))
      else mmu.setROM1000(null)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_ROMAT2000_PROP) {
        val rom = configuration.getProperty(ROM.CBM2_ROMAT2000_PROP)
        if (rom.isEmpty) mmu.setROM2000(null) else mmu.setROM2000(loadInternalROM(rom))
      }
    }
    preferences.add(PREF_CBM2_ROM2000, "Set rom at 2000 path", "", Set.empty, false) { file =>
      if (file != "") mmu.setROM2000(loadInternalROM(file)) else mmu.setROM2000(null)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_ROMAT4000_PROP) {
        val rom = configuration.getProperty(ROM.CBM2_ROMAT4000_PROP)
        if (rom.isEmpty) mmu.setROM4000(null) else mmu.setROM4000(loadInternalROM(rom))
      }
    }
    preferences.add(PREF_CBM2_ROM4000, "Set rom at 4000 path", "", Set.empty, false) { file =>
      if (file != "") mmu.setROM4000(loadInternalROM(file)) else mmu.setROM4000(null)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.CBM2_ROMAT6000_PROP) {
        val rom = configuration.getProperty(ROM.CBM2_ROMAT6000_PROP)
        if (rom.isEmpty) mmu.setROM6000(null) else mmu.setROM6000(loadInternalROM(rom))
      }
    }
    preferences.add(PREF_CBM2_ROM6000, "Set rom at 6000 path", "", Set.empty, false) { file =>
      if (file != "") mmu.setROM6000(loadInternalROM(file)) else mmu.setROM6000(null)
    }
  }

  protected def loadInternalROM(file:String): Array[Int] = {
    val f = new File(file)
    if (f.length != 0x2000) throw new IllegalArgumentException(s"Invalid ROM size: expected 8192 bytes, found ${file.length}")
    java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
  }

  protected def updateScreenDimension(dim:Dimension): Unit = {
    displayFrame.getContentPane.remove(display)
    display.setPreferredSize(dim)
    displayFrame.getContentPane.add("Center",display)
    displayFrame.pack()
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
      if (!tracer.isTracing) clock.pause
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
    val modelMenu = new JMenu("Model")
    optionMenu.add(modelMenu)
    val groupR = new ButtonGroup
    val items = for (m <- Array(_610PAL, _610NTSC, _620PAL, _620NTSC, _710NTSC, _720NTSC)) yield {
      val item = new JRadioButtonMenuItem(m.name)
      groupR.add(item)
      if (m == _610PAL) item.setSelected(true)
      modelMenu.add(item)
      item.addActionListener(_ => setModel(m,true) )
      item
    }
    preferences.add(PREF_CBM2_MODEL, "Set the model (610pal,610ntsc,620pal,620ntsc,710ntsc,720ntsc)", "610pal", Set("610pal","610ntsc","620pal","620ntsc","710ntsc","720ntsc")) { model =>
      model match {
        case "610pal" =>
          items(0).setSelected(true)
          setModel(_610PAL,false)
        case "610ntsc" =>
          items(1).setSelected(true)
          setModel(_610NTSC, false)
        case "620pal" =>
          items(2).setSelected(true)
          setModel(_620PAL, false)
        case "620ntsc" =>
          items(3).setSelected(true)
          setModel(_620NTSC, false)
        case "710ntsc" =>
          items(4).setSelected(true)
          setModel(_710NTSC, false)
        case "720ntsc" =>
          items(5).setSelected(true)
          setModel(_720NTSC, false)
      }
    }

    optionMenu.addSeparator()

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

    val crtMenu = new JMenu("CRT")
    optionMenu.add(crtMenu)
    crtMenu.add(crtOwnThreadMenu)
    crtOwnThreadMenu.setSelected(false)
    crtOwnThreadMenu.addActionListener(_ => setCRTOwnThread(crtOwnThreadMenu.isSelected))
    setRenderingSettings(crtMenu)
    val restoreCRTDim = new JMenuItem("Restore screen size")
    restoreCRTDim.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_0, java.awt.event.InputEvent.ALT_DOWN_MASK))
    crtMenu.add(restoreCRTDim)
    restoreCRTDim.addActionListener(_ => updateScreenDimension(model.preferredFrameSize))

    val displayEffectsItem = new JMenuItem("CRT's display effects ...")
    crtMenu.add(displayEffectsItem)
    displayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(displayFrame, display, "CRT").setVisible(true))

    optionMenu.addSeparator()

    val fullScreenItem = new JMenuItem("Full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setCRTFullScreen())
    optionMenu.add(fullScreenItem)

    setVolumeSettings(optionMenu)

    setWarpModeSettings(optionMenu)

    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot)
    optionMenu.add(snapshotItem)

    val gifRecorderItem = new JMenuItem("GIF recorder...")
    gifRecorderItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.ALT_DOWN_MASK))
    gifRecorderItem.addActionListener(_ => openGIFRecorder)
    optionMenu.add(gifRecorderItem)

    setPauseSettings(optionMenu)

    optionMenu.addSeparator()

    setPrinterSettings(optionMenu)

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    val aciaItem = new JMenuItem("ACIA Internet configuration ...")
    aciaItem.addActionListener(_ => showACIAConfigPanel() )
    IOItem.add(aciaItem)

    optionMenu.addSeparator()

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener(_ => {
      clock.pause
      ROMPanel.showROMPanel(displayFrame, configuration, cbmModel, false, () => {
        saveSettings(false)
        reset(false)
      })
      clock.play
    })
  }

  protected def showACIAConfigPanel(): Unit = {
    ACIA6551.showConfigDialog(displayFrame)
  }

  protected def setCRTOwnThread(enabled:Boolean): Unit = {
    if (enabled) crt.setOwnThread(2000000) else crt.stopOwnThread()
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
        /*
        if (id == 0) {
          diskTraceDialog.mem = drives(id).getMem
          diskTraceDialog.traceListener = drives(id)
        }
         */
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
    clock.cancel("50_60Hz")
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
  }

  override def init(): Unit = {
    Log.setInfo

    Log.info("Building the system ...")

    mmu.setCPU(cpu.asInstanceOf[CPU6510_CE])
    mmu.setModel(model)

    val irq = new Switcher("IRQ",low => cpu.irqRequest(low))

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
    ACIA6551.setACIA(acia)

    // 50/60 Hz source
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))

    TestCart.setCartLocation(0xFDAFF)

    if (headless) Log.setOutput(null)

    // ============== DISPLAY ===============================================
    display = new Display(CRTC6845.SCREEN_WIDTH, CRTC6845.SCREEN_HEIGHT, displayFrame.getTitle, displayFrame)
    display.setRenderingHints(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    display.setPreferredSize(model.preferredFrameSize)
    displayFrame.setFocusTraversalKeysEnabled(false)

    // drive
    initializedDrives(DriveType._8050)

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
    if (headless) Log.setOutput(null)

    displayFrame.setTransferHandler(DNDHandler)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame, Array(display), Array("CRT"))

    // trace
    tracer.addDevice(Tracer.TracedDevice("Main 6509 CPU", mmu, cpu, true))
    tracer.setDisplay(new TracedDisplay {
      override def getRasterLineAndCycle(): (Int, Int) = (crt.getRasterLine, crt.getRasterCycle)
      override def setDisplayRasterLine(line: Int): Unit = display.setRasterLineAt(line)
      override def enableDisplayRasterLine(enabled: Boolean): Unit = display.setDrawRasterLine(enabled)
    })
  }

  private def _50_60_Hz(cycles: Long): Unit = {
    tpiIeee.setInterruptPin(MOS6525.INT_I0, 1)
    tpiIeee.setInterruptPin(MOS6525.INT_I0, 0)
    clock.schedule(new ClockEvent("50_60Hz", cycles + _50_60_CYCLES, _50_60_Hz _))
  }

  override def afterInitHook(): Unit = {
    // frequency 2Mhz: here to notify all listeners
    sid.setCPUFrequency(2000000)
    clock.setClockHz(2000000)

    mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
    mmu.setKernalROM(CBM2MMU.KERNAL_ROM.getROMBytes())

    crt = new CRTC6845(mmu.getCRTCRam,CBM2MMU.CHAR_ROM.getROMBytes(),16)
    crt.setDisplay(display)
    crt.setClipping(model.crtClip._1,model.crtClip._2,model.crtClip._3,model.crtClip._4)
    add(crt)
    crt.initComponent()
    //crt.play()
    mmu.setIO(crt,ciaieee,ciaip,tpiKb,tpiIeee,sid,acia)
    inspectDialog = InspectPanel.getInspectDialog(displayFrame, this,cbmModel)
  }

  protected def setModel(newModel:CBM2Model,play:Boolean,updateMMU:Boolean = true): Unit = {
    if (model != newModel) {
      clock.pause()
      model = newModel
      crt.setClipping(model.crtClip._1, model.crtClip._2, model.crtClip._3, model.crtClip._4)
      updateScreenDimension(model.preferredFrameSize)
      CBM2MMU.CHAR_ROM.resourceName = model.charROMPropName
      CBM2MMU.BASIC_ROM.resourceName = model.basicROMPropName
      CBM2MMU.CHAR_ROM.reload()
      crt.charRom = CBM2MMU.CHAR_ROM.getROMBytes()
      CBM2MMU.BASIC_ROM.reload()
      mmu.setBasicROM(CBM2MMU.BASIC_ROM.getROMBytes())
      if (updateMMU) mmu.setModel(model)
      preferences.updateWithoutNotify(PREF_CBM2_MODEL,model.toString.toLowerCase.substring(1))
      if (play) hardReset(true)
    }
  }

  protected def takeSnapshot(): Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        display.saveSnapshot(file)
      case _ =>
    }
  }

  protected def setPrinterSettings(parent: JMenu): Unit = {
    // PRINTER-ENABLED =====================================================================================
    val printerPreviewItem = new JMenuItem("Printer preview ...")
    printerPreviewItem.addActionListener(_ => showPrinterPreview)
    parent.add(printerPreviewItem)

    val printerEnabledItem = new JCheckBoxMenuItem("Printer enabled")
    printerEnabledItem.setSelected(false)
    printerEnabledItem.addActionListener(_ => preferences(PREF_PRINTERENABLED) = printerEnabledItem.isSelected)
    parent.add(printerEnabledItem)
    preferences.add(PREF_PRINTERENABLED, "Enable printer", false) { pe =>
      enablePrinter(pe)
      printerEnabledItem.setSelected(pe)
    }
  }

  protected def setCRTFullScreen(): Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(displayFrame,
      display,
      CRTC6845.SCREEN_WIDTH,
      CRTC6845.SCREEN_HEIGHT * 2,
      new MouseAdapter {},
      keyb)
  }

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeObject(model)
  }

  override protected def loadState(in: ObjectInputStream): Unit = {
    val model = in.readObject().asInstanceOf[CBM2Model]
    preferences.update(PREF_CBM2_MODEL,model.option)
    setModel(model,false,false)
  }

  override def load(in:ObjectInputStream) : Unit = {
    super.load(in)
    clock.cancel("50_60Hz")
    clock.schedule(new ClockEvent("50_60Hz", clock.nextCycles, _50_60_Hz _))
  }

  override protected def allowsStateRestoring: Boolean = true
}
