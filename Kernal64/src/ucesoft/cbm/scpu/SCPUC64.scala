package ucesoft.cbm.scpu

import java.awt.datatransfer.DataFlavor
import java.awt.{Toolkit, _}
import java.io._

import javax.swing._
import javax.swing.filechooser.FileFilter
import ucesoft.cbm._
import ucesoft.cbm.cpu.wd65816.CPU65816
import ucesoft.cbm.expansion._
import ucesoft.cbm.expansion.cpm.CPMCartridge
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.bus.BusSnoop
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.vic.Palette
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.trace.{InspectPanel, TraceDialog}
import ucesoft.cbm.c64._

object SCPUC64 extends App {
  CBMComputer.turnOn(new SCPUC64,args)
}

class SCPUC64 extends CBMComputer {
  val componentID = "Commodore 64 SCPU"
  val componentType = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "SCPU Kernal64"
  protected val CONFIGURATION_FILENAME = "SCPUC64.config"
  //override protected val PRG_RUN_DELAY_CYCLES = 220000

  protected val keybMapper: keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)), "/resources/default_keyboard_c64")

  protected val mmu = new SCPUC64MMU.SCPU_MMU(cpuFastMode _, simmUsage _)
  override protected lazy val cpu = new CPU65816(mmu)
  protected val busSnooper = new BusSnoop(bus)
  protected var busSnooperActive = false

  private[this] val mmuStatusPanel = new MMUStatusPanel
  private[this] var cpuClocks = 1

  override protected val tapeAllowed = false

  def reset : Unit = {
    dma = false
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
    cia12Running(0) = true
    cia12Running(1) = true
    cpuClocks = 1
  }

  def init : Unit = {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo

    Log.info("Building the system ...")
    cpu.setInterruptVectorMapper(mmu.interruptVectorMapper _)
    cpu.setNativeModeListener(cpuNativeMode _)
    mmu.setClockStretchingRequestListener(cpu.requestClockStretching _)
    mmu.setCacheWriteWaitListener(cpu.setBaLow _)
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame, AVAILABLE_RS232)
    ExpansionPort.addConfigurationListener(mmu)
    // drive
    initializedDrives(DriveType._1541)
    // -----------------------
    ProgramLoader.cpu = cpu
    ProgramLoader.warpModeListener = warpMode _
    add(clock)
    add(mmu)
    add(cpu)
    add(keyb)
    add(controlPortA)
    add(controlPortB)
    add(bus)
    add(expansionPort)
    add(rs232)
    // -----------------------
    val vicMemory = new C64VICMemory(mmu, mmu.CHAR_ROM, cpu)
    add(vicMemory)
    ExpansionPort.setMemoryForEmptyExpansionPort(vicMemory)
    ExpansionPort.addConfigurationListener(vicMemory)
    import cia._
    // control ports
    val cia1CP1 = new CIA1Connectors.PortAConnector(keyb, controlPortA)
    val cia1CP2 = new CIA1Connectors.PortBConnector(keyb, controlPortB, () => vicChip.triggerLightPen)
    add(cia1CP1)
    add(cia1CP2)
    add(irqSwitcher)
    // CIAs
    cia1 = new CIA("CIA1",
      0xDC00,
      cia1CP1,
      cia1CP2,
      irqSwitcher.setLine(Switcher.CIA,_),
      idle => cia12Running(0) = !idle)
    val cia2CP1 = new CIA2Connectors.PortAConnector(vicMemory, bus, rs232)
    val cia2CP2 = new CIA2Connectors.PortBConnector(rs232)
    add(cia2CP1)
    add(cia2CP2)
    add(nmiSwitcher)
    cia2 = new CIA("CIA2",
      0xDD00,
      cia2CP1,
      cia2CP2,
      nmiSwitcher.setLine(Switcher.CIA,_),
      idle => cia12Running(1) = !idle)
    rs232.setCIA12(cia1, cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory, mmu.COLOR_RAM, irqSwitcher.setLine(Switcher.VIC,_), baLow _)
    mmu.setLastByteReadMemory(vicMemory)
    // mapping I/O chips in memory
    mmu.setIO(cia1, cia2, sid, vicChip)
    display = new vic.Display(vicChip.SCREEN_WIDTH, vicChip.SCREEN_HEIGHT, displayFrame.getTitle, displayFrame)
    add(display)
    display.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH, vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(display)
    displayFrame.getContentPane.add("Center", display)
    displayFrame.addKeyListener(keyb)
    displayFrame.addKeyListener(keypadControlPort)
    displayFrame.addKeyListener(keyboardControlPort)
    display.addMouseListener(keypadControlPort)
    display.addMouseListener(controlport.ControlPort.emptyControlPort)
    val lightPen = new LightPenButtonListener
    add(lightPen)
    display.addMouseListener(lightPen)
    configureJoystick
    // tracing
    if (!headless) {
      traceDialog = TraceDialog.getTraceDialog("CPU Debugger", displayFrame, mmu, cpu, display, vicChip)
      diskTraceDialog = TraceDialog.getTraceDialog("Drive 8 Debugger", displayFrame, drives(0).getMem, drives(0))
      Log.setOutput(traceDialog.logPanel.writer)
    }
    else Log.setOutput(null)
    // tape
    // printer
    add(printer)
    // Flyer
    add(flyerIEC)

    // info panel
    val infoPanel = makeInfoPanel(false)
    infoPanel.add("West", mmuStatusPanel)
    mmuStatusPanel.setJiffyDosAction(mmu.setJiffyDOS _)
    mmuStatusPanel.setSys1MhzAction(mmu.setSystem1Mhz _)
    displayFrame.getContentPane.add("South", infoPanel)
    displayFrame.setTransferHandler(DNDHandler)
    Log.info(sw.toString)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame,Array(display),Array("VIC"))
  }

  private def loadSettings(args:Array[String]) : Unit = {
    def loadFile(fn:String) : Unit = {
      val cmd = s"""LOAD"$fn",8,1""" + 13.toChar + "RUN" + 13.toChar
      clock.schedule(new ClockEvent("Loading",clock.currentCycles + PRG_RUN_DELAY_CYCLES,_ => Keyboard.insertTextIntoKeyboardBuffer(cmd,mmu,true) ))
    }
    settings.load(configuration)
    // AUTOPLAY
    settings.parseAndLoad(args) match {
      case None =>
        // run the given file name
        settings.get[String]("RUNFILE") match {
          case None =>
          case Some(fn) =>
            loadFile(fn)
        }
      case Some(f) =>
        settings.get[String]("DRIVE_8_FILE") match {
          case None =>
            handleDND(new File(f),false,true)
          case Some(_) =>
            // here we have both drive8 and PRG set: we load the given PRG file from disk 8
            val fn = new File(f).getName
            val dot = fn.indexOf('.')
            val cbmFile = if (dot > 0) fn.substring(0,dot) else f
            loadFile(cbmFile)
        }
    }
    DrivesConfigPanel.registerDrives(displayFrame,drives,setDriveType(_,_,false),enableDrive(_,_,true),attachDisk(_,_,true),attachDiskFile(_,_,_,None),drivesEnabled)
  }

  override def afterInitHook : Unit = {
    inspectDialog = InspectPanel.getInspectDialog(displayFrame, this)
    // deactivate drives > 8
    for(d <- 1 until TOTAL_DRIVES) {
      drives(d).setActive(false)
      driveLeds(d).setVisible(false)
    }
  }

  protected def mainLoop(cycles: Long) : Unit = {
    // VIC PHI1
    vicChip.clock
    // CIAs
    if (cia12Running(0)) cia1.clock(false)
    if (cia12Running(1)) cia2.clock(false)
    //DRIVES
    var d = 0
    while (d < TOTAL_DRIVES) {
      if (drivesEnabled(d) && drivesRunning(d)) drives(d).clock(cycles)
      d += 1
    }
    if (device12DriveEnabled) device12Drive.clock(cycles)
    // bus snoop
    if (busSnooperActive) busSnooper.clock(cycles)
    // printer
    if (printerEnabled) printer.clock(cycles)
    // Flyer
    if (isFlyerEnabled) flyerIEC.clock(cycles)
    // check cart freezing button
    if (cartButtonRequested && cpu.isFetchingInstruction) {
      cartButtonRequested = false
      ExpansionPort.getExpansionPort.freezeButton
    }
    // CPU PHI2
    ProgramLoader.checkLoadingInWarpMode(true)
    cpu.fetchAndExecute(cpuClocks)
    // SID
    if (sidCycleExact) sid.clock
  }

  protected def setDMA(dma: Boolean): Unit = {
    this.dma = dma
    cpu.setDMA(dma)
  }

  private def baLow(low: Boolean) : Unit = {
    //cpu.setBaLow(low)
    mmu.setBALow(low)
    expansionPort.setBaLow(low)
  }

  private def cpuNativeMode(native: Boolean): Unit = {
    mmu.cpuOnNativeMode(native)
    mmuStatusPanel.setNativeMode(native)
  }

  private def cpuFastMode(fastMode: Boolean): Unit = {
    mmuStatusPanel.setCPU20Mhz(fastMode)
    cpuClocks = if (fastMode) 20 else 1
  }

  private def simmUsage(usage:Float) = mmuStatusPanel.setSIMMUsage(usage)

  override def isHeadless = headless

  // ======================================== Settings ==============================================
  override protected def enableDrive(id:Int,enabled:Boolean,updateFrame:Boolean) : Unit = {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    if (updateFrame) adjustRatio
  }

  protected def setDisplayRendering(hints: java.lang.Object) : Unit = {
    display.setRenderingHints(hints)
  }


  private def adjustRatio : Unit = {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim)
    displayFrame.pack
  }

  protected def loadPRGFile(file: File, autorun: Boolean) : Unit = {
    val (start, end) = ProgramLoader.loadPRG(mmu, file, true, 8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar, mmu, true)
    }
  }

  protected def attachDiskFile(driveID: Int, file: File, autorun: Boolean, fileToLoad: Option[String], emulateInserting: Boolean = true) : Unit = {
    try {
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
      if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      val isD64 = file.getName.toUpperCase.endsWith(".D64")
      val disk = Diskette(file.toString)
      disk.canWriteOnDisk = canWriteOnDisk
      disk.flushListener = diskFlusher
      drives(driveID).getFloppy.close
      if (!traceDialog.isTracing) clock.pause
      drives(driveID).setDriveReader(disk, emulateInserting)
      clock.play

      loadFileItems(driveID).setEnabled(isD64)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      val drive = driveID + 8
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""LOAD"$fn",$drive,1""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          Keyboard.insertTextIntoKeyboardBuffer(cmd, mmu, true)
        case None if autorun =>
          Keyboard.insertSmallTextIntoKeyboardBuffer(s"""LOAD"*",$drive,1""" + 13.toChar + "RUN" + 13.toChar, mmu, true)
        case _ =>
      }
      driveLeds(driveID).setToolTipText(disk.toString)
    }
    catch {
      case t: Throwable =>
        t.printStackTrace

        showError("Disk attaching error", t.toString)
    }
  }

  protected def savePrg: Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")

      def getDescription = "PRG files"
    })
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        configuration.setProperty(CONFIGURATION_LASTDISKDIR, fc.getSelectedFile.getParentFile.toString)
        val (start, end) = ProgramLoader.savePRG(fc.getSelectedFile, mmu, true)
        Log.info(s"BASIC program saved from $start to $end")
      case _ =>
    }
  }

  private def takeSnapshot : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        display.saveSnapshot(file)
      case _ =>
    }
  }

  protected def paste: Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      Keyboard.insertTextIntoKeyboardBuffer(str, mmu, true)
    }
  }

  protected def setSettingsMenu(optionMenu: JMenu) : Unit = {
    val driveMenu = new JMenuItem("Drives ...")
    driveMenu.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.ALT_DOWN_MASK))
    optionMenu.add(driveMenu)
    driveMenu.addActionListener(_ => DrivesConfigPanel.getDriveConfigDialog.setVisible(true))
    optionMenu.addSeparator

    val keybMenu = new JMenu("Keyboard")
    optionMenu.add(keybMenu)

    val keybEditorItem = new JMenuItem("Keyboard editor ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor(true))
    keybMenu.add(keybEditorItem)
    val loadKeybItem = new JMenuItem("Set keyboard layout ...")
    loadKeybItem.addActionListener(_ => loadKeyboard)
    keybMenu.add(loadKeybItem)

    optionMenu.addSeparator

    setVolumeSettings(optionMenu)

    optionMenu.addSeparator

    setWarpModeSettings(optionMenu)

    optionMenu.addSeparator

    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio)
    optionMenu.add(adjustRatioItem)

    val zoomItem = new JMenu("Zoom")
    val groupZ = new ButtonGroup
    optionMenu.add(zoomItem)
    for (z <- 1 to 2) {
      val zoom1Item = new JRadioButtonMenuItem(s"Zoom x $z")
      zoom1Item.addActionListener(_ => vicZoom(z))
      val kea = z match {
        case 1 => java.awt.event.KeyEvent.VK_1
        case 2 => java.awt.event.KeyEvent.VK_2
      }
      zoom1Item.setAccelerator(KeyStroke.getKeyStroke(kea, java.awt.event.InputEvent.ALT_DOWN_MASK))
      zoomItem.add(zoom1Item)
      groupZ.add(zoom1Item)
    }

    val vicItem = new JMenu("VIC")
    optionMenu.add(vicItem)
    setRenderingSettings(vicItem)
    setVICModel(vicItem)
    setVICBorderMode(vicItem)

    setFullScreenSettings(optionMenu)
    // -----------------------------------

    optionMenu.addSeparator

    setJoysticsSettings(optionMenu)

    setLightPenSettings(optionMenu)

    setMouseSettings(optionMenu)

    optionMenu.addSeparator

    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot)
    optionMenu.add(snapshotItem)

    val gifRecorderItem = new JMenuItem("GIF recorder...")
    gifRecorderItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK))
    gifRecorderItem.addActionListener(_ => openGIFRecorder )
    optionMenu.add(gifRecorderItem)

    optionMenu.addSeparator

    setPauseSettings(optionMenu)

    optionMenu.addSeparator

    setPrinterSettings(optionMenu)
    // -----------------------------------
    optionMenu.addSeparator

    setSIDSettings(optionMenu)
    // -----------------------------------
    optionMenu.addSeparator

    setDrivesSettings

    val busSnooperActiveItem = new JCheckBoxMenuItem("Bus snoop active")
    busSnooperActiveItem.setSelected(false)
    busSnooperActiveItem.addActionListener(e => busSnooperActive = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected)
    optionMenu.add(busSnooperActiveItem)

    optionMenu.addSeparator

    setRemotingSettings(optionMenu)

    optionMenu.addSeparator

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    optionMenu.addSeparator

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232)
    IOItem.add(rs232Item)

    IOItem.addSeparator

    setFlyerSettings(IOItem)

    setREUSettings(IOItem)

    setGEORamSettings(IOItem)

    // -----------------------------------

    IOItem.addSeparator

    setDigiMAXSettings(IOItem)

    IOItem.addSeparator

    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2)
    IOItem.add(gmod2Item)

    IOItem.addSeparator

    setCPMSettings(IOItem)

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener(_ => ROMPanel.showROMPanel(displayFrame, configuration, true,true, () => saveSettings(false)))

    val scpuRamItem = new JMenu("SCPU RAM")
    optionMenu.add(scpuRamItem)
    val groupscpu = new ButtonGroup
    val scpuNORAMItem = new JRadioButtonMenuItem("0M")
    scpuNORAMItem.addActionListener(_ => mmu.setSIMMSize(0))
    groupscpu.add(scpuNORAMItem)
    scpuRamItem.add(scpuNORAMItem)
    val scpu1MRAMItem = new JRadioButtonMenuItem("1M")
    scpu1MRAMItem.addActionListener(_ => mmu.setSIMMSize(1))
    groupscpu.add(scpu1MRAMItem)
    scpuRamItem.add(scpu1MRAMItem)
    val scpu4MRAMItem = new JRadioButtonMenuItem("4M")
    scpu4MRAMItem.addActionListener(_ => mmu.setSIMMSize(4))
    groupscpu.add(scpu4MRAMItem)
    scpuRamItem.add(scpu4MRAMItem)
    val scpu8MRAMItem = new JRadioButtonMenuItem("8M")
    scpu8MRAMItem.addActionListener(_ => mmu.setSIMMSize(8))
    groupscpu.add(scpu8MRAMItem)
    scpuRamItem.add(scpu8MRAMItem)
    val scpu16MRAMItem = new JRadioButtonMenuItem("16M")
    scpu16MRAMItem.addActionListener(_ => mmu.setSIMMSize(16))
    groupscpu.add(scpu16MRAMItem)
    scpuRamItem.add(scpu16MRAMItem)
    settings.add("scpu-ram",
      s"super ram size: none,1,4,8,16",
      "SCPU_MEM_SIZE",
      (size: String) => {
        if (size == "" || size == null) { scpu16MRAMItem.setSelected(true) ; mmu.setSIMMSize(16) }
        else if (size == "none") { scpuNORAMItem.setSelected(true) ; mmu.setSIMMSize(0) }
        else if (size == "1") { scpu1MRAMItem.setSelected(true) ; mmu.setSIMMSize(1) }
        else if (size == "4") { scpu4MRAMItem.setSelected(true) ; mmu.setSIMMSize(4) }
        else if (size == "8") { scpu8MRAMItem.setSelected(true) ; mmu.setSIMMSize(8) }
        else if (size == "16") {scpu16MRAMItem.setSelected(true)  ; mmu.setSIMMSize(16) }
        else throw new IllegalArgumentException(s"Bad SIMM size: $size")
      },
      if (scpuNORAMItem.isSelected) "none"
      else if (scpu1MRAMItem.isSelected) "1"
      else if (scpu4MRAMItem.isSelected) "4"
      else if (scpu8MRAMItem.isSelected) "8"
      else "16"
    )

    settings.add("scpu-jiffydos-enabled",
      s"Enables JiffyDOS at startup",
      "SCPU_JIFFYDOS_ENABLED",
      (jiffyEnabled: Boolean) => {
        mmu.setJiffyDOS(jiffyEnabled)
        mmuStatusPanel.enableJiffyDOS(jiffyEnabled)
      },
      mmu.isJiffyDOSEnabled
    )
  }

  def turnOff : Unit = {
    if (!headless) saveSettings(configuration.getProperty(CONFIGURATION_AUTOSAVE, "false").toBoolean)
    for (d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }

  protected def saveSettings(save: Boolean): Unit = {
    if (!zoomOverride) {
      val dimension = display.getSize()
      configuration.setProperty(CONFIGURATION_FRAME_DIM, dimension.width + "," + dimension.height)
    }
    configuration.setProperty(CONFIGURATION_FRAME_XY, displayFrame.getX + "," + displayFrame.getY)
    if (save) {
      settings.save(configuration)
      println("Settings saved")
    }
    try {
      val propsFile = new File(new File(scala.util.Properties.userHome), CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "C64 configuration file")
      out.close
    }
    catch {
      case _: IOException =>
    }
  }

  protected def getRAM = mmu.getRAM

  protected def getCharROM = mmu.CHAR_ROM

  // state
  protected def saveState(out: ObjectOutputStream) : Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeInt(cpuClocks)
  }

  protected def loadState(in: ObjectInputStream) : Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    cpuClocks = in.readInt
  }

  protected def allowsStateRestoring: Boolean = true

  // -----------------------------------------------------------------------------------------

  def turnOn(args: Array[String]) : Unit = {
    swing {
      setMenu
    }
    // check help
    if (settings.checkForHelp(args)) {
      println(s"Kernal64, SuperCPU emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      settings.printUsage("file to attach")
      sys.exit(0)
    }
    // --headless handling to disable logging & debugging
    if (args.exists(_ == "--headless")) headless = true
    swing { initComponent }
    // VIC
    swing { displayFrame.pack }
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    // screen's dimension and size restoration
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map {
        _.toInt
      }
      swing { updateVICScreenDimension(new Dimension(dim(0), dim(1))) }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map {
        _.toInt
      }
      swing { displayFrame.setLocation(xy(0), xy(1)) }
    }
    // SETTINGS
    loadSettings(args)
    // VIEW
    swing {
      displayFrame.setVisible(!headless)
      if (fullScreenAtBoot) setVicFullScreen
    }
    // PLAY
    clock.play
  }
}