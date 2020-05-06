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
  val componentID = "Commodore 64"
  val componentType = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "SCPU Kernal64"
  protected val CONFIGURATION_FILENAME = "SCPUC64.config"

  protected val keybMapper: keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)), "/resources/default_keyboard_c64", C64KeyboardMapper)

  protected val mmu = new SCPUC64MMU.SCPU_MMU(cpuFastMode _)
  override protected lazy val cpu = new CPU65816(mmu)
  protected val busSnooper = new BusSnoop(bus)
  protected var busSnooperActive = false
  protected val c1541 = new C1541Emu(bus, DriveLed8Listener)

  private[this] val mmuStatusPanel = new MMUStatusPanel
  private[this] var cpuClocks = 1

  override protected val tapeAllowed = false

  def reset {
    dma = false
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
    cia12Running(0) = true
    cia12Running(1) = true
    cpuClocks = 1
  }

  def init {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo

    Log.info("Building the system ...")
    cpu.setInterruptVectorMapper(mmu.interruptVectorMapper _)
    cpu.setNativeModeListener(cpuNativeMode _)
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame, AVAILABLE_RS232)
    ExpansionPort.addConfigurationListener(mmu)
    // drive
    initDrive(0, DriveType._1541)
    initDrive(1, DriveType._1541)
    drivesEnabled(1) = false
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
    floppyComponents(0) = new FloppyComponent(8, drives(0), driveLeds(0))
    add(floppyComponents(0))
    floppyComponents(1) = new FloppyComponent(9, drives(1), driveLeds(1))
    add(floppyComponents(1))
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
      irqSwitcher.ciaIRQ _,
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
      nmiSwitcher.cia2NMIAction _,
      idle => cia12Running(1) = !idle)
    rs232.setCIA12(cia1, cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory, mmu.COLOR_RAM, irqSwitcher.vicIRQ _, baLow _)
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
    traceDialog = TraceDialog.getTraceDialog(displayFrame, mmu, cpu, display, vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog(displayFrame, drives(0).getMem, drives(0))
    // drive leds
    add(driveLeds(0))
    add(driveLeds(1))
    configureJoystick
    add(c1541)
    Log.setOutput(traceDialog.logPanel.writer)
    // tape
    // printer
    add(printer)
    // Flyer
    add(flyerIEC)

    // info panel
    val infoPanel = new JPanel(new BorderLayout)
    val rowPanel = new JPanel(new BorderLayout(0, 0))
    val row1Panel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    val row2Panel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    rowPanel.add("North", row1Panel)
    rowPanel.add("South", row2Panel)
    row1Panel.add(diskProgressPanels(0))
    row1Panel.add(driveLeds(0))
    row2Panel.add(diskProgressPanels(1))
    row2Panel.add(driveLeds(1))
    infoPanel.add("East", rowPanel)
    infoPanel.add("West", mmuStatusPanel)
    mmuStatusPanel.setJiffyDosAction(mmu.setJiffyDOS _)
    mmuStatusPanel.setSys1MhzAction(mmu.setSystem1Mhz _)
    displayFrame.getContentPane.add("South", infoPanel)
    displayFrame.setTransferHandler(DNDHandler)
    Log.info(sw.toString)
  }

  private def loadSettings(args: Array[String]): Unit = {
    settings.load(configuration)
    // AUTOPLAY
    settings.parseAndLoad(args) match {
      case None =>
        // run the given file name
        settings.get[String]("RUNFILE") match {
          case None =>
          case Some(fn) =>
            val cmd = s"""LOAD"$fn",8,1""" + 13.toChar + "RUN" + 13.toChar
            clock.schedule(new ClockEvent("Loading", clock.currentCycles + 2200000, (cycles) => Keyboard.insertTextIntoKeyboardBuffer(cmd, mmu, true)))
        }
      case Some(f) =>
        handleDND(new File(f), false, true)
    }
    DrivesConfigPanel.registerDrives(displayFrame, drives, setDriveType(_, _, false), enableDrive _, attachDisk(_, _, true), attachDiskFile(_, _, _, None), drivesEnabled)
  }

  override def afterInitHook {
    inspectDialog = InspectPanel.getInspectDialog(displayFrame, this)
    // deactivate drive 9
    drives(1).setActive(false)
    driveLeds(1).setVisible(false)
    //cpu.setTrace(true)
    //traceDialog.forceTracing(true)
  }

  protected def mainLoop(cycles: Long) {
    // VIC PHI1
    vicChip.clock
    // CIAs
    if (cia12Running(0)) cia1.clock(false)
    if (cia12Running(1)) cia2.clock(false)
    //DRIVES
    var d = 0
    while (d < 2) {
      if (drivesEnabled(d) && drivesRunning(d)) drives(d).clock(cycles)

      d += 1
    }
    if (device10DriveEnabled) device10Drive.clock(cycles)
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

  private def baLow(low: Boolean) {
    //cpu.setBaLow(low)
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

  override def isHeadless = headless

  // ======================================== Settings ==============================================
  protected def enableDrive(id: Int, enabled: Boolean): Unit = {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    adjustRatio
  }

  private def setDisplayRendering(hints: java.lang.Object) {
    display.setRenderingHints(hints)
  }


  private def adjustRatio {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim)
    displayFrame.pack
  }

  protected def loadPRGFile(file: File, autorun: Boolean) {
    val (start, end) = ProgramLoader.loadPRG(mmu, file, true, 8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar, mmu, true)
    }
  }

  protected def attachDiskFile(driveID: Int, file: File, autorun: Boolean, fileToLoad: Option[String], emulateInserting: Boolean = true) {
    try {
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
      if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      val isD64 = file.getName.toUpperCase.endsWith(".D64")
      if (drives(driveID) == c1541 && !isD64) {

        showError("Disk attaching error", "Format not allowed on a 1541 not in true emulation mode")
        return
      }
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

  private def zoom(f: Int) {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f, vicChip.VISIBLE_SCREEN_HEIGHT * f)
    updateScreenDimension(dim)
  }

  private def updateScreenDimension(dim: Dimension): Unit = {
    display.setPreferredSize(dim)
    display.invalidate
    display.repaint()
    displayFrame.pack
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

  private def takeSnapshot {
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

  protected def setSettingsMenu(optionMenu: JMenu) {
    import ucesoft.cbm.misc.Settings._

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

    val volumeItem = new JMenuItem("Volume settings ...")
    volumeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, java.awt.event.InputEvent.ALT_DOWN_MASK))
    volumeItem.addActionListener(_ => volumeDialog.setVisible(true))
    optionMenu.add(volumeItem)

    optionMenu.addSeparator

    maxSpeedItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.ALT_DOWN_MASK))
    maxSpeedItem.setSelected(clock.maximumSpeed)
    maxSpeedItem.addActionListener(e => warpMode(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected))
    optionMenu.add(maxSpeedItem)

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
      zoom1Item.addActionListener(_ => zoom(z))
      val kea = z match {
        case 1 => java.awt.event.KeyEvent.VK_1
        case 2 => java.awt.event.KeyEvent.VK_2
      }
      zoom1Item.setAccelerator(KeyStroke.getKeyStroke(kea, java.awt.event.InputEvent.ALT_DOWN_MASK))
      zoomItem.add(zoom1Item)
      groupZ.add(zoom1Item)
    }

    val vicItem = new JMenu("VIC")
    val renderingItem = new JMenu("Rendering")
    vicItem.add(renderingItem)
    val groupR = new ButtonGroup
    optionMenu.add(vicItem)
    val renderingDefault1Item = new JRadioButtonMenuItem("Default")
    renderingDefault1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
    renderingItem.add(renderingDefault1Item)
    groupR.add(renderingDefault1Item)
    val renderingBilinear1Item = new JRadioButtonMenuItem("Bilinear")
    renderingBilinear1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR))
    renderingItem.add(renderingBilinear1Item)
    groupR.add(renderingBilinear1Item)
    val renderingBicubic1Item = new JRadioButtonMenuItem("Bicubic")
    renderingBicubic1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BICUBIC))
    renderingItem.add(renderingBicubic1Item)
    groupR.add(renderingBicubic1Item)

    val paletteItem = new JMenu("Palette")
    vicItem.add(paletteItem)
    val groupP = new ButtonGroup
    val vicePalItem = new JRadioButtonMenuItem("VICE")
    vicePalItem.addActionListener(_ => Palette.setPalette(PaletteType.VICE))
    paletteItem.add(vicePalItem)
    groupP.add(vicePalItem)
    val brightPalItem = new JRadioButtonMenuItem("Bright")
    brightPalItem.addActionListener(_ => Palette.setPalette(PaletteType.BRIGHT))
    paletteItem.add(brightPalItem)
    groupP.add(brightPalItem)
    val peptoPalItem = new JRadioButtonMenuItem("Pepto")
    peptoPalItem.addActionListener(_ => Palette.setPalette(PaletteType.PEPTO))
    paletteItem.add(peptoPalItem)
    groupP.add(peptoPalItem)
    // Setting ---------------------------
    settings.add("vic-palette",
      "Set the palette type (bright,vice,pepto)",
      "PALETTE",
      (dt: String) => {
        dt match {
          case "bright" | "" =>
            Palette.setPalette(PaletteType.BRIGHT)
            brightPalItem.setSelected(true)
          case "vice" =>
            Palette.setPalette(PaletteType.VICE)
            vicePalItem.setSelected(true)
          case "pepto" =>
            Palette.setPalette(PaletteType.PEPTO)
            peptoPalItem.setSelected(true)
          case _ =>
        }
      },
      if (brightPalItem.isSelected) "bright"
      else if (vicePalItem.isSelected) "vice"
      else if (peptoPalItem.isSelected) "pepto"
      else "bright"
    )
    settings.add("rendering-type",
      "Set the rendering type (default,bilinear,bicubic)",
      "RENDERING_TYPE",
      (dt: String) => {
        dt match {
          case "bilinear" | "" =>
            setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR)
            renderingBilinear1Item.setSelected(true)
          case "bicubic" =>
            setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BICUBIC)
            renderingBicubic1Item.setSelected(true)
          case "default" =>
            setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
            renderingDefault1Item.setSelected(true)
          case _ =>
        }
      },
      if (renderingDefault1Item.isSelected) "default"
      else if (renderingBilinear1Item.isSelected) "bilinear"
      else if (renderingBicubic1Item.isSelected) "bicubic"
      else "default"
    )

    val fullScreenItem = new JMenuItem("Full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setVicFullScreen)
    optionMenu.add(fullScreenItem)
    // -----------------------------------

    optionMenu.addSeparator

    val joyAItem = new JMenuItem("Joystick...")
    joyAItem.addActionListener(_ => joySettings)
    optionMenu.add(joyAItem)

    val swapJoyAItem = new JMenuItem("Swap joysticks")
    swapJoyAItem.addActionListener(_ => swapJoysticks)
    optionMenu.add(swapJoyAItem)

    val lightPenMenu = new JMenu("Light pen")
    optionMenu.add(lightPenMenu)
    val group3 = new ButtonGroup
    val noPenItem = new JRadioButtonMenuItem("No light pen")
    noPenItem.setSelected(true)
    noPenItem.addActionListener(_ => setLightPen(LIGHT_PEN_NO_BUTTON))
    group3.add(noPenItem)
    lightPenMenu.add(noPenItem)
    val penUp = new JRadioButtonMenuItem("Light pen with button up on control port 2")
    penUp.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_UP))
    group3.add(penUp)
    lightPenMenu.add(penUp)
    val penLeft = new JRadioButtonMenuItem("Light pen with button left on control port 2")
    penLeft.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_LEFT))
    group3.add(penLeft)
    lightPenMenu.add(penLeft)

    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled on port 1")
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.setSelected(false)
    mouseEnabledItem.addActionListener(e => enableMouse(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected, display))
    optionMenu.add(mouseEnabledItem)

    optionMenu.addSeparator

    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot)
    optionMenu.add(snapshotItem)

    optionMenu.addSeparator

    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        display.setPaused
      } else clock.play)
    optionMenu.add(pauseItem)

    optionMenu.addSeparator

    val printerPreviewItem = new JMenuItem("Printer preview ...")
    printerPreviewItem.addActionListener(_ => showPrinterPreview)
    optionMenu.add(printerPreviewItem)

    val printerEnabledItem = new JCheckBoxMenuItem("Printer enabled")
    printerEnabledItem.setSelected(false)
    printerEnabledItem.addActionListener(e => {
      printerEnabled = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected; printer.setActive(printerEnabled)
    })
    optionMenu.add(printerEnabledItem)
    // Setting ---------------------------
    settings.add("printer-enabled",
      "Enable printer",
      "PRINTER_ENABLED",
      (pe: Boolean) => {
        enablePrinter(pe)
        printerEnabledItem.setSelected(pe)
      },
      printerEnabledItem.isSelected
    )
    // -----------------------------------


    optionMenu.addSeparator

    val sidItem = new JMenu("SID")
    optionMenu.add(sidItem)
    val group7 = new ButtonGroup
    val sidTypeItem = new JMenu("SID Type")
    sidItem.add(sidTypeItem)
    val sid6581Item = new JRadioButtonMenuItem("MOS 6581")
    sid6581Item.setSelected(true)
    sid6581Item.addActionListener(_ => sid.setModel(true))
    sidTypeItem.add(sid6581Item)
    group7.add(sid6581Item)
    val sid8580Item = new JRadioButtonMenuItem("MOS 8580")
    sid8580Item.setSelected(false)
    sid8580Item.addActionListener(_ => sid.setModel(false))
    sidTypeItem.add(sid8580Item)
    group7.add(sid8580Item)
    // Setting ---------------------------
    settings.add("sid-8580",
      "Enable sid 8580 type",
      "SID_8580",
      (sid8580: Boolean) => {
        sid.setModel(!sid8580)
        sid8580Item.setSelected(sid8580)
      },
      sid8580Item.isSelected
    )
    // -----------------------------------
    val sid2Item = new JMenu("Dual SID")
    sidItem.add(sid2Item)
    val group8 = new ButtonGroup
    val nosid2Item = new JRadioButtonMenuItem("None")
    sid2Item.add(nosid2Item)
    nosid2Item.setSelected(true)
    nosid2Item.addActionListener(_ => setDualSID(None))
    group8.add(nosid2Item)
    for (adr <- DualSID.validAddresses(true)) {
      val sid2AdrItem = new JRadioButtonMenuItem(adr)
      sid2Item.add(sid2AdrItem)
      sid2AdrItem.setSelected(false)
      sid2AdrItem.addActionListener(_ => setDualSID(Some(Integer.parseInt(adr, 16))))
      group8.add(sid2AdrItem)
    }
    val sidCycleExactItem = new JCheckBoxMenuItem("SID cycle exact emulation")
    sidCycleExactItem.setSelected(false)
    sidCycleExactItem.addActionListener(_ => {
      sidCycleExact = sidCycleExactItem.isSelected
      sid.setCycleExact(sidCycleExactItem.isSelected)
    }
    )
    sidItem.add(sidCycleExactItem)
    // Setting ---------------------------
    settings.add("sid-cycle-exact",
      "With this option enabled the SID emulation is more accurate with a decrease of performance",
      "SID_CYCLE_EXACT",
      (sce: Boolean) => {
        clock.pause
        sidCycleExact = sce
        sid.setCycleExact(sidCycleExact)
        sidCycleExactItem.setSelected(sidCycleExact)
        clock.play
      },
      sidCycleExact
    )
    // -----------------------------------
    optionMenu.addSeparator

    for (drive <- 0 to 1) {
      // Setting ---------------------------
      settings.add(s"drive${drive + 8}-type",
        "Set the driver's type (1541,1571,1581)",
        s"DRIVER${drive + 8}_TYPE",
        (dt: String) => {
          dt match {
            case "1541" =>
              setDriveType(drive, DriveType._1541, true)
            case "1571" =>
              setDriveType(drive, DriveType._1571, true)
            case "1581" =>
              setDriveType(drive, DriveType._1581, true)
            case _ =>
          }
        },
        if (drives(drive).driveType == DriveType._1541) "1541"
        else if (drives(drive).driveType == DriveType._1571) "1571"
        else if (drives(drive).driveType == DriveType._1581) "1581"
        else "1571"
      )

      settings.add(s"drive${drive + 8}-file",
        s"Attach a file to drive ${drive + 8}",
        s"DRIVE_${drive + 8}_FILE",
        (df: String) => {
          if (df != "") attachDiskFile(drive, new File(df), false, None)
        },
        floppyComponents(drive).drive.getFloppy.file
      )
    }

    val busSnooperActiveItem = new JCheckBoxMenuItem("Bus snoop active")
    busSnooperActiveItem.setSelected(false)
    busSnooperActiveItem.addActionListener(e => busSnooperActive = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected)
    optionMenu.add(busSnooperActiveItem)

    optionMenu.addSeparator

    val remoteItem = new JMenu("Remoting")
    optionMenu.add(remoteItem)

    val group10 = new ButtonGroup
    val remoteDisabledItem = new JRadioButtonMenuItem("Off")
    remoteDisabledItem.setSelected(true)
    remoteDisabledItem.addActionListener(_ => setRemote(None))
    group10.add(remoteDisabledItem)
    remoteItem.add(remoteDisabledItem)
    val remoteEnabledItem = new JRadioButtonMenuItem("On ...")
    remoteEnabledItem.addActionListener(e => setRemote(Some(e.getSource.asInstanceOf[JRadioButtonMenuItem])))
    group10.add(remoteEnabledItem)
    remoteItem.add(remoteEnabledItem)

    optionMenu.addSeparator

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    optionMenu.addSeparator

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232)
    IOItem.add(rs232Item)

    IOItem.addSeparator

    val flyerItem = new JMenu("Flyer internet modem")
    IOItem.add(flyerItem)

    val fylerEnabledItem = new JCheckBoxMenuItem("Flyer enabled on 7")
    fylerEnabledItem.setSelected(false)
    flyerItem.add(fylerEnabledItem)
    fylerEnabledItem.addActionListener(e => enableFlyer(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected))
    val flyerDirectoryItem = new JMenuItem("Set disks repository ...")
    flyerItem.add(flyerDirectoryItem)
    flyerDirectoryItem.addActionListener(_ => chooseFlyerDir)

    val reuItem = new JMenu("REU")
    val group5 = new ButtonGroup
    val noReuItem = new JRadioButtonMenuItem("None")
    noReuItem.setSelected(true)
    noReuItem.addActionListener(_ => setREU(None, None))
    group5.add(noReuItem)
    reuItem.add(noReuItem)
    val reu128Item = new JRadioButtonMenuItem("128K")
    reu128Item.addActionListener(_ => setREU(Some(REU.REU_1700), None))
    group5.add(reu128Item)
    reuItem.add(reu128Item)
    val reu256Item = new JRadioButtonMenuItem("256K")
    reu256Item.addActionListener(_ => setREU(Some(REU.REU_1764), None))
    group5.add(reu256Item)
    reuItem.add(reu256Item)
    val reu512Item = new JRadioButtonMenuItem("512K")
    reu512Item.addActionListener(_ => setREU(Some(REU.REU_1750), None))
    group5.add(reu512Item)
    reuItem.add(reu512Item)
    val reu16MItem = new JRadioButtonMenuItem("16M ...")
    reu16MItem.addActionListener(_ => choose16MREU)
    group5.add(reu16MItem)
    reuItem.add(reu16MItem)
    IOItem.add(reuItem)

    val geoItem = new JMenu("GeoRAM")
    val groupgeo = new ButtonGroup
    val noGeoItem = new JRadioButtonMenuItem("None")
    noGeoItem.setSelected(true)
    noGeoItem.addActionListener(_ => setGeoRAM(false))
    groupgeo.add(noGeoItem)
    geoItem.add(noGeoItem)
    val _256kGeoItem = new JRadioButtonMenuItem("256K")
    _256kGeoItem.addActionListener(_ => setGeoRAM(true, 256))
    groupgeo.add(_256kGeoItem)
    geoItem.add(_256kGeoItem)
    val _512kGeoItem = new JRadioButtonMenuItem("512K")
    _512kGeoItem.addActionListener(_ => setGeoRAM(true, 512))
    groupgeo.add(_512kGeoItem)
    geoItem.add(_512kGeoItem)

    IOItem.add(geoItem)
    // Setting ---------------------------
    settings.add("geo-ram",
      "Set the georam size (none,256,512)",
      "GEO_RAM",
      (geo: String) => {
        if (geo == "512") {
          _512kGeoItem.setSelected(true)
          setGeoRAM(true, 512)
        }
        else if (geo == "256") {
          _256kGeoItem.setSelected(true)
          setGeoRAM(true, 256)
        }
      },
      if (_512kGeoItem.isSelected) "512"
      else if (_256kGeoItem.isSelected) "256"
      else "none"
    )
    settings.add("reu-type",
      "Set the reu type (none,128,256,512,16384)",
      "REU_TYPE",
      (reu: String) => {
        val reuPars = reu.split(",")
        if (reuPars(0) == "" || reuPars(0) == "none") setREU(None, None)
        else
          reuPars(0).toInt match {
            case REU.REU_1700 =>
              setREU(Some(REU.REU_1700), None)
              reu128Item.setSelected(true)
            case REU.REU_1750 =>
              setREU(Some(REU.REU_1750), None)
              reu512Item.setSelected(true)
            case REU.REU_1764 =>
              setREU(Some(REU.REU_1764), None)
              reu256Item.setSelected(true)
            case REU.REU_16M =>
              setREU(Some(REU.REU_16M), if (reuPars.length == 2 && reuPars(1) != "null") Some(reuPars(1)) else None)
              reu16MItem.setSelected(true)
          }
      },
      if (noReuItem.isSelected) "none"
      else if (reu128Item.isSelected) "128"
      else if (reu256Item.isSelected) "256"
      else if (reu512Item.isSelected) "512"
      else if (reu16MItem.isSelected) "16384," + REU.attached16MFileName
      else "none"
    )
    // -----------------------------------

    IOItem.addSeparator

    val digimaxItem = new JMenu("DigiMAX")
    IOItem.add(digimaxItem)
    val digiMaxSampleRateItem = new JMenuItem("DigiMax sample rate ...")
    digiMaxSampleRateItem.addActionListener(_ => chooseDigiMaxSampleRate)
    digimaxItem.add(digiMaxSampleRateItem)
    val group6 = new ButtonGroup
    val digimaxDisabledItem = new JRadioButtonMenuItem("Disabled")
    digimaxDisabledItem.setSelected(true)
    digimaxDisabledItem.addActionListener(_ => setDigiMax(false, None))
    digimaxItem.add(digimaxDisabledItem)
    group6.add(digimaxDisabledItem)
    val digimaxOnUserPortItem = new JRadioButtonMenuItem("On UserPort")
    digimaxOnUserPortItem.addActionListener(_ => setDigiMax(true, None))
    group6.add(digimaxOnUserPortItem)
    digimaxItem.add(digimaxOnUserPortItem)
    val digimaxDE00Item = new JRadioButtonMenuItem("On DE00")
    digimaxDE00Item.addActionListener(_ => setDigiMax(true, Some(0xDE00)))
    group6.add(digimaxDE00Item)
    digimaxItem.add(digimaxDE00Item)
    val digimaxDF00Item = new JRadioButtonMenuItem("On DF00")
    digimaxDF00Item.addActionListener(_ => setDigiMax(true, Some(0xDF00)))
    group6.add(digimaxDF00Item)
    digimaxItem.add(digimaxDF00Item)

    IOItem.addSeparator

    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2)
    IOItem.add(gmod2Item)

    IOItem.addSeparator

    val cpmItem = new JCheckBoxMenuItem("CP/M Cartdrige")
    cpmItem.addActionListener(e => enableCPMCart(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected))
    IOItem.add(cpmItem)
    settings.add("cpm64-enabled",
      s"Attach the CP/M cart",
      "CPM64",
      (cpm: Boolean) => {
        if (cpm) enableCPMCart(true)
      },
      ExpansionPort.getExpansionPort.isInstanceOf[CPMCartridge]
    )

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener(_ => ROMPanel.showROMPanel(displayFrame, configuration, true,true, () => saveSettings(false)))
  }

  override protected def setGlobalCommandLineOptions: Unit = {
    super.setGlobalCommandLineOptions
    settings.add("screen-dim",
      "Zoom factor. Valued accepted are 1 and 2",
      (f: Int) => if (f == 1 || f == 2) {
        zoom(f)
        zoomOverride = true
      }
    )
  }

  def turnOff {
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
  protected def saveState(out: ObjectOutputStream) {
    out.writeChars("KERNAL64")
    out.writeObject(ucesoft.cbm.Version.VERSION)
    out.writeLong(System.currentTimeMillis)
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
  }

  protected def loadState(in: ObjectInputStream) {
    val header = "KERNAL64"
    for (i <- 0 until header.length) if (in.readChar != header(i)) throw new IOException("Bad header")
    val ver = in.readObject.asInstanceOf[String]
    val ts = in.readLong
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    if (!loadStateFromOptions) {
      val msg = s"<html><b>Version:</b> $ver<br><b>Date:</b> ${new java.util.Date(ts)}</html>"
      JOptionPane.showConfirmDialog(displayFrame, msg, "State loading confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
        case JOptionPane.YES_OPTION =>
        case _ => throw new IOException("State loading aborted")
      }
    }
  }

  protected def allowsStateRestoring: Boolean = true

  // -----------------------------------------------------------------------------------------

  def turnOn(args: Array[String]) {
    swing {
      setMenu
    }
    // check help
    if (settings.checkForHelp(args)) {
      println(s"Kernal64, Commodore 64 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      settings.printUsage
      sys.exit(0)
    }
    swing {
      initComponent
    }
    // VIC
    swing {
      displayFrame.pack
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map {
        _.toInt
      }
      swing {
        updateScreenDimension(new Dimension(dim(0), dim(1)))
      }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map {
        _.toInt
      }
      swing {
        displayFrame.setLocation(xy(0), xy(1))
      }
    }
    // SETTINGS
    loadSettings(args)
    // VIEW
    swing {
      displayFrame.setVisible(!headless)
    }
    // PLAY
    clock.play
  }
}