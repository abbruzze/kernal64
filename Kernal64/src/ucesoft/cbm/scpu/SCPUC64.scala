package ucesoft.cbm.scpu

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.c64._
import ucesoft.cbm.cpu.wd65816.CPU65816
import ucesoft.cbm.cpu.{Memory, ROM}
import ucesoft.cbm.expansion._
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.bus.BusSnoop
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard
import ucesoft.cbm.peripheral.vic.VICType
import ucesoft.cbm.trace.Tracer
import ucesoft.cbm.trace.Tracer.TracedDisplay

import java.awt._
import java.io._
import javax.swing._

object SCPUC64 extends App {
  CBMComputer.turnOn(new SCPUC64,args)
}

class SCPUC64 extends CBMHomeComputer {
  override protected val cbmModel: CBMComputerModel = C64Model

  val componentID = "Commodore 64 SCPU"
  val componentType: Type = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "SCPU Kernal64"
  protected val CONFIGURATION_FILENAME = "SCPUC64.config"
  //override protected val PRG_RUN_DELAY_CYCLES = 220000
  protected val DEFAULT_KEYBOARD_RESOURCE_NAME = "/resources/default_keyboard_c64"

  override protected val mmu = new SCPUC64MMU.SCPU_MMU(cpuFastMode _, simmUsage _)
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
    ProgramLoader.warpModeListener = warpMode(_,true)
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
    WiC64.flag2Action = cia2.setFlagLow _
    wic64Panel = new WiC64Panel(displayFrame,preferences)
    WiC64.setListener(wic64Panel)
    add(WiC64)
    rs232.setBitReceivedListener(cia2.setFlagLow _)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC_II(vicMemory, mmu.COLOR_RAM, irqSwitcher.setLine(Switcher.VIC,_), baLow _)
    mmu.setLastByteReadMemory(vicMemory)
    // mapping I/O chips in memory
    mmu.setIO(cia1, cia2, sid, vicChip.asInstanceOf[vic.VIC_II])
    display = new vic.Display(vicChip.SCREEN_WIDTH, vicChip.SCREEN_HEIGHT, displayFrame.getTitle, displayFrame)
    add(display)
    display.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH, vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(display)
    displayFrame.getContentPane.add("Center", display)
    displayFrame.addKeyListener(this)
    display.addMouseListener(keypadControlPort)
    display.addMouseListener(controlport.ControlPort.emptyControlPort)
    val lightPen = new LightPenButtonListener
    add(lightPen)
    display.addMouseListener(lightPen)
    configureJoystick
    // tracing
    if (headless) Log.setOutput(null)
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

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame,Array(display),Array("VIC"))

    // trace
    tracer.addDevice(Tracer.TracedDevice("Main 65816 CPU", mmu, cpu, true))
    tracer.setDisplay(new TracedDisplay {
      override def getRasterLineAndCycle(): (Int, Int) = (vicChip.getRasterLine, vicChip.getRasterCycle)
      override def setDisplayRasterLine(line: Int): Unit = display.setRasterLineAt(line)
      override def enableDisplayRasterLine(enabled: Boolean): Unit = display.setDrawRasterLine(enabled)
    })
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
    ProgramLoader.checkLoadingInWarpMode(cbmModel,true)
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

  private def simmUsage(usage:Float): Unit = mmuStatusPanel.setSIMMUsage(usage)

  override def isHeadless: Boolean = headless

  // ======================================== Settings ==============================================
  override protected def enableDrive(id:Int,enabled:Boolean,updateFrame:Boolean) : Unit = {
    super.enableDrive(id,enabled,updateFrame)
    if (updateFrame) adjustRatio
  }

  private def adjustRatio() : Unit = {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim)
    displayFrame.pack()
  }

  protected def loadPRGFile(file: File, autorun: Boolean) : Unit = {
    val (start, end) = ProgramLoader.loadPRG(mmu, file, true, 8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      HomeKeyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar, mmu, true)
    }
  }

  private def takeSnapshot() : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        display.saveSnapshot(file)
      case _ =>
    }
  }

  protected def setSettingsMenu(optionMenu: JMenu) : Unit = {
    import Preferences._

    val driveMenu = new JMenuItem("Drives ...")
    driveMenu.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.ALT_DOWN_MASK))
    optionMenu.add(driveMenu)
    driveMenu.addActionListener(_ => DrivesConfigPanel.getDriveConfigDialog.setVisible(true))
    optionMenu.addSeparator()

    val keybEditorItem = new JMenuItem("Keyboard settings ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor())
    optionMenu.add(keybEditorItem)

    optionMenu.addSeparator()

    setVolumeSettings(optionMenu)

    optionMenu.addSeparator()

    setWarpModeSettings(optionMenu)

    optionMenu.addSeparator()

    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio)
    optionMenu.add(adjustRatioItem)

    val zoomItem = new JMenu("Zoom")
    val groupZ = new ButtonGroup
    optionMenu.add(zoomItem)
    for(z <- 1 to 3) {
      val zoom1Item = new JRadioButtonMenuItem(s"Zoom x $z")
      zoom1Item.addActionListener(_ => vicZoom(z) )
      val kea = z match {
        case 1 => java.awt.event.KeyEvent.VK_1
        case 2 => java.awt.event.KeyEvent.VK_2
        case 3 => java.awt.event.KeyEvent.VK_3
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

    optionMenu.addSeparator()

    setJoysticsSettings(optionMenu)

    setLightPenSettings(optionMenu)

    setMouseSettings(optionMenu)

    optionMenu.addSeparator()

    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot)
    optionMenu.add(snapshotItem)

    val gifRecorderItem = new JMenuItem("GIF recorder...")
    gifRecorderItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK))
    gifRecorderItem.addActionListener(_ => openGIFRecorder )
    optionMenu.add(gifRecorderItem)

    optionMenu.addSeparator()

    setPauseSettings(optionMenu)

    optionMenu.addSeparator()

    setPrinterSettings(optionMenu)
    // -----------------------------------
    optionMenu.addSeparator()

    setSIDSettings(optionMenu)
    // -----------------------------------
    optionMenu.addSeparator()

    setDrivesSettings

    val busSnooperActiveItem = new JCheckBoxMenuItem("Bus snoop active")
    busSnooperActiveItem.setSelected(false)
    busSnooperActiveItem.addActionListener(e => busSnooperActive = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected)
    optionMenu.add(busSnooperActiveItem)

    optionMenu.addSeparator()

    setRemotingSettings(optionMenu)

    optionMenu.addSeparator()

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    optionMenu.addSeparator()

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232)
    IOItem.add(rs232Item)

    IOItem.addSeparator()

    setFlyerSettings(IOItem)

    setWiC64Settings(IOItem)

    setREUSettings(IOItem)

    setGEORamSettings(IOItem)

    // -----------------------------------

    IOItem.addSeparator()

    setDigiMAXSettings(IOItem)

    IOItem.addSeparator()

    setGMOD3FlashSettings(IOItem)

    setEasyFlashSettings(IOItem)

    IOItem.addSeparator()

    setCPMSettings(IOItem)

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener(_ => {
      clock.pause
      ROMPanel.showROMPanel(displayFrame, configuration, cbmModel,true, () => {
        saveSettings(false)
        reset(false)
      })
      clock.play
    })

    // SCPU-RAM ===========================================================================================
    val scpuRamItem = new JMenu("SCPU RAM")
    optionMenu.add(scpuRamItem)
    val groupscpu = new ButtonGroup
    val scpuNORAMItem = new JRadioButtonMenuItem("0M")
    scpuNORAMItem.addActionListener(_ => preferences(PREF_SCPURAM) = "none" )
    groupscpu.add(scpuNORAMItem)
    scpuRamItem.add(scpuNORAMItem)
    val scpu1MRAMItem = new JRadioButtonMenuItem("1M")
    scpu1MRAMItem.addActionListener(_ => preferences(PREF_SCPURAM) = "1" )
    groupscpu.add(scpu1MRAMItem)
    scpuRamItem.add(scpu1MRAMItem)
    val scpu4MRAMItem = new JRadioButtonMenuItem("4M")
    scpu4MRAMItem.addActionListener(_ => preferences(PREF_SCPURAM) = "4" )
    groupscpu.add(scpu4MRAMItem)
    scpuRamItem.add(scpu4MRAMItem)
    val scpu8MRAMItem = new JRadioButtonMenuItem("8M")
    scpu8MRAMItem.addActionListener(_ => preferences(PREF_SCPURAM) = "8" )
    groupscpu.add(scpu8MRAMItem)
    scpuRamItem.add(scpu8MRAMItem)
    val scpu16MRAMItem = new JRadioButtonMenuItem("16M")
    scpu16MRAMItem.addActionListener(_ => preferences(PREF_SCPURAM) = "16" )
    scpu16MRAMItem.setSelected(true)
    groupscpu.add(scpu16MRAMItem)
    scpuRamItem.add(scpu16MRAMItem)
    preferences.add(PREF_SCPURAM,"super ram size: none,1,4,8,16","none",Set("none","1","4","8","16")) { size =>
      if (size == "" || size == null) { scpu16MRAMItem.setSelected(true) ; mmu.setSIMMSize(16) }
      else if (size == "none") { scpuNORAMItem.setSelected(true) ; mmu.setSIMMSize(0) }
      else if (size == "1") { scpu1MRAMItem.setSelected(true) ; mmu.setSIMMSize(1) }
      else if (size == "4") { scpu4MRAMItem.setSelected(true) ; mmu.setSIMMSize(4) }
      else if (size == "8") { scpu8MRAMItem.setSelected(true) ; mmu.setSIMMSize(8) }
      else if (size == "16") {scpu16MRAMItem.setSelected(true)  ; mmu.setSIMMSize(16) }
      else throw new IllegalArgumentException(s"Bad SIMM size: $size")
    }
    // SCPU-JIFFYDOS-ENABLED ==============================================================================
    preferences.add(PREF_SCPUJIFFYDOSENABLED,"Enables JiffyDOS at startup",false) { jiffyEnabled =>
      mmu.setJiffyDOS(jiffyEnabled)
      mmuStatusPanel.enableJiffyDOS(jiffyEnabled)
    }
  }

  protected def saveSettings(save: Boolean): Unit = {
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

  protected def getRAM: Memory = mmu.getRAM

  protected def getCharROM: Memory = mmu.CHAR_ROM

  // state
  protected def saveState(out: ObjectOutputStream) : Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeInt(cpuClocks)
    out.writeObject(vicChip.getVICModel.VIC_TYPE.toString)
  }

  protected def loadState(in: ObjectInputStream) : Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    cpuClocks = in.readInt
    val vicModel = VICType.withName(in.readObject.toString)
    setVICModel(vicModel,false,false,false)
  }

  protected def allowsStateRestoring: Boolean = true

  override protected def setGlobalCommandLineOptions : Unit = {
    import Preferences._
    super.setGlobalCommandLineOptions

    preferences.remove("kernel")
    preferences.add(PREF_KERNEL,"Set scpu kernel rom path","") { kp =>
      if (kp != "") reloadROM(ROM.SCPU64_ROM_PROP,kp)
    }
  }

  // -----------------------------------------------------------------------------------------

  def turnOn(args: Array[String]) : Unit = {
    swing {
      setMenu
    }
    // check help
    if (preferences.checkForHelp(args)) {
      println(s"Kernal64, SuperCPU emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      preferences.printUsage("file to attach")
      sys.exit(0)
    }
    // --headless handling to disable logging & debugging
    if (args.exists(_ == "--headless")) headless = true
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    swing { initComponent }
    // VIC
    swing { displayFrame.pack() }

    // screen's dimension and size restoration
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map {
        _.toInt
      }
      swing { updateVICScreenDimension(new Dimension(dim(0), dim(1))) }
    }
    else vicZoom(2)
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map {
        _.toInt
      }
      swing { displayFrame.setLocation(xy(0), xy(1)) }
    }
    else displayFrame.setLocationByPlatform(true)
    // SETTINGS
    loadSettings(args)
    // VIEW
    swing {
      displayFrame.setVisible(!headless)
      if (fullScreenAtBoot) setVicFullScreen
    }
    // PLAY
    clock.play
    // KEYBOARD LAYOUT
    swing {
      checkKeyboardLayout()
    }
  }
}