package ucesoft.cbm.vic20

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion._
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard
import ucesoft.cbm.peripheral.sid.DefaultAudioDriver
import ucesoft.cbm.peripheral.vic.VICType
import ucesoft.cbm.trace.TraceDialog

import java.awt._
import java.io._
import javax.swing._

object VIC20 extends App {
  CBMComputer.turnOn(new VIC20, args)
}

class VIC20 extends CBMHomeComputer {
  override protected def PRG_LOAD_ADDRESS() = 0x1001
  override protected val cbmModel: CBMComputerModel = VIC20Model

  override  protected val DEFAULT_GAME_PROVIDERS = java.util.Arrays.asList((new ucesoft.cbm.game.PouetDemoVIC20Spi).asInstanceOf[ucesoft.cbm.game.GameProvider])

  val componentID = "Commodore VIC 20"
  val componentType: Type = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "Kernal64 - VIC20"
  protected val CONFIGURATION_FILENAME = "VIC20.config"

  // TODO change
  protected val keybMapper: keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)), "/resources/default_keyboard_vic20", VIC20Model)

  protected val mmu = new VIC20MMU

  protected var via1 : VIC20Via1 = _
  protected var via2 : VIC20Via2 = _
  protected val audioDriver = new DefaultAudioDriver(44100,26)
  override protected lazy val volumeDialog : JDialog = VolumeSettingsPanel.getDialog(displayFrame,audioDriver)

  override protected lazy val keyb = new keyboard.HomeKeyboard(keybMapper,low => via1.restoreKeyPressed(low),false)

  override protected def warpMode(warpOn: Boolean, play: Boolean = true): Unit = {
    super.warpMode(warpOn, play)
    if (play) clock.pause
    audioDriver.setSoundOn(!warpOn)
    if (play) clock.play
  }

  def reset: Unit = {
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
    audioDriver.reset
  }

  def init: Unit = {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo

    clock.setClockHz(vic.VIC_I_PAL.CPU_FREQ)

    Log.info("Building the system ...")
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame, AVAILABLE_RS232)
    // drive
    initializedDrives(DriveType._1541)
    // -----------------------
    ProgramLoader.cpu = cpu
    ProgramLoader.warpModeListener = warpMode(_, true)
    add(clock)
    add(mmu)
    add(cpu)
    add(keyb)
    add(controlPortA)
    add(bus)
    add(rs232)
    // ROMs
    add(VIC20MMU.CHAR_ROM)
    add(VIC20MMU.KERNAL_ROM)
    add(VIC20MMU.BASIC_ROM)
    // -----------------------
    // TODO
    //rs232.setCIA12(cia1, cia2)
    vicChip = new vic.VIC_I(mmu,audioDriver)
    add(vicChip)
    // tape
    datassette = new Datassette(() => via2.datassetteReadLine() )
    add(datassette)
    // VIAs
    via1 = new VIC20Via1(bus,controlPortA,datassette,cpu.nmiRequest _)
    via2 = new VIC20Via2(bus,keyb,controlPortA,datassette,cpu.irqRequest _,via1)
    add(via1)
    add(via2)
    // mapping I/O chips in memory
    mmu.setIO(via1,via2,vicChip.asInstanceOf[vic.VIC_I])
    display = new vic.Display(vicChip.SCREEN_WIDTH, vicChip.SCREEN_HEIGHT, displayFrame.getTitle, displayFrame)
    add(display)
    // TODO
    display.setPreferredSize(vicChip.STANDARD_DIMENSION)
    //display.setPreferredSize(new java.awt.Dimension(784,vicChip.VISIBLE_SCREEN_HEIGHT<<1))
    //TODO testbench screen dim => display.setPreferredSize(new java.awt.Dimension(568,284))
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
    if (!headless) {
      traceDialog = TraceDialog.getTraceDialog("CPU Debugger", displayFrame, mmu, cpu, display, vicChip)
      diskTraceDialog = TraceDialog.getTraceDialog("Drive 8 Debugger", displayFrame, drives(0).getMem, drives(0))
      Log.setOutput(traceDialog.logPanel.writer)
    }
    else Log.setOutput(null)

    // TestCart
    TestCart.setCartLocation(0x910F)

    // printer
    add(printer)

    displayFrame.getContentPane.add("South", makeInfoPanel(true))
    displayFrame.setTransferHandler(DNDHandler)
    Log.info(sw.toString)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame, Array(display), Array("VIC"))
  }

  override def afterInitHook : Unit = {
    super.afterInitHook
    mmu.setCharROM(VIC20MMU.CHAR_ROM.getROMBytes())
    mmu.setKernelROM(VIC20MMU.KERNAL_ROM.getROMBytes())
    mmu.setBasicROM(VIC20MMU.BASIC_ROM.getROMBytes())
  }

  protected def mainLoop(cycles: Long): Unit = {
    // VIC PHI1
    vicChip.clock()
    // VIAs
    via1.clock(cycles)
    via2.clock(cycles)
    //DRIVES
    var d = 0
    while (d < TOTAL_DRIVES) {
      if (drivesEnabled(d) && drivesRunning(d)) drives(d).clock(cycles)
      d += 1
    }
    if (device12DriveEnabled) device12Drive.clock(cycles)
    // printer
    if (printerEnabled) printer.clock(cycles)
    // CPU PHI2
    // TODO
    //ProgramLoader.checkLoadingInWarpMode(true)
    cpu.fetchAndExecute(1)
  }

  protected def setDMA(dma: Boolean): Unit = {}

  override def isHeadless: Boolean = headless

  override protected def vicZoom(f: Int): Unit = {
    val dim = f match {
      case 1 => new Dimension(vicChip.VISIBLE_SCREEN_WIDTH << 1, vicChip.VISIBLE_SCREEN_HEIGHT)
      case 2 => vicChip.STANDARD_DIMENSION
    }
    vicZoomFactor = f
    updateVICScreenDimension(dim)
  }

  // ======================================== Settings ==============================================
  override protected def enableDrive(id: Int, enabled: Boolean, updateFrame: Boolean): Unit = {
    super.enableDrive(id, enabled, updateFrame)
    if (updateFrame) adjustRatio
  }

  private def adjustRatio(): Unit = {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim)
    displayFrame.pack()
  }

  protected def loadPRGFile(file: File, autorun: Boolean): Unit = {
    val (start, end) = ProgramLoader.loadPRG(mmu, file, true, 8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      HomeKeyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar, mmu, true)
    }
  }

  private def takeSnapshot(): Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        display.saveSnapshot(file)
      case _ =>
    }
  }

  override protected def setCartMenu(fileMenu: JMenu): Unit = {
    import Preferences._
    val attachCtrItem = new JMenuItem("Attach cartridge ...")
    attachCtrItem.addActionListener(_ => attachCtr)
    fileMenu.add(attachCtrItem)
    preferences.add(PREF_CART, "Attach the given cartridge", "") { cart =>
      if (cart != "") loadCartridgeFile(new File(cart))
    }
    val attachRawCtrItem = new JMenuItem("Attach raw cartridge ...")
    attachRawCtrItem.addActionListener(_ => attachRawCtr())
    fileMenu.add(attachRawCtrItem)
    preferences.add(PREF_RAW_CART, "Attach the given raw cartridge", "") { cart =>
      if (cart != "") loadRawCartridgeFile(new File(cart))
    }
  }

  override def detachCtr(): Unit = {
    if (Thread.currentThread != Clock.systemClock) clock.pause
    mmu.detachAllCarts()
    reset(true)
    detachCtrItem.setEnabled(false)
    cartMenu.setVisible(false)
  }

  protected def attachRawCtr(): Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileView(new C64FileView)
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        loadRawCartridgeFile(fc.getSelectedFile)
      case _ =>
    }
  }

  protected def loadRawCartridgeFile(file:File, stateLoading: Boolean = false): Unit = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause
      val crt = new Cartridge(file.toString,true)
      if (!mmu.attachCart(crt)) {
        showError("Cartridge error", s"Cannot attach cartridge: address not valid or conflict with another cartridge")
        return
      }
      crt.cbmType = Cartridge.CBMType.VIC20
      println(crt)
      cartMenu.setVisible(true)
      Log.info(s"Attached raw cartridge ${crt.name}")
      preferences.updateWithoutNotify(Preferences.PREF_RAW_CART, file.toString)
      if (!stateLoading) reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
    }
    catch {
      case t: Throwable =>
        if (traceDialog != null) t.printStackTrace(traceDialog.logPanel.writer)

        showError("Cartridge loading error", t.toString)
    }
    finally {
      if (!stateLoading) clock.play
    }
  }

  override protected def loadCartridgeFile(file: File, stateLoading: Boolean = false): Unit = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause
      val crt = new Cartridge(file.toString)
      if (crt.cbmType != Cartridge.CBMType.VIC20) throw new IllegalArgumentException(s"Unsupported cartridge signature '${crt.cbmType}'")
      if (!mmu.attachCart(crt)) {
        showError("Cartridge error",s"Cannot attach cartridge: address not valid or conflict with another cartridge")
        return
      }
      println(crt)
      cartMenu.setVisible(true)
      Log.info(s"Attached cartridge ${crt.name}")
      preferences.updateWithoutNotify(Preferences.PREF_CART, file.toString)
      if (!stateLoading) reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
    }
    catch {
      case t: Throwable =>
        if (traceDialog != null) t.printStackTrace(traceDialog.logPanel.writer)

        showError("Cartridge loading error", t.toString)
    }
    finally {
      if (!stateLoading) clock.play
    }
  }

  override protected def showCartInfo: Unit = {
    val cols: Array[Object] = Array("Name","Type", "Addresses")
    val data : Array[Array[Object]] = for(cart <- mmu.getAttachedCarts().toArray) yield {
      Array(cart.name,cart.ctrType.toString,cart.chips.map(_.startingLoadAddress.toHexString).mkString(","))
    }
    val table = new JTable(data, cols)
    val sp = new JScrollPane(table)
    val panel = new JPanel(new BorderLayout)
    panel.add("Center", sp)
    JOptionPane.showMessageDialog(displayFrame, panel, "Cart info", JOptionPane.INFORMATION_MESSAGE, new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  protected def setSettingsMenu(optionMenu: JMenu): Unit = {
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
    val vicDisplayEffectsItem = new JMenuItem("VIC's display effects ...")
    vicItem.add(vicDisplayEffectsItem)
    vicDisplayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(displayFrame, display, "VIC").setVisible(true))

    setVICModel(vicItem)

    setVICBorderMode(vicItem)

    setFullScreenSettings(optionMenu)
    setOneFrameMode(vicItem, display, java.awt.event.KeyEvent.VK_N)
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
    gifRecorderItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.ALT_DOWN_MASK))
    gifRecorderItem.addActionListener(_ => openGIFRecorder)
    optionMenu.add(gifRecorderItem)

    optionMenu.addSeparator()

    setPauseSettings(optionMenu)

    optionMenu.addSeparator()

    setPrinterSettings(optionMenu)

    // -----------------------------------
    optionMenu.addSeparator()

    setDrivesSettings

    optionMenu.addSeparator()

    setRemotingSettings(optionMenu)

    optionMenu.addSeparator()

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    optionMenu.addSeparator()

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232)
    IOItem.add(rs232Item)

    // -----------------------------------

    IOItem.addSeparator()

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener(_ => {
      clock.pause
      ROMPanel.showROMPanel(displayFrame, configuration, true, false, () => {
        saveSettings(false)
        reset(false)
      })
      clock.play
    })
  }

  override protected def setGlobalCommandLineOptions: Unit = {
    import Preferences._
    super.setGlobalCommandLineOptions
    // TODO
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

  protected def getRAM: Memory = mmu

  protected def getCharROM: Memory = VIC20MMU.CHAR_ROM

  // state
  protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeObject(vicChip.getVICModel.VIC_TYPE.toString)
  }

  protected def loadState(in: ObjectInputStream): Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    val vicModel = VICType.withName(in.readObject.toString)
    setVICModel(vicModel, false, false, false)
  }

  protected def allowsStateRestoring: Boolean = true
  // -----------------------------------------------------------------------------------------

  def turnOn(args: Array[String]): Unit = {
    swing {
      setMenu
    }
    // check help
    if (preferences.checkForHelp(args)) {
      println(s"Kernal64, Commodore VIC 20 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      preferences.printUsage("file to attach")
      sys.exit(0)
    }
    // --headless handling to disable logging & debugging
    if (args.exists(_ == "--headless")) headless = true
    swing {
      initComponent
    }
    // VIC
    swing {
      displayFrame.pack()
    }
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    // screen's dimension and size restoration
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map {
        _.toInt
      }
      swing {
        updateVICScreenDimension(new Dimension(dim(0), dim(1)))
      }
    }
    //else vicZoom(2)
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map {
        _.toInt
      }
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
      if (fullScreenAtBoot) setVicFullScreen
    }
    // PLAY
    clock.play
  }
}
