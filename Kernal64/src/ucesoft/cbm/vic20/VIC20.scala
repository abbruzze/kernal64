package ucesoft.cbm.vic20

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.cpu.{Memory, ROM}
import ucesoft.cbm.expansion._
import ucesoft.cbm.expansion.vic20.{VIC201112IEEE488, VIC20ExpansionPort, VIC20FE3, VIC20GeoRAM, VIC20Ultimem}
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.controlport.JoystickSettingDialog
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard
import ucesoft.cbm.peripheral.sid.DefaultAudioDriver
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.peripheral.vic.{Palette, VICType}
import ucesoft.cbm.trace.Tracer
import ucesoft.cbm.trace.Tracer.TracedDisplay
//import ucesoft.cbm.trace.TraceDialog

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import java.io._
import java.util.Properties
import javax.swing._

object VIC20 extends App {
  CBMComputer.turnOn(new VIC20, args)
}

class VIC20 extends CBMHomeComputer {
  override protected val cbmModel: CBMComputerModel = VIC20Model

  override  protected val DEFAULT_GAME_PROVIDERS = java.util.Arrays.asList((new ucesoft.cbm.game.PouetDemoVIC20Spi).asInstanceOf[ucesoft.cbm.game.GameProvider])

  val componentID = "Commodore VIC 20"
  val componentType: Type = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "Kernal64 - VIC20"
  protected val CONFIGURATION_FILENAME = "VIC20.config"
  protected val DEFAULT_KEYBOARD_RESOURCE_NAME = "/resources/default_keyboard_vic20"

  protected val mmu = new VIC20MMU

  protected var via1 : VIC20Via1 = _
  protected var via2 : VIC20Via2 = _
  protected val audioDriver = new DefaultAudioDriver(44100,26)
  override protected lazy val volumeDialog : VolumeSettingsPanel.VolumeDialog = VolumeSettingsPanel.getDialog(displayFrame,audioDriver)

  override protected lazy val keyb = new keyboard.HomeKeyboard(keybMapper,low => via1.restoreKeyPressed(low),cbmModel)

  protected val memoryConfigLabel = new JLabel()
  protected val interlaceModeLabel = new JLabel()

  protected val specialCartLoaderMap : Map[VIC20ExpansionPort.VICExpansionPortType.Value,VIC20ExpansionPort.VIC20ExpansionPortStateHandler] = Map(
    VIC20ExpansionPort.VICExpansionPortType.ULTIMEM -> VIC20Ultimem,
    VIC20ExpansionPort.VICExpansionPortType.GEORAM -> VIC20GeoRAM,
    VIC20ExpansionPort.VICExpansionPortType.IEEE488 -> VIC201112IEEE488,
    VIC20ExpansionPort.VICExpansionPortType.FE3 -> VIC20FE3,
  )

  protected var signals : VIC20ExpansionPort.Signals = _

  override protected def PRG_LOAD_ADDRESS() = {
    import VIC20MMU._
    val config = mmu.getExpansionSettings()
    if (config == NO_EXP || config == EXP_BLK0) 0x1001 else 0x1201
  }

  override def PRG_RUN_DELAY_CYCLES: Int = {
    import VIC20MMU._
    val config = mmu.getExpansionSettings()
    if (config == EXP_BLK1) 1600000
    else if (config == (EXP_BLK1 | EXP_BLK2)) 2160000
    else if (config == (EXP_BLK1 | EXP_BLK2 | EXP_BLK3)) 2760000
    else if (config == (EXP_BLK0 | EXP_BLK1 | EXP_BLK2 | EXP_BLK3 | EXP_BLK5)) 2900000
    else if (config == (EXP_BLK1 | EXP_BLK2 | EXP_BLK3 | EXP_BLK5)) 2900000
    else super.PRG_RUN_DELAY_CYCLES
  }

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
    add(controlPortB)
    add(bus)
    add(ieee488Bus)
    add(rs232)
    // ROMs
    add(VIC20MMU.CHAR_ROM)
    add(VIC20MMU.KERNAL_PAL_ROM)
    add(VIC20MMU.KERNAL_NTSC_ROM)
    add(VIC20MMU.BASIC_ROM)
    // -----------------------
    vicChip = new vic.VIC_I(mmu,audioDriver)
    add(vicChip)
    // tape
    datassette = new Datassette(() => via2.datassetteReadLine() )
    add(datassette)
    // VIAs
    via1 = new VIC20Via1(bus,controlPortB,controlPortA,rs232,datassette,cpu.nmiRequest _, vicChip.triggerLightPen _)
    via2 = new VIC20Via2(bus,keyb,controlPortB,datassette,cpu.irqRequest _,via1)
    add(via1)
    add(via2)
    // WiC64
    WiC64.flag2Action = () => {
      via1.CB1In(true) ; via1.CB1In(false)
    }
    wic64Panel = new WiC64Panel(displayFrame,preferences)
    WiC64.setListener(wic64Panel)
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
    if (headless) Log.setOutput(null)

    // TestCart
    TestCart.setCartLocation(0x910F)

    // printer
    add(printer)

    val infoPanel = makeInfoPanel(true)
    val memPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    memPanel.add(interlaceModeLabel)
    memPanel.add(memoryConfigLabel)
    vicChip.asInstanceOf[vic.VIC_I].setInterlaceModeListener(enabled => {
      if (enabled) interlaceModeLabel.setText("Interlace on") else interlaceModeLabel.setText("")
    })
    infoPanel.add("West",memPanel)
    displayFrame.getContentPane.add("South", infoPanel)
    displayFrame.setTransferHandler(DNDHandler)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame, Array(display), Array("VIC"))

    signals = VIC20ExpansionPort.Signals(preferences,cpu.irqRequest _,cpu.nmiRequest _,() => reset(true),bus,ieee488Bus,mmu)

    // trace
    tracer.addDevice(Tracer.TracedDevice("Main 6502 CPU",mmu,cpu,true))
    tracer.setDisplay(new TracedDisplay {
      override def getRasterLineAndCycle(): (Int, Int) = (vicChip.getRasterLine, vicChip.getRasterCycle)
      override def setDisplayRasterLine(line: Int): Unit = display.setRasterLineAt(line)
      override def enableDisplayRasterLine(enabled: Boolean): Unit = display.setDrawRasterLine(enabled)
    })
  }

  override def afterInitHook : Unit = {
    super.afterInitHook
    mmu.setCharROM(VIC20MMU.CHAR_ROM.getROMBytes())
    mmu.setKernelPALROM(VIC20MMU.KERNAL_PAL_ROM.getROMBytes())
    mmu.setKernelNTSCROM(VIC20MMU.KERNAL_NTSC_ROM.getROMBytes())
    mmu.setBasicROM(VIC20MMU.BASIC_ROM.getROMBytes())
    mmu.setVICType(VICType.PAL)
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
    ProgramLoader.checkLoadingInWarpMode(cbmModel,true)
    cpu.fetchAndExecute(1)
    // special cart
    mmu.clock(cycles)
  }

  protected def setDMA(dma: Boolean): Unit = {}

  override def isHeadless: Boolean = headless

  override protected def vicZoom(f: Int): Unit = {
    val dim = f match {
      case 1 => new Dimension(vicChip.VISIBLE_SCREEN_WIDTH << 1, vicChip.VISIBLE_SCREEN_HEIGHT)
      case 2 => vicChip.STANDARD_DIMENSION
      case 3 => vicChip.TESTBENCH_DIMENSION
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
    val stdDim = vicChip.STANDARD_DIMENSION
    if (dim.width > stdDim.width)
      dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    else
      dim.width = (dim.height * vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim)
    displayFrame.pack()
  }

  protected def loadPRGFile(file: File, autorun: Boolean): Unit = {
    val (start, end) = ProgramLoader.loadPRG(mmu, file, true, 8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
    if (autorun) {
      HomeKeyboard.insertTextIntoKeyboardBuffer("RUN" + 13.toChar, mmu, true)
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

  protected def loadRawCartridgeFile(file:File, stateLoading: Boolean = false): Option[Cartridge] = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause
      val crt = new Cartridge(file.toString,true)
      if (!mmu.attachCart(crt)) {
        showError("Cartridge error", s"Cannot attach cartridge: address not valid or conflict with another cartridge")
        return None
      }
      crt.cbmType = Cartridge.CBMType.VIC20
      println(crt)
      cartMenu.setVisible(true)
      Log.info(s"Attached raw cartridge ${crt.name}")
      preferences.updateWithoutNotify(Preferences.PREF_RAW_CART, file.toString)
      if (!stateLoading) reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
      Some(crt)
    }
    catch {
      case t: Throwable =>
        Log.info(t.toString)
        showError("Cartridge loading error", t.toString)
        None
    }
    finally {
      if (!stateLoading) clock.play
    }
  }

  override protected def loadCartridgeFile(file: File, stateLoading: Boolean = false): Option[Cartridge] = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause
      val crt = new Cartridge(file.toString)
      if (crt.cbmType != Cartridge.CBMType.VIC20) throw new IllegalArgumentException(s"Unsupported cartridge signature '${crt.cbmType}'")
      if (!mmu.attachCart(crt)) {
        showError("Cartridge error",s"Cannot attach cartridge: address not valid or conflict with another cartridge")
        return None
      }
      println(crt)
      cartMenu.setVisible(true)
      Log.info(s"Attached cartridge ${crt.name}")
      preferences.updateWithoutNotify(Preferences.PREF_CART, file.toString)
      if (!stateLoading) reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR, file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
      Some(crt)
    }
    catch {
      case t: Throwable =>
        Log.info(t.toString)
        showError("Cartridge loading error", t.toString)
        None
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

  protected def showRAMConfig(): Unit = {
    import VIC20MMU._

    val configPanel = new JPanel(new GridLayout(0,1))
    //configPanel.setLayout(new BoxLayout(configPanel,BoxLayout.Y_AXIS))
    val configTemplate = new JComboBox[String](Array("No expansion","3K (block 0)","8K (block 1)","16K (block 1|2)","24K (block 1|2|3)","All (block 0|1|2|3|5)","Custom"))
    val bChecks = Array(
      new JCheckBox("3K (0400 - 0FFF)"),
      new JCheckBox("8K (2000 - 3FFF)"),
      new JCheckBox("8K (4000 - 5FFF)"),
      new JCheckBox("8K (6000 - 7FFF)"),
      new JCheckBox("8K (A000 - BFFF)")
    )

    def updateConfig(config:Int): Unit = {
      bChecks(0).setSelected((config & EXP_BLK0) > 0)
      bChecks(1).setSelected((config & EXP_BLK1) > 0)
      bChecks(2).setSelected((config & EXP_BLK2) > 0)
      bChecks(3).setSelected((config & EXP_BLK3) > 0)
      bChecks(4).setSelected((config & EXP_BLK5) > 0)

      if (config == NO_EXP) {
        configTemplate.setSelectedIndex(0)
      }
      else if (config == EXP_BLK0) {
        configTemplate.setSelectedIndex(1)
      }
      else if (config == EXP_BLK1) {
        configTemplate.setSelectedIndex(2)
      }
      else if (config == (EXP_BLK1 | EXP_BLK2)) {
        configTemplate.setSelectedIndex(3)
      }
      else if (config == (EXP_BLK1 | EXP_BLK2 | EXP_BLK3)) {
        configTemplate.setSelectedIndex(4)
      }
      else if (config == (EXP_BLK0 | EXP_BLK1 | EXP_BLK2 | EXP_BLK3 | EXP_BLK5)) {
        configTemplate.setSelectedIndex(5)
      }
      else configTemplate.setSelectedIndex(6)
    }

    val oldConfig = mmu.getExpansionSettings()
    var config = oldConfig
    updateConfig(config)

    val checkAction = new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        config = 0
        if (bChecks(0).isSelected) config |= EXP_BLK0
        if (bChecks(1).isSelected) config |= EXP_BLK1
        if (bChecks(2).isSelected) config |= EXP_BLK2
        if (bChecks(3).isSelected) config |= EXP_BLK3
        if (bChecks(4).isSelected) config |= EXP_BLK5
        updateConfig(config)
      }
    }

    for(b <- bChecks) b.addActionListener(checkAction)
    configTemplate.addActionListener(_ => {
      configTemplate.getSelectedIndex match {
        case 0 => config = NO_EXP ; updateConfig(config)
        case 1 => config = EXP_BLK0 ; updateConfig(config)
        case 2 => config = EXP_BLK1 ; updateConfig(config)
        case 3 => config = EXP_BLK1 | EXP_BLK2 ; updateConfig(config)
        case 4 => config = EXP_BLK1 | EXP_BLK2 | EXP_BLK3 ; updateConfig(config)
        case 5 => config = EXP_BLK0 | EXP_BLK1 | EXP_BLK2 | EXP_BLK3 | EXP_BLK5; updateConfig(config)
        case 6 =>
      }
    })

    val comboPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    comboPanel.add(new JLabel("Memory templates:"))
    comboPanel.add(configTemplate)
    configPanel.add(comboPanel)
    var dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(new JLabel("Memory blocks:"))
    configPanel.add(dummyPanel)
    for(b <- bChecks) {
      dummyPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
      dummyPanel.add(b)
      configPanel.add(dummyPanel)
    }

    dummyPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    val okB = new JButton("Apply")
    val cancelB = new JButton("Cancel")
    dummyPanel.add(okB)
    dummyPanel.add(cancelB)

    val dialog = new JDialog(displayFrame,"Memory configuration",true)
    okB.addActionListener(_ => {
      updateMemoryConfig(config)
      dialog.dispose()
    })
    cancelB.addActionListener(_ => dialog.dispose() )

    dialog.getContentPane.add("Center",configPanel)
    dialog.getContentPane.add("South",dummyPanel)
    dialog.pack()
    dialog.setLocationRelativeTo(displayFrame)
    dialog.setResizable(false)
    dialog.setVisible(true)
  }

  protected def updateMemoryConfig(config:Int,play:Boolean = true): Unit = {
    import Preferences._
    clock.pause()
    mmu.setExpansion(config)
    memoryConfigLabel.setText(VIC20MMU.getLabelConfig(config))
    preferences.update(PREF_VIC20_MEM_CONFIG,VIC20MMU.getStringConfig(config))
    //reset(true)
    if (play) clock.play()
  }

  override protected def setPaletteSettings(parent:JMenu): Unit = {
    import Preferences._
    // VIC-PALETTE =========================================================================================
    val paletteItem = new JMenu("Palette")
    parent.add(paletteItem)
    val groupP = new ButtonGroup
    val vicePalItem = new JRadioButtonMenuItem("VICE")
    vicePalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "vice")
    vicePalItem.setSelected(true)
    paletteItem.add(vicePalItem)
    groupP.add(vicePalItem)
    val peptoPalItem = new JRadioButtonMenuItem("Mike's PAL")
    peptoPalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "mike_pal")
    paletteItem.add(peptoPalItem)
    groupP.add(peptoPalItem)
    val colordorePalItem = new JRadioButtonMenuItem("Colodore")
    colordorePalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "colodore")
    paletteItem.add(colordorePalItem)
    groupP.add(colordorePalItem)
    val filePalItem = new JRadioButtonMenuItem("From file ...")
    filePalItem.addActionListener(_ => {
      loadPaletteFromFile(PREF_VICPALETTE_FILE) match {
        case Some(file) =>
          preferences(PREF_VICPALETTE_FILE) = file
        case None =>
      }
    })
    paletteItem.add(filePalItem)
    groupP.add(filePalItem)

    preferences.add(PREF_VICPALETTE_FILE, "Load VIC's palette from vpl file", "") { vpl =>
      if (Palette.setVICPaletteFromFile(vpl)) {
        preferences.updateWithoutNotify(PREF_VICPALETTE, "")
        filePalItem.setSelected(true)
      }
    }

    preferences.add(PREF_VICPALETTE, "Set the palette type (vice,mike_pal,colodore)", "", Set("vice", "mike_pal", "colodore","")) { pal =>
      pal match {
        case "vice" =>
          Palette.setVICPalette(PaletteType.VIC20_VICE)
          vicePalItem.setSelected(true)
        case "mike_pal"| "" =>
          Palette.setVICPalette(PaletteType.VIC20_MIKE_PAL)
          peptoPalItem.setSelected(true)
        case "colodore" =>
          Palette.setVICPalette(PaletteType.VIC20_COLODORE)
          colordorePalItem.setSelected(true)
        case _ =>
      }
    }
    // =====================================================================================================
  }

  override protected def setVICModel(model:VICType.Value,preserveDisplayDim:Boolean = false,resetFlag:Boolean,play:Boolean = true) : Unit = {
    super.setVICModel(model,preserveDisplayDim, resetFlag, play)
    updateVICScreenDimension(vicChip.STANDARD_DIMENSION)
    mmu.setVICType(model)
  }

  override protected def updateVICScreenDimension(dim: Dimension): Unit = {
    display.setPreferredSize(dim)
    display.invalidate()
    display.repaint()
    displayFrame.pack()
    if (vicZoomFactor == 3) {
      vicChip.asInstanceOf[vic.VIC_I].setTestBenchMode(true)
    }
    else vicChip.asInstanceOf[vic.VIC_I].setTestBenchMode(false)
    // TODO: vicZoomFactor
  }

  override protected def joySettings(): Unit = {
    Clock.systemClock.pause
    try {
      val dialog = new JoystickSettingDialog(displayFrame, configuration, gameControlPort,Array("Control Port","User Port"))
      dialog.setVisible(true)
      configureJoystick
    }
    finally {
      Clock.systemClock.play
    }
  }

  override protected def setGEORamSettings(parent: JMenu): Unit = {
    import Preferences._
    // GEO-RAM =============================================================================================
    val geoItem = new JMenu("GeoRAM")
    val groupgeo = new ButtonGroup
    val noGeoItem = new JRadioButtonMenuItem("None")
    noGeoItem.setSelected(true)
    noGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "none")
    groupgeo.add(noGeoItem)
    geoItem.add(noGeoItem)
    val _512kGeoItem = new JRadioButtonMenuItem("512K")
    _512kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "512")
    groupgeo.add(_512kGeoItem)
    geoItem.add(_512kGeoItem)
    val _1024kGeoItem = new JRadioButtonMenuItem("1024K")
    _1024kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "1024")
    groupgeo.add(_1024kGeoItem)
    geoItem.add(_1024kGeoItem)
    val _2048kGeoItem = new JRadioButtonMenuItem("2048K")
    _2048kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "2048")
    groupgeo.add(_2048kGeoItem)
    geoItem.add(_2048kGeoItem)
    val _4096kGeoItem = new JRadioButtonMenuItem("4096K")
    _4096kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "4096")
    groupgeo.add(_4096kGeoItem)
    geoItem.add(_4096kGeoItem)

    parent.add(geoItem)
    preferences.add(PREF_GEORAM, "Set the georam size (none,512,1024,2048,4096)", "none", Set("none","512","1024","2048","4096")) { geo =>
      if (geo == "512") {
        _512kGeoItem.setSelected(true)
        setGeoRAM(true, 512)
      }
      else if (geo == "1024") {
        _1024kGeoItem.setSelected(true)
        setGeoRAM(true, 1024)
      }
      else if (geo == "2048") {
        _2048kGeoItem.setSelected(true)
        setGeoRAM(true, 2048)
      }
      else if (geo == "4096") {
        _4096kGeoItem.setSelected(true)
        setGeoRAM(true, 4096)
      }
      else {
        noGeoItem.setSelected(true)
        setGeoRAM(false)
      }
    }

    // reset setting
    resetSettingsActions = (() => {
      noGeoItem.setSelected(true)
      setGeoRAM(false)
    }) :: resetSettingsActions
  }

  override protected def setGeoRAM(enabled:Boolean,size:Int = 0): Unit = {
    if (!enabled) mmu.detachSpecialCart()
    else {
      mmu.attachSpecialCart(new VIC20GeoRAM(size,signals))
    }
  }

  protected def setUltimemSettings(parent:JMenu): Unit = {
    import Preferences._
    val showUlti = new JMenuItem("Ultimem configuration ...")
    showUlti.addActionListener(_ => showUltimemConfig() )
    parent.add(showUlti)

    preferences.add(PREF_VIC20_ULTIMEM, "enables ultimem cartridge with the given rom", "") { filePath =>
      if (!filePath.isEmpty) setUltimem(filePath)
    }
  }

  protected def showUltimemConfig(): Unit = {
    VIC20Ultimem.showConfPanel(displayFrame,preferences,(enabled,romPath) => {
      clock.pause()
      if (enabled) setUltimem(romPath) else mmu.detachSpecialCart()
      reset(true)
    })
  }

  protected def setUltimem(romPath:String): Unit = {
    VIC20Ultimem.make(romPath,signals) match {
      case Right(ultimem) =>
        mmu.attachSpecialCart(ultimem)
      case Left(t) =>
        showError("Ultimem cartridge",s"Rom loading error: $t")
    }
  }

  protected def setFE3Settings(parent: JMenu): Unit = {
    import Preferences._
    val showFe3 = new JMenuItem("Final Expansion 3 configuration ...")
    showFe3.addActionListener(_ => showFE3Config())
    parent.add(showFe3)

    preferences.add(PREF_VIC20_FE3, "enables final expansion 3 cartridge with the given rom", "") { filePath =>
      if (!filePath.isEmpty) setFE3(filePath)
    }
  }

  protected def showFE3Config(): Unit = {
    VIC20FE3.showConfPanel(displayFrame, preferences, (enabled, romPath) => {
      clock.pause()
      if (enabled) setFE3(romPath) else mmu.detachSpecialCart()
      reset(true)
    })
  }

  protected def setFE3(romPath: String): Unit = {
    VIC20FE3.make(romPath, signals) match {
      case Right(fe3) =>
        mmu.attachSpecialCart(fe3)
      case Left(t) =>
        showError("Final Expansion 3 cartridge", s"Rom loading error: $t")
    }
  }

  protected def showVIC1112IEEE488Config(): Unit = {
    VIC201112IEEE488.showConfPanel(displayFrame, preferences, (enabled, romPath) => {
      clock.pause()
      if (enabled) setVIC1112IEEE488(romPath) else mmu.detachSpecialCart()
      reset(true)
    })
  }

  protected def setVIC1112IEEE488Settings(parent: JMenu): Unit = {
    import Preferences._
    val showUlti = new JMenuItem("IEEE488 (VIC 1112) configuration ...")
    showUlti.addActionListener(_ => showVIC1112IEEE488Config())
    parent.add(showUlti)

    preferences.add(PREF_IEEE488_ROM, "enables vic 1112 IEEE488 cartridge with the given rom", "") { filePath =>
      if (!filePath.isEmpty) setVIC1112IEEE488(filePath)
    }
  }

  protected def setVIC1112IEEE488(romPath: String): Unit = {
    VIC201112IEEE488.make(romPath, signals) match {
      case Right(ieee488) =>
        mmu.attachSpecialCart(ieee488)
      case Left(t) =>
        showError("VIC 1112 IEEE488", s"Rom loading error: $t")
    }
  }

  protected def setSettingsMenu(optionMenu: JMenu): Unit = {
    import Preferences._
    val ramConfigItem = new JMenuItem("RAM configuration ...")
    ramConfigItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.ALT_DOWN_MASK))
    ramConfigItem.addActionListener(_ => showRAMConfig() )
    optionMenu.add(ramConfigItem)
    preferences.add(PREF_VIC20_MEM_CONFIG, "memory configuration: comma separated list of enabled memory block. Memory blocks: 400,2000,4000,6000,A000", "") { config =>
      VIC20MMU.parseConfig(config,updateMemoryConfig(_,false) )
    }
    preferences.add(PREF_VIC20_8K_EXP, "8K RAM expansion", false,Set(),false) { _8K =>
      if (_8K) updateMemoryConfig(VIC20MMU.EXP_BLK1,false)
    }
    preferences.add(PREF_VIC20_16K_EXP, "16K RAM expansion", false, Set(), false) { _16K =>
      if (_16K) updateMemoryConfig(VIC20MMU.EXP_BLK1 | VIC20MMU.EXP_BLK2,false)
    }
    preferences.add(PREF_VIC20_24K_EXP, "24K RAM expansion", false, Set(), false) { _24K =>
      if (_24K) updateMemoryConfig(VIC20MMU.EXP_BLK1 | VIC20MMU.EXP_BLK2 | VIC20MMU.EXP_BLK3,false)
    }
    preferences.add(PREF_VIC20_32K_EXP, "32K RAM expansion", false, Set(), false) { _32K =>
      if (_32K) updateMemoryConfig(VIC20MMU.EXP_BLK1 | VIC20MMU.EXP_BLK2 | VIC20MMU.EXP_BLK3 | VIC20MMU.EXP_BLK5,false)
    }
    preferences.add(PREF_VIC20_IO2_ENABLED, "enables IO2 memory block as RAM", false) { enabled => mmu.setIO2RAM(enabled) }
    preferences.add(PREF_VIC20_IO3_ENABLED, "enables IO3 memory block as RAM", false) { enabled => mmu.setIO3RAM(enabled) }

    setDriveMenu(optionMenu)

    val keybEditorItem = new JMenuItem("Keyboard settings ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor())
    optionMenu.add(keybEditorItem)

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

    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio)
    optionMenu.add(adjustRatioItem)

    val vicItem = new JMenu("VIC")
    optionMenu.add(vicItem)
    setRenderingSettings(vicItem)
    val vicDisplayEffectsItem = new JMenuItem("VIC's display effects ...")
    vicItem.add(vicDisplayEffectsItem)
    vicDisplayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(displayFrame, display, "VIC").setVisible(true))

    setVICModel(vicItem)

    setFullScreenSettings(optionMenu)
    setOneFrameMode(vicItem, display, java.awt.event.KeyEvent.VK_N)
    // -----------------------------------

    optionMenu.addSeparator()

    setJoysticsSettings(optionMenu)

    setLightPenSettings(optionMenu,"")

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

    optionMenu.addSeparator()

    setDrivesSettings

    setRemotingSettings(optionMenu)

    optionMenu.addSeparator()

    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)

    optionMenu.addSeparator()

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232)
    IOItem.add(rs232Item)

    setGEORamSettings(IOItem)
    setUltimemSettings(IOItem)
    setFE3Settings(IOItem)
    setVIC1112IEEE488Settings(IOItem)
    setWiC64Settings(IOItem)

    // -----------------------------------

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
    preferences.add(PREF_SCREENDIM, "Zoom factor. Valued accepted are 1,2,3. 3 is for testbench only", 0, Set(1,2,3), false) { dim =>
      vicZoom(dim)
      zoomOverride = true
    }
    preferences.add(PREF_FULLSCREEN, "Starts the emulator in full screen mode", false, Set(), false) {
      fullScreenAtBoot = _
    }
    preferences.add(PREF_IGNORE_CONFIG_FILE, "Ignore configuration file and starts emulator with default configuration", false, Set(), false) {
      ignoreConfig = _
    }
    ROM.addReloadListener { res =>
      if (res == ROM.VIC20_KERNAL_PAL_ROM_PROP) mmu.setKernelPALROM(VIC20MMU.KERNAL_PAL_ROM.getROMBytes())
    }
    preferences.add(PREF_VIC20_KERNEL_PAL, "Set pal kernel rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.VIC20_KERNAL_PAL_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.VIC20_KERNAL_NTSC_ROM_PROP) mmu.setKernelNTSCROM(VIC20MMU.KERNAL_NTSC_ROM.getROMBytes())
    }
    preferences.add(PREF_VIC20_KERNEL_NTSC, "Set ntsc kernel rom path", "", Set.empty, false) { file =>
      if (file != "") reloadROM(ROM.VIC20_KERNAL_NTSC_ROM_PROP, file)
    }
    ROM.addReloadListener { res =>
      if (res == ROM.VIC20_BASIC_ROM_PROP) mmu.setBasicROM(VIC20MMU.BASIC_ROM.getROMBytes())
    }
    preferences.add(PREF_BASIC, "Set basic rom path", "", Set.empty, false) { file => if (file != "") reloadROM(ROM.VIC20_BASIC_ROM_PROP, file) }
    ROM.addReloadListener { res =>
      if (res == ROM.VIC20_CHAR_ROM_PROP) mmu.setCharROM(VIC20MMU.CHAR_ROM.getROMBytes())
    }
    preferences.add(PREF_CHARROM, "Set char rom path", "", Set.empty, false) { file => if (file != "") reloadROM(ROM.VIC20_CHAR_ROM_PROP, file) }
    preferences.add(PREF_1541DOS, "Set 1541 dos rom path", "", Set.empty, false) { file => if (file != "") reloadROM(ROM.D1541_DOS_ROM_PROP, file) }
    preferences.add(PREF_1571DOS, "Set 1571 dos rom path", "", Set.empty, false) { file => if (file != "") reloadROM(ROM.D1571_DOS_ROM_PROP, file) }
    preferences.add(PREF_1581DOS, "Set 1581 dos rom path", "", Set.empty, false) { file => if (file != "") reloadROM(ROM.D1581_DOS_ROM_PROP, file) }

    preferences.add(PREF_TRACE, "Starts the emulator in trace mode", false, Set(), false) { trace =>
      traceOption = trace
      tracer.enableTracing(trace)
    }

    preferences.add(PREF_WIC64_NETWORK, "Sets the network interface of WiC64", "") {
      wic64Panel.setNetwork(_)
    }
    preferences.add(PREF_WIC64_ENABLED, "Enables/disables WiC64 at startup", false) { enabled =>
      wic64Panel.setWiC64Enabled(enabled)
      if (!headless) wic64Panel.dialog.setVisible(true)
    }
    preferences.add(PREF_MOUSE_DELAY_MILLIS, "Sets the mouse delay parameter in millis", 20) { delay =>
      MouseCage.setRatioMillis(delay)
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

  override protected def setDefaultProperties(configuration: Properties): Unit = {
    import Preferences._
    configuration.setProperty(PREF_RENDERINGTYPE, "default")
    configuration.setProperty(PREF_WRITEONDISK, "true")
  }

  protected def getRAM: Memory = mmu

  protected def getCharROM: Memory = VIC20MMU.CHAR_ROM

  // state
  protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeObject(vicChip.getVICModel.VIC_TYPE.toString)

    // Special carts
    mmu.getAttachedSpecialCart() match {
      case Some(cart) =>
        out.writeBoolean(true)
        out.writeObject(cart.portType.toString)
        specialCartLoaderMap.get(cart.portType) match {
          case Some(saver) =>
            saver.save(cart,out)
          case None =>
            throw new IllegalArgumentException(s"Cannot save special cartridge of type: ${cart.portType}")
        }
      case None =>
        out.writeBoolean(false)
    }

    // Carts
    val carts = mmu.getAttachedCarts()
    out.writeInt(carts.length)
    for(cart <- carts) {
      cart.saveState(out)
    }
  }

  protected def loadState(in: ObjectInputStream): Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    val vicModel = VICType.withName(in.readObject.toString)
    setVICModel(vicModel, false, false, false)

    mmu.detachSpecialCart()
    mmu.detachAllCarts()

    // Special carts
    if (in.readBoolean()) {
      val portType = VIC20ExpansionPort.VICExpansionPortType.withName(in.readObject().toString)
      specialCartLoaderMap.get(portType) match {
        case Some(loader) =>
          val cart = loader.load(in,signals)
          cart.load(in)
          mmu.attachSpecialCart(cart)
        case None =>
          throw new IllegalArgumentException(s"Cannot load special cartridge of type: $portType")
      }
    }
    // Carts
    val cartSize = in.readInt()
    for(_ <- 1 to cartSize) {
      val (file,raw) = Cartridge.createCRTFileFromState(in)
      val cart = if (raw) loadRawCartridgeFile(file,true) else loadCartridgeFile(file,true)
      cart match {
        case Some(c) =>
          if (!mmu.attachCart(c)) println(s"Cannot load state from a cartridge")
        case None =>
          println(s"Cannot load state from a cartridge")
      }
    }
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
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    swing {
      initComponent
    }
    // VIC
    swing {
      displayFrame.pack()
    }
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
    // KEYBOARD LAYOUT
    swing {
      checkKeyboardLayout()
    }
  }

  override def shutdown(): Unit = {
    mmu.detachAllCarts()
    mmu.detachSpecialCart()
  }
}
