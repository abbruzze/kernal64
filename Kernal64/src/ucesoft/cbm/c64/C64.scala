package ucesoft.cbm.c64

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm._
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.expansion._
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.bus.BusSnoop
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard
import ucesoft.cbm.peripheral.vic.renderer.DriveRenderer
import ucesoft.cbm.peripheral.vic.{VICType, VIC_II}
import ucesoft.cbm.trace.Tracer
import ucesoft.cbm.trace.Tracer.TracedDisplay

import java.awt._
import java.io._
import javax.swing._

object C64 extends App {
  CBMComputer.turnOn(new C64,args)
}

class C64 extends CBMHomeComputer {
  override protected val cbmModel: CBMComputerModel = C64Model

  val componentID = "Commodore 64"
  val componentType: Type = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "Kernal64"
  protected val CONFIGURATION_FILENAME = "C64.config"
  protected val DEFAULT_KEYBOARD_RESOURCE_NAME = "/resources/default_keyboard_c64"

  protected val mmu = new C64MMU.MAIN_MEMORY
  protected val busSnooper = new BusSnoop(bus)
  protected var busSnooperActive = false
  private[this] val vicMemory = new C64VICMemory(mmu,mmu.CHAR_ROM,cpu)

  def reset  : Unit = {
    dma = false
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
    cia12Running(0) = true
    cia12Running(1) = true
  }
  
  def init  : Unit = {
    Log.setInfo
    
    Log.info("Building the system ...")
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame,AVAILABLE_RS232)
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
    add(vicMemory)
    ExpansionPort.setMemoryForEmptyExpansionPort(vicMemory)
    ExpansionPort.addConfigurationListener(vicMemory)    
    import cia._
    // control ports
    val cia1CP1 = new CIA1Connectors.PortAConnector(keyb,controlPortA)
    val cia1CP2 = new CIA1Connectors.PortBConnector(keyb,controlPortB,() => vicChip.triggerLightPen)
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
    val cia2CP1 = new CIA2Connectors.PortAConnector(vicMemory,bus,rs232)
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
    vicChip = new vic.VIC_II(vicMemory,mmu.COLOR_RAM,irqSwitcher.setLine(Switcher.VIC,_),baLow _)
    mmu.setLastByteReadMemory(vicMemory)
    // mapping I/O chips in memory
    mmu.setIO(cia1,cia2,sid,vicChip)
    display = new vic.Display(vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,displayFrame.getTitle,displayFrame)
    add(display)
    display.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH,vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(display)
    displayFrame.getContentPane.add("Center",display)
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
    datassette = new Datassette(cia1.setFlagLow _)
    mmu.setDatassette(datassette)
    add(datassette)
    // printer
    add(printer)
    // Flyer
    add(flyerIEC)
    
    displayFrame.getContentPane.add("South",makeInfoPanel(true))
    displayFrame.setTransferHandler(DNDHandler)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame,Array(display),Array("VIC"))

    // trace
    tracer.addDevice(Tracer.TracedDevice("Main 6510 CPU", mmu, cpu, true))
    tracer.setDisplay(new TracedDisplay {
      override def getRasterLineAndCycle(): (Int, Int) = (vicChip.getRasterLine,vicChip.getRasterCycle)
      override def setDisplayRasterLine(line: Int): Unit = display.setRasterLineAt(line)
      override def enableDisplayRasterLine(enabled: Boolean): Unit = display.setDrawRasterLine(enabled)
    })
  }
  
  protected def mainLoop(cycles:Long) : Unit = {
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
    cpu.fetchAndExecute(1)
    // SID
    if (sidCycleExact) sid.clock
  }

  protected def setDMA(dma:Boolean) : Unit = {
    this.dma = dma
    cpu.setDMA(dma)
  }
  
  private def baLow(low:Boolean) : Unit = {
    cpu.setBaLow(low)
    expansionPort.setBaLow(low)
  }

  override def isHeadless: Boolean = headless

  // ======================================== Settings ==============================================
  override protected def enableDrive(id:Int,enabled:Boolean,updateFrame:Boolean) : Unit = {
    super.enableDrive(id,enabled,updateFrame)
    if (updateFrame) adjustRatio
  }
  
  private def adjustRatio()  : Unit = {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim) 
    displayFrame.pack()
  } 

  protected def loadPRGFile(file:File,autorun:Boolean) : Unit = {
    val (start,end) = ProgramLoader.loadPRG(mmu,file,true,8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      HomeKeyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,true)
    }
  }

  private def takeSnapshot()  : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
      	display.saveSnapshot(file)
      case _ =>
    }
  }
  
  protected def setSettingsMenu(optionMenu:JMenu) : Unit = {
    setDriveMenu(optionMenu)

    optionMenu.addSeparator()

    val keybEditorItem = new JMenuItem("Keyboard settings ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor() )
    optionMenu.add(keybEditorItem)
    
    optionMenu.addSeparator()

    setVolumeSettings(optionMenu)
    
    optionMenu.addSeparator()

    setWarpModeSettings(optionMenu)
    
    optionMenu.addSeparator()
    
    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio )
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
      zoom1Item.setAccelerator(KeyStroke.getKeyStroke(kea,java.awt.event.InputEvent.ALT_DOWN_MASK))
      zoomItem.add(zoom1Item)
      groupZ.add(zoom1Item)
    }

    val vicItem = new JMenu("VIC")
    optionMenu.add(vicItem)
    setRenderingSettings(vicItem)
    val vicDisplayEffectsItem = new JMenuItem("VIC's display effects ...")
    vicItem.add(vicDisplayEffectsItem)
    vicDisplayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(displayFrame,display,"VIC").setVisible(true))

    setVICModel(vicItem)

    setVICBorderMode(vicItem)

    setFullScreenSettings(optionMenu)
    setOneFrameMode(vicItem,display,java.awt.event.KeyEvent.VK_N)
    // -----------------------------------
    
    optionMenu.addSeparator()

    setJoysticsSettings(optionMenu)

    setLightPenSettings(optionMenu)

    setMouseSettings(optionMenu)
    
    optionMenu.addSeparator()

    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot )
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
    busSnooperActiveItem.addActionListener(e => busSnooperActive = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected )
    optionMenu.add(busSnooperActiveItem)
    // reset setting
    resetSettingsActions = (() => {
      busSnooperActiveItem.setSelected(false)
      busSnooperActive = false
    }) :: resetSettingsActions
    
    optionMenu.addSeparator()

    setRemotingSettings(optionMenu)
    
    optionMenu.addSeparator()
    
    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)
    
    optionMenu.addSeparator()

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232 )
    IOItem.add(rs232Item)
    
    IOItem.addSeparator()

    setFlyerSettings(IOItem)

    setWiC64Settings(IOItem)

    setREUSettings(IOItem)

    setGEORamSettings(IOItem)

    setBeamRacerSettings(IOItem)

    // -----------------------------------
    
    IOItem.addSeparator()

    setDigiMAXSettings(IOItem)
    
    IOItem.addSeparator()

    setGMOD3FlashSettings(IOItem)

    setEasyFlashSettings(IOItem)
    
    IOItem.addSeparator()

    setCPMSettings(IOItem)

    IOItem.addSeparator()

    val ren = new JCheckBoxMenuItem("FreeSpin cable enabled")
    IOItem.add(ren)
    ren.addActionListener(_ => {
      clock.pause()
      val rend = if (ren.isSelected) new DriveRenderer(bus, display, volumeDialog) else null
      vicChip.asInstanceOf[VIC_II].setExternalRenderer(rend)
      clock.play()
    })

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener( _ => {
      clock.pause
      ROMPanel.showROMPanel(displayFrame,configuration,cbmModel,false,() => {
        saveSettings(false)
        reset(false)
      })
      clock.play
    })
  }

  override protected def setGlobalCommandLineOptions : Unit = {
    import Preferences._
    super.setGlobalCommandLineOptions
    // CUSTOM-GLUE-LOGIC ==================================================================================
    preferences.add(PREF_CUSTOMGLUELOGIC,"Set internal glue logic to custom (C64C)",false) { vicMemory.setCustomGlueLogic(_) }
    preferences.add(PREF_VICIINEW,"Set VICII new model",false) { vicChip.asInstanceOf[vic.VIC_II].setNEWVICModel(_) }
  }
  
  protected def saveSettings(save:Boolean) : Unit = {
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
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeObject(vicChip.getVICModel.VIC_TYPE.toString)

    // VIC Coprocessor
    vicChip.getCoprocessor match {
      case None =>
        out.writeBoolean(false)
      case Some(cop) =>
        out.writeBoolean(true)
        out.writeObject(cop.componentID)
    }
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    val vicModel = VICType.withName(in.readObject.toString)
    setVICModel(vicModel,false,false,false)

    // VIC Coprocessor
    if (in.readBoolean) {
      in.readObject.toString match { // copy factory
        case "VASYL" =>
          preferences(Preferences.PREF_BEAMRACERENABLED) = true
        case cn =>
          throw new IllegalArgumentException(s"Coprocessor $cn unknown")
      }
    } else {

    }
  }
  protected def allowsStateRestoring : Boolean = true
  // -----------------------------------------------------------------------------------------
  
  def turnOn(args:Array[String]) : Unit = {
    swing { setMenu }
    // check help
    if (preferences.checkForHelp(args)) {
      println(s"Kernal64, Commodore 64 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
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
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing { updateVICScreenDimension(new Dimension(dim(0),dim(1))) }
    }
    else vicZoom(2)
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      swing { displayFrame.setLocation(xy(0),xy(1)) }
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