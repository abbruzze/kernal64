package ucesoft.cbm.c128

import java.awt._
import java.awt.event._
import java.io._
import javax.swing._
import ucesoft.cbm._
import ucesoft.cbm.cpu._
import ucesoft.cbm.expansion._
import ucesoft.cbm.formats._
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.peripheral.bus.{IECBus, IECBusLine, IECBusListener}
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.vdc.VDC
import ucesoft.cbm.peripheral.vic.VICType
import ucesoft.cbm.trace._

import java.util.Properties

object C128 extends App {
  CBMComputer.turnOn(new C128,args)
}

class C128 extends CBMComputer with MMUChangeListener {
  val componentID = "Commodore 128"
  val componentType = CBMComponentType.INTERNAL

  protected val APPLICATION_NAME = "Kernal128"
  protected val CONFIGURATION_FILENAME = "C128.config"
  private[this] val CONFIGURATION_VDC_FRAME_XY = "vdc.frame.xy"
  private[this] val CONFIGURATION_VDC_FRAME_DIM = "vdc.frame.dim"
  override protected def PRG_RUN_DELAY_CYCLES = if (isC64Mode) super.PRG_RUN_DELAY_CYCLES else 5400000


  protected var vdcFullScreenAtBoot = false // used with --vdc-full-screen

  protected val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_c128")
  private[this] var vdcEnabled = true // used with --vdc-disabled
  override protected val mmu = new C128MMU(this)
  private[this] val z80 = new Z80(mmu,mmu)
  override protected val irqSwitcher = new Switcher("IRQ",irqRequest _)
  private[this] var cpuFrequency = 1
  private[this] var c64Mode = false
  private[this] var z80Active = true
  private[this] var clockSpeed = 1
  private[this] val vdc = new ucesoft.cbm.peripheral.vdc.VDC
  private[this] var vdcDisplay : vic.Display = _
  private[this] var internalFunctionROMFileName,externalFunctionROMFileName : String = _
  private[this] val vdcDisplayFrame = {
    val f = new JFrame("Kernal128 " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = turnOff
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)    
    f
  }
  // ------------- MMU Status Panel ------------
  private[this] val mmuStatusPanel = new MMUStatusPanel
  // -------------------------------------------
  private[this] var FSDIRasInput = true

  private[this] var z80ScaleFactor = 2000000 / clock.getClockHz

  override protected def isC64Mode : Boolean = c64Mode

  def reset  : Unit = {
    dma = false
    z80Active = true
    clockSpeed = 1
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
    cia12Running(0) = true
    cia12Running(1) = true
  }

  def init  : Unit = {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo
    
    Log.info("Building the system ...")
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame,AVAILABLE_RS232)
    ExpansionPort.addConfigurationListener(mmu)       
    // drive
    initializedDrives(DriveType._1571)
    // -----------------------
    ProgramLoader.cpu = cpu
    ProgramLoader.warpModeListener = warpMode(_,true)
    //clock.setClockHz(1000000)
    mmu.setKeyboard(keyb)
    mmu.setCPU(cpu)
    add(clock)
    add(mmu)
    add(cpu)
    add(z80)
    add(keyb)
    add(controlPortA)
    add(controlPortB)
    add(bus)
    add(expansionPort)
    add(rs232)
    // -----------------------
    val vicMemory = mmu
    ExpansionPort.setMemoryForEmptyExpansionPort(mmu)
    ExpansionPort.addConfigurationListener(mmu)    
    import cia._
    // control ports
    val cia1CP1 = new CIA1Connectors.PortAConnector(keyb,controlPortA)
    val cia1CP2 = new CIA1Connectors.PortBConnector(keyb,controlPortB,() => { vicChip.triggerLightPen ; vdc.triggerLightPen })
    add(cia1CP1)
    add(cia1CP2)    
    
    add(irqSwitcher)    
    // CIAs
    // CIA1 must be modified to handle fast serial ...
    cia1 = new CIA("CIA1",
    				   0xDC00,
    				   cia1CP1,
    				   cia1CP2,
    				   irqSwitcher.setLine(Switcher.CIA,_),idle => cia12Running(0) = !idle) with IECBusListener {
      val busid = name
      
      bus.registerListener(this)
      
      override def srqTriggered = if (FSDIRasInput) cia1.serialIN(bus.data == IECBus.GROUND)
      
      setSerialOUT(bitOut => {
        if (!FSDIRasInput && !c64Mode) {
          bus.setLine(this,IECBusLine.DATA,if (bitOut) IECBus.GROUND else IECBus.VOLTAGE)
          bus.triggerSRQ(this)
        }
      })
    }
    add(nmiSwitcher)
    val cia2CP1 = new CIA2Connectors.PortAConnector(mmu,bus,rs232)
    val cia2CP2 = new CIA2Connectors.PortBConnector(rs232)    
    add(cia2CP1)
    add(cia2CP2)
    cia2 = new CIA("CIA2",
    				   0xDD00,
    				   cia2CP1,
    				   cia2CP2,
    				   nmiSwitcher.setLine(Switcher.CIA,_),idle => cia12Running(1) = !idle)
    rs232.setCIA12(cia1,cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory,mmu.colorRAM,irqSwitcher.setLine(Switcher.VIC,_),baLow _,true)
    // I/O set
    mmu.setIO(cia1,cia2,vicChip,sid,vdc)
    // VIC display
    display = new vic.Display(vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,displayFrame.getTitle,displayFrame)
    add(display)
    display.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH,vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(display)
    displayFrame.getContentPane.add("Center",display)
    displayFrame.addKeyListener(keyb)
    displayFrame.addKeyListener(keypadControlPort)
    displayFrame.addKeyListener(keyboardControlPort)
    display.addMouseListener(keypadControlPort)
    display.addMouseListener(controlport.ControlPort.emptyControlPort)
    // VDC display
    vdcDisplay = new vic.Display(ucesoft.cbm.peripheral.vdc.VDC.SCREEN_WIDTH,ucesoft.cbm.peripheral.vdc.VDC.SCREEN_HEIGHT,vdcDisplayFrame.getTitle,vdcDisplayFrame)
    add(vdcDisplay)
    vdcDisplay.setPreferredSize(ucesoft.cbm.peripheral.vdc.VDC.PREFERRED_FRAME_SIZE)
    vdc.setDisplay(vdcDisplay)
    
    vdcDisplayFrame.getContentPane.add("Center",vdcDisplay)
    vdcDisplayFrame.addKeyListener(keyb)
    vdcDisplayFrame.addKeyListener(keypadControlPort)
    vdcDisplayFrame.addKeyListener(keyboardControlPort)
    vdcDisplay.addMouseListener(keypadControlPort)
    vdcDisplay.addMouseListener(controlport.ControlPort.emptyControlPort)
    val resRootPanel = new JPanel(new BorderLayout())
    val resPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val resLabel = new JLabel("Initializing ...")
    val adaptResolutionCheck = new JCheckBox("AMSR")
    adaptResolutionCheck.setToolTipText("Adapt monitor's height to screen resolution")
    adaptResolutionCheck.setSelected(true)
    adaptResolutionCheck.setFocusable(false)
    adaptResolutionCheck.addActionListener( _ => vdc.setAdaptScreenResolution(adaptResolutionCheck.isSelected) )
    resPanel.add(resLabel)
    resRootPanel.add("West",resPanel)
    resRootPanel.add("East",adaptResolutionCheck)
    vdcDisplayFrame.getContentPane.add("South",resRootPanel)
    vdc.setGeometryUpdateListener { msg =>
      resLabel.setText(msg)
    }
    // VDC KEYSTROKES =======================================================    
    vdcDisplayFrame.addKeyListener(new KeyAdapter {
      private var mouseEnabled = false
      override def keyPressed(e:KeyEvent) : Unit = {
        e.getKeyCode match {
          // mouse
          case java.awt.event.KeyEvent.VK_P if e.isAltDown => 
            if (Clock.systemClock.isPaused) Clock.systemClock.play
            else {
              Clock.systemClock.pause
              display.setPaused
              vdcDisplay.setPaused
            }
          case java.awt.event.KeyEvent.VK_M if e.isAltDown =>
            mouseEnabled ^= true
            enableMouse(mouseEnabled,vdcDisplay)
          // reset
          case java.awt.event.KeyEvent.VK_R if e.isAltDown && e.isShiftDown =>
            hardReset(true)
          case java.awt.event.KeyEvent.VK_R if e.isAltDown =>
            reset(true)
          // warp-mode
          case java.awt.event.KeyEvent.VK_W if e.isAltDown =>
            warpMode(!clock.maximumSpeed)
          // adjust ratio
          case java.awt.event.KeyEvent.VK_D if e.isAltDown =>
            adjustRatio(false,true)
          // adjust ratio: normal size
          case java.awt.event.KeyEvent.VK_1 if e.isAltDown && e.isShiftDown =>
            adjustRatio(false,false)
          case java.awt.event.KeyEvent.VK_2 if e.isAltDown && e.isShiftDown =>
            adjustRatio(false,false,true)
          case java.awt.event.KeyEvent.VK_ENTER if e.isAltDown =>
            setVDCFullScreen
          case java.awt.event.KeyEvent.VK_N if e.isAltDown && e.isShiftDown =>
            vdcDisplay.advanceOneFrame
          case _ =>
        }
      }
    })
    // ======================================================================
    // light pen
    val lightPen = new LightPenButtonListener    
    add(lightPen)
    display.addMouseListener(lightPen)
    configureJoystick
    // tracing
    if (!headless) {
      traceDialog = TraceDialog.getTraceDialog("CPU Debugger",displayFrame,mmu,z80,display,vicChip)
      diskTraceDialog = TraceDialog.getTraceDialog("Disk8 Debugger",displayFrame,drives(0).getMem,drives(0))
      Log.setOutput(traceDialog.logPanel.writer)
    }
    else Log.setOutput(null)
    // tape
    datassette = new c2n.Datassette(cia1.setFlagLow _)
    mmu.setDatassette(datassette)
    add(datassette)
    // printer
    add(printer)
    // Flyer
    add(flyerIEC)

    mmuStatusPanel.setVisible(false)
    val statusPanel = makeInfoPanel(true)
    statusPanel.add("West",mmuStatusPanel)
    displayFrame.getContentPane.add("South",statusPanel)
    displayFrame.setTransferHandler(DNDHandler)    
    Log.info(sw.toString)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame,Array(display,vdcDisplay),Array("VIC","VDC"))
    // clock freq change listener
    clock.addChangeFrequencyListener(f => z80ScaleFactor = 2000000 / f)
  }
  
  override def afterInitHook  : Unit = {
	  super.afterInitHook
    // set the correct CPU configuration
    cpuChanged(false)
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
    // printer
    if (printerEnabled) printer.clock(cycles)
    // Flyer
    if (isFlyerEnabled) flyerIEC.clock(cycles)
    // CPU PHI2
    // check cart freezing button
    if (cartButtonRequested && cpu.isFetchingInstruction) {
      cartButtonRequested = false
      ExpansionPort.getExpansionPort.freezeButton
    }
    if (z80Active) z80.clock(cycles,z80ScaleFactor) // 2Mhz / 985248 = 2.0299
    else {
      ProgramLoader.checkLoadingInWarpMode(c64Mode)
      cpu.fetchAndExecute(1)
      if (cpuFrequency == 2 && !mmu.isIOACC && !vicChip.isRefreshCycle) cpu.fetchAndExecute(1)
    }
    // SID
    if (sidCycleExact) sid.clock
  }

  private def irqRequest(low:Boolean) : Unit = {
    cpu.irqRequest(low)
    z80.irq(low)
  }
  
  protected def setDMA(dma:Boolean) : Unit = {
    this.dma = dma
    mmu.setDMA(dma)
    if (z80Active) z80.requestBUS(dma) else cpu.setDMA(dma)    
  }
  
  private def baLow(low:Boolean) : Unit = {
    if (z80Active) z80.requestBUS(low) else cpu.setBaLow(low)
    expansionPort.setBaLow(low)
  }
  
  // MMU change listener
  def frequencyChanged(f:Int) : Unit = {
    Log.debug(s"Frequency set to $f Mhz")
    mmuStatusPanel.frequencyChanged(f)
    cpuFrequency = f
    val _2MhzMode = f == 2
    vicChip.set2MhzMode(_2MhzMode)
  }
  def cpuChanged(is8502:Boolean) : Unit = {
    if (is8502) {
      if (traceDialog != null) traceDialog.traceListener = cpu
      z80Active = false
      cpu.setDMA(false)
      z80.requestBUS(true)
    }
    else {
      if (traceDialog != null) traceDialog.traceListener = z80
      z80Active = true
      z80.requestBUS(false)
      cpu.setDMA(true)      
    }
    if (traceDialog != null) traceDialog.forceTracing(traceDialog.isTracing)
    mmuStatusPanel.cpuChanged(is8502)
    Log.debug("Enabling CPU " + (if (z80Active) "Z80" else "8502"))
  }
  def c64Mode(c64Mode:Boolean) : Unit = {
    mmuStatusPanel.c64Mode(c64Mode)
    this.c64Mode = c64Mode
  }  
  
  def fastSerialDirection(input:Boolean) : Unit = {
    FSDIRasInput = input
    if (input) bus.setLine(cia1.asInstanceOf[IECBusListener],IECBusLine.DATA,IECBus.VOLTAGE)
    //println(s"FSDIR set to input $input")
  }
  
  def _1571mode(_1571Mode:Boolean) : Unit = {
    mmuStatusPanel._1571mode(_1571Mode)
  }

  override def isHeadless = headless
  // ======================================== Settings ==============================================

  override protected def enableDrive(id:Int,enabled:Boolean,updateFrame:Boolean) : Unit = {
    super.enableDrive(id,enabled,updateFrame)
    if (updateFrame) adjustRatio()
  }

  private def enableVDC80(enabled:Boolean) : Unit = {
    keyb.set4080Pressed(enabled)
  }
  private def enableVDC(enabled:Boolean) : Unit = {
    if (enabled) vdc.play else vdc.pause
    vdcDisplayFrame.setVisible(enabled)
  }
  private def enableMMUPanel(enabled:Boolean) : Unit = {
    mmuStatusPanel.setVisible(enabled)
  }

  override protected def setDisplayRendering(hints:java.lang.Object) : Unit = {
    super.setDisplayRendering(hints)
    vdcDisplay.setRenderingHints(hints)
  }

  private def checkFunctionROMS: Unit = {
    val extFunRom = configuration.getProperty(ROM.C128_EXTERNAL_ROM_PROP)
    if (extFunRom != null && extFunRom != "") loadFunctionROM(false,Some(extFunRom))
    else mmu.configureFunctionROM(false,null,FunctionROMType.NORMAL)
    val intFunRom = configuration.getProperty(ROM.C128_INTERNAL_ROM_PROP)
    if (intFunRom != null && intFunRom != "") {
      val (name,rt) = intFunRom.split(",") match {
        case Array(n,t) => (n,FunctionROMType.withName(t))
        case _ => (intFunRom,FunctionROMType.NORMAL)
      }
      loadFunctionROM(true,Some(name),rt)
    }
    else mmu.configureFunctionROM(true,null,FunctionROMType.NORMAL)
  }
  // ================================================================================================
  
  private def loadFunctionROM(internal:Boolean,fileName:Option[String] = None,romType:FunctionROMType.Value = FunctionROMType.NORMAL) : Unit = {
    val fn = fileName match {
      case None =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
        fc.setDialogTitle("Choose a ROM to load")
        fc.showOpenDialog(displayFrame) match {
          case JFileChooser.APPROVE_OPTION =>
            fc.getSelectedFile.toString
          case _ => 
            return
        }
      case Some(filename) => filename
    }
    
    try {
      val file = new File(fn)
      romType match {
        case FunctionROMType.NORMAL if file.length > 32768 => throw new IllegalArgumentException("ROM's size must be less than 32K")
        case _ =>
      }      
      val rom = Array.ofDim[Byte](file.length.toInt)
      val f = new DataInputStream(new FileInputStream(fn))
      f.readFully(rom)
      f.close
      mmu.configureFunctionROM(internal,rom,romType)
      if (!fileName.isDefined) JOptionPane.showMessageDialog(displayFrame,"ROM loaded. Reset to turn it on", "ROM loaded successfully",JOptionPane.INFORMATION_MESSAGE)
      internal match {
        case true => internalFunctionROMFileName = fn
        case false => externalFunctionROMFileName = fn
      }
    }
    catch {
      case t:Throwable =>
        showError("ROM loading error","Can't load ROM. Unexpected error occurred: " + t)
        t.printStackTrace
    }
  }

  private def adjustRatio(vic:Boolean=true,vdcResize:Boolean=false,vdcHalfSize:Boolean = false) : Unit = {
    if (vic) {
      val dim = display.asInstanceOf[java.awt.Component].getSize
      dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
      display.setPreferredSize(dim) 
      displayFrame.pack
    }
    else {
      if (!vdcResize) {
        val dim = if (vdcHalfSize) new Dimension((VDC.PREFERRED_FRAME_SIZE.width / 1.5).toInt,(VDC.PREFERRED_FRAME_SIZE.height / 1.5).toInt) else VDC.PREFERRED_FRAME_SIZE
        vdcDisplay.setPreferredSize(dim)
        vdcDisplayFrame.pack
      }
      else {
        val dim = vdcDisplay.asInstanceOf[java.awt.Component].getSize
        val vdcNormalSize = VDC.PREFERRED_FRAME_SIZE
        val aspectRatio = vdcNormalSize.height.toDouble / vdcNormalSize.width
        
        dim.height = (dim.width * aspectRatio).round.toInt
        vdcDisplay.setPreferredSize(dim) 
        vdcDisplayFrame.pack
      }
    }
  } 

  protected def loadPRGFile(file:File,autorun:Boolean) : Unit = {
    val (start,end) = ProgramLoader.loadPRG(mmu.getBank0RAM,file,c64Mode,8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,c64Mode)
    }
  }

  private def updateVDCScreenDimension(dim:Dimension): Unit = {
    vdcDisplay.setPreferredSize(dim)
    vdcDisplay.invalidate
    vdcDisplay.repaint()
    vdcDisplayFrame.pack
  }

  private def takeSnapshot(vic:Boolean) : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
      	vic match {
          case true => display.saveSnapshot(file)
          case false => vdcDisplay.saveSnapshot(file)
        }
      case _ =>
    }
  }

  override def setSettingsMenu(optionMenu:JMenu) : Unit = {
    import Preferences._
    setDriveMenu(optionMenu)

    optionMenu.addSeparator
    
    val vdcMenu = new JMenu("VDC")
    optionMenu.add(vdcMenu)
    val _80enabledAtStartupItem = new JCheckBoxMenuItem("80 columns enabled at startup")
    _80enabledAtStartupItem.setSelected(false)
    _80enabledAtStartupItem.addActionListener(_ => preferences(PREF_VDC80STARTUP) = _80enabledAtStartupItem.isSelected )
    vdcMenu.add(_80enabledAtStartupItem)
    // VDC-80-STARTUP =====================================================================================
    preferences.add(PREF_VDC80STARTUP,"Enable VDC 80 columns at startup",false) { vdc80 =>
      enableVDC80(vdc80)
      _80enabledAtStartupItem.setSelected(vdc80)
    }
    // ====================================================================================================

    // VDC-DISABLED =======================================================================================
    val vdcEnabled = new JCheckBoxMenuItem("VDC enabled")
    vdcEnabled.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E,java.awt.event.InputEvent.ALT_DOWN_MASK))
    vdcEnabled.setSelected(true)
    vdcEnabled.addActionListener(_ => preferences(PREF_VDCDISABLED) = !vdcEnabled.isSelected )
    vdcMenu.add(vdcEnabled)
    preferences.add(PREF_VDCDISABLED,"Disable VDC monitor",false) { vdcE =>
      this.vdcEnabled &= !vdcE
      vdcEnabled.setSelected(!vdcE)
      enableVDC(!vdcE)
    }
    // ====================================================================================================

    val vdcSeparateThreadItem = new JCheckBoxMenuItem("VDC on its own thread")
    vdcSeparateThreadItem.setSelected(false)
    vdcSeparateThreadItem.addActionListener(e => if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) vdc.setOwnThread else vdc.stopOwnThread )
    vdcMenu.add(vdcSeparateThreadItem)

    // MMU-PANEL-ENABLED ==================================================================================
    val statusPanelItem = new JCheckBoxMenuItem("MMU status panel enabled")
    statusPanelItem.setSelected(false)
    statusPanelItem.addActionListener(_ => preferences(PREF_MMUPANELENABLED) = statusPanelItem.isSelected )
    optionMenu.add(statusPanelItem)
    preferences.add(PREF_MMUPANELENABLED,"Enable mmu panel",false) { mmuE =>
      enableMMUPanel(mmuE)
      statusPanelItem.setSelected(mmuE)
    }
    // ====================================================================================================
    
    optionMenu.addSeparator
    
    val keybMenu = new JMenu("Keyboard")
    optionMenu.add(keybMenu)
    
    val enableKeypadItem = new JCheckBoxMenuItem("Keypad enabled")
    enableKeypadItem.setSelected(true)
    enableKeypadItem.addActionListener(e => keyb.enableKeypad(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    keybMenu.add(enableKeypadItem)
    
    val keybEditorItem = new JMenuItem("Keyboard editor ...")
    keybEditorItem.addActionListener(_ => showKeyboardEditor(c64Mode) )
    keybMenu.add(keybEditorItem)
    val loadKeybItem = new JMenuItem("Set keyboard layout ...")
    loadKeybItem.addActionListener(_ => loadKeyboard )
    keybMenu.add(loadKeybItem)
    
    optionMenu.addSeparator

    setVolumeSettings(optionMenu)
    
    optionMenu.addSeparator

    setWarpModeSettings(optionMenu)
    
    optionMenu.addSeparator
    
    val adjustMenu = new JMenu("Display")
    optionMenu.add(adjustMenu)
    val vicAdjMenu = new JMenu("VIC")
    val vdcAdjMenu = new JMenu("VDC")

    adjustMenu.add(vicAdjMenu)
    adjustMenu.add(vdcAdjMenu)

    val adjustRatioItem = new JMenuItem("Adjust VIC display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio(true) )
    vicAdjMenu.add(adjustRatioItem)
    
    val adjustVDCRatioItem = new JMenuItem("Adjust VDC display ratio")
    adjustVDCRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustVDCRatioItem.addActionListener(_ => adjustRatio(false,true) )
    vdcAdjMenu.add(adjustVDCRatioItem)
    
    val vdcResetSizeItem = new JMenuItem("VDC normal size")
    vdcResetSizeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    vdcResetSizeItem.addActionListener(_ => adjustRatio(false) )
    vdcAdjMenu.add(vdcResetSizeItem)

    val vdcHalfSizeItem = new JMenuItem("VDC smaller size")
    vdcHalfSizeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    vdcHalfSizeItem.addActionListener(_ => adjustRatio(false,false,true) )
    vdcAdjMenu.add(vdcHalfSizeItem)
    
    val zoomItem = new JMenu("VIC Zoom")
    val groupZ = new ButtonGroup
    vicAdjMenu.add(zoomItem)
    setVICModel(vicAdjMenu)
    for(z <- 1 to 2) {
      val zoom1Item = new JRadioButtonMenuItem(s"Zoom x $z")
      zoom1Item.addActionListener(_ => vicZoom(z) )
      val kea = z match {
        case 1 => java.awt.event.KeyEvent.VK_1
        case 2 => java.awt.event.KeyEvent.VK_2
      }
      zoom1Item.setAccelerator(KeyStroke.getKeyStroke(kea,java.awt.event.InputEvent.ALT_DOWN_MASK))
      zoomItem.add(zoom1Item)
      groupZ.add(zoom1Item)
    }

    setVICBorderMode(vicAdjMenu)

    setFullScreenSettings(adjustMenu)
    
    val renderingItem = new JMenu("Rendering")
    adjustMenu.add(renderingItem)
    setRenderingSettings(renderingItem)

    val vicDisplayEffectsItem = new JMenuItem("VIC's display effects ...")
    val vdcDisplayEffectsItem = new JMenuItem("VDC's display effects ...")
    vicAdjMenu.add(vicDisplayEffectsItem)
    vdcAdjMenu.add(vdcDisplayEffectsItem)
    vicDisplayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(displayFrame,display,"VIC").setVisible(true))
    vdcDisplayEffectsItem.addActionListener(_ => DisplayEffectPanel.createDisplayEffectPanel(vdcDisplayFrame,vdcDisplay,"VDC").setVisible(true))

    val deinterlaceModeItem = new JCheckBox("VDC deinterlace mode enabled")
    vdcAdjMenu.add(deinterlaceModeItem)
    deinterlaceModeItem.setSelected(true)
    deinterlaceModeItem.addActionListener(_ => vdc.setDeinterlaceMode(deinterlaceModeItem.isSelected) )

    setOneFrameMode(vicAdjMenu,display,java.awt.event.KeyEvent.VK_N)
    setOneFrameMode(vdcAdjMenu,vdcDisplay,java.awt.event.KeyEvent.VK_N,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK)
    // -----------------------------------
    
    optionMenu.addSeparator

    setJoysticsSettings(optionMenu)

    setLightPenSettings(optionMenu)

    setMouseSettings(optionMenu)
    
    optionMenu.addSeparator
    
    val snapMenu = new JMenu("Take a snapshot")
    optionMenu.add(snapMenu)
    
    val vicSnapshotItem = new JMenuItem("VIC ...")
    vicSnapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,java.awt.event.InputEvent.ALT_DOWN_MASK))
    vicSnapshotItem.addActionListener(_ => takeSnapshot(true) )
    snapMenu.add(vicSnapshotItem)
    val vdcSnapshotItem = new JMenuItem("VDC ...")
    vdcSnapshotItem.addActionListener(_ => takeSnapshot(false) )
    snapMenu.add(vdcSnapshotItem)

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

    setDrivesSettings
    
    optionMenu.addSeparator

    setRemotingSettings(optionMenu)
    
    optionMenu.addSeparator
    
    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)
    
    optionMenu.addSeparator

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232 )
    IOItem.add(rs232Item)
    
    IOItem.addSeparator

    setFlyerSettings(IOItem)

    setREUSettings(IOItem)

    setGEORamSettings(IOItem)

    // -----------------------------------
    
    IOItem.addSeparator

    setDigiMAXSettings(IOItem)
    
    IOItem.addSeparator

    setGMOD3FlashSettings(IOItem)

    setEasyFlashSettings(IOItem)
    
    IOItem.addSeparator

    setCPMSettings(IOItem)
    
    val ramItem = new JMenu("RAM")
    optionMenu.add(ramItem)
    val _256RamEnabledItem = new JCheckBoxMenuItem("256K")
    _256RamEnabledItem.setSelected(false)
    ramItem.add(_256RamEnabledItem)
    _256RamEnabledItem.addActionListener(e => mmu.RAM.setExpansionBanks(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener( _ => {
      clock.pause
      ROMPanel.showROMPanel(displayFrame,configuration,false,false,() => {
        saveSettings(false)
        checkFunctionROMS
        reset()
      })
      clock.play
    } )
  }

  private def setVDCFullScreen : Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(vdcDisplayFrame,
      vdcDisplay,
      VDC.SCREEN_WIDTH,
      VDC.SCREEN_WIDTH,
      keypadControlPort,
      keyb,
      keypadControlPort,
      keyboardControlPort)
  }

  override protected def setGlobalCommandLineOptions : Unit = {
    import Preferences._
    super.setGlobalCommandLineOptions

    // VDCSCREENSHOT ======================================================================================
    preferences.add(PREF_VDCSCREENSHOT,"Take a screenshot of VDC screen and save it on the given file path. Used with --testcart only.","") { file =>
      TestCart.screenshotFile = Some(file)
      TestCart.screeshotHandler = vdcDisplay.waitFrameSaveSnapshot _
    }
    // EXT-ROM ============================================================================================
    preferences.add(PREF_128EXTROM,"External function ROM path","") { extRom =>
      configuration.setProperty(ROM.C128_EXTERNAL_ROM_PROP, extRom)
      checkFunctionROMS
    }
    // INT-ROM ============================================================================================
    preferences.add(PREF_128INTROM,"Internal function ROM (<rom path>,NORMAL|MEGABIT)","") { intRom =>
      configuration.setProperty(ROM.C128_INTERNAL_ROM_PROP, intRom)
      checkFunctionROMS
    }
    // GO-64 ==============================================================================================
    preferences.add(PREF_128GO64,"Run in 64 mode",false) { go64 => if (go64) mmu.go64 }
    // VDC-FULLSCREEN =====================================================================================
    preferences.add(PREF_VDCFULLSCREEN,"Starts the emulator with VDC in full screen mode",false) { vdcFullScreenAtBoot = _ }
    // KERNEL128 ==========================================================================================
    preferences.add(PREF_KERNEL128,"Set kernel 128 rom path","",Set.empty,false) { kp =>
      if (kp != "") reloadROM(ROM.C128_KERNAL_ROM_PROP,kp)
    }
    // CHARROM128 =========================================================================================
    preferences.add(PREF_CHARROM128,"Set char rom 128 path","",Set.empty,false) { cp =>
      if (cp != "") reloadROM(ROM.C128_CHAR_ROM_PROP,cp)
    }
  }
  
  protected def saveSettings(save:Boolean) : Unit = {
    if (!ignoreConfig) {
      configuration.setProperty(CONFIGURATION_FRAME_XY, displayFrame.getX + "," + displayFrame.getY)
      if (!zoomOverride) {
        val dimension = display.getSize()
        configuration.setProperty(CONFIGURATION_FRAME_DIM, dimension.width + "," + dimension.height)
      }

      configuration.setProperty(CONFIGURATION_VDC_FRAME_XY, vdcDisplayFrame.getX + "," + vdcDisplayFrame.getY)
      val vdcDimension = vdcDisplay.getSize()
      configuration.setProperty(CONFIGURATION_VDC_FRAME_DIM, vdcDimension.width + "," + vdcDimension.height)
      if (save) {
        preferences.save(configuration)
        println("Settings saved")
      }
      saveConfigurationFile
    }
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeBoolean(z80Active)
    out.writeInt(clockSpeed)
    out.writeBoolean(FSDIRasInput)
    out.writeBoolean(c64Mode)
    out.writeInt(cpuFrequency)
    out.writeObject(vicChip.getVICModel.VIC_TYPE.toString)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    z80Active = in.readBoolean
    clockSpeed = in.readInt
    FSDIRasInput = in.readBoolean
    c64Mode = in.readBoolean
    cpuFrequency = in.readInt
    val vicModel = VICType.withName(in.readObject.toString)
    setVICModel(vicModel,false,false,false)
  }
  protected def allowsStateRestoring : Boolean = true
  // -----------------------------------------------------------------------------------------
  protected def getRAM = mmu.RAM
  protected def getCharROM = mmu.CHAR_ROM

  override protected def setDefaultProperties(configuration:Properties) : Unit = {
    import Preferences._
    super.setDefaultProperties(configuration)
    configuration.setProperty(PREF_RENDERINGTYPE,"bilinear")
  }

  def turnOn(args:Array[String]) : Unit = {
    swing { setMenu }
    // check help
    if (preferences.checkForHelp(args)) {
      println(s"Kernal64, Commodore 128 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      preferences.printUsage("file to attach")
      sys.exit(0)
    }
    // --headless handling to disable logging & debugging
    if (args.exists(_ == "--headless")) headless = true
    swing{ initComponent }
    checkFunctionROMS
    // --ignore-config-file handling
    if (args.exists(_ == "--ignore-config-file")) configuration.clear()
    // screen's dimension and size restoration
    // VDC
    swing { vdcDisplayFrame.pack }    
    if (configuration.getProperty(CONFIGURATION_VDC_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_VDC_FRAME_DIM) split "," map { _.toInt }
      swing { updateVDCScreenDimension(new Dimension(dim(0),dim(1))) }
    }
    if (configuration.getProperty(CONFIGURATION_VDC_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_VDC_FRAME_XY) split "," map { _.toInt }
      swing { vdcDisplayFrame.setLocation(xy(0),xy(1)) }
    }
    else vdcDisplayFrame.setLocationByPlatform(true)
    // VIC
    swing { displayFrame.pack }
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
      vdcDisplayFrame.setVisible(!headless && vdcEnabled)
      displayFrame.setVisible(!headless)
    }
    // FULL SCREEN
    swing {
      if (fullScreenAtBoot) setVicFullScreen
      else
      if (vdcFullScreenAtBoot) setVDCFullScreen
    }
    // PLAY    
    vdc.play
    clock.play
  }
}