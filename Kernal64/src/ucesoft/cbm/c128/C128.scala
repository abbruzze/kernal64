package ucesoft.cbm.c128

import java.awt._
import java.awt.datatransfer.DataFlavor
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
import ucesoft.cbm.peripheral.vic.Palette
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.trace._

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
  override protected val PRG_RUN_DELAY_CYCLES = 6500000

  protected var vdcFullScreenAtBoot = false // used with --vdc-full-screen

  protected val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_c128")
  private[this] var vdcEnabled = true // used with --vdc-disabled
  protected val mmu = new C128MMU(this)
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
    initDrive(0,DriveType._1571)
    initDrive(1,DriveType._1571)
    drivesEnabled(1) = false
    // -----------------------
    ProgramLoader.cpu = cpu
    ProgramLoader.warpModeListener = warpMode _
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
    floppyComponents(0) = new FloppyComponent(8,drives(0),driveLeds(0))
    add(floppyComponents(0))
    floppyComponents(1) = new FloppyComponent(9,drives(1),driveLeds(1))
    add(floppyComponents(1))
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
          case java.awt.event.KeyEvent.VK_R if e.isAltDown =>
            reset(true)
          // warp-mode
          case java.awt.event.KeyEvent.VK_W if e.isAltDown =>
            clock.maximumSpeed = !clock.maximumSpeed
            sid.setFullSpeed(clock.maximumSpeed)
            maxSpeedItem.setSelected(clock.maximumSpeed)
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
    // tracing
    traceDialog = TraceDialog.getTraceDialog("CPU Debugger",displayFrame,mmu,z80,display,vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog("Disk8 Debugger",displayFrame,drives(0).getMem,drives(0))
    //diskTraceDialog.forceTracing(true)
    // drive leds
    add(driveLeds(0))        
    add(driveLeds(1))       
    configureJoystick
    Log.setOutput(traceDialog.logPanel.writer)
    // tape
    datassette = new c2n.Datassette(cia1.setFlagLow _)
    mmu.setDatassette(datassette)
    add(datassette)
    // printer
    add(printer)
    // Flyer
    add(flyerIEC)
    
    // info panel
    val infoPanel = new JPanel(new BorderLayout)
    val rowPanel = new JPanel(new BorderLayout(0,0))
    val row1Panel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    val row2Panel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    rowPanel.add("North",row1Panel)
    rowPanel.add("South",row2Panel)
    val tapePanel = new TapeState(datassette)
    datassette.setTapeListener(tapePanel)
    row1Panel.add(tapePanel)
    row1Panel.add(tapePanel.progressBar)
    row1Panel.add(diskProgressPanels(0))
    row1Panel.add(driveLeds(0))
    row2Panel.add(diskProgressPanels(1))
    row2Panel.add(driveLeds(1))
    infoPanel.add("East",rowPanel)
    infoPanel.add("West",mmuStatusPanel)
    mmuStatusPanel.setVisible(false)
    displayFrame.getContentPane.add("South",infoPanel)
    displayFrame.setTransferHandler(DNDHandler)    
    Log.info(sw.toString)

    // GIF Recorder
    gifRecorder = GIFPanel.createGIFPanel(displayFrame,Array(display,vdcDisplay),Array("VIC","VDC"))
    // clock freq change listener
    clock.addChangeFrequencyListener(f => z80ScaleFactor = 2000000 / f)
  }

  private def loadSettings(args:Array[String]) : Unit = {
    settings.load(configuration)
    // AUTOPLAY
    settings.parseAndLoad(args) match {
      case None =>
        // run the given file name
        settings.get[String]("RUNFILE") match {
          case None =>
          case Some(fn) =>
            val cmd = s"""RUN"$fn"""" + 13.toChar
            clock.schedule(new ClockEvent("Loading",clock.currentCycles + 2200000,(cycles) => Keyboard.insertTextIntoKeyboardBuffer(cmd,mmu,false) ))
        }
      case Some(f) =>
        handleDND(new File(f),false,true)
    }
    DrivesConfigPanel.registerDrives(displayFrame,drives,setDriveType(_,_,false),enableDrive(_,_,true),attachDisk(_,_,c64Mode),attachDiskFile(_,_,_,None),drivesEnabled)
  }
  
  override def afterInitHook  : Unit = {
	  inspectDialog = InspectPanel.getInspectDialog(displayFrame,this)    
    // deactivate drive 9
    drives(1).setActive(false)    
    driveLeds(1).setVisible(false)
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
    while (d < 2) {
      if (drivesEnabled(d) && drivesRunning(d)) drives(d).clock(cycles)

      d += 1
    }
    if (device10DriveEnabled) device10Drive.clock(cycles)
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
      traceDialog.traceListener = cpu      
      z80Active = false
      cpu.setDMA(false)
      z80.requestBUS(true)
    }
    else {
      traceDialog.traceListener = z80
      z80Active = true
      z80.requestBUS(false)
      cpu.setDMA(true)      
    }
    traceDialog.forceTracing(traceDialog.isTracing)
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
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    if (updateFrame) adjustRatio()
  }

  private def enableVDC80(enabled:Boolean) : Unit = {
    keyb.set4080Pressed(enabled)
  }
  private def enabledVDC(enabled:Boolean) : Unit = {
    if (enabled) vdc.play else vdc.pause
    vdcDisplayFrame.setVisible(enabled)
  }
  private def enableMMUPanel(enabled:Boolean) : Unit = {
    mmuStatusPanel.setVisible(enabled)
  }

  protected def setDisplayRendering(hints:java.lang.Object) : Unit = {
    display.setRenderingHints(hints)
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

  protected def attachDiskFile(driveID:Int,file:File,autorun:Boolean,fileToLoad:Option[String],emulateInserting:Boolean = true) : Unit = {
    try {
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
      if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      val isG64 = file.getName.toUpperCase.endsWith(".G64")
      val disk = Diskette(file.toString)
      disk.canWriteOnDisk = canWriteOnDisk
      disk.flushListener = diskFlusher
      drives(driveID).getFloppy.close
      if (!traceDialog.isTracing) clock.pause
      drives(driveID).setDriveReader(disk,emulateInserting)
      clock.play

      loadFileItems(driveID).setEnabled(!isG64)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""LOAD"$fn",8,1""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          Keyboard.insertTextIntoKeyboardBuffer(cmd,mmu,c64Mode)
        case None if autorun =>
          Keyboard.insertSmallTextIntoKeyboardBuffer("LOAD\"*\",8,1" + 13.toChar + "RUN" + 13.toChar,mmu,c64Mode)
        case _ =>
      }
      driveLeds(driveID).setToolTipText(disk.toString)
    }
    catch {
      case t:Throwable =>
        t.printStackTrace
        showError("Disk attaching error",t.toString)
    }
  }

  private def updateVDCScreenDimension(dim:Dimension): Unit = {
    vdcDisplay.setPreferredSize(dim)
    vdcDisplay.invalidate
    vdcDisplay.repaint()
    vdcDisplayFrame.pack
  }
  
  protected def savePrg : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
        val (start,end) = ProgramLoader.savePRG(fc.getSelectedFile,mmu,c64Mode)
        Log.info(s"BASIC program saved from $start to $end")
      case _ =>
    }
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

  protected def paste : Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      Keyboard.insertTextIntoKeyboardBuffer(str,mmu,c64Mode)
    }
  }

  override def setSettingsMenu(optionMenu:JMenu) : Unit = {
    setDriveMenu(optionMenu)

    optionMenu.addSeparator
    
    val vdcMenu = new JMenu("VDC")
    optionMenu.add(vdcMenu)
    val _80enabledAtStartupItem = new JCheckBoxMenuItem("80 columns enabled at startup")
    _80enabledAtStartupItem.setSelected(false)
    _80enabledAtStartupItem.addActionListener(e => enableVDC80(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    vdcMenu.add(_80enabledAtStartupItem)
    // Setting ---------------------------
    settings.add("vdc-80-startup",
                 "Enable VDC 80 columns at startup",
                 "80COLATSTARTUP",
                 (vdc80:Boolean) => {
                   enableVDC80(vdc80)
                   _80enabledAtStartupItem.setSelected(vdc80)
                 },
                 _80enabledAtStartupItem.isSelected
    )
    // -----------------------------------
    val vdcEnabled = new JCheckBoxMenuItem("VDC enabled")
    vdcEnabled.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E,java.awt.event.InputEvent.ALT_DOWN_MASK))
    vdcEnabled.setSelected(true)
    vdcEnabled.addActionListener(e => enabledVDC(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    vdcMenu.add(vdcEnabled)
    // Setting ---------------------------
    settings.add("vdc-disabled",
                 "Disable VDC monitor",
                 "VDCDISABLED",
                 (vdcE:Boolean) => {
                   this.vdcEnabled &= !vdcE
                   vdcEnabled.setSelected(!vdcE)
                 },
                 !vdcEnabled.isSelected
    )
    // -----------------------------------
    val vdcSeparateThreadItem = new JCheckBoxMenuItem("VDC on its own thread")
    vdcSeparateThreadItem.setSelected(false)
    vdcSeparateThreadItem.addActionListener(e => if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) vdc.setOwnThread else vdc.stopOwnThread )
    vdcMenu.add(vdcSeparateThreadItem)
    
    val statusPanelItem = new JCheckBoxMenuItem("MMU status panel enabled")
    statusPanelItem.setSelected(false)
    statusPanelItem.addActionListener(e => enableMMUPanel(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    optionMenu.add(statusPanelItem)
    // Setting ---------------------------
    settings.add("mmupanel-enabled",
                 "Enable mmu panel",
                 "MMUSTATUS",
                 (mmuE:Boolean) => { 
                   enableMMUPanel(mmuE)
                   statusPanelItem.setSelected(mmuE)
                 },
                 statusPanelItem.isSelected
    )
    // -----------------------------------
    
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
    
    optionMenu.addSeparator

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
    
    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2 )
    IOItem.add(gmod2Item)
    
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
    romItem.addActionListener( _ => ROMPanel.showROMPanel(displayFrame,configuration,false,false,() => {
      saveSettings(false)
      checkFunctionROMS
    }) )
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
    super.setGlobalCommandLineOptions

    settings.add("vdcscreenshot",
      "Take a screenshot of VDC screen and save it on the given file path. Used with --testcart only.",
      (file:String) => if (file != "") {
        TestCart.screenshotFile = Some(file)
        TestCart.screeshotHandler = vdcDisplay.saveSnapshot _
      }
    )
    settings.add("ext-rom",
      "External function ROM path",
      (extRom:String) => {
        if (extRom != null && extRom != "") {
          configuration.setProperty(ROM.C128_EXTERNAL_ROM_PROP, extRom)
          checkFunctionROMS
        }
      }
    )
    settings.add("int-rom",
      "Internal function ROM (<rom path>,NORMAL|MEGABIT)",
      (intRom:String) => {
        if (intRom != null && intRom != "") {
          configuration.setProperty(ROM.C128_INTERNAL_ROM_PROP, intRom)
          checkFunctionROMS
        }
      }
    )
    settings.add("go64",
      "Run in 64 mode",
      (go64:Boolean) => if (go64) mmu.go64
    )
    settings.add("vdc-fullscreen",
      "Starts the emulator with VDC in full screen mode",
      (fs:Boolean) => if (fs) vdcFullScreenAtBoot = true
    )
  }
  
  def turnOff  : Unit = {
    if (!headless) saveSettings(configuration.getProperty(CONFIGURATION_AUTOSAVE,"false").toBoolean)
    for(d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }
  
  protected def saveSettings(save:Boolean) : Unit = {
    configuration.setProperty(CONFIGURATION_FRAME_XY,displayFrame.getX + "," + displayFrame.getY)
    if (!zoomOverride) {
      val dimension = display.getSize()
      configuration.setProperty(CONFIGURATION_FRAME_DIM,dimension.width + "," + dimension.height)
    }

    configuration.setProperty(CONFIGURATION_VDC_FRAME_XY,vdcDisplayFrame.getX + "," + vdcDisplayFrame.getY)
    val vdcDimension = vdcDisplay.getSize()
    configuration.setProperty(CONFIGURATION_VDC_FRAME_DIM,vdcDimension.width + "," + vdcDimension.height)
    if (save) {
      settings.save(configuration)
      println("Settings saved")
    }
    try {
      val propsFile = new File(new File(scala.util.Properties.userHome),CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "C128 configuration file")
      out.close
    }
    catch {
      case _:IOException =>
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
  }
  protected def allowsStateRestoring : Boolean = true
  // -----------------------------------------------------------------------------------------
  protected def getRAM = mmu.RAM
  protected def getCharROM = mmu.CHAR_ROM

  def turnOn(args:Array[String]) : Unit = {
    swing { setMenu }
    // check help
    if (settings.checkForHelp(args)) {
      println(s"Kernal64, Commodore 128 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      settings.printUsage("file to attach")
      sys.exit(0)
    }
    swing{ initComponent }
    checkFunctionROMS
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
    // VIC
    swing { displayFrame.pack }
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing { updateVICScreenDimension(new Dimension(dim(0),dim(1))) }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      swing { displayFrame.setLocation(xy(0),xy(1)) }
    }     
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