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
  override protected val PRG_RUN_DELAY_CYCLES = 6000000

  protected val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_c128",C128KeyboardMapper)
  private[this] var vdcEnabled = true // used with --vdc-disabled
  protected val mmu = new C128MMU(this)
  private[this] val z80 = new Z80(mmu,mmu)
  override protected val irqSwitcher = new IRQSwitcher(irqRequest _)
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
  private[this] var baLow = false
  private[this] var FSDIRasInput = true

  def reset  : Unit = {
    baLow = false
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
    				   irqSwitcher.ciaIRQ _,idle => cia12Running(0) = !idle) with IECBusListener {
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
    				   nmiSwitcher.cia2NMIAction _,idle => cia12Running(1) = !idle)
    rs232.setCIA12(cia1,cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory,mmu.colorRAM,irqSwitcher.vicIRQ _,baLow _)
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
    resPanel.add(resLabel)
    resRootPanel.add("West",resPanel)
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
            ucesoft.cbm.misc.FullScreenMode.goFullScreen(vdcDisplayFrame,
                                                         vdcDisplay,
                                                         VDC.SCREEN_WIDTH,
                                                         VDC.SCREEN_WIDTH,
                                                         keypadControlPort,
                                                         keyb,
                                                         keypadControlPort,
                                                         keyboardControlPort)
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
    traceDialog = TraceDialog.getTraceDialog(displayFrame,mmu,z80,display,vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog(displayFrame,drives(0).getMem,drives(0))    
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
    val tapePanel = new TapeState
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
    DrivesConfigPanel.registerDrives(displayFrame,drives,setDriveType(_,_,false),enableDrive _,attachDisk(_,_,c64Mode),attachDiskFile(_,_,_,None),drivesEnabled)
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
    if (z80Active) z80.clock(cycles,2.0299) // 2Mhz / 985248
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
    baLow = low
    if (z80Active) z80.requestBUS(baLow) else cpu.setBaLow(low)
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

  protected def enableDrive(id:Int,enabled:Boolean) : Unit = {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    adjustRatio(true)
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

  private def setDisplayRendering(hints:java.lang.Object) : Unit = {
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
  
  private def zoom(f:Int) : Unit = {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f,vicChip.VISIBLE_SCREEN_HEIGHT * f)
    updateScreenDimension(dim)
  }

  private def updateScreenDimension(dim:Dimension): Unit = {
    display.setPreferredSize(dim)
    display.invalidate
    display.repaint()
    displayFrame.pack
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
    val driveMenu = new JMenuItem("Drives ...")
    driveMenu.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    optionMenu.add(driveMenu)
    driveMenu.addActionListener(_ => DrivesConfigPanel.getDriveConfigDialog.setVisible(true) )
    
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
    
    val volumeItem = new JMenuItem("Volume settings ...")
    volumeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.event.InputEvent.ALT_DOWN_MASK))
    volumeItem.addActionListener(_ => volumeDialog.setVisible(true) )
    optionMenu.add(volumeItem)
    
    optionMenu.addSeparator
        
    maxSpeedItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W,java.awt.event.InputEvent.ALT_DOWN_MASK))
    maxSpeedItem.setSelected(clock.maximumSpeed)
    maxSpeedItem.addActionListener(e => warpMode(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    optionMenu.add(maxSpeedItem)
    
    optionMenu.addSeparator
    
    val adjustMenu = new JMenu("Adjust display")
    optionMenu.add(adjustMenu)
    val adjustRatioItem = new JMenuItem("Adjust VIC display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio(true) )
    adjustMenu.add(adjustRatioItem)
    
    val adjustVDCRatioItem = new JMenuItem("Adjust VDC display ratio")
    adjustVDCRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustVDCRatioItem.addActionListener(_ => adjustRatio(false,true) )
    adjustMenu.add(adjustVDCRatioItem)
    
    val vdcResetSizeItem = new JMenuItem("VDC normal size")
    vdcResetSizeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    vdcResetSizeItem.addActionListener(_ => adjustRatio(false) )
    adjustMenu.add(vdcResetSizeItem)

    val vdcHalfSizeItem = new JMenuItem("VDC smaller size")
    vdcHalfSizeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    vdcHalfSizeItem.addActionListener(_ => adjustRatio(false,false,true) )
    adjustMenu.add(vdcHalfSizeItem)
    
    val zoomItem = new JMenu("VIC Zoom")
    val groupZ = new ButtonGroup
    adjustMenu.add(zoomItem)
    for(z <- 1 to 2) {
      val zoom1Item = new JRadioButtonMenuItem(s"Zoom x $z")
      zoom1Item.addActionListener(_ => zoom(z) )
      val kea = z match {
        case 1 => java.awt.event.KeyEvent.VK_1
        case 2 => java.awt.event.KeyEvent.VK_2
      }
      zoom1Item.setAccelerator(KeyStroke.getKeyStroke(kea,java.awt.event.InputEvent.ALT_DOWN_MASK))
      zoomItem.add(zoom1Item)
      groupZ.add(zoom1Item)
    }

    val fullScreenItem = new JMenuItem("VIC full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setVicFullScreen )
    adjustMenu.add(fullScreenItem)
    
    val renderingItem = new JMenu("Rendering")
    val groupR = new ButtonGroup
    adjustMenu.add(renderingItem)
    val renderingDefault1Item = new JRadioButtonMenuItem("Default")
    renderingDefault1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR) )
    renderingItem.add(renderingDefault1Item)
    groupR.add(renderingDefault1Item)
    val renderingBilinear1Item = new JRadioButtonMenuItem("Bilinear")
    renderingBilinear1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR) )
    renderingItem.add(renderingBilinear1Item)
    groupR.add(renderingBilinear1Item)
    val renderingBicubic1Item = new JRadioButtonMenuItem("Bicubic")
    renderingBicubic1Item.addActionListener(_ => setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BICUBIC) )
    renderingItem.add(renderingBicubic1Item)
    groupR.add(renderingBicubic1Item)
    // Setting ---------------------------
    settings.add("rendering-type",
                 "Set the rendering type (default,bilinear,bicubic)",
                 "RENDERING_TYPE",
     (dt:String) => {
       dt match {
         case "bilinear"|"" =>
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
     else
     if (renderingBilinear1Item.isSelected) "bilinear"
     else
     if (renderingBicubic1Item.isSelected) "bicubic"
     else "default"
    )

    val paletteItem = new JMenu("Palette")
    adjustMenu.add(paletteItem)
    val groupP = new ButtonGroup
    val vicePalItem = new JRadioButtonMenuItem("VICE")
    vicePalItem.addActionListener(_ => Palette.setPalette(PaletteType.VICE) )
    paletteItem.add(vicePalItem)
    groupP.add(vicePalItem)
    val brightPalItem = new JRadioButtonMenuItem("Bright")
    brightPalItem.addActionListener(_ => Palette.setPalette(PaletteType.BRIGHT) )
    paletteItem.add(brightPalItem)
    groupP.add(brightPalItem)
    val peptoPalItem = new JRadioButtonMenuItem("Pepto")
    peptoPalItem.addActionListener(_ => Palette.setPalette(PaletteType.PEPTO) )
    paletteItem.add(peptoPalItem)
    groupP.add(peptoPalItem)
    // Setting ---------------------------
    settings.add("vic-palette",
      "Set the palette type (bright,vice,pepto)",
      "PALETTE",
      (dt:String) => {
        dt match {
          case "bright"|"" =>
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
      else
      if (vicePalItem.isSelected) "vice"
      else
      if (peptoPalItem.isSelected) "pepto"
      else "bright"
    )

    val deinterlaceModeItem = new JCheckBox("VDC deinterlace mode enabled")
    adjustMenu.add(deinterlaceModeItem)
    deinterlaceModeItem.setSelected(true)
    deinterlaceModeItem.addActionListener(_ => vdc.setDeinterlaceMode(deinterlaceModeItem.isSelected) )
    // -----------------------------------
    
    optionMenu.addSeparator
        
    val joyAItem = new JMenuItem("Joystick...")
    joyAItem.addActionListener(_ => joySettings )
    optionMenu.add(joyAItem)
    
    val swapJoyAItem = new JMenuItem("Swap joysticks")
    swapJoyAItem.addActionListener(_ => swapJoysticks )
    optionMenu.add(swapJoyAItem)
    
    val lightPenMenu = new JMenu("Light pen")
    optionMenu.add(lightPenMenu)
    val group3 = new ButtonGroup
    val noPenItem = new JRadioButtonMenuItem("No light pen")
    noPenItem.setSelected(true)
    noPenItem.addActionListener(_ => setLightPen(LIGHT_PEN_NO_BUTTON) )
    group3.add(noPenItem)
    lightPenMenu.add(noPenItem)
    val penUp = new JRadioButtonMenuItem("Light pen with button up")
    penUp.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_UP) )
    group3.add(penUp)
    lightPenMenu.add(penUp)
    val penLeft = new JRadioButtonMenuItem("Light pen with button left")
    penLeft.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_LEFT) )
    group3.add(penLeft)
    lightPenMenu.add(penLeft)
    
    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled on port 1")
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M,java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.setSelected(false)
    mouseEnabledItem.addActionListener(e => enableMouse(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected,display) )
    optionMenu.add(mouseEnabledItem)
    
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
    
    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        display.setPaused
        vdcDisplay.setPaused
      } else clock.play )
    optionMenu.add(pauseItem)
    
    optionMenu.addSeparator
    
    val printerPreviewItem = new JMenuItem("Printer preview ...")    
    printerPreviewItem.addActionListener(_ => showPrinterPreview )
    optionMenu.add(printerPreviewItem)
    
    val printerEnabledItem = new JCheckBoxMenuItem("Printer enabled")
    printerEnabledItem.setSelected(false)
    printerEnabledItem.addActionListener(e => { printerEnabled = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected ; printer.setActive(printerEnabled) } )
    optionMenu.add(printerEnabledItem)
    // Setting ---------------------------
    settings.add("printer-enabled",
                 "Enable printer",
                 "PRINTER_ENABLED",
                 (pe:Boolean) => {
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
    sid6581Item.addActionListener(_ => sid.setModel(true) )
    sidTypeItem.add(sid6581Item)
    group7.add(sid6581Item)
    val sid8580Item = new JRadioButtonMenuItem("MOS 8580")
    sid8580Item.setSelected(false)
    sid8580Item.addActionListener(_ => sid.setModel(false) )
    sidTypeItem.add(sid8580Item)
    group7.add(sid8580Item)
    // Setting ---------------------------
    settings.add("sid-8580",
                 "Enable sid 8580 type",
                 "SID_8580",
                 (sid8580:Boolean) => {
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
    nosid2Item.addActionListener(_ => setDualSID(None) )
    group8.add(nosid2Item)
    for(adr <- DualSID.validAddresses(false)) {
      val sid2AdrItem = new JRadioButtonMenuItem(adr)
      sid2Item.add(sid2AdrItem)
      sid2AdrItem.setSelected(false)
      sid2AdrItem.addActionListener(_ => setDualSID(Some(Integer.parseInt(adr,16))) )
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
      (sce:Boolean) => {
        clock.pause
        sidCycleExact = sce
        sid.setCycleExact(sidCycleExact)
        sidCycleExactItem.setSelected(sidCycleExact)
        clock.play
      },
      sidCycleExact
    )
    
    optionMenu.addSeparator

    for(drive <- 0 to 1) {
      // Setting ---------------------------
      settings.add(s"drive${drive + 8}-type",
        "Set the driver's type (1541,1571,1581)",
        s"DRIVER${drive + 8}_TYPE",
        (dt: String) => {
          dt match {
            case "1541" =>
              setDriveType(drive,DriveType._1541, true)
            case "1571" =>
              setDriveType(drive,DriveType._1571, true)
            case "1581" =>
              setDriveType(drive,DriveType._1581, true)
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
    
    optionMenu.addSeparator
    
    val remoteItem = new JMenu("Remoting")
    optionMenu.add(remoteItem)
    
    val group10 = new ButtonGroup
    val remoteDisabledItem = new JRadioButtonMenuItem("Off")
    remoteDisabledItem.setSelected(true)
    remoteDisabledItem.addActionListener(_ => setRemote(None) )
    group10.add(remoteDisabledItem)
    remoteItem.add(remoteDisabledItem)
    val remoteEnabledItem = new JRadioButtonMenuItem("On ...")
    remoteEnabledItem.addActionListener(e => setRemote(Some(e.getSource.asInstanceOf[JRadioButtonMenuItem])) )
    group10.add(remoteEnabledItem)
    remoteItem.add(remoteEnabledItem)
    
    optionMenu.addSeparator
    
    val IOItem = new JMenu("I/O")
    optionMenu.add(IOItem)
    
    optionMenu.addSeparator

    val rs232Item = new JMenuItem("RS-232 ...")
    rs232Item.addActionListener(_ => manageRS232 )
    IOItem.add(rs232Item)
    
    IOItem.addSeparator
    
    val flyerItem = new JMenu("Flyer internet modem")
    IOItem.add(flyerItem)
    
    val fylerEnabledItem = new JCheckBoxMenuItem("Flyer enabled on 7")
    fylerEnabledItem.setSelected(false)
    flyerItem.add(fylerEnabledItem)
    fylerEnabledItem.addActionListener(e => enableFlyer(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    val flyerDirectoryItem = new JMenuItem("Set disks repository ...")
    flyerItem.add(flyerDirectoryItem)
    flyerDirectoryItem.addActionListener(_ => chooseFlyerDir )
    
    val reuItem = new JMenu("REU")
    val group5 = new ButtonGroup
    val noReuItem = new JRadioButtonMenuItem("None")
    noReuItem.setSelected(true)
    noReuItem.addActionListener(_ => setREU(None,None) )
    group5.add(noReuItem)
    reuItem.add(noReuItem)
    val reu128Item = new JRadioButtonMenuItem("128K")
    reu128Item.addActionListener(_ => setREU(Some(REU.REU_1700),None) )
    group5.add(reu128Item)
    reuItem.add(reu128Item)
    val reu256Item = new JRadioButtonMenuItem("256K")
    reu256Item.addActionListener(_ => setREU(Some(REU.REU_1764),None) )
    group5.add(reu256Item)
    reuItem.add(reu256Item)
    val reu512Item = new JRadioButtonMenuItem("512K")
    reu512Item.addActionListener(_ => setREU(Some(REU.REU_1750),None) )
    group5.add(reu512Item)
    reuItem.add(reu512Item)
    val reu16MItem = new JRadioButtonMenuItem("16M ...")
    reu16MItem.addActionListener(_ => choose16MREU )
    group5.add(reu16MItem)
    reuItem.add(reu16MItem)    
    IOItem.add(reuItem)

    val geoItem = new JMenu("GeoRAM")
    val groupgeo = new ButtonGroup
    val noGeoItem = new JRadioButtonMenuItem("None")
    noGeoItem.setSelected(true)
    noGeoItem.addActionListener(_ => setGeoRAM(false) )
    groupgeo.add(noGeoItem)
    geoItem.add(noGeoItem)
    val _256kGeoItem = new JRadioButtonMenuItem("256K")
    _256kGeoItem.addActionListener(_ => setGeoRAM(true,256) )
    groupgeo.add(_256kGeoItem)
    geoItem.add(_256kGeoItem)
    val _512kGeoItem = new JRadioButtonMenuItem("512K")
    _512kGeoItem.addActionListener(_ => setGeoRAM(true,512) )
    groupgeo.add(_512kGeoItem)
    geoItem.add(_512kGeoItem)

    IOItem.add(geoItem)
    // Setting ---------------------------
    settings.add("geo-ram",
      "Set the georam size (none,256,512)",
      "GEO_RAM",
      (geo:String) => {
        if (geo == "512") {
          _512kGeoItem.setSelected(true)
          setGeoRAM(true,512)
        }
        else
        if (geo == "256") {
          _256kGeoItem.setSelected(true)
          setGeoRAM(true,256)
        }
      },
      if (_512kGeoItem.isSelected) "512"
      else
      if (_256kGeoItem.isSelected) "256"
      else "none"
    )
    settings.add("reu-type",
                 "Set the reu type (none,128,256,512,16384). In case of 16384: 16384,<filename>",
                 "REU_TYPE",
     (reu:String) => {
       val reuPars = reu.split(",")
       if (reuPars(0) == "" || reuPars(0) == "none") setREU(None,None)
       else
       reuPars(0).toInt match {
         case REU.REU_1700 =>
           setREU(Some(REU.REU_1700),None)
           reu128Item.setSelected(true)
         case REU.REU_1750 =>
           setREU(Some(REU.REU_1750),None)
           reu512Item.setSelected(true)
         case REU.REU_1764 =>
           setREU(Some(REU.REU_1764),None)
           reu256Item.setSelected(true)
         case REU.REU_16M =>
           setREU(Some(REU.REU_16M),if (reuPars.length == 2 && reuPars(1) != "null") Some(reuPars(1)) else None)
           reu16MItem.setSelected(true)
       }
     },
     if (noReuItem.isSelected) "none"
     else
     if (reu128Item.isSelected) "128"
     else
     if (reu256Item.isSelected) "256"
     else
     if (reu512Item.isSelected) "512"
     else
     if (reu16MItem.isSelected) "16384," + REU.attached16MFileName
     else "none"
    )
    // -----------------------------------
    
    IOItem.addSeparator
    
    val digimaxItem = new JMenu("DigiMAX")
    IOItem.add(digimaxItem)
    val digiMaxSampleRateItem  = new JMenuItem("DigiMax sample rate ...")
    digiMaxSampleRateItem.addActionListener(_ => chooseDigiMaxSampleRate )
    digimaxItem.add(digiMaxSampleRateItem)
    val group6 = new ButtonGroup
    val digimaxDisabledItem = new JRadioButtonMenuItem("Disabled")
    digimaxDisabledItem.setSelected(true)
    digimaxDisabledItem.addActionListener(_ => setDigiMax(false,None) )
    digimaxItem.add(digimaxDisabledItem)
    group6.add(digimaxDisabledItem)
    val digimaxOnUserPortItem = new JRadioButtonMenuItem("On UserPort")
    digimaxOnUserPortItem.addActionListener(_ => setDigiMax(true,None) )
    group6.add(digimaxOnUserPortItem)
    digimaxItem.add(digimaxOnUserPortItem)
    val digimaxDE00Item = new JRadioButtonMenuItem("On DE00")
    digimaxDE00Item.addActionListener(_ => setDigiMax(true,Some(0xDE00)) )
    group6.add(digimaxDE00Item)
    digimaxItem.add(digimaxDE00Item)
    val digimaxDF00Item = new JRadioButtonMenuItem("On DF00")
    digimaxDF00Item.addActionListener(_ => setDigiMax(true,Some(0xDF00)) )
    group6.add(digimaxDF00Item)
    digimaxItem.add(digimaxDF00Item)
    
    IOItem.addSeparator
    
    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2 )
    IOItem.add(gmod2Item)
    
    IOItem.addSeparator
    
    val cpmItem = new JCheckBoxMenuItem("CP/M Cartdrige")
    cpmItem.addActionListener(e => enableCPMCart(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    IOItem.add(cpmItem)
    settings.add("cpm64-enabled",
      s"Attach the CP/M cart",
      "CPM64",
      (cpm: Boolean) => {
        if (cpm) enableCPMCart(true)
      },
      ExpansionPort.getExpansionPort.isInstanceOf[ucesoft.cbm.expansion.cpm.CPMCartridge]
    )
    
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

  override protected def setGlobalCommandLineOptions : Unit = {
    super.setGlobalCommandLineOptions

    settings.add("screen-dim",
      "Zoom factor. Valued accepted are 1 and 2",
      (f:Int) => if (f == 1 || f == 2) {
        zoom(f)
        zoomOverride = true
      }
    )
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
      case io:IOException =>
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
      settings.printUsage
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
      swing { updateScreenDimension(new Dimension(dim(0),dim(1))) }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      swing { displayFrame.setLocation(xy(0),xy(1)) }
    }     
    // SETTINGS
    loadSettings(args)
    // VIEW
    vdcDisplayFrame.setVisible(!headless && vdcEnabled)
    swing { displayFrame.setVisible(!headless) }
    // PLAY    
    vdc.play
    clock.play
  }
}