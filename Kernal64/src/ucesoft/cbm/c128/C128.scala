package ucesoft.cbm.c128

import javax.swing._
import java.awt.event._

import ucesoft.cbm.game.GamePlayer
import ucesoft.cbm.trace._
import ucesoft.cbm._
import ucesoft.cbm.cpu._
import ucesoft.cbm.peripheral._
import ucesoft.cbm.misc._
import ucesoft.cbm.expansion._
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.rs232._
import java.awt._
import java.util.Properties
import java.io._

import ucesoft.cbm.formats._
import java.awt.datatransfer.DataFlavor
import java.util

import scala.util.Success
import scala.util.Failure
import java.util.ServiceLoader

import ucesoft.cbm.game.GameUI
import ucesoft.cbm.peripheral.controlport.JoystickSettingDialog
import ucesoft.cbm.peripheral.bus.IECBusLine
import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.remote.RemoteC64
import ucesoft.cbm.peripheral.bus.IECBusListener
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.vdc.VDC
import ucesoft.cbm.peripheral.vic.Palette
import ucesoft.cbm.peripheral.vic.Palette.PaletteType

object C128 extends App {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  val c128 = new C128
  c128.run(args)  
}

class C128 extends CBMComponent with GamePlayer with MMUChangeListener {
  val componentID = "Commodore 128"
  val componentType = CBMComponentType.INTERNAL
  
  private[this] val settings = new ucesoft.cbm.misc.Settings
  private[this] val CONFIGURATION_FILENAME = "C128.config"
  private[this] val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  private[this] val CONFIGURATION_FRAME_XY = "frame.xy"  
  private[this] val CONFIGURATION_FRAME_DIM = "frame.dim"
  private[this] val CONFIGURATION_VDC_FRAME_XY = "vdc.frame.xy"  
  private[this] val CONFIGURATION_VDC_FRAME_DIM = "vdc.frame.dim"
  private[this] val CONFIGURATION_KEYB_MAP_FILE = "keyb.map.file"
  private[this] val CONFIGURATION_GMOD2_FILE = "gmod2.file"
  private[this] val CONFIGURATION_AUTOSAVE = "autosave"
  
  private[this] val configuration = {
    val props = new Properties
    val propsFile = new File(new File(scala.util.Properties.userHome),CONFIGURATION_FILENAME)
    if (propsFile.exists) {
      try {
        props.load(new FileReader(propsFile))
      }
      catch {
        case io:IOException =>
      }
    }
    ROM.props = props
    props
  }
  private[this] var cartButtonRequested = false
  private[this] var headless = false // used with --headless command line option
  private[this] var cpujamContinue = false // used with --cpujam-continue
  private[this] var zoomOverride = false // used with --screen-dim
  private[this] var sidCycleExact = false // used with --sid-cycle-exact
  private[this] val clock = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)
  private[this] val mmu = new C128MMU(this)
  private[this] val cpu = CPU6510.make(mmu)  
  private[this] val z80 = new Z80(mmu,mmu)
  private[this] var cpuFrequency = 1
  private[this] var c64Mode = false
  private[this] var z80Active = true
  private[this] var clockSpeed = 1
  private[this] var vicChip : vic.VIC = _
  private[this] var cia1,cia2 : cia.CIA = _
  private[this] val vdc = new ucesoft.cbm.peripheral.vdc.VDC
  private[this] val sid = new ucesoft.cbm.peripheral.sid.SID
  private[this] var vicDisplay : vic.Display = _
  private[this] var vdcDisplay : vic.Display = _
  private[this] var internalFunctionROMFileName,externalFunctionROMFileName : String = _
  private[this] val nmiSwitcher = new NMISwitcher(cpu.nmiRequest _)
  private[this] val irqSwitcher = new IRQSwitcher(irqRequest _)
  private[this] val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_c128",C128KeyboardMapper)
  private[this] val keyb = new keyboard.Keyboard(keybMapper,nmiSwitcher.keyboardNMIAction _,true)	// key listener
  private[this] val vicDisplayFrame = {
    val f = new JFrame("Kernal128 " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) {
        close
      }
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)    
    f
  }
  private[this] val vdcDisplayFrame = {
    val f = new JFrame("Kernal128 " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) {
        close
      }
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)    
    f
  }
  // ----------------- REMOTE ------------------
  private[this] var remote : Option[RemoteC64] = None
  // ------------- MMU Status Panel ------------
  private[this] val mmuStatusPanel = new MMUStatusPanel
  // -------------- MENU ITEMS -----------------
  private[this] val maxSpeedItem = new JCheckBoxMenuItem("Warp mode")
  private[this] val loadFileItems = Array(new JMenuItem("Load file from attached disk 8 ..."), new JMenuItem("Load file from attached disk 9 ..."))
  private[this] val tapeMenu = new JMenu("Tape control...")
  private[this] val detachCtrItem = new JMenuItem("Detach cartridge")
  private[this] val cartMenu = new JMenu("Cartridge")
  // -------------------------------------------
  private[this] val bus = new ucesoft.cbm.peripheral.bus.IECBus
  private[this] var baLow = false
  private[this] var dma = false
  private[this] val expansionPort = ExpansionPort.getExpansionPort
  // -------------------- TRACE ----------------
  private[this] var traceDialog : TraceDialog = _
  private[this] var diskTraceDialog : TraceDialog = _
  private[this] var inspectDialog : InspectPanelDialog = _
  private[this] var traceItem,traceDiskItem : JCheckBoxMenuItem = _
  // -------------------- DISK -----------------
  private[this] val drivesRunning = Array(true,true)
  private[this] val drivesEnabled = Array(true,true)
  private[this] val diskFlusher = new FloppyFlushUI(vicDisplayFrame)
  private[this] val driveLeds = Array(new DriveLed,new DriveLed)
  private[this] val diskProgressPanels = Array(new DriveLoadProgressPanel,new DriveLoadProgressPanel)
  private[this] val flyerIEC = new FlyerIEC(bus,file => attachDiskFile(0,file,false,None))
  private[this] var isFlyerEnabled = false
  private[this] val drives : Array[Drive with TraceListener] = Array.ofDim(2)
  private[this] val floppyComponents = Array.ofDim[FloppyComponent](2)
  private[this] var device10Drive : Drive = _
  private[this] var device10DriveEnabled = false
  private[this] var FSDIRasInput = true
  private[this] var canWriteOnDisk = true
  // -------------------- TAPE -----------------
  private[this] var datassette : c2n.Datassette = _
  // ----------------- RS-232 ------------------
  private[this] val rs232 = BridgeRS232
  private[this] val AVAILABLE_RS232 : Array[RS232] = Array(TelnetRS232,
                                                           TCPRS232,
                                                           FileRS232,
                                                           SwiftLink.getSL(nmiSwitcher.expansionPortNMI,None),
                                                           SwiftLink.getSL(nmiSwitcher.expansionPortNMI _,Some(REU.getREU(REU.REU_1750,mmu,setDMA _,irqSwitcher.expPortIRQ _,None))),
                                                           ProcessRS232)
  // -------------------- PRINTER --------------
  private[this] var printerEnabled = false
  private[this] val printerGraphicsDriver = new ucesoft.cbm.peripheral.printer.MPS803GFXDriver(new ucesoft.cbm.peripheral.printer.MPS803ROM)
  private[this] val printer = new ucesoft.cbm.peripheral.printer.MPS803(bus,printerGraphicsDriver)  
  private[this] val printerDialog = {
    val dialog = new JDialog(vicDisplayFrame,"Print preview")
    val sp = new JScrollPane(printerGraphicsDriver)
    sp.getViewport.setBackground(Color.BLACK)
    dialog.getContentPane.add("Center",sp)
    printerGraphicsDriver.checkSize
    val buttonPanel = new JPanel
    val exportPNGBUtton = new JButton("Export as PNG")
    buttonPanel.add(exportPNGBUtton)
    exportPNGBUtton.addActionListener(_ => printerSaveImage )
    val clearButton = new JButton("Clear")
    buttonPanel.add(clearButton)
    clearButton.addActionListener(_ => printerGraphicsDriver.clearPages )
    dialog.getContentPane.add("South",buttonPanel)
    dialog.pack
    dialog
  }
  // -------------- AUDIO ----------------------
  private[this] val volumeDialog : JDialog = VolumeSettingsPanel.getDialog(vicDisplayFrame,sid.getDriver)
  // ------------ Control Port -----------------------
  private[this] val gameControlPort = new controlport.GamePadControlPort(configuration)
  private[this] val keypadControlPort = controlport.ControlPort.keypadControlPort
  private[this] val keyboardControlPort = controlport.ControlPort.userDefinedKeyControlPort(configuration)
  private[this] val controlPortA = new controlport.ControlPortBridge(keypadControlPort,"Control Port 1")  
  private[this] val controlPortB = new controlport.ControlPortBridge(gameControlPort,"Control port 2")
  // -------------- Light Pen -------------------------
  private[this] val LIGHT_PEN_NO_BUTTON = 0
  private[this] val LIGHT_PEN_BUTTON_UP = 1
  private[this] val LIGHT_PEN_BUTTON_LEFT = 2
  private[this] var lightPenButtonEmulation = LIGHT_PEN_NO_BUTTON
  
  private[this] class LightPenButtonListener extends MouseAdapter with CBMComponent {
    val componentID = "Light pen"
    val componentType = CBMComponentType.INPUT_DEVICE 
    
    override def mousePressed(e:MouseEvent) {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case LIGHT_PEN_BUTTON_UP => controlPortB.emulateUp         
        case LIGHT_PEN_BUTTON_LEFT => controlPortB.emulateLeft
      }
    }
    override def mouseReleased(e:MouseEvent) {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case _ => controlPortB.releaseEmulated
      }
    }
    //override def mouseDragged(e:MouseEvent) = mouseClicked(e)
    
    def init {}
    def reset {}
    // state
    protected def saveState(out:ObjectOutputStream) {}
    protected def loadState(in:ObjectInputStream) {}
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
  // ------------------------------------ Drag and Drop ----------------------------
  private[this] val DNDHandler = new DNDHandler(handleDND(_,true,true))
  
  private object DriveLed8Listener extends AbstractDriveLedListener(driveLeds(0),diskProgressPanels(0))
  
  private object DriveLed9Listener extends AbstractDriveLedListener(driveLeds(1),diskProgressPanels(1)) {
    driveLeds(1).setVisible(false)
  }  
  
  def reset {
    baLow = false
    dma = false
    z80Active = true
    clockSpeed = 1
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
  }

  private def initDrive(id:Int,driveType:DriveType.Value) {
    val old = Option(drives(id))
    old match {
      case Some(od) if od.driveType == driveType => return
      case _ =>
    }
    id match {
      case 0 =>
        drives(0) = driveType match {
          case DriveType._1571 =>
            DriveLed8Listener.setPowerLedMode(false)
            new D1571(0x00,bus,DriveLed8Listener,_ => {})
          case DriveType._1541 =>
            DriveLed8Listener.setPowerLedMode(false)
            new C1541(0x00,bus,DriveLed8Listener)
          case DriveType._1581 =>
            DriveLed8Listener.setPowerLedMode(true)
            new D1581(0x00,bus,DriveLed8Listener)
        }
      case 1 =>
        drives(1) = driveType match {
          case DriveType._1571 =>
            DriveLed9Listener.setPowerLedMode(false)
            new D1571(0x01,bus,DriveLed9Listener,_ => {})
          case DriveType._1541 =>
            DriveLed9Listener.setPowerLedMode(false)
            new C1541(0x01,bus,DriveLed9Listener)
          case DriveType._1581 =>
            DriveLed9Listener.setPowerLedMode(true)
            new D1581(0x01,bus,DriveLed9Listener)
        }
    }

    old match {
      case None =>
        add(drives(id))
      case Some(c) =>
        floppyComponents(id).drive = drives(id)
        c.getFloppy.close
        c.disconnect
        drives(id).initComponent
        change(c,drives(id))
        inspectDialog.updateRoot
        if (id == 0) {
          diskTraceDialog.mem = drives(id).getMem
          diskTraceDialog.traceListener = drives(id)
        }
    }

    drives(id).runningListener = running => {
      drivesRunning(id) = running
    }
  }
  
  def init {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo
    
    Log.info("Building the system ...")
    RS232ConfigPanel.registerAvailableRS232Drivers(vicDisplayFrame,AVAILABLE_RS232)
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
    				   irqSwitcher.ciaIRQ _) with IECBusListener {
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
    				   nmiSwitcher.cia2NMIAction _)
    rs232.setCIA12(cia1,cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory,mmu.colorRAM,irqSwitcher.vicIRQ _,baLow _)
    // I/O set
    mmu.setIO(cia1,cia2,vicChip,sid,vdc)
    // VIC vicDisplay
    vicDisplay = new vic.Display(vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,vicDisplayFrame.getTitle,vicDisplayFrame)
    add(vicDisplay)
    vicDisplay.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH,vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(vicDisplay)
    vicDisplayFrame.getContentPane.add("Center",vicDisplay)
    vicDisplayFrame.addKeyListener(keyb)
    vicDisplayFrame.addKeyListener(keypadControlPort)
    vicDisplayFrame.addKeyListener(keyboardControlPort)
    vicDisplay.addMouseListener(keypadControlPort)
    // VDC vicDisplay
    vdcDisplay = new vic.Display(ucesoft.cbm.peripheral.vdc.VDC.SCREEN_WIDTH,ucesoft.cbm.peripheral.vdc.VDC.SCREEN_HEIGHT,vdcDisplayFrame.getTitle,vdcDisplayFrame)
    add(vdcDisplay)
    vdcDisplay.setPreferredSize(ucesoft.cbm.peripheral.vdc.VDC.PREFERRED_FRAME_SIZE)
    vdc.setDisplay(vdcDisplay)
    
    vdcDisplayFrame.getContentPane.add("Center",vdcDisplay)
    vdcDisplayFrame.addKeyListener(keyb)
    vdcDisplayFrame.addKeyListener(keypadControlPort)
    vdcDisplayFrame.addKeyListener(keyboardControlPort)
    vdcDisplay.addMouseListener(keypadControlPort)
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
      override def keyPressed(e:KeyEvent) {
        e.getKeyCode match {
          // mouse
          case java.awt.event.KeyEvent.VK_P if e.isAltDown => 
            if (Clock.systemClock.isPaused) Clock.systemClock.play
            else {
              Clock.systemClock.pause
              vicDisplay.setPaused
              vdcDisplay.setPaused
            }
          case java.awt.event.KeyEvent.VK_M if e.isAltDown =>
            mouseEnabled = !mouseEnabled
            sid.setMouseEnabled(mouseEnabled)
            if (mouseEnabled) MouseCage.enableMouseCageOn(vdcDisplay) else MouseCage.disableMouseCage
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
    vicDisplay.addMouseListener(lightPen)    
    // tracing
    traceDialog = TraceDialog.getTraceDialog(vicDisplayFrame,mmu,z80,vicDisplay,vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog(vicDisplayFrame,drives(0).getMem,drives(0))    
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
    vicDisplayFrame.getContentPane.add("South",infoPanel)
    vicDisplayFrame.setTransferHandler(DNDHandler)    
    Log.info(sw.toString)
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
    DrivesConfigPanel.registerDrives(vicDisplayFrame,drives,setDriveType(_,_,false),enableDrive _,attachDisk _,attachDiskFile(_,_,_,None),drivesEnabled)
  }
  
  override def afterInitHook {    
	  inspectDialog = InspectPanel.getInspectDialog(vicDisplayFrame,this)    
    // deactivate drive 9
    drives(1).setActive(false)    
    driveLeds(1).setVisible(false)
    // set the correct CPU configuration
    cpuChanged(false)
  }
  
  private def errorHandler(t:Throwable) {
    import CPU6510.CPUJammedException
    t match {
      case j:CPUJammedException if !cpujamContinue =>
        JOptionPane.showConfirmDialog(vicDisplayFrame,
          s"CPU[${j.cpuID}] jammed at " + Integer.toHexString(j.pcError) + ". Do you want to open debugger (yes), reset (no) or continue (cancel) ?",
          "CPU jammed",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.ERROR_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            traceDialog.forceTracing(true)
            trace(true,true)
          case JOptionPane.CANCEL_OPTION => // continue
          case _ =>
            reset(true)
        }
      case j:CPUJammedException => // continue
      case _ =>
        Log.info("Fatal error occurred: " + cpu + "-" + t)
        Log.info(CPU6510.disassemble(mmu,cpu.getCurrentInstructionPC).toString)
        t.printStackTrace(Log.getOut)
        t.printStackTrace
        if (headless) sys.exit(1) // exit if headless
        JOptionPane.showMessageDialog(vicDisplayFrame,t.toString + " [PC=" + Integer.toHexString(cpu.getCurrentInstructionPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
        //trace(true,true)
        reset(true)
    }    
  }
  
  private def mainLoop(cycles:Long) { 
    // VIC PHI1
    vicChip.clock
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

  private def irqRequest(low:Boolean) {
    cpu.irqRequest(low)
    z80.irq(low)
  }
  
  private def setDMA(dma:Boolean) {
    this.dma = dma
    if (z80Active) z80.requestBUS(dma) else cpu.setDMA(dma)    
  }
  
  private def baLow(low:Boolean) {
    baLow = low
    if (z80Active) z80.requestBUS(baLow) else cpu.setBaLow(low)
    expansionPort.setBaLow(low)
  }
  
  // MMU change listener
  def frequencyChanged(f:Int) {
    Log.debug(s"Frequency set to $f Mhz")
    mmuStatusPanel.frequencyChanged(f)
    cpuFrequency = f
    val _2MhzMode = f == 2
    vicChip.set2MhzMode(_2MhzMode)
  }
  def cpuChanged(is8502:Boolean) {
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
  def c64Mode(c64Mode:Boolean) {
    mmuStatusPanel.c64Mode(c64Mode)
    this.c64Mode = c64Mode
  }  
  
  def fastSerialDirection(input:Boolean) {
    FSDIRasInput = input
    if (input) bus.setLine(cia1.asInstanceOf[IECBusListener],IECBusLine.DATA,IECBus.VOLTAGE)
    //println(s"FSDIR set to input $input")
  }
  
  def _1571mode(_1571Mode:Boolean) {
    mmuStatusPanel._1571mode(_1571Mode)
  }
  // ======================================== Settings ==============================================
  private def writeOnDiskSetting(enabled:Boolean) {    
    canWriteOnDisk = enabled
    for(d <- 0 to 1) drives(d).getFloppy.canWriteOnDisk = canWriteOnDisk
  }
  private def enableDrive(id:Int,enabled:Boolean) {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    adjustRatio(true)
  }
  private def enableDrive10(enabled:Boolean,fn:Option[String]) {    
    if (enabled) {
      device10Drive = new LocalDrive(bus,10)
      changeLocalDriveDir(fn)
    }
    device10DriveEnabled = enabled
  }
  private def enableMouse(mouseEnabled:Boolean) {
    keypadControlPort.setMouse1351Emulation(mouseEnabled)
    sid.setMouseEnabled(mouseEnabled)
    if (mouseEnabled) MouseCage.enableMouseCageOn(vicDisplay) else MouseCage.disableMouseCage
  }
  private def enablePrinter(enable:Boolean) {
    printerEnabled = enable
    printer.setActive(enable)
  }
  private def setDriveType(drive:Int,dt:DriveType.Value,dontPlay:Boolean = false) {
    clock.pause
    initDrive(drive,dt)
    if (!dontPlay) clock.play
  }
  private def enableVDC80(enabled:Boolean) {
    keyb.set4080Pressed(enabled)
  }
  private def enabledVDC(enabled:Boolean) {
    if (enabled) vdc.play else vdc.pause
    vdcDisplayFrame.setVisible(enabled)
  }
  private def enableMMUPanel(enabled:Boolean) {
    mmuStatusPanel.setVisible(enabled)
  }
  private def setREU(reu:Option[Int],reu16FileName:Option[String]) {
    reu match {
      case None =>
        ExpansionPort.getExpansionPort.eject
        ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      case Some(REU.REU_16M) =>
        val reu = REU.getREU(REU.REU_16M,mmu,setDMA _,irqSwitcher.expPortIRQ _,reu16FileName map { new File(_) } )	          
  	    ExpansionPort.setExpansionPort(reu)
  	    reu16FileName match {
          case Some(file) => REU.attached16MFileName = file
          case None =>
        }
      case Some(reuSize) =>
        ExpansionPort.setExpansionPort(REU.getREU(reuSize,mmu,setDMA _,irqSwitcher.expPortIRQ _,None))
    }    
  }
  private def setDisplayRendering(hints:java.lang.Object) {
    vicDisplay.setRenderingHints(hints)
    vdcDisplay.setRenderingHints(hints)
  }

  private def warpMode(warpOn:Boolean): Unit = {
    maxSpeedItem.setSelected(warpOn)
    clock.maximumSpeed = warpOn
    clock.pause
    sid.setFullSpeed(warpOn)
    clock.play
  }

  private def setLightPen(setting:Int): Unit = {
    lightPenButtonEmulation = setting
    vicChip.enableLightPen(setting != LIGHT_PEN_NO_BUTTON)
    keypadControlPort.setLightPenEmulation(setting != LIGHT_PEN_NO_BUTTON)
  }

  private def choose16MREU: Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".REU")
      def getDescription = "REU files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION|JFileChooser.CANCEL_OPTION =>
        try {
          setREU(Some(REU.REU_16M),if (fc.getSelectedFile == null) None else Some(fc.getSelectedFile.toString))
        }
        catch {
          case t:Throwable =>
            JOptionPane.showMessageDialog(vicDisplayFrame,t.toString, "REU loading error",JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }

  private def enableCPMCart(enabled:Boolean): Unit = {
    ExpansionPort.getExpansionPort.eject
    if (enabled) {
      ExpansionPort.setExpansionPort(new ucesoft.cbm.expansion.cpm.CPMCartridge(mmu,setDMA _,setTraceListener _))
      detachCtrItem.setEnabled(true)
    }
    else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
  }

  private def setDualSID(address:Option[Int]): Unit = {
    DualSID.setDualSID(address,sid)
  }

  private def enableFlyer(enabled:Boolean): Unit = {
    if (enabled != isFlyerEnabled) flyerIEC.reset
    isFlyerEnabled = enabled
  }

  private def chooseFlyerDir: Unit = {
    val fc = new JFileChooser
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.setCurrentDirectory(flyerIEC.getFloppyRepository)
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        flyerIEC.setFloppyRepository(fc.getSelectedFile)
      case _ =>
    }
  }

  private def setRemote(source:Option[JRadioButtonMenuItem]): Unit = {
    source match {
      case Some(source) =>
        if (remote.isDefined) remote.get.stopRemoting
        val listeningPort = JOptionPane.showInputDialog(vicDisplayFrame,"Listening port", "Remoting port configuration",JOptionPane.QUESTION_MESSAGE,null,null,"8064")
        if (listeningPort == null) {
          source.setSelected(false)
          vicDisplay.setRemote(None)
        }
        else {
          try {
            val clip = vicDisplay.getClipArea
            val remote = new ucesoft.cbm.remote.RemoteC64Server(listeningPort.toString.toInt,keyboardControlPort :: keyb :: keypadControlPort :: Nil,vicDisplay.displayMem,vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,clip._1.x,clip._1.y,clip._2.x,clip._2.y)
            this.remote = Some(remote)
            vicDisplay.setRemote(this.remote)
            remote.start
          }
          catch {
            case io:IOException =>
              JOptionPane.showMessageDialog(vicDisplayFrame,io.toString, "Remoting init error",JOptionPane.ERROR_MESSAGE)
              source.setSelected(false)
              vicDisplay.setRemote(None)
          }
        }
    case None =>
      remote match {
        case Some(rem) =>
          rem.stopRemoting
          vicDisplay.setRemote(None)
          remote = None
        case None =>
      }
    }
  }

  private def showKeyboardEditor: Unit = {
    val source = configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE,"default")
    val kbef = new JFrame(s"Keyboard editor ($source)")
    val kbe = new KeyboardEditor(keyb,keybMapper,false)
    kbef.getContentPane.add("Center",kbe)
    kbef.pack
    kbef.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    kbef.setVisible(true)
  }

  private def setDigiMax(on:Boolean,address:Option[Int]): Unit = {
    if (on) {
      address match {
        case None =>
          DigiMAX.enabled(true,true)
        case Some(a) =>
          ExpansionPort.getExpansionPort.eject
          ExpansionPort.setExpansionPort(new DigiMaxCart(a))
      }
    }
    else {
      DigiMAX.enabled(false,false)
      if (ExpansionPort.getExpansionPort.isInstanceOf[DigiMaxCart]) ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    }
  }

  private def chooseDigiMaxSampleRate: Unit = {
    Option(JOptionPane.showInputDialog(vicDisplayFrame,"DigiMax sample rate Hz",DigiMAX.getSampleRate.toString)) match {
      case None =>
      case Some(rate) =>
        DigiMAX.setSampleRate(rate.toInt)
    }
  }

  private def chooseGMod2: Unit = {
    var gmod2Path = configuration.getProperty(CONFIGURATION_GMOD2_FILE,"./gmod2_eeprom")
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(gmod2Path).getParentFile)
    fc.setDialogTitle("Choose a file where to save gmod2 cart eeprom")
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        gmod2Path = fc.getSelectedFile.toString
      case _ =>
    }
    configuration.setProperty(CONFIGURATION_GMOD2_FILE,gmod2Path)
  }

  private def setVicFullScreen: Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(vicDisplayFrame,
                                                vicDisplay,
                                                vicChip.SCREEN_WIDTH,
                                                vicChip.SCREEN_HEIGHT,
                                                keypadControlPort,
                                                keyb,
                                                keypadControlPort,
                                                keyboardControlPort)
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
  
  private def loadKeyboard {
    JOptionPane.showConfirmDialog(vicDisplayFrame,"Would you like to set default keyboard or load a configuration from file ?","Keyboard layout selection", JOptionPane.YES_NO_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE) match {
      case JOptionPane.YES_OPTION =>
        configuration.remove(CONFIGURATION_KEYB_MAP_FILE)
        JOptionPane.showMessageDialog(vicDisplayFrame,"Reboot the emulator to activate the new keyboard", "Keyboard..",JOptionPane.INFORMATION_MESSAGE)
      case JOptionPane.NO_OPTION =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
        fc.setDialogTitle("Choose a keyboard layout")
        fc.showOpenDialog(vicDisplayFrame) match {
          case JFileChooser.APPROVE_OPTION =>
            val in = new BufferedReader(new InputStreamReader(new FileInputStream(fc.getSelectedFile)))
            try {
              keyboard.KeyboardMapperStore.load(in)
              configuration.setProperty(CONFIGURATION_KEYB_MAP_FILE,fc.getSelectedFile.toString)
              JOptionPane.showMessageDialog(vicDisplayFrame,"Reboot the emulator to activate the new keyboard", "Keyboard..",JOptionPane.INFORMATION_MESSAGE)
            }
            catch {
              case _:IllegalArgumentException =>
                JOptionPane.showMessageDialog(vicDisplayFrame,"Invalid keyboard layout file", "Keyboard..",JOptionPane.ERROR_MESSAGE)
            }
            finally {
              in.close
            }
          case _ =>
        }
      case JOptionPane.CANCEL_OPTION =>
    }
  }
  
  private def loadFunctionROM(internal:Boolean,fileName:Option[String] = None,romType:FunctionROMType.Value = FunctionROMType.NORMAL) {
    val fn = fileName match {
      case None =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
        fc.setDialogTitle("Choose a ROM to load")
        fc.showOpenDialog(vicDisplayFrame) match {
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
      if (!fileName.isDefined) JOptionPane.showMessageDialog(vicDisplayFrame,"ROM loaded. Reset to turn it on", "ROM loaded successfully",JOptionPane.INFORMATION_MESSAGE)
      internal match {
        case true => internalFunctionROMFileName = fn
        case false => externalFunctionROMFileName = fn
      }
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(vicDisplayFrame,"Can't load ROM. Unexpected error occurred: " + t, "ROM loading error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace
    }
  }
  
  private def loadState {
    clock.pause
    var in : ObjectInputStream = null
    try {
      val canLoad = allowsState(vicDisplayFrame)
      if (!canLoad) {
        JOptionPane.showMessageDialog(vicDisplayFrame,"Can't load state", "State saving error",JOptionPane.ERROR_MESSAGE)
        return
      }
      val fc = new JFileChooser
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
	    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
	      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
	      def getDescription = "Kernal64 state files"
	    })
      fc.setDialogTitle("Choose a state file to load")
      val fn = fc.showOpenDialog(vicDisplayFrame) match {
        case JFileChooser.APPROVE_OPTION =>
          fc.getSelectedFile
        case _ =>
          return
      }
      in = new ObjectInputStream(new FileInputStream(fn))
      reset(false)
      load(in)
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(vicDisplayFrame,"Can't load state. Unexpected error occurred: " + t, "State loading error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace
        reset(false)
    }
    finally {
      if (in != null) in.close
      clock.play
    }
  }
  
  private def saveState {
    clock.pause
    var out : ObjectOutputStream = null
    try {
      val canSave = allowsState(vicDisplayFrame)
      if (!canSave) {
        JOptionPane.showMessageDialog(vicDisplayFrame,"Can't save state", "State saving error",JOptionPane.ERROR_MESSAGE)
        return
      }
      val fc = new JFileChooser
      fc.setDialogTitle("Choose where to save current state")
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
	    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
	      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
	      def getDescription = "Kernal64 state files"
	    })
      val fn = fc.showSaveDialog(vicDisplayFrame) match {
        case JFileChooser.APPROVE_OPTION =>
          if (fc.getSelectedFile.getName.toUpperCase.endsWith(".K64")) fc.getSelectedFile.toString else fc.getSelectedFile.toString + ".k64" 
        case _ =>
          return
      }
      out = new ObjectOutputStream(new FileOutputStream(fn))
      save(out)
      out.close
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(vicDisplayFrame,"Can't save state. Unexpected error occurred: " + t, "State saving error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace
    }
    finally {
      if (out != null) out.close
      clock.play
    }
  }
  
  private def adjustRatio(vic:Boolean=true,vdcResize:Boolean=false,vdcHalfSize:Boolean = false) {
    if (vic) {
      val dim = vicDisplay.asInstanceOf[java.awt.Component].getSize
      dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
      vicDisplay.setPreferredSize(dim) 
      vicDisplayFrame.pack
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
    
  private def changeLocalDriveDir(fileName:Option[String] = None) {
    fileName match {
      case None =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(device10Drive.asInstanceOf[LocalDrive].getCurrentDir)
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
        fc.setDialogTitle("Choose the current device 10 local directory")
        fc.showOpenDialog(vicDisplayFrame) match {
          case JFileChooser.APPROVE_OPTION =>
            device10Drive.asInstanceOf[LocalDrive].setCurrentDir(fc.getSelectedFile)
          case _ =>
        }
      case Some(fn) =>
        device10Drive.asInstanceOf[LocalDrive].setCurrentDir(new File(fn))
    }
  }
  
  private def manageRS232 {
    RS232ConfigPanel.RS232ConfigDialog.setVisible(true)
  }
  
  private def makeDisk {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setDialogTitle("Make an empty disk")
    fc.showSaveDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION => 
        try {          
          Diskette.makeEmptyDisk(fc.getSelectedFile.toString)         
        }
        catch {
          case t:Throwable => 
            JOptionPane.showMessageDialog(vicDisplayFrame,t.toString, "Disk making error",JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }
    
  // GamePlayer interface
  def play(file:File) = {
    ExpansionPort.getExpansionPort.eject
    ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    handleDND(file,true,true)
  }
  def attachDevice(file:File) : Unit = attachDevice(file,false)
  
  private def handleDND(file:File,_reset:Boolean,autorun:Boolean) {
    val name = file.getName.toUpperCase
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
    else {
      if (_reset) reset(false)
      if (autorun) {
        clock.schedule(new ClockEvent("Loading",clock.currentCycles + 2200000,(cycles) => { attachDevice(file,true,false) }))
        clock.play
      }
      else {
        attachDevice(file,false)
      }
    }
  }
  
  private def attachDevice(file:File,autorun:Boolean,emulateInserting:Boolean = true) {
    val name = file.getName.toUpperCase
    
    if (name.endsWith(".PRG")) loadPRGFile(file,autorun)
    else    
    if (name.endsWith(".D64") || name.endsWith(".G64") || name.endsWith(".D71") || name.endsWith(".D81")) attachDiskFile(0,file,autorun,None,emulateInserting)
    else
    if (name.endsWith(".TAP")) attachTapeFile(file,autorun)
    else
    if (name.endsWith(".T64")) attachT64File(file,autorun)
    else
    if (name.endsWith(".ZIP")) attachZIPFile(file,autorun)
    else
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
  }
  
  private def attachZip {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".ZIP")
      def getDescription = "ZIP files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>                
        attachZIPFile(fc.getSelectedFile,false)
      case _ =>        
    }
  }

  private def loadPRGFile(file:File,autorun:Boolean) {
    val (start,end) = ProgramLoader.loadPRG(mmu.getBank0RAM,file,c64Mode,8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,c64Mode)
    }
  }
  
  private def loadCartridgeFile(file:File) {
    try {          
      if (Thread.currentThread != Clock.systemClock) clock.pause
      val ep = ExpansionPortFactory.loadExpansionPort(file.toString,irqSwitcher.expPortIRQ _,nmiSwitcher.expansionPortNMI _,mmu.RAM,configuration)
      println(ep)
      if (ep.isFreezeButtonSupported) cartMenu.setVisible(true)
      ExpansionPort.setExpansionPort(ep)
      Log.info(s"Attached cartridge ${ExpansionPort.getExpansionPort.name}")
      reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)  
      detachCtrItem.setEnabled(true)
    }
    catch {
      case t:Throwable =>
        t.printStackTrace(traceDialog.logPanel.writer)
        JOptionPane.showMessageDialog(vicDisplayFrame,t.toString, "Cartridge loading error",JOptionPane.ERROR_MESSAGE)
    }
    finally {
      clock.play
    }
  }

  private def attachDiskFile(driveID:Int,file:File,autorun:Boolean,fileToLoad:Option[String],emulateInserting:Boolean = true) {
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
        JOptionPane.showMessageDialog(vicDisplayFrame,t.toString, "Disk attaching error",JOptionPane.ERROR_MESSAGE)
    }
  }
  
  private def attachTapeFile(file:File,autorun:Boolean) {
    datassette.setTAP(Some(new TAP(file.toString)))
    tapeMenu.setEnabled(true)
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("LOAD" + 13.toChar + "RUN" + 13.toChar,mmu,c64Mode)
    }
  }
  // -------------------------------------------------
  
  private def printerSaveImage {
    val fc = new JFileChooser
    fc.showSaveDialog(printerDialog) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
      	printerGraphicsDriver.saveAsPNG(file)
      case _ =>
    }
  }
  
  private def showPrinterPreview {
    printerGraphicsDriver.checkSize
    printerDialog.setVisible(true)
  }
  
  private def showAbout {
    val about = new AboutCanvas(mmu.CHAR_ROM,ucesoft.cbm.Version.VERSION.toUpperCase + " (" + ucesoft.cbm.Version.BUILD_DATE.toUpperCase + ")")
    JOptionPane.showMessageDialog(vicDisplayFrame,about,"About",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))    
  }
  
  private def zoom(f:Int) {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f,vicChip.VISIBLE_SCREEN_HEIGHT * f)
    updateScreenDimension(dim)
  }

  private def updateScreenDimension(dim:Dimension): Unit = {
    vicDisplay.setPreferredSize(dim)
    vicDisplay.invalidate
    vicDisplay.repaint()
    vicDisplayFrame.pack
  }

  private def updateVDCScreenDimension(dim:Dimension): Unit = {
    vdcDisplay.setPreferredSize(dim)
    vdcDisplay.invalidate
    vdcDisplay.repaint()
    vdcDisplayFrame.pack
  }
  
  private def savePrg {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showSaveDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
        val (start,end) = ProgramLoader.savePRG(fc.getSelectedFile,mmu,c64Mode)
        Log.info(s"BASIC program saved from $start to $end")
      case _ =>
    }
  }
  
  private def loadPrg {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>                
        loadPRGFile(fc.getSelectedFile,false)
      case _ =>
    }
  }
  
  private def takeSnapshot(vic:Boolean) {
    val fc = new JFileChooser
    fc.showSaveDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
      	vic match {
          case true => vicDisplay.saveSnapshot(file)
          case false => vdcDisplay.saveSnapshot(file)
        }
      case _ =>
    }
  }
  
  private def paste {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      Keyboard.insertTextIntoKeyboardBuffer(str,mmu,c64Mode)
    }
  }
  
  private def trace(cpu:Boolean,on:Boolean) {
    if (cpu) {
      Log.setOutput(traceDialog.logPanel.writer)
      traceDialog.setVisible(on)
      traceItem.setSelected(on)
    }
    else {
      if (on) Log.setOutput(diskTraceDialog.logPanel.writer) 
      else Log.setOutput(traceDialog.logPanel.writer)
      diskTraceDialog.setVisible(on)
      traceDiskItem.setSelected(on)
    }
  }
  
  private def reset(play:Boolean=true) {
    traceDialog.forceTracing(false)
    diskTraceDialog.forceTracing(false)
    if (Thread.currentThread != Clock.systemClock) clock.pause
    //if (play) ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    resetComponent
    if (play) clock.play
  } 
  
  private def detachCtr {
    if (ExpansionPort.getExpansionPort.isEmpty) JOptionPane.showMessageDialog(vicDisplayFrame,"No cartridge attached!", "Detach error",JOptionPane.ERROR_MESSAGE)
    else {
      if (Thread.currentThread != Clock.systemClock) clock.pause
      ExpansionPort.getExpansionPort.eject
      ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      reset(true)
    }
    detachCtrItem.setEnabled(false)
    cartMenu.setVisible(false)
  }
  
  private def attachCtr {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".CRT")
      def getDescription = "CRT files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        loadCartridgeFile(fc.getSelectedFile)
      case _ =>
    }
  }
  
  private def ejectDisk(driveID:Int) {
    drives(driveID).getFloppy.close
    driveLeds(driveID).setToolTipText("")
    if (!traceDialog.isTracing) clock.pause
    if (drives(driveID).driveType == DriveType._1581) drives(driveID).setDriveReader(D1581.MFMEmptyFloppy,true) 
    else drives(driveID).setDriveReader(EmptyFloppy,true)
    loadFileItems(driveID).setEnabled(false)
    clock.play
  }

  private def attachDisk(driveID:Int,autorun:Boolean) {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc,mmu.CHAR_ROM,c64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory || drives(driveID).formatExtList.exists { ext => try { f.toString.toUpperCase.endsWith(ext) } catch { case _:Throwable=> false } }
      def getDescription = s"${drives(driveID).formatExtList.mkString(",")} files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID,fc.getSelectedFile,autorun,canvas.selectedFile)
      case _ =>
    }
  }
  
  private def loadFileFromAttachedFile(driveID:Int,relocate:Boolean) {
    val floppy = drives(driveID).getFloppy
    if (floppy.isEmpty) JOptionPane.showMessageDialog(vicDisplayFrame,"No disk attached!", "Loading error",JOptionPane.ERROR_MESSAGE)
    else {
      Option(JOptionPane.showInputDialog(vicDisplayFrame,"Load file","*")) match {
        case None =>
        case Some(fileName) =>
          try {
            floppy.asInstanceOf[Diskette].loadInMemory(mmu.getBank0RAM,fileName,relocate,c64Mode,driveID + 8)
          }
          catch {
            case t:Throwable =>
              JOptionPane.showMessageDialog(vicDisplayFrame, "Errore while loading from disk: " + t.getMessage,"Loading error",JOptionPane.ERROR_MESSAGE)
          }
      }
    }
  }    
  
  private def loadFileFromTape {
    val fc = new JFileChooser
    val canvas = new T64Canvas(fc,mmu.CHAR_ROM,c64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".T64")
      def getDescription = "T64 files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachT64File(new File(fc.getSelectedFile.toString),false)
      case _ =>
    }
  }
  
  private def attachT64File(file:File,autorun:Boolean) {
    val tape = new T64(file.toString)
    try {
      val values = tape.entries map { e => e.asInstanceOf[Object] }
      JOptionPane.showInputDialog(vicDisplayFrame,"Select file to open:","Open file in " + file,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
        case null =>
        case entry:T64Entry =>
          tape.loadInMemory(mmu.getBank0RAM,entry,c64Mode)
          configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
          if (autorun) {
            Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,c64Mode)
          }
      }
    }
    finally {
      tape.close
    }
  }
  
  private def attachZIPFile(file:File,autorun:Boolean) {
    ZIP.zipEntries(file) match {
      case Success(entries) =>
        if (entries.size > 0) {
          val values = entries map { e => e.asInstanceOf[Object] } toArray
          
          JOptionPane.showInputDialog(vicDisplayFrame,"Select file to open:","Open file in " + file,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
            case null =>
            case e@ZIP.ArchiveEntry(_,_,_) =>
              ZIP.extractEntry(e,new File(scala.util.Properties.tmpDir)) match {
                case Some(f) =>
                  attachDevice(f,autorun)
                case None =>
              }          
          } 
        }
      case Failure(t) =>
        JOptionPane.showMessageDialog(vicDisplayFrame,t.toString,s"Error while opening zip file $file",JOptionPane.ERROR_MESSAGE)
    }    
  }
  
  private def attachTape {
    val fc = new JFileChooser
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP")
      def getDescription = "TAP files"
    })
    fc.showOpenDialog(vicDisplayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachTapeFile(fc.getSelectedFile,false)
      case _ =>
    }
  }
  
  private def setMenu {
    val menuBar = new JMenuBar
    val fileMenu = new JMenu("File")
    val editMenu = new JMenu("Edit")
    val stateMenu = new JMenu("State")
    val traceMenu = new JMenu("Trace")
    val optionMenu = new JMenu("Settings")
    val gamesMenu = new JMenu("Games")
    val helpMenu = new JMenu("Help")    
    cartMenu.setVisible(false)
    
    menuBar.add(fileMenu)
    menuBar.add(editMenu)
    menuBar.add(stateMenu)
    menuBar.add(traceMenu)
    menuBar.add(optionMenu)         
    menuBar.add(cartMenu)
    menuBar.add(gamesMenu)
    menuBar.add(helpMenu)

    val warpModeOnLoad = new JCheckBoxMenuItem("Warp mode on load")
    warpModeOnLoad.setSelected(true)
    warpModeOnLoad.addActionListener(e => ProgramLoader.loadingWithWarpEnabled = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected )
    fileMenu.add(warpModeOnLoad)
    // Setting ---------------------------
    settings.add("warponload",
      "Enabled/disable warp mode on load",
      "WARPLOAD",
      (wpl:Boolean) => {
        ProgramLoader.loadingWithWarpEnabled = wpl
        warpModeOnLoad.setSelected(wpl)
      },
      warpModeOnLoad.isSelected
    )
    // -----------------------------------
    
    val zipItem = new JMenuItem("Attach zip ...")
    zipItem.addActionListener(_ => attachZip )
    fileMenu.add(zipItem)
    
    val tapeItem = new JMenuItem("Load file from tape ...")
    tapeItem.addActionListener(_ => loadFileFromTape )
    fileMenu.add(tapeItem)
    
    val attachTapeItem = new JMenuItem("Attach tape ...")
    attachTapeItem.addActionListener(_ => attachTape )
    fileMenu.add(attachTapeItem)
        
    tapeMenu.setEnabled(false)
    fileMenu.add(tapeMenu)
    
    val tapePlayItem = new JMenuItem("Cassette press play")
    tapePlayItem.addActionListener(_ => datassette.pressPlay )
    tapeMenu.add(tapePlayItem)
    
    val tapeStopItem = new JMenuItem("Cassette press stop")
    tapeStopItem.addActionListener(_ => datassette.pressStop )
    tapeMenu.add(tapeStopItem)
    
    val tapeRecordItem = new JMenuItem("Cassette press record & play")
    tapeRecordItem.addActionListener(_ => datassette.pressRecordAndPlay )
    tapeMenu.add(tapeRecordItem)
    
    val tapeRewindItem = new JMenuItem("Cassette press rewind")
    tapeRewindItem.addActionListener(_ => datassette.pressRewind )
    tapeMenu.add(tapeRewindItem)
    
    fileMenu.addSeparator
    
    val makeDiskItem = new JMenuItem("Make empty disk ...")
    makeDiskItem.addActionListener(_ => makeDisk )
    fileMenu.add(makeDiskItem)
    
    val autorunDiskItem = new JMenuItem("Autorun disk ...")
    autorunDiskItem.addActionListener(_ => attachDisk(0,true) )
    fileMenu.add(autorunDiskItem)
    
    val attachDisk0Item = new JMenuItem("Attach disk 8...")
    attachDisk0Item.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_8,java.awt.event.InputEvent.ALT_DOWN_MASK))
    attachDisk0Item.addActionListener(_ => attachDisk(0,false) )
    fileMenu.add(attachDisk0Item)
    val attachDisk1Item = new JMenuItem("Attach disk 9...")
    attachDisk1Item.addActionListener(_ => attachDisk(1,false) )
    attachDisk1Item.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_9,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fileMenu.add(attachDisk1Item)
    // For settings see below, after drive type    
    
    val ejectMenu = new JMenu("Eject disk")
    fileMenu.add(ejectMenu)
    val ejectDisk0Item = new JMenuItem("Eject disk 8...")
    ejectDisk0Item.addActionListener(_ => ejectDisk(0) )
    ejectMenu.add(ejectDisk0Item)
    val ejectDisk1Item = new JMenuItem("Eject disk 9...")
    ejectDisk1Item.addActionListener(_ => ejectDisk(1) )
    ejectMenu.add(ejectDisk1Item)
        
    loadFileItems(0).setEnabled(false)
    loadFileItems(0).addActionListener(_ => loadFileFromAttachedFile(0,true) )
    fileMenu.add(loadFileItems(0)) 
    loadFileItems(1).setEnabled(false)
    loadFileItems(1).addActionListener(_ => loadFileFromAttachedFile(1,true) )
    fileMenu.add(loadFileItems(1))
    fileMenu.addSeparator
    
    val loadPrgItem = new JMenuItem("Load PRG file from local disk ...")
    loadPrgItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G,java.awt.event.InputEvent.ALT_DOWN_MASK))
    loadPrgItem.addActionListener(_ => loadPrg )
    fileMenu.add(loadPrgItem)
    
    val savePrgItem = new JMenuItem("Save PRG file to local disk ...")
    savePrgItem.addActionListener(_ => savePrg )
    fileMenu.add(savePrgItem)
    
    // Setting ---------------------------
    settings.add("drive9-enabled",
      "Enabled/disable driver 9",
      "DRIVE_9_ENABLED",
      (d9e:Boolean) => {
        enableDrive(1,d9e)
      },
      drivesEnabled(1)
    )
    // -----------------------------------
    
    val localDriveItem = new JMenu("Drive on device 10 ...")
    fileMenu.add(localDriveItem)
    val group0 = new ButtonGroup
    val noLocalDriveItem = new JRadioButtonMenuItem("Disabled")
    noLocalDriveItem.setSelected(true)
    noLocalDriveItem.addActionListener(_ => enableDrive10(false,None) )
    group0.add(noLocalDriveItem)
    localDriveItem.add(noLocalDriveItem)
    val localDriveEnabled = new JRadioButtonMenuItem("Local drive ...")
    localDriveEnabled.addActionListener(_ => enableDrive10(true,None) )
    group0.add(localDriveEnabled)
    localDriveItem.add(localDriveEnabled)
    // Setting ---------------------------
    settings.add("drive10-local-path",
                 "Enabled driver 10 to the given local path",
                 "DRIVE_10_PATH",
                 (d10p:String) => {
                   val enabled = d10p != ""
                   enableDrive10(enabled,if (enabled) Some(d10p) else None)
                   localDriveEnabled.setSelected(enabled)
                 },
                 if (device10DriveEnabled) device10Drive.asInstanceOf[LocalDrive].getCurrentDir.toString
                 else ""
    )
    // -----------------------------------
    
    val writeOnDiskCheckItem = new JCheckBoxMenuItem("Write changes on disk")
    writeOnDiskCheckItem.setSelected(true)
    writeOnDiskCheckItem.addActionListener(e => writeOnDiskSetting(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    fileMenu.add(writeOnDiskCheckItem)
    // Setting ---------------------------
    settings.add("write-on-disk",
                 "Tells if the modifications made on disks must be written on file",
                 "WRITE_ON_DISK",
                 (wod:Boolean) => {
                   writeOnDiskCheckItem.setSelected(wod)
                   writeOnDiskSetting(wod)
                 },
                 writeOnDiskCheckItem.isSelected
    )
    // -----------------------------------
    
    fileMenu.addSeparator
    
    val resetItem = new JMenuItem("Reset")
    resetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK))
    resetItem.addActionListener(_ => reset(true) )
    fileMenu.add(resetItem)
    
    fileMenu.addSeparator
    
    val attachCtrItem = new JMenuItem("Attach cartridge ...")
    attachCtrItem.addActionListener(_ => attachCtr )
    fileMenu.add(attachCtrItem)
    // Setting ---------------------------
    settings.add("cart",
                 "Attach the given cartridge",
                 "ATTACH_CTR",
                 (cart:String) => if (cart != "") loadCartridgeFile(new File(cart))
                 ,
                 ExpansionPort.currentCartFileName
    )
    // -----------------------------------
        
    detachCtrItem.setEnabled(false)
    detachCtrItem.addActionListener(_ => detachCtr )
    fileMenu.add(detachCtrItem)
    
    fileMenu.addSeparator
    
    val autoSaveCheckItem = new JCheckBoxMenuItem("Autosave settings on exit")
    autoSaveCheckItem.setSelected(configuration.getProperty(CONFIGURATION_AUTOSAVE,"false").toBoolean)
    autoSaveCheckItem.addActionListener(e => configuration.setProperty(CONFIGURATION_AUTOSAVE,e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected.toString) )
    fileMenu.add(autoSaveCheckItem)
    
    val saveSettingsCheckItem = new JMenuItem("Save settings")
    saveSettingsCheckItem.addActionListener(_ => saveSettings(true) )
    fileMenu.add(saveSettingsCheckItem)
    
    fileMenu.addSeparator
    
    val exitItem = new JMenuItem("Exit")
    exitItem.addActionListener(_ => close )
    fileMenu.add(exitItem)
    
    // edit
        
    val pasteItem = new JMenuItem("Paste text")
    pasteItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.Event.CTRL_MASK))
    pasteItem.addActionListener(_ => paste )
    editMenu.add(pasteItem)
    val listItem = new JMenuItem("List BASIC to editor")
    listItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I,java.awt.Event.ALT_MASK))
    listItem.addActionListener(_ => ucesoft.cbm.misc.BasicListExplorer.list(mmu,if (c64Mode) 0x801 else 0x1C01) )
    editMenu.add(listItem)
    
    //state
    val saveStateItem = new JMenuItem("Save state ...")
    saveStateItem.addActionListener(_ => saveState )
    stateMenu.add(saveStateItem)
    val loadStateItem = new JMenuItem("Load state ...")
    loadStateItem.addActionListener(_ => loadState )
    stateMenu.add(loadStateItem)
    
    // trace
    
    traceItem = new JCheckBoxMenuItem("Trace CPU")
    traceItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T,java.awt.event.InputEvent.ALT_DOWN_MASK))
    traceItem.setSelected(false)
    traceItem.addActionListener(e => trace(true,e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    traceMenu.add(traceItem)  
    
    traceDiskItem = new JCheckBoxMenuItem("Trace Disk CPU")
    traceDiskItem.setSelected(false)
    traceDiskItem.addActionListener(e => trace(false,e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    traceMenu.add(traceDiskItem)
    
    val inspectItem = new JCheckBoxMenuItem("Inspect components ...")
    inspectItem.setSelected(false)
    inspectItem.addActionListener(e => inspectDialog.setVisible(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    traceMenu.add(inspectItem)
    
    // settings

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
    settings.add("vdc-enabled",
                 "Enable VDC monitor",
                 "VDCENABLED",
                 (vdcE:Boolean) => { 
                   enabledVDC(vdcE)
                   vdcEnabled.setSelected(vdcE)
                 },
                 vdcEnabled.isSelected
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
    keybEditorItem.addActionListener(_ => showKeyboardEditor )
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
    
    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled")
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M,java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.setSelected(false)
    mouseEnabledItem.addActionListener(e => enableMouse(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
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
    
    optionMenu.addSeparator
    
    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        vicDisplay.setPaused
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
    // Setting ---------------------------
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
    romItem.addActionListener( _ => ROMPanel.showROMPanel(vicDisplayFrame,configuration,false,() => {
      saveSettings(false)
      checkFunctionROMS
    }) )

    // cartridge
    val cartButtonItem = new JMenuItem("Press cartridge button...")
    cartButtonItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,java.awt.event.InputEvent.ALT_DOWN_MASK))
    cartButtonItem.addActionListener(_ => cartButtonRequested = true )
    cartMenu.add(cartButtonItem)

    // non-saveable settings
    settings.add("ext-rom",
      "External function ROM path",
      (extRom:String) => {
        configuration.setProperty(ROM.C128_EXTERNAL_ROM_PROP,extRom)
        checkFunctionROMS
      }
    )
    settings.add("int-rom",
      "Internal function ROM (<rom path>,NORMAL|MEGABIT)",
      (intRom:String) => {
        configuration.setProperty(ROM.C128_INTERNAL_ROM_PROP,intRom)
        checkFunctionROMS
      }
    )
    settings.add("go64",
                 "Run in 64 mode",
                 (go64:Boolean) => if (go64) mmu.go64
    )
    settings.add("warp",
                 "Run warp mode",
                 (warp:Boolean) => if (warp) {
                   clock.maximumSpeed = true
                   maxSpeedItem.setSelected(true)
                 }
    )
    settings.add("headless",
                 "Run with no windows",
                 (hl:Boolean) => if (hl) headless = true                 
    )
    settings.add("testcart",
                 "Run with test cart",
                 (tc:Boolean) => if (tc) TestCart.enabled = true                 
    )
    settings.add("limitcycles",
                 "Run at most the number of cycles specified",
                 (cycles:String) => if (cycles != "" && cycles.toLong > 0) clock.limitCyclesTo(cycles.toLong)
    )
    settings.add("run-file",
      "Run the given file taken from the attached disk",
      "RUNFILE",
      (runFile:String) => {},
      ""
    )
    settings.add("screenshot",
      "Take a screenshot of VIC screen and save it on the given file path. Used with --testcart only.",
      (file:String) => if (file != "") {
        TestCart.screenshotFile = Some(file)
        TestCart.screeshotHandler = vicDisplay.saveSnapshot _
      }
    )
    settings.add("vdcscreenshot",
      "Take a screenshot of VDC screen and save it on the given file path. Used with --testcart only.",
      (file:String) => if (file != "") {
        TestCart.screenshotFile = Some(file)
        TestCart.screeshotHandler = vdcDisplay.saveSnapshot _
      }
    )
    settings.add("cpujam-continue",
      "On cpu jam continue execution",
      (jam:Boolean) => cpujamContinue = jam
    )
    settings.add("screen-dim",
      "Zoom factor. Valued accepted are 1 and 2",
      (f:Int) => if (f == 1 || f == 2) {
        zoom(f)
        zoomOverride = true
      }
    )
    settings.add("cia-model",
      "Set the CIA model (both cia1 and cia2). 6526 for old cia, 8521 for the new one. Default is 6526. ",
      (cm:String) => if (cm == "8521") {
        cia1.setCIAModel(CIA.CIA_MODEL_8521)
        cia2.setCIAModel(CIA.CIA_MODEL_8521)
      }
    )
    // games
    val loader = ServiceLoader.load(classOf[ucesoft.cbm.game.GameProvider])
    var providers = loader.iterator
    try {
      if (!providers.hasNext) providers = java.util.Arrays.asList((new ucesoft.cbm.game.GameBaseSpi).asInstanceOf[ucesoft.cbm.game.GameProvider],(new ucesoft.cbm.game.PouetDemoSpi).asInstanceOf[ucesoft.cbm.game.GameProvider]).iterator
      val names = new collection.mutable.HashSet[String]
      while (providers.hasNext) {
        val provider = providers.next
        Log.info(s"Loaded ${provider.name} provider")
        if (!names.contains(provider.name)) {
          names += provider.name
          val item = new JCheckBoxMenuItem(provider.name)
          gamesMenu.add(item)
          item.addActionListener(new ActionListener {
            def actionPerformed(e:ActionEvent) {
              try {
                val ui = GameUI.getUIFor(item,vicDisplayFrame,provider,C128.this)
                ui.setVisible(item.isSelected)
              }
              catch {
                case t:Throwable =>
                  JOptionPane.showMessageDialog(vicDisplayFrame,t.toString,s"Error while contacting provider ${provider.name}'s server",JOptionPane.ERROR_MESSAGE)
              }
            }
          })
        }
      }
    }
    catch {
      case t:Throwable =>
        t.printStackTrace
    }
    
    // help    
    val aboutItem = new JMenuItem("About")
    aboutItem.addActionListener(_ => showAbout )
    helpMenu.add(aboutItem)
    
    vicDisplayFrame.setJMenuBar(menuBar)
  }
  
  private def configureJoystick {
    import ucesoft.cbm.peripheral.controlport.Joysticks._
    def getControlPortFor(id:String) = configuration.getProperty(id) match {
      case CONFIGURATION_KEYPAD_VALUE => keypadControlPort
      case CONFIGURATION_JOYSTICK_VALUE => gameControlPort
      case CONFIGURATION_KEYBOARD_VALUE => keyboardControlPort
      case _ => controlport.ControlPort.emptyControlPort
    }
    
    controlPortA.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_2)
    controlPortB.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_1)
  }
  
  private def joySettings {
    Clock.systemClock.pause
    try {
      val dialog = new JoystickSettingDialog(vicDisplayFrame,configuration,gameControlPort)
      dialog.setVisible(true)
      configureJoystick
    }
    finally {
      Clock.systemClock.play
    }
  }

  private def swapJoysticks {
    import ucesoft.cbm.peripheral.controlport.Joysticks._
    val j1 = configuration.getProperty(CONFIGURATION_JOY_PORT_1)
    val j2 = configuration.getProperty(CONFIGURATION_JOY_PORT_2)
    if (j2 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_1,j2) else configuration.remove(CONFIGURATION_JOY_PORT_1)
    if (j1 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_2,j1) else configuration.remove(CONFIGURATION_JOY_PORT_2)
    configureJoystick
  }
  
  private def close {
    if (!headless) saveSettings(configuration.getProperty(CONFIGURATION_AUTOSAVE,"false").toBoolean)
    for(d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }
  
  private def saveSettings(save:Boolean) {
    configuration.setProperty(CONFIGURATION_FRAME_XY,vicDisplayFrame.getX + "," + vicDisplayFrame.getY)
    if (!zoomOverride) {
      val dimension = vicDisplay.getSize()
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

  private def setTraceListener(tl:Option[TraceListener]) {
    tl match {
      case None => 
        traceDialog.traceListener = cpu
      case Some(t) => 
        traceDialog.traceListener = t
    }
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeChars("KERNAL128")
    out.writeObject(ucesoft.cbm.Version.VERSION)
    out.writeLong(System.currentTimeMillis)
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
    out.writeBoolean(z80Active)
    out.writeInt(clockSpeed)
    out.writeBoolean(FSDIRasInput)
    out.writeBoolean(c64Mode)
    out.writeInt(cpuFrequency)
  }
  protected def loadState(in:ObjectInputStream) {
    val header = "KERNAL128"
    for(i <- 0 until header.length) if (in.readChar != header(i)) throw new IOException("Bad header")
    val ver = in.readObject.asInstanceOf[String]
    val ts = in.readLong
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean
    z80Active = in.readBoolean
    clockSpeed = in.readInt
    FSDIRasInput = in.readBoolean
    c64Mode = in.readBoolean
    cpuFrequency = in.readInt
    val msg = s"<html><b>Version:</b> $ver<br><b>Date:</b> ${new java.util.Date(ts)}</html>"
    JOptionPane.showConfirmDialog(vicDisplayFrame,msg,"State loading confirmation",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
      case JOptionPane.YES_OPTION =>
      case _ => throw new IOException("State loading aborted")
    }
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  // -----------------------------------------------------------------------------------------
  
  private def swing(f: => Unit) {
    SwingUtilities.invokeAndWait(() => f)
  }
  
  def run(args:Array[String]) {        
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
    swing { vicDisplayFrame.pack }
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing { updateScreenDimension(new Dimension(dim(0),dim(1))) }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      swing { vicDisplayFrame.setLocation(xy(0),xy(1)) }
    }     
    // SETTINGS
    loadSettings(args)
    // VIEW
    if (headless) vdcDisplayFrame.setVisible(false)
    swing { vicDisplayFrame.setVisible(!headless) }
    // PLAY    
    vdc.play
    clock.play
  }
}