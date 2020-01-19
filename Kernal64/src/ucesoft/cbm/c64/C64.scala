package ucesoft.cbm.c64

import ucesoft.cbm.cpu._
import ucesoft.cbm.peripheral._
import javax.swing._
import java.awt.event._
import java.awt._

import ucesoft.cbm.expansion.ExpansionPort
import ucesoft.cbm.formats._
import ucesoft.cbm.formats.ExpansionPortFactory
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceDialog
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor

import ucesoft.cbm.peripheral.bus.IECBus
import java.io._

import javax.swing.filechooser.FileFilter
import ucesoft.cbm.peripheral.drive.C1541Emu
import java.util.Properties

import ucesoft.cbm.peripheral.controlport.JoystickSettingDialog
import ucesoft.cbm.peripheral.controlport.Joysticks._
import ucesoft.cbm.peripheral.drive.C1541
import ucesoft.cbm.peripheral.drive.Drive
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.trace.InspectPanel
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.bus.BusSnoop
import ucesoft.cbm.peripheral.printer.MPS803
import ucesoft.cbm.peripheral.printer.MPS803GFXDriver
import ucesoft.cbm.peripheral.printer.MPS803ROM
import ucesoft.cbm.cpu.CPU6510.CPUJammedException
import ucesoft.cbm.expansion.REU
import ucesoft.cbm.peripheral.rs232._
import ucesoft.cbm.peripheral.drive.LocalDrive
import ucesoft.cbm.expansion.SwiftLink
import ucesoft.cbm.peripheral.drive.ParallelCable
import ucesoft.cbm.expansion.DualSID
import ucesoft.cbm.peripheral.drive.FlyerIEC
import ucesoft.cbm.remote.RemoteC64
import ucesoft.cbm.game.GamePlayer
import java.util.ServiceLoader

import ucesoft.cbm.game.GameUI

import scala.util.Success
import scala.util.Failure
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Clock
import ucesoft.cbm.Log
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral.drive.DriveType
import ucesoft.cbm.peripheral.drive.D1571
import ucesoft.cbm.peripheral.drive.D1581
import ucesoft.cbm.trace.InspectPanelDialog
import ucesoft.cbm.expansion.DigiMAX
import ucesoft.cbm.expansion.DigiMaxCart
import ucesoft.cbm.expansion.cpm.CPMCartridge
import ucesoft.cbm.peripheral.drive.EmptyFloppy
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.vic.Palette
import ucesoft.cbm.peripheral.vic.Palette.PaletteType

object C64 extends App {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  val c64 = new C64
  c64.run(args)  
}

class C64 extends CBMComponent with GamePlayer {
  val componentID = "Commodore 64"
  val componentType = CBMComponentType.INTERNAL
  
  private[this] val settings = new ucesoft.cbm.misc.Settings
  private[this] val CONFIGURATION_FILENAME = "C64.config"
  private[this] val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  private[this] val CONFIGURATION_FRAME_XY = "frame.xy"  
  private[this] val CONFIGURATION_FRAME_DIM = "frame.dim"
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
  private[this] var headless = false // used with --testcart command options
  private[this] val clock = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)
  private[this] val mem = new C64MMU.MAIN_MEMORY
  private[this] val cpu = CPU6510.make(mem)  
  private[this] var vicChip : vic.VIC = _
  private[this] var cia1,cia2 : CIA = _
  private[this] val sid = new ucesoft.cbm.peripheral.sid.SID
  private[this] var display : vic.Display = _
  private[this] val nmiSwitcher = new NMISwitcher(cpu.nmiRequest _)
  private[this] val irqSwitcher = new IRQSwitcher(cpu.irqRequest _)
  private[this] val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),"/resources/default_keyboard_c64",C64KeyboardMapper)
  private[this] val keyb = new keyboard.Keyboard(keybMapper,nmiSwitcher.keyboardNMIAction _)	// key listener
  private[this] val displayFrame = {
    val f = new JFrame("Kernal64 " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) {
        close
      }
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)    
    f
  }
  // -------------- MENU ITEMS -----------------
  private[this] val maxSpeedItem = new JCheckBoxMenuItem("Warp mode")
  private[this] val loadFileItems = Array(new JMenuItem("Load file from attached disk 8 ..."), new JMenuItem("Load file from attached disk 9 ..."))
  private[this] val tapeMenu = new JMenu("Tape control...")
  private[this] val detachCtrItem = new JMenuItem("Detach cartridge")
  private[this] val cartMenu = new JMenu("Cartridge")
  // -------------------------------------------
  private[this] val bus = new IECBus
  private[this] var dma = false
  private[this] val expansionPort = ExpansionPort.getExpansionPort
  // -------------------- TRACE ----------------
  private[this] var cpuTracer : TraceListener = cpu
  private[this] var traceDialog : TraceDialog = _
  private[this] var diskTraceDialog : TraceDialog = _
  private[this] var inspectDialog : InspectPanelDialog = _
  private[this] var traceItem,traceDiskItem : JCheckBoxMenuItem = _
  private[this] val busSnooper = new BusSnoop(bus)
  private[this] var busSnooperActive = false
  // ----------------- REMOTE ------------------
  private[this] var remote : Option[RemoteC64] = None
  // -------------------- DISK -----------------
  private[this] val drivesRunning = Array(true,true)
  private[this] val drivesEnabled = Array(true,true)
  private[this] val diskFlusher = new FloppyFlushUI(displayFrame)
  private[this] val driveLeds = Array(new DriveLed,new DriveLed)
  private[this] val floppyComponents = Array.ofDim[FloppyComponent](2)
  private[this] val diskProgressPanels = Array(new DriveLoadProgressPanel,new DriveLoadProgressPanel)
  private[this] val c1541 = new C1541Emu(bus,DriveLed8Listener)
  private[this] val flyerIEC = new FlyerIEC(bus,file => attachDiskFile(0,file,false,None))
  private[this] var isFlyerEnabled = false
  private[this] val drives : Array[Drive with TraceListener] = Array.ofDim(2)
  private[this] var device10Drive : Drive = _
  private[this] var device10DriveEnabled = false
  private[this] var canWriteOnDisk = true
  // -------------------- TAPE -----------------
  private[this] var datassette : Datassette = _
  // ----------------- RS-232 ------------------
  private[this] val rs232 = BridgeRS232
  private[this] val AVAILABLE_RS232 : Array[RS232] = Array(//UP9600,
                                                           TelnetRS232,
                                                           TCPRS232,
                                                           FileRS232,
                                                           SwiftLink.getSL(nmiSwitcher.expansionPortNMI,None),
                                                           SwiftLink.getSL(nmiSwitcher.expansionPortNMI _,Some(REU.getREU(REU.REU_1750,mem,setDMA _,irqSwitcher.expPortIRQ _,None))),
                                                           ProcessRS232)
  // -------------------- PRINTER --------------
  private[this] var printerEnabled = false
  private[this] val printerGraphicsDriver = new MPS803GFXDriver(new MPS803ROM)
  private[this] val printer = new MPS803(bus,printerGraphicsDriver)  
  private[this] val printerDialog = {
    val dialog = new JDialog(displayFrame,"Print preview")
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
  private[this] val volumeDialog : JDialog = VolumeSettingsPanel.getDialog(displayFrame,sid.getDriver)
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
  
  def reset {
    dma = false
    clock.maximumSpeed = false
    maxSpeedItem.setSelected(false)
    ProgramLoader.reset
  }
  
  def init {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo
    
    Log.info("Building the system ...")
    RS232ConfigPanel.registerAvailableRS232Drivers(displayFrame,AVAILABLE_RS232)
    ExpansionPort.addConfigurationListener(mem)
    // drive
    initDrive(0,DriveType._1541)
    initDrive(1,DriveType._1541)
    drivesEnabled(1) = false
    // -----------------------
    ProgramLoader.cpu = cpu
    ProgramLoader.warpModeListener = warpMode _
    add(clock)
    add(mem)
    add(cpu)
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
    val vicMemory = new C64VICMemory(mem,mem.CHAR_ROM,cpu)
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
    				   irqSwitcher.ciaIRQ _)
    val cia2CP1 = new CIA2Connectors.PortAConnector(vicMemory,bus,rs232)
    val cia2CP2 = new CIA2Connectors.PortBConnector(rs232)    
    add(cia2CP1)
    add(cia2CP2)
    add(nmiSwitcher)    
    cia2 = new CIA("CIA2",
    				   0xDD00,
    				   cia2CP1,
    				   cia2CP2,
    				   nmiSwitcher.cia2NMIAction _)
    rs232.setCIA12(cia1,cia2)
    ParallelCable.ca2Callback = cia2.setFlagLow _
    add(ParallelCable)
    vicChip = new vic.VIC(vicMemory,mem.COLOR_RAM,irqSwitcher.vicIRQ _,baLow _)      
    mem.setLastByteReadMemory(vicMemory)
    // mapping I/O chips in memory
    val io = mem.IO    
    io.addBridge(cia1)
    io.addBridge(cia2)
    io.addBridge(vicChip)
    io.addBridge(sid)    
    display = new vic.Display(vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,displayFrame.getTitle,displayFrame)
    add(display)
    display.setPreferredSize(new java.awt.Dimension(vicChip.VISIBLE_SCREEN_WIDTH,vicChip.VISIBLE_SCREEN_HEIGHT))
    vicChip.setDisplay(display)
    displayFrame.getContentPane.add("Center",display)
    displayFrame.addKeyListener(keyb)
    displayFrame.addKeyListener(keypadControlPort)
    displayFrame.addKeyListener(keyboardControlPort)
    display.addMouseListener(keypadControlPort)    
    val lightPen = new LightPenButtonListener
    add(lightPen)
    display.addMouseListener(lightPen)
    traceDialog = TraceDialog.getTraceDialog(displayFrame,mem,cpu,display,vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog(displayFrame,drives(0).getMem,drives(0))
    // drive leds
    add(driveLeds(0))        
    add(driveLeds(1))
    configureJoystick
    add(c1541)
    Log.setOutput(traceDialog.logPanel.writer)
    // tape
    datassette = new Datassette(cia1.setFlagLow _)
    mem.setDatassette(datassette)
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
    displayFrame.getContentPane.add("South",infoPanel)
    displayFrame.setTransferHandler(DNDHandler)
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
            val cmd = s"""LOAD"$fn",8,1""" + 13.toChar + "RUN" + 13.toChar
            clock.schedule(new ClockEvent("Loading",clock.currentCycles + 2200000,(cycles) => Keyboard.insertTextIntoKeyboardBuffer(cmd,mem,true) ))
        }
      case Some(f) =>
        handleDND(new File(f),false,true)
    }
    DrivesConfigPanel.registerDrives(displayFrame,drives,setDriveType(_,_,false),enableDrive _,attachDisk _,attachDiskFile(_,_,_,None),drivesEnabled)
  }
  
  override def afterInitHook {    
	  inspectDialog = InspectPanel.getInspectDialog(displayFrame,this)    
    // deactivate drive 9
    drives(1).setActive(false)    
    driveLeds(1).setVisible(false)        
  }
  
  private def errorHandler(t:Throwable) {
    t match {
      case j:CPUJammedException =>
        JOptionPane.showConfirmDialog(displayFrame,
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
      case _ =>
        Log.info("Fatal error occurred: " + cpu + "-" + t)
        Log.info(CPU6510.disassemble(mem,cpu.getCurrentInstructionPC).toString)
        t.printStackTrace(Log.getOut)
        t.printStackTrace
        JOptionPane.showMessageDialog(displayFrame,t.toString + " [PC=" + Integer.toHexString(cpu.getCurrentInstructionPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
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
    cpu.fetchAndExecute(1)
  }

  private def setDMA(dma:Boolean) {
    this.dma = dma
    cpu.setDMA(dma)
  }
  
  private def baLow(low:Boolean) {
    cpu.setBaLow(low)
    expansionPort.setBaLow(low)
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
    adjustRatio
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
    if (mouseEnabled) MouseCage.enableMouseCageOn(display) else MouseCage.disableMouseCage
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
  private def setREU(reu:Option[Int],reu16FileName:Option[String]) {
    reu match {
      case None =>
        ExpansionPort.getExpansionPort.eject
        ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      case Some(REU.REU_16M) =>
        val reu = REU.getREU(REU.REU_16M,mem,setDMA _,irqSwitcher.expPortIRQ _,reu16FileName map { new File(_) } )	          
  	    ExpansionPort.setExpansionPort(reu)
  	    reu16FileName match {
          case Some(file) => REU.attached16MFileName = file
          case None =>
        }
      case Some(reuSize) =>
        ExpansionPort.setExpansionPort(REU.getREU(reuSize,mem,setDMA _,irqSwitcher.expPortIRQ _,None))
    }    
  }
  private def setDisplayRendering(hints:java.lang.Object) {
    display.setRenderingHints(hints)
  }

  private def showKeyboardEditor: Unit = {
    val source = configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE,"default")
    val kbef = new JFrame(s"Keyboard editor ($source)")
    val kbe = new KeyboardEditor(keyb,keybMapper,true)
    kbef.getContentPane.add("Center",kbe)
    kbef.pack
    kbef.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    kbef.setVisible(true)
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

  private def setDualSID(address:Option[Int]): Unit = {
    DualSID.setDualSID(address,sid)
  }

  private def setRemote(source:Option[JRadioButtonMenuItem]): Unit = {
    source match {
      case Some(source) =>
        if (remote.isDefined) remote.get.stopRemoting
        val listeningPort = JOptionPane.showInputDialog(displayFrame,"Listening port", "Remoting port configuration",JOptionPane.QUESTION_MESSAGE,null,null,"8064")
        if (listeningPort == null) {
          source.setSelected(false)
          display.setRemote(None)
        }
        else {
          try {
            val clip = display.getClipArea
            val remote = new ucesoft.cbm.remote.RemoteC64Server(listeningPort.toString.toInt,keyboardControlPort :: keyb :: keypadControlPort :: Nil,display.displayMem,vicChip.SCREEN_WIDTH,vicChip.SCREEN_HEIGHT,clip._1.x,clip._1.y,clip._2.x,clip._2.y)
            this.remote = Some(remote)
            display.setRemote(this.remote)
            remote.start
          }
          catch {
            case io:IOException =>
              JOptionPane.showMessageDialog(displayFrame,io.toString, "Remoting init error",JOptionPane.ERROR_MESSAGE)
              source.setSelected(false)
              display.setRemote(None)
          }
        }
      case None =>
        remote match {
          case Some(rem) =>
            rem.stopRemoting
            display.setRemote(None)
            remote = None
          case None =>
        }
    }
  }

  private def enableFlyer(enabled:Boolean): Unit = {
    if (enabled != isFlyerEnabled) flyerIEC.reset
    isFlyerEnabled = enabled
  }

  private def chooseFlyerDir: Unit = {
    val fc = new JFileChooser
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.setCurrentDirectory(flyerIEC.getFloppyRepository)
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        flyerIEC.setFloppyRepository(fc.getSelectedFile)
      case _ =>
    }
  }

  private def choose16MREU: Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".REU")
      def getDescription = "REU files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION|JFileChooser.CANCEL_OPTION =>
        try {
          setREU(Some(REU.REU_16M),if (fc.getSelectedFile == null) None else Some(fc.getSelectedFile.toString))
        }
        catch {
          case t:Throwable =>
            JOptionPane.showMessageDialog(displayFrame,t.toString, "REU loading error",JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
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
    Option(JOptionPane.showInputDialog(displayFrame,"DigiMax sample rate Hz",DigiMAX.getSampleRate.toString)) match {
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
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        gmod2Path = fc.getSelectedFile.toString
      case _ =>
    }
    configuration.setProperty(CONFIGURATION_GMOD2_FILE,gmod2Path)
  }

  private def enableCPMCart(enabled:Boolean): Unit = {
    ExpansionPort.getExpansionPort.eject
    if (enabled) {
      ExpansionPort.setExpansionPort(new ucesoft.cbm.expansion.cpm.CPMCartridge(mem,setDMA _,setTraceListener _))
      detachCtrItem.setEnabled(true)
    }
    else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
  }

  private def setVicFullScreen: Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(displayFrame,
      display,
      vicChip.SCREEN_WIDTH,
      vicChip.SCREEN_HEIGHT,
      keypadControlPort,
      keyb,
      keypadControlPort,
      keyboardControlPort)
  }
  // ================================================================================================
  
  private def loadKeyboard {
    JOptionPane.showConfirmDialog(displayFrame,"Would you like to set default keyboard or load a configuration from file ?","Keyboard layout selection", JOptionPane.YES_NO_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE) match {
      case JOptionPane.YES_OPTION =>
        configuration.remove(CONFIGURATION_KEYB_MAP_FILE)
        JOptionPane.showMessageDialog(displayFrame,"Reboot the emulator to activate the new keyboard", "Keyboard..",JOptionPane.INFORMATION_MESSAGE)
      case JOptionPane.NO_OPTION =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
        fc.setDialogTitle("Choose a keyboard layout")
        fc.showOpenDialog(displayFrame) match {
          case JFileChooser.APPROVE_OPTION =>
            val in = new BufferedReader(new InputStreamReader(new FileInputStream(fc.getSelectedFile)))
            try {
              keyboard.KeyboardMapperStore.load(in)
              configuration.setProperty(CONFIGURATION_KEYB_MAP_FILE,fc.getSelectedFile.toString)
              JOptionPane.showMessageDialog(displayFrame,"Reboot the emulator to activate the new keyboard", "Keyboard..",JOptionPane.INFORMATION_MESSAGE)
            }
            catch {
              case _:IllegalArgumentException =>
                JOptionPane.showMessageDialog(displayFrame,"Invalid keyboard layout file", "Keyboard..",JOptionPane.ERROR_MESSAGE)
            }
            finally {
              in.close
            }
          case _ =>
        }
      case JOptionPane.CANCEL_OPTION =>
    }
  }
  
  private def loadState {
    clock.pause
    var in : ObjectInputStream = null
    try {
      val canLoad = allowsState(displayFrame)
      if (!canLoad) {
        JOptionPane.showMessageDialog(displayFrame,"Can't load state", "State saving error",JOptionPane.ERROR_MESSAGE)
        return
      }
      val fc = new JFileChooser
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
	    fc.setFileFilter(new FileFilter {
	      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
	      def getDescription = "Kernal64 state files"
	    })
      fc.setDialogTitle("Choose a state file to load")
      val fn = fc.showOpenDialog(displayFrame) match {
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
        JOptionPane.showMessageDialog(displayFrame,"Can't load state. Unexpected error occurred: " + t, "State loading error",JOptionPane.ERROR_MESSAGE)
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
      val canSave = allowsState(displayFrame)
      if (!canSave) {
        JOptionPane.showMessageDialog(displayFrame,"Can't save state", "State saving error",JOptionPane.ERROR_MESSAGE)
        return
      }
      val fc = new JFileChooser
      fc.setDialogTitle("Choose where to save current state")
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
	    fc.setFileFilter(new FileFilter {
	      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
	      def getDescription = "Kernal64 state files"
	    })
      val fn = fc.showSaveDialog(displayFrame) match {
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
        JOptionPane.showMessageDialog(displayFrame,"Can't save state. Unexpected error occurred: " + t, "State saving error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace
    }
    finally {
      if (out != null) out.close
      clock.play
    }
  }
  
  private def adjustRatio {
    val dim = display.asInstanceOf[java.awt.Component].getSize
    dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).round.toInt
    display.setPreferredSize(dim) 
    displayFrame.pack
  } 
    
  private def changeLocalDriveDir(fileName:Option[String] = None) {
    fileName match {
      case None =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(device10Drive.asInstanceOf[LocalDrive].getCurrentDir)
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
        fc.setDialogTitle("Choose the current device 10 local directory")
        fc.showOpenDialog(displayFrame) match {
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
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION => 
        try {          
          Diskette.makeEmptyDisk(fc.getSelectedFile.toString)         
        }
        catch {
          case t:Throwable => 
            JOptionPane.showMessageDialog(displayFrame,t.toString, "Disk making error",JOptionPane.ERROR_MESSAGE)
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
        clock.schedule(new ClockEvent("Loading",clock.currentCycles + 2200000,(cycles) => { attachDevice(file,true,None,false) }))
        clock.play
      }
      else {
        attachDevice(file,false)
      }
    }
  }
  
  private def attachDevice(file:File,autorun:Boolean,fileToLoad:Option[String] = None,emulateInserting:Boolean = true) {
    val name = file.getName.toUpperCase
    
    if (name.endsWith(".PRG")) loadPRGFile(file,autorun)
    else    
    if (name.endsWith(".D64") || name.endsWith(".G64") || name.endsWith(".D71") || name.endsWith(".D81")) attachDiskFile(0,file,autorun,fileToLoad,emulateInserting)
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
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".ZIP")
      def getDescription = "ZIP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>                
        attachZIPFile(fc.getSelectedFile,false)
      case _ =>        
    }
  }
  
  private def loadPRGFile(file:File,autorun:Boolean) {
    val (start,end) = ProgramLoader.loadPRG(mem,file,true,8)
    Log.info(s"BASIC program loaded from $start to $end")
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mem,true)
    }
  }
  
  private def loadCartridgeFile(file:File) {
    try {          
      if (Thread.currentThread != Clock.systemClock) clock.pause
      val ep = ExpansionPortFactory.loadExpansionPort(file.toString,irqSwitcher.expPortIRQ _,nmiSwitcher.expansionPortNMI _,mem.getRAM,configuration)
      println(ep)
      if (ep.isFreezeButtonSupported) cartMenu.setVisible(true)
      ExpansionPort.setExpansionPort(ep)
      ExpansionPort.currentCartFileName = file.toString
      Log.info(s"Attached cartridge ${ExpansionPort.getExpansionPort.name}")
      reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)  
      detachCtrItem.setEnabled(true)
    }
    catch {
      case t:Throwable =>
        t.printStackTrace(traceDialog.logPanel.writer)
        JOptionPane.showMessageDialog(displayFrame,t.toString, "Cartridge loading error",JOptionPane.ERROR_MESSAGE)
    }
    finally {
      clock.play
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
  
  private def attachDiskFile(driveID:Int,file:File,autorun:Boolean,fileToLoad:Option[String],emulateInserting:Boolean = true) {
    try {   
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
      if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      val isD64 = file.getName.toUpperCase.endsWith(".D64")
      if (drives(driveID) == c1541 && !isD64) {
        JOptionPane.showMessageDialog(displayFrame,"Format not allowed on a 1541 not in true emulation mode", "Disk attaching error",JOptionPane.ERROR_MESSAGE)
        return
      }
      val disk = Diskette(file.toString)
      disk.canWriteOnDisk = canWriteOnDisk
      disk.flushListener = diskFlusher
      drives(driveID).getFloppy.close
      if (!traceDialog.isTracing) clock.pause
      drives(driveID).setDriveReader(disk,emulateInserting)
      clock.play
            
      loadFileItems(driveID).setEnabled(isD64)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      val drive = driveID + 8
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""LOAD"$fn",$drive,1""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          Keyboard.insertTextIntoKeyboardBuffer(cmd,mem,true)
        case None if autorun =>
          Keyboard.insertSmallTextIntoKeyboardBuffer(s"""LOAD"*",$drive,1""" + 13.toChar + "RUN" + 13.toChar,mem,true)
        case _ =>
      }
      driveLeds(driveID).setToolTipText(disk.toString)
    }
    catch {
      case t:Throwable =>
        t.printStackTrace
        JOptionPane.showMessageDialog(displayFrame,t.toString, "Disk attaching error",JOptionPane.ERROR_MESSAGE)
    }
  }
  
  private def attachTapeFile(file:File,autorun:Boolean) {
    datassette.setTAP(Some(new TAP(file.toString)))
    tapeMenu.setEnabled(true)
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      Keyboard.insertSmallTextIntoKeyboardBuffer("LOAD" + 13.toChar + "RUN" + 13.toChar,mem,true)
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
    val about = new AboutCanvas(mem.CHAR_ROM,ucesoft.cbm.Version.VERSION.toUpperCase + " (" + ucesoft.cbm.Version.BUILD_DATE.toUpperCase + ")")
    JOptionPane.showMessageDialog(displayFrame,about,"About",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))    
  }
  
 private def zoom(f:Int) {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f,vicChip.VISIBLE_SCREEN_HEIGHT * f)
    display.setPreferredSize(dim)
    display.invalidate
    display.repaint()
    displayFrame.pack
  }
  
  private def savePrg {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
        val (start,end) = ProgramLoader.savePRG(fc.getSelectedFile,mem,true)
        Log.info(s"BASIC program saved from $start to $end")
      case _ =>
    }
  }
  
  private def loadPrg {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>                
        loadPRGFile(fc.getSelectedFile,false)
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
  
  private def paste {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      Keyboard.insertTextIntoKeyboardBuffer(str,mem,true)
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
    if (ExpansionPort.getExpansionPort.isEmpty) JOptionPane.showMessageDialog(displayFrame,"No cartridge attached!", "Detach error",JOptionPane.ERROR_MESSAGE)
    else {
      if (Thread.currentThread != Clock.systemClock) clock.pause
      ExpansionPort.getExpansionPort.eject
      ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      reset(true)
    }
    detachCtrItem.setEnabled(false)
    cartMenu.setVisible(false)
    ExpansionPort.currentCartFileName = ""
  }
  
  private def attachCtr {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".CRT")
      def getDescription = "CRT files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        loadCartridgeFile(fc.getSelectedFile)
      case _ =>
    }
  }
  
  private def attachDisk(driveID:Int,autorun:Boolean) {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc,mem.CHAR_ROM,true)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f:File) = f.isDirectory || drives(driveID).formatExtList.exists { ext => try { f.toString.toUpperCase.endsWith(ext) } catch { case _:Throwable=> false } }
      def getDescription = s"${drives(driveID).formatExtList.mkString(",")} files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID,fc.getSelectedFile,autorun,canvas.selectedFile)
      case _ =>
    }
  }
  
  private def loadFileFromAttachedFile(driveID:Int,relocate:Boolean) {
    val floppy = drives(driveID).getFloppy
    if (floppy.isEmpty) JOptionPane.showMessageDialog(displayFrame,"No disk attached!", "Loading error",JOptionPane.ERROR_MESSAGE)
    else {
      Option(JOptionPane.showInputDialog(displayFrame,"Load file","*")) match {
        case None =>
        case Some(fileName) =>
          try {
            floppy.asInstanceOf[Diskette].loadInMemory(mem,fileName,relocate,true,driveID + 8)
          }
          catch {
            case t:Throwable =>
              JOptionPane.showMessageDialog(displayFrame, "Errore while loading from disk: " + t.getMessage,"Loading error",JOptionPane.ERROR_MESSAGE)
          }
      }
    }
  }    
  
  private def loadFileFromTape {
    val fc = new JFileChooser
    val canvas = new T64Canvas(fc,mem.CHAR_ROM,true)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".T64")
      def getDescription = "T64 files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachT64File(new File(fc.getSelectedFile.toString),false)
      case _ =>
    }
  }
  
  private def attachT64File(file:File,autorun:Boolean) {
    val tape = new T64(file.toString)
    try {
      val values = tape.entries map { e => e.asInstanceOf[Object] }
      JOptionPane.showInputDialog(displayFrame,"Select file to open:","Open file in " + file,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
        case null =>
        case entry:T64Entry =>
          tape.loadInMemory(mem,entry)
          configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
          if (autorun) {
            Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mem,true)
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
          
          JOptionPane.showInputDialog(displayFrame,"Select file to open:","Open file in " + file,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
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
        JOptionPane.showMessageDialog(displayFrame,t.toString,s"Error while opening zip file $file",JOptionPane.ERROR_MESSAGE)
    }    
  }
  
  private def attachTape {
    val fc = new JFileChooser
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP")
      def getDescription = "TAP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachTapeFile(fc.getSelectedFile,false)
      case _ =>
    }
  }
  
  private def setMenu {
    import ucesoft.cbm.misc.Settings._
    
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
    attachDisk1Item.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_9,java.awt.event.InputEvent.ALT_DOWN_MASK))
    attachDisk1Item.addActionListener(_ => attachDisk(1,false) )
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
    listItem.addActionListener(_ => ucesoft.cbm.misc.BasicListExplorer.list(mem,0x801) )
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
    optionMenu.addSeparator

    val keybMenu = new JMenu("Keyboard")
    optionMenu.add(keybMenu)

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
    
    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK))
    adjustRatioItem.addActionListener(_ => adjustRatio )
    optionMenu.add(adjustRatioItem)

    val zoomItem = new JMenu("Zoom")
    val groupZ = new ButtonGroup
    optionMenu.add(zoomItem)
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

    val vicItem = new JMenu("VIC")
    val renderingItem = new JMenu("Rendering")
    vicItem.add(renderingItem)
    val groupR = new ButtonGroup
    optionMenu.add(vicItem)
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

    val paletteItem = new JMenu("Palette")
    vicItem.add(paletteItem)
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

    val fullScreenItem = new JMenuItem("Full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setVicFullScreen )
    optionMenu.add(fullScreenItem)
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
    
    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,java.awt.event.InputEvent.ALT_DOWN_MASK))
    snapshotItem.addActionListener(_ => takeSnapshot )
    optionMenu.add(snapshotItem)
    
    optionMenu.addSeparator
    
    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        display.setPaused
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
    for(adr <- DualSID.validAddresses(true)) {
      val sid2AdrItem = new JRadioButtonMenuItem(adr)
      sid2Item.add(sid2AdrItem)
      sid2AdrItem.setSelected(false)
      sid2AdrItem.addActionListener(_ => setDualSID(Some(Integer.parseInt(adr,16))) )
      group8.add(sid2AdrItem)
    }

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

    val busSnooperActiveItem = new JCheckBoxMenuItem("Bus snoop active")
    busSnooperActiveItem.setSelected(false)
    busSnooperActiveItem.addActionListener(e => busSnooperActive = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected )
    optionMenu.add(busSnooperActiveItem)
    
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
                 "Set the reu type (none,128,256,512,16384)",
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
      ExpansionPort.getExpansionPort.isInstanceOf[CPMCartridge]
    )

    val romItem = new JMenuItem("ROMs ...")
    optionMenu.add(romItem)
    romItem.addActionListener( _ => ROMPanel.showROMPanel(displayFrame,configuration,true,() => saveSettings(false)) )
        
    // cartridge
    val cartButtonItem = new JMenuItem("Press cartridge button...")
    cartButtonItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,java.awt.event.InputEvent.ALT_DOWN_MASK))
    cartButtonItem.addActionListener(_ => cartButtonRequested = true )
    cartMenu.add(cartButtonItem)
    
    // non-saveable settings
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
                 (cycles:Int) => if (cycles > 0) clock.limitCyclesTo(cycles)                 
    )
    settings.add("run-file",
      "Run the given file taken from the attached disk",
      "RUNFILE",
      (runFile:String) => {},
      ""
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
                val ui = GameUI.getUIFor(item,displayFrame,provider,C64.this)
                ui.setVisible(item.isSelected)
              }
              catch {
                case t:Throwable =>
                  JOptionPane.showMessageDialog(displayFrame,t.toString,s"Error while contacting provider ${provider.name}'s server",JOptionPane.ERROR_MESSAGE)
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
    
    displayFrame.setJMenuBar(menuBar)
  }
  
  private def configureJoystick {
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
      val dialog = new JoystickSettingDialog(displayFrame,configuration,gameControlPort)
      dialog.setVisible(true)
      configureJoystick
    }
    finally {
      Clock.systemClock.play
    }
  }
  
  private def swapJoysticks {
    val j1 = configuration.getProperty(CONFIGURATION_JOY_PORT_1)
    val j2 = configuration.getProperty(CONFIGURATION_JOY_PORT_2)
    if (j2 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_1,j2)
    if (j1 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_2,j1)
    configureJoystick
  }
  
  private def close {
    saveSettings(configuration.getProperty(CONFIGURATION_AUTOSAVE,"false").toBoolean)
    for(d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }
  
  private def saveSettings(save:Boolean) {
    configuration.setProperty(CONFIGURATION_FRAME_XY,displayFrame.getX + "," + displayFrame.getY)
    configuration.setProperty(CONFIGURATION_FRAME_DIM,displayFrame.getSize.width + "," + displayFrame.getSize.height)
    if (save) {
      settings.save(configuration)
      println("Settings saved")
    }
    try {
      val propsFile = new File(new File(scala.util.Properties.userHome),CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "C64 configuration file")
      out.close
    }
    catch {
      case io:IOException =>
    }
  }
  
  // ------------------------------- TRACE LISTENER ------------------------------------------
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
    out.writeChars("KERNAL64")
    out.writeObject(ucesoft.cbm.Version.VERSION)
    out.writeLong(System.currentTimeMillis)
    out.writeBoolean(drivesEnabled(0))
    out.writeBoolean(drivesEnabled(1))
    out.writeBoolean(printerEnabled)
  }
  protected def loadState(in:ObjectInputStream) {
    val header = "KERNAL64"
    for(i <- 0 until header.length) if (in.readChar != header(i)) throw new IOException("Bad header")
    val ver = in.readObject.asInstanceOf[String]
    val ts = in.readLong
    drivesEnabled(0) = in.readBoolean
    drivesEnabled(1) = in.readBoolean
    printerEnabled = in.readBoolean 
    val msg = s"<html><b>Version:</b> $ver<br><b>Date:</b> ${new java.util.Date(ts)}</html>"
    JOptionPane.showConfirmDialog(displayFrame,msg,"State loading confirmation",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
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
      println(s"Kernal64, Commodore 64 emulator ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      settings.printUsage
      sys.exit(0)
    }
    swing { initComponent }
    // VIC
    swing { displayFrame.pack }
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      swing { displayFrame.setSize(dim(0),dim(1)) }
    }
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      swing { displayFrame.setLocation(xy(0),xy(1)) }
    }
    // SETTINGS
    loadSettings(args)
    // VIEW
    swing { displayFrame.setVisible(!headless) }
    // PLAY
    clock.play
  }
}