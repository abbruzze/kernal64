package ucesoft.cbm

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Toolkit}
import java.awt.event._
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.util.{Properties, ServiceLoader}
import javax.swing._
import javax.swing.filechooser.FileFilter
import ucesoft.cbm.cpu.{CPU65xx, Memory, ROM}
import ucesoft.cbm.expansion._
import ucesoft.cbm.expansion.cpm.CPMCartridge
import ucesoft.cbm.formats._
import ucesoft.cbm.formats.cart.{EasyFlash, GMOD3}
import ucesoft.cbm.game.{GamePlayer, GameUI}
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.controlport.JoystickSettingDialog
import ucesoft.cbm.peripheral.controlport.Joysticks._
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.printer.{MPS803, MPS803GFXDriver, MPS803ROM}
import ucesoft.cbm.peripheral.rs232._
import ucesoft.cbm.peripheral.vic.{Display, Palette, VICType, VIC_NTSC, VIC_PAL}
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.peripheral.vic.coprocessor.VASYL
import ucesoft.cbm.peripheral.{controlport, keyboard, vic}
import ucesoft.cbm.remote.RemoteC64
import ucesoft.cbm.trace.{InspectPanel, InspectPanelDialog, TraceDialog, TraceListener}

import java.awt.datatransfer.DataFlavor
import scala.util.{Failure, Success}

object CBMComputer {
  def turnOn(computer : => CBMComputer,args:Array[String]) : Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

    val cbm = computer
    try {
      cbm.turnOn(args)
    }
    catch {
      case i:Preferences.PreferenceIllegalArgumentException =>
        println(s"Bad command line argument: ${i.getMessage}")
        sys.exit(100)
      case t:Throwable =>
        cbm.errorHandler(t)
        if (cbm.isHeadless) sys.exit(1)
    }
  }
}

trait CBMComputer extends CBMComponent with GamePlayer { cbmComputer =>
  protected val APPLICATION_NAME : String
  protected val CONFIGURATION_FILENAME : String
  protected val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  protected val CONFIGURATION_FRAME_XY = "frame.xy"
  protected val CONFIGURATION_FRAME_DIM = "frame.dim"
  protected val CONFIGURATION_KEYB_MAP_FILE = "keyb.map.file"
  protected val CONFIGURATION_GMOD2_FILE = "gmod2.file"

  protected def PRG_RUN_DELAY_CYCLES = 2200000
  protected var lastLoadedPrg : Option[File] = None

  // -------------- MENU ITEMS -----------------
  protected val maxSpeedItem = new JCheckBoxMenuItem("Warp mode")
  protected val loadFileItems = for(d <- 0 until TOTAL_DRIVES) yield new JMenuItem(s"Load file from attached disk ${d + 8} ...")
  protected val tapeMenu = new JMenu("Tape control...")
  protected val detachCtrItem = new JMenuItem("Detach cartridge")
  protected val cartMenu = new JMenu("Cartridge")
  protected val easyFlashWriteChangesItem = new JMenuItem("Write changes")
  protected val GMOD3WriteChangesItem = new JMenuItem("GMOD3 Write changes")

  protected val preferences = new Preferences

  protected lazy val configuration = {
    val kernalConfigHome = System.getProperty("kernal.config",scala.util.Properties.userHome)
    val props = new Properties
    val propsFile = new File(new File(kernalConfigHome),CONFIGURATION_FILENAME)
    if (propsFile.exists) {
      try {
        props.load(new FileReader(propsFile))
      }
      catch {
        case _:IOException =>
          setDefaultProperties(props)
      }
    }
    else setDefaultProperties(props)
    ROM.props = props
    props
  }

  protected var cartButtonRequested = false
  protected var headless = false // used with --testcart command options
  protected var cpujamContinue = false // used with --cpujam-continue
  protected var zoomOverride = false // used with --screen-dim
  protected var sidCycleExact = false // used with --sid-cycle-exact
  protected var loadStateFromOptions = false // used with --load-state
  protected var traceOption = false // used with --trace
  protected var fullScreenAtBoot = false // used with --fullscreen
  protected var ignoreConfig = false // used with --ignore-config-file
  protected var loadPRGasDisk = false // used with --prg-as-disk
  protected var disk8LoadedAsPRG = false

  // memory & main cpu
  protected val mmu : Memory
  protected lazy val cpu = CPU65xx.make(mmu)
  // main chips
  protected val clock = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)
  protected var vicChip : vic.VIC = _
  protected var vicZoomFactor : Int = 1
  protected var cia1,cia2 : CIA = _
  protected val cia12Running = Array(true,true)
  protected val sid = new ucesoft.cbm.peripheral.sid.SID
  protected var display : vic.Display = _
  protected var gifRecorder : JDialog = _
  protected val nmiSwitcher = new Switcher("NMI",cpu.nmiRequest _)//new NMISwitcher(cpu.nmiRequest _)
  protected val irqSwitcher = new Switcher("IRQ",cpu.irqRequest _)//new IRQSwitcher(cpu.irqRequest _)
  protected val dmaSwitcher = new Switcher("DMA",setDMA _)
  protected val keybMapper : keyboard.KeyboardMapper
  protected lazy val keyb = new keyboard.Keyboard(keybMapper,nmiSwitcher.setLine(Switcher.KB,_),!isC64Mode)	// key listener

  protected val bus = new IECBus
  protected var dma = false
  protected val expansionPort = ExpansionPort.getExpansionPort

  protected lazy val displayFrame = {
    val f = new JFrame(s"$APPLICATION_NAME " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = turnOff
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    f
  }

  protected var resetSettingsActions : List[() => Unit] = Nil

  // ----------------- REMOTE ------------------
  protected var remote : Option[RemoteC64] = None
  // -------------------- TRACE ----------------
  protected var traceDialog : TraceDialog = _
  protected var diskTraceDialog : TraceDialog = _
  protected var inspectDialog : InspectPanelDialog = _
  protected var traceItem,traceDiskItem : JCheckBoxMenuItem = _
  // -------------------- DISK -----------------
  final protected val TOTAL_DRIVES = Preferences.TOTALDRIVES
  protected val drivesRunning = Array.fill[Boolean](TOTAL_DRIVES)(true)
  protected val drivesEnabled = Array.fill[Boolean](TOTAL_DRIVES)(true)
  protected lazy val diskFlusher = new FloppyFlushUI(displayFrame)
  protected val driveLeds = (for(d <- 0 until TOTAL_DRIVES) yield {
    val led = new DriveLed(d + 8)
    led.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = attachDisk(d,false,isC64Mode)
    })
    led
  }).toArray
  protected val floppyComponents = Array.ofDim[FloppyComponent](TOTAL_DRIVES)
  protected val driveLedListeners = {
    (for(d <- 0 until TOTAL_DRIVES) yield {
      new AbstractDriveLedListener(driveLeds(d)) {
        if (d > 0) driveLeds(d).setVisible(false)
      }
    }).toArray
  }
  protected val flyerIEC = new FlyerIEC(bus,file => attachDiskFile(0,file,false,None))
  protected var isFlyerEnabled = false
  protected val drives : Array[Drive with TraceListener] = Array.ofDim(TOTAL_DRIVES)
  protected var device12Drive : Drive = _
  protected var device12DriveEnabled = false
  protected var canWriteOnDisk = true
  // -------------------- TAPE -----------------
  protected var datassette : Datassette = _
  // ----------------- RS-232 ------------------
  protected val rs232 = BridgeRS232
  protected val AVAILABLE_RS232 : Array[RS232] = Array(//UP9600,
    TelnetRS232,
    TCPRS232,
    FileRS232,
    SwiftLink.getSL(nmiSwitcher.setLine(Switcher.CRT,_),None),
    SwiftLink.getSL(nmiSwitcher.setLine(Switcher.CRT,_),Some(REU.getREU(REU.REU_1750,mmu,dmaSwitcher.setLine(Switcher.CRT,_),irqSwitcher.setLine(Switcher.CRT,_),None))),
    ProcessRS232)
  // -------------------- PRINTER --------------
  protected var printerEnabled = false
  protected val printerGraphicsDriver = new MPS803GFXDriver(new MPS803ROM)
  protected val printer = new MPS803(bus,printerGraphicsDriver)
  protected lazy val printerDialog = {
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
  protected lazy val volumeDialog : JDialog = VolumeSettingsPanel.getDialog(displayFrame,sid.getDriver)
  // ------------ Control Port -----------------------
  protected lazy val gameControlPort = new controlport.GamePadControlPort(configuration)
  protected val keypadControlPort = controlport.ControlPort.keypadControlPort
  protected lazy val keyboardControlPort = controlport.ControlPort.userDefinedKeyControlPort(configuration)
  protected val controlPortA = new controlport.ControlPortBridge(keypadControlPort,"Control Port 1")
  protected lazy val controlPortB = new controlport.ControlPortBridge(gameControlPort,"Control port 2")
  // -------------- Light Pen -------------------------
  protected val LIGHT_PEN_NO_BUTTON = 0
  protected val LIGHT_PEN_BUTTON_UP = 1
  protected val LIGHT_PEN_BUTTON_LEFT = 2
  protected var lightPenButtonEmulation = LIGHT_PEN_NO_BUTTON

  protected class LightPenButtonListener extends MouseAdapter with CBMComponent {
    val componentID = "Light pen"
    val componentType = CBMComponentType.INPUT_DEVICE

    override def mousePressed(e:MouseEvent) : Unit = {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case LIGHT_PEN_BUTTON_UP => controlPortB.emulateUp
        case LIGHT_PEN_BUTTON_LEFT => controlPortB.emulateLeft
      }
    }
    override def mouseReleased(e:MouseEvent) : Unit = {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case _ => controlPortB.releaseEmulated
      }
    }

    def init  : Unit = {}
    def reset  : Unit = {}
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {}
    protected def loadState(in:ObjectInputStream) : Unit = {}
    protected def allowsStateRestoring : Boolean = true
  }
  // ------------------------------------ Drag and Drop ----------------------------
  protected val DNDHandler = new DNDHandler(handleDND(_,true,true))
  // ------------------- INITIALIZATION ------------------------
  initComputer

  protected def initComputer : Unit = {
    ExpansionPort.setExpansionPortStateHandler(expansionPortStateHandler _)
  }

  override def afterInitHook : Unit = {
    inspectDialog = InspectPanel.getInspectDialog(displayFrame, this)
    // deactivate drives > 8
    for(d <- 1 until TOTAL_DRIVES) {
      drives(d).setActive(false)
      driveLeds(d).setVisible(false)
    }
  }
  // -----------------------------------------------------------

  protected def isC64Mode : Boolean = true

  def turnOn(args:Array[String]) : Unit

  def turnOff : Unit = {
    if (!headless) saveSettings(preferences[Boolean](Preferences.PREF_PREFAUTOSAVE).getOrElse(false))
    for (d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }

  protected val tapeAllowed = true

  protected def getRAM : Memory
  protected def getCharROM : Memory

  protected def setDMA(dma:Boolean) : Unit

  protected def mainLoop(cycles:Long) : Unit

  protected def reset(play:Boolean=true,loadAndRunLastPrg:Boolean = false) : Unit = {
    if (traceDialog != null) traceDialog.forceTracing(false)
    if (diskTraceDialog != null) diskTraceDialog.forceTracing(false)
    if (Thread.currentThread != Clock.systemClock) clock.pause
    resetComponent
    if (loadAndRunLastPrg) lastLoadedPrg.foreach( f =>
      clock.schedule(new ClockEvent("RESET_PRG",clock.currentCycles + PRG_RUN_DELAY_CYCLES,(cycles) => loadPRGFile(f,true)))
    )

    if (play) clock.play
  }

  protected def hardReset(play:Boolean=true) : Unit = {
    if (traceDialog != null) traceDialog.forceTracing(false)
    if (diskTraceDialog != null) diskTraceDialog.forceTracing(false)
    if (Thread.currentThread != Clock.systemClock) clock.pause
    hardResetComponent

    if (play) clock.play
  }

  protected def loadPRGFile(file:File,autorun:Boolean) : Unit

  protected def saveSettings(save:Boolean) : Unit

  protected def setSettingsMenu(optionsMenu:JMenu) : Unit

  protected def paste : Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      Keyboard.insertTextIntoKeyboardBuffer(str,mmu,isC64Mode)
    }
  }

  protected def setDisplayRendering(hints:java.lang.Object) : Unit = {
    display.setRenderingHints(hints)
  }

  protected def savePrg : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
        val (start,end) = ProgramLoader.savePRG(fc.getSelectedFile,mmu,isC64Mode)
        Log.info(s"BASIC program saved from $start to $end")
      case _ =>
    }
  }

  protected def errorHandler(t:Throwable) : Unit = {
    import CPU65xx.CPUJammedException
    t match {
      case j:CPUJammedException if !cpujamContinue =>
        JOptionPane.showConfirmDialog(displayFrame,
          s"CPU[${j.cpuID}] jammed at " + Integer.toHexString(j.pcError) + ". Do you want to open debugger (yes), reset (no) or continue (cancel) ?",
          "CPU jammed",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.ERROR_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            if (traceDialog != null) traceDialog.forceTracing(true)
            trace(true,true)
          case JOptionPane.CANCEL_OPTION => // continue
          case _ =>
            reset(true)
        }
      case _:CPUJammedException => // continue
      case _ =>
        Log.info("Fatal error occurred: " + cpu + "-" + t)
        try Log.info(CPU65xx.disassemble(mmu,cpu.getCurrentInstructionPC).toString) catch { case _:Throwable => }
        t.printStackTrace(Log.getOut)
        t.printStackTrace
        if (headless) {
          println(s"Fatal error occurred on cycle ${clock.currentCycles}: $cpu\n${CPU65xx.disassemble(mmu,cpu.getCurrentInstructionPC)}")
          t.printStackTrace
          sys.exit(1)
        } // exit if headless
        JOptionPane.showMessageDialog(displayFrame,t.toString + " [PC=" + Integer.toHexString(cpu.getCurrentInstructionPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
        //trace(true,true)
        reset(true)
    }
  }

  protected def loadSettings(args:Array[String]) : Unit = {
    def loadFile(fn:String) : Unit = {
      val cmd = if (isC64Mode) s"""LOAD"$fn",8,1""" + 13.toChar + "RUN" + 13.toChar else s"""RUN"$fn"""" + 13.toChar
      clock.schedule(new ClockEvent("Loading",clock.currentCycles + PRG_RUN_DELAY_CYCLES,_ => Keyboard.insertTextIntoKeyboardBuffer(cmd,mmu,isC64Mode) ))
    }
    // AUTOPLAY
    preferences.parseAndLoad(args,configuration) match {
      case None =>
        // run the given file name
        preferences[String](Preferences.PREF_RUNFILE) match {
          case Some(fn) if fn != null && fn != "" =>
            loadFile(fn)
          case _ =>
        }
      case Some(f) =>
        preferences[String](Preferences.PREF_DRIVE_X_FILE(0)) match {
          case None =>
            handleDND(new File(f),false,true)
          case Some(_) =>
            // here we have both drive8 and PRG set: we load the given PRG file from disk 8
            val fn = new File(f).getName
            val dot = fn.indexOf('.')
            val cbmFile = if (dot > 0) fn.substring(0,dot) else f
            loadFile(cbmFile)
        }
    }
    DrivesConfigPanel.registerDrives(displayFrame,drives,setDriveType(_,_,false),enableDrive(_,_,true),attachDisk(_,_,isC64Mode),attachDiskFile(_,_,_,None),drivesEnabled)
  }

  protected def attachDiskFile(driveID:Int,file:File,autorun:Boolean,fileToLoad:Option[String],emulateInserting:Boolean = true) : Unit = {
    try {
      if (!file.exists) throw new FileNotFoundException(s"Cannot attach file $file on drive ${driveID + 8}: file not found")
      if (!file.isDirectory) {
        val validExt = drives(driveID).formatExtList.exists { ext => file.toString.toUpperCase.endsWith(ext) }
        if (!validExt) throw new IllegalArgumentException(s"$file cannot be attached to disk, format not valid")
      }
      val isD64 = file.getName.toUpperCase.endsWith(".D71") || file.getName.toUpperCase.endsWith(".D64") || file.isDirectory

      val disk = if (file.isDirectory) D64LocalDirectory.createDiskFromLocalDir(file) else Diskette(file.toString)
      disk.canWriteOnDisk = canWriteOnDisk
      disk.flushListener = diskFlusher
      drives(driveID).getFloppy.close
      if (traceDialog != null && !traceDialog.isTracing) clock.pause
      drives(driveID).setDriveReader(disk,emulateInserting)
      preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_FILE(driveID),file.toString)
      clock.play

      loadFileItems(driveID).setEnabled(isD64)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      val drive = driveID + 8
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""LOAD"$fn",$drive,1""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          Keyboard.insertTextIntoKeyboardBuffer(cmd,mmu,isC64Mode)
        case None if autorun =>
          Keyboard.insertSmallTextIntoKeyboardBuffer(s"""LOAD"*",$drive,1""" + 13.toChar + "RUN" + 13.toChar,mmu,isC64Mode)
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

  protected def enableDrive(id:Int,enabled:Boolean,updateFrame:Boolean) : Unit = {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    driveLeds(id).setVisible(enabled)
    preferences(Preferences.PREF_DRIVE_X_ENABLED(id)) = enabled
  }

  protected def makeInfoPanel(includeTape:Boolean) : JPanel = {
    val infoPanel = new JPanel(new BorderLayout)
    val rowPanel = new JPanel()
    rowPanel.setLayout(new BoxLayout(rowPanel,BoxLayout.Y_AXIS))
    for(d <- 0 until TOTAL_DRIVES) {
      val row1Panel = new JPanel(new FlowLayout(FlowLayout.RIGHT,5,2))
      rowPanel.add(row1Panel)
      if (d == 0 && includeTape) {
        val tapePanel = new TapeState(datassette)
        datassette.setTapeListener(tapePanel)
        row1Panel.add(tapePanel)
        row1Panel.add(tapePanel.progressBar)
      }
      row1Panel.add(driveLeds(d))
    }
    infoPanel.add("East",rowPanel)
    infoPanel
  }

  protected def swing(f: => Unit) : Unit = SwingUtilities.invokeAndWait(() => f)

  protected def loadState(fileName:Option[String]) : Unit = {
    clock.pause
    var in : ObjectInputStream = null

    try {
      val canLoad = allowsState
      if (!canLoad) {

        showError("State saving error","Can't load state")
        return
      }
      val fn = fileName match {
        case None =>
          val fc = new JFileChooser
          fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
          fc.setFileFilter(new FileFilter {
            def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
            def getDescription = "Kernal64 state files"
          })
          fc.setDialogTitle("Choose a state file to load")
          fc.showOpenDialog(getActiveFrame.get) match {
            case JFileChooser.APPROVE_OPTION =>
              Some(fc.getSelectedFile)
            case _ =>
              None
          }
        case Some(fn) =>
          Some(new File(fn))
      }

      var loaded = false
      var asked = false
      var aborted = !fn.isDefined

      while (!loaded && !aborted) try {
        in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(fn.get)))
        for(i <- 0 until APPLICATION_NAME.length) if (in.readChar != APPLICATION_NAME(i)) throw new IOException(s"Bad header: expected $APPLICATION_NAME")
        val ver = in.readObject.asInstanceOf[String]
        val ts = in.readLong
        if (!loadStateFromOptions && !asked) {
          val msg = s"<html><b>Version:</b> $ver<br><b>Date:</b> ${new java.util.Date(ts)}</html>"
          JOptionPane.showConfirmDialog(displayFrame, msg, "State loading confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
            case JOptionPane.YES_OPTION =>
              asked = true
              resetSettings
              reset(false)
              load(in)
              loaded = true
            case _ =>
              aborted = true
          }
        }
        else {
          reset(false)
          load(in)
          loaded = true
        }
      }
      catch {
        case dm: DriveIDMismatch =>
          in.close
          reset(false)
          val newDriveType = if (dm.expectedDriveComponentID.startsWith("C1541")) DriveType._1541
          else if (dm.expectedDriveComponentID.startsWith("D1571")) DriveType._1571
          else if (dm.expectedDriveComponentID.startsWith("D1581")) DriveType._1581
          else throw new IllegalArgumentException(s"Bad drive component type: ${dm.expectedDriveComponentID}")
          initDrive(dm.driveID,newDriveType)
      }
    }
    catch {
      case t:Throwable =>
        showError(s"State loading error",s"Can't load state: ${t.getMessage}")
        t.printStackTrace
        reset(false)
    }
    finally {
      if (in != null) in.close
      clock.play
    }
  }

  protected def resetSettings : Unit = {
    for(a <- resetSettingsActions) a()
  }

  protected def saveState() : Unit = {
    clock.pause
    var out : ObjectOutputStream = null
    try {
      val canSave = allowsState
      if (!canSave) {

        showError("State saving error","Can't save state")
        return
      }
      val fc = new JFileChooser
      fc.setDialogTitle("Choose where to save current state")
      fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
      fc.setFileFilter(new FileFilter {
        def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
        def getDescription = "Kernal64 state files"
      })
      val fn = fc.showSaveDialog(getActiveFrame.get) match {
        case JFileChooser.APPROVE_OPTION =>
          if (fc.getSelectedFile.getName.toUpperCase.endsWith(".K64")) fc.getSelectedFile.toString else fc.getSelectedFile.toString + ".k64"
        case _ =>
          return
      }
      out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(fn)))
      out.writeChars(APPLICATION_NAME)
      out.writeObject(ucesoft.cbm.Version.VERSION)
      out.writeLong(System.currentTimeMillis)
      save(out)
      out.close
    }
    catch {
      case t:Throwable =>

        showError("State saving error","Can't save state. Unexpected error occurred: " + t)
        t.printStackTrace
    }
    finally {
      if (out != null) out.close
      clock.play
    }
  }

  def expansionPortStateHandler(in:ObjectInputStream,portType:ExpansionPortType.Value) : Unit = {
    import ExpansionPortType._
    import Preferences._

    portType match {
      case CRT =>
        val crtFile = Cartridge.createCRTFileFromState(in)
        loadCartridgeFile(crtFile,true)
      case CPM =>
        preferences(PREF_CPMCARTENABLED) = true
      case GEORAM =>
        preferences(PREF_GEORAM) = in.readInt.toString
      case REU =>
        preferences(PREF_REUTYPE) = in.readInt.toString
      case DUALSID =>
        preferences(PREF_DUALSID) = in.readInt.toString
      case _ =>
    }
  }

  // ------------------------------- TRACE LISTENER ------------------------------------------
  def setTraceListener(tl:Option[TraceListener]) : Unit = {
    tl match {
      case None =>
        if (traceDialog != null) traceDialog.traceListener = cpu
      case Some(t) =>
        if (traceDialog != null) traceDialog.traceListener = t
    }
  }
  // ------------------------------------------------------------------------------------------
  protected def handleDND(file:File,_reset:Boolean,autorun:Boolean) : Unit = {
    val name = file.getName.toUpperCase
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
    else {
      if (_reset) reset(false)
      if (autorun) {
        clock.schedule(new ClockEvent("Loading",clock.currentCycles + PRG_RUN_DELAY_CYCLES,(cycles) => { attachDevice(file,true,None,false) }))
        clock.play
      }
      else {
        attachDevice(file,false)
      }
    }
  }

  def attachDevice(file:File) : Unit = attachDevice(file,false)

  protected def attachPRGAsDisk(file:File,autorun:Boolean = true) : Unit = {
    val name = file.getName.toUpperCase
    val disk = File.createTempFile("prgdrive8",".d64")
    disk.deleteOnExit()
    Diskette.makeEmptyDisk(disk.toString)
    val d64 = new D64_D71(disk.toString,false)
    d64.rename("AUTOSTART")
    val in = new java.io.FileInputStream(file)
    val prg = in.readAllBytes()
    in.close
    val startAddress = prg(0) | prg(1) << 8
    val content = prg.drop(2)
    val prgName = name.dropRight(4)
    d64.addPRG(content,prgName,startAddress)
    d64.close
    disk8LoadedAsPRG = true
    attachDiskFile(0,disk,autorun,Some(prgName),false)
  }

  protected def attachDevice(file:File,autorun:Boolean,fileToLoad:Option[String] = None,emulateInserting:Boolean = true) : Unit = {
    val name = file.getName.toUpperCase

    if (name.endsWith(".PRG") && loadPRGasDisk) attachPRGAsDisk(file)
    else
    if (name.endsWith(".PRG")) {
      loadPRGFile(file,autorun)
      lastLoadedPrg = Some(file)
    }
    else
    if (name.endsWith(".D64") || name.endsWith(".G64") || name.endsWith(".D71") || name.endsWith(".D81")) attachDiskFile(0,file,autorun,fileToLoad,emulateInserting)
    else
    if (tapeAllowed && name.endsWith(".TAP")) attachTapeFile(file,None,autorun)
    else
    if (name.endsWith(".T64")) attachT64File(file,autorun)
    else
    if (name.endsWith(".ZIP")) attachZIPFile(file,autorun)
    else
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
  }

  protected def printerSaveImage  : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(printerDialog) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        printerGraphicsDriver.saveAsPNG(file)
      case _ =>
    }
  }

  protected def attachZip  : Unit = {
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

  protected def loadCartridgeFile(file:File,stateLoading : Boolean = false) : Unit = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause
      ExpansionPort.getExpansionPort.eject
      val ep = ExpansionPortFactory.loadExpansionPort(file.toString,irqSwitcher.setLine(Switcher.CRT,_),nmiSwitcher.setLine(Switcher.CRT,_),getRAM,mmu,configuration)
      println(ep)
      cartMenu.setVisible(true)
      ExpansionPort.setExpansionPort(ep)
      ExpansionPort.currentCartFileName = file.toString
      Log.info(s"Attached cartridge ${ExpansionPort.getExpansionPort.name}")
      preferences.updateWithoutNotify(Preferences.PREF_CART,file.toString)
      if (!stateLoading) reset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
      // easyflash
      easyFlashWriteChangesItem.setEnabled(ep.isInstanceOf[EasyFlash])
      // GMOD3
      GMOD3WriteChangesItem.setEnabled(ep.isInstanceOf[GMOD3])
    }
    catch {
      case t:Throwable =>
        if (traceDialog != null) t.printStackTrace(traceDialog.logPanel.writer)

        showError("Cartridge loading error",t.toString)
    }
    finally {
      if (!stateLoading) clock.play
    }
  }

  protected def attachLocalDir(driveID:Int) : Unit = {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc,getCharROM,isC64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setDialogTitle(s"Attach local directory to drive ${driveID + 8}")
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID,fc.getSelectedFile,false,canvas.selectedFile)
      case _ =>
    }
  }

  protected def attachDisk(driveID:Int,autorun:Boolean,c64Mode:Boolean) : Unit = {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc,getCharROM,c64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setDialogTitle(s"Attach disk to drive ${driveID + 8}")
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

  protected def loadFileFromAttachedFile(driveID:Int,relocate:Boolean,c64Mode:Boolean) : Unit = {
    val floppy = drives(driveID).getFloppy
    if (floppy.isEmpty) showError("Loading error","No disk attached!")
    else {
      Option(JOptionPane.showInputDialog(displayFrame,"Load file","*")) match {
        case None =>
        case Some(fileName) =>
          try {
            floppy.asInstanceOf[Diskette].loadInMemory(mmu,fileName,relocate,true,driveID + 8)
          }
          catch {
            case t:Throwable =>

              showError("Loading error","Error while loading from disk: " + t.getMessage)
          }
      }
    }
  }

  protected def loadFileFromTape  : Unit = {
    val fc = new JFileChooser
    val canvas = new T64Canvas(fc,getCharROM,isC64Mode)
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

  protected def attachT64File(file:File,autorun:Boolean) : Unit = {
    val tape = new T64(file.toString)
    try {
      val values = tape.entries map { e => e.asInstanceOf[Object] }
      JOptionPane.showInputDialog(displayFrame,"Select file to open:","Open file in " + file,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
        case null =>
        case entry:T64Entry =>
          tape.loadInMemory(mmu,entry)
          configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
          if (autorun) {
            Keyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,true)
          }
      }
    }
    finally {
      tape.close
    }
  }

  protected def attachZIPFile(file:File,autorun:Boolean) : Unit = {
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

        showError(s"Error while opening zip file $file",t.toString)
    }
  }

  protected def attachTape  : Unit = {
    val fc = new JFileChooser
    val canvas = new TAPCanvas(fc,getCharROM,isC64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP")
      def getDescription = "TAP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val tapFile = canvas.selectedObject.asInstanceOf[Option[TAP.TAPHeader]]
        attachTapeFile(fc.getSelectedFile,tapFile,tapFile.isDefined)
      case _ =>
    }
  }

  protected def attachTapeFile(file:File,tapFile:Option[TAP.TAPHeader],autorun:Boolean) : Unit = {
    val tap = new TAP(file.toString)
    datassette.setTAP(Some(tap),tapFile.map(_.tapOffset.toInt))
    tapeMenu.setEnabled(true)
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      datassette.pressPlay
      Keyboard.insertSmallTextIntoKeyboardBuffer("LOAD" + 13.toChar + "RUN" + 13.toChar,mmu,isC64Mode)
    }
  }

  protected def loadPrg  : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
      def getDescription = "PRG files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        if (loadPRGasDisk) attachPRGAsDisk(fc.getSelectedFile,false)
        else {
          loadPRGFile(fc.getSelectedFile, false)
          lastLoadedPrg = Some(fc.getSelectedFile)
        }
      case _ =>
    }
  }

  protected def detachCtr  : Unit = {
    if (ExpansionPort.getExpansionPort.isEmpty) showError("Detach error","No cartridge attached!")
    else {
      if (Thread.currentThread != Clock.systemClock) clock.pause
      ExpansionPort.getExpansionPort.eject
      ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      reset(true)
    }
    detachCtrItem.setEnabled(false)
    cartMenu.setVisible(false)
    // easyflash
    easyFlashWriteChangesItem.setEnabled(false)
    // GMOD 3
    GMOD3WriteChangesItem.setEnabled(false)
    ExpansionPort.currentCartFileName = ""
  }

  protected def attachCtr  : Unit = {
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

  protected def initializedDrives(defaultDriveType:DriveType.Value) : Unit = {
    for(d <- 0 until TOTAL_DRIVES) {
      initDrive(d,defaultDriveType)
      if (d > 0) drivesEnabled(d) = false
      floppyComponents(d) = new FloppyComponent(8 + d,drives(d),driveLeds(d))
      add(floppyComponents(d))
      add(driveLeds(d))
    }
  }

  protected def initDrive(id:Int,driveType:DriveType.Value) : Unit = {
    val old = Option(drives(id))
    old match {
      case Some(od) if od.driveType == driveType => return
      case _ =>
    }
    drives(id) = driveType match {
      case DriveType._1571 =>
        driveLedListeners(id).setPowerLedMode(false)
        new D1571(id,bus,driveLedListeners(id),_ => {})
      case DriveType._1541 =>
        driveLedListeners(id).setPowerLedMode(false)
        new C1541(id,bus,driveLedListeners(id))
      case DriveType._1581 =>
        driveLedListeners(id).setPowerLedMode(true)
        new D1581(id,bus,driveLedListeners(id))
    }

    old match {
      case None =>
        add(drives(id))
      case Some(c) =>
        floppyComponents(id).drive = drives(id)
        c.getFloppy.close
        c.disconnect
        drivesRunning(id) = true
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

  protected def loadKeyboard  : Unit = {
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

                showError("Keyboard..","Invalid keyboard layout file")
            }
            finally {
              in.close
            }
          case _ =>
        }
      case JOptionPane.CANCEL_OPTION =>
    }
  }

  protected def writeOnDiskSetting(enabled:Boolean) : Unit = {
    canWriteOnDisk = enabled
    for(d <- 0 until TOTAL_DRIVES) drives(d).getFloppy.canWriteOnDisk = canWriteOnDisk
  }

  protected def enableDrive12(enabled:Boolean, fn:Option[String]) : Unit = {
    if (enabled) {
      device12Drive = new LocalDrive(bus,12)
      changeLocalDriveDir(fn)
    }
    device12DriveEnabled = enabled
  }

  protected def changeLocalDriveDir(fileName:Option[String] = None) : Unit = {
    fileName match {
      case None =>
        val fc = new JFileChooser
        fc.setCurrentDirectory(device12Drive.asInstanceOf[LocalDrive].getCurrentDir)
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
        fc.setDialogTitle("Choose the current device 12 local directory")
        fc.showOpenDialog(displayFrame) match {
          case JFileChooser.APPROVE_OPTION =>
            device12Drive.asInstanceOf[LocalDrive].setCurrentDir(fc.getSelectedFile)
            preferences.updateWithoutNotify(Preferences.PREF_DRIVE12LOCALPATH,fc.getSelectedFile.toString)
          case _ =>
        }
      case Some(fn) =>
        device12Drive.asInstanceOf[LocalDrive].setCurrentDir(new File(fn))
        preferences.updateWithoutNotify(Preferences.PREF_DRIVE12LOCALPATH,fn)
    }
  }

  protected def enableMouse(mouseEnabled:Boolean,display:vic.Display) : Unit = {
    controlPortA.setMouse1351Emulation(mouseEnabled)
    sid.setMouseEnabled(mouseEnabled)
    if (mouseEnabled) MouseCage.enableMouseCageOn(display) else MouseCage.disableMouseCage
  }

  protected def enablePrinter(enable:Boolean) : Unit = {
    printerEnabled = enable
    printer.setActive(enable)
  }

  protected def setDriveType(drive:Int,dt:DriveType.Value,dontPlay:Boolean = false) : Unit = {
    clock.pause
    initDrive(drive,dt)
    preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_TYPE(drive),dt.toString)
    if (!dontPlay) clock.play
  }

  protected def setGeoRAM(enabled:Boolean,size:Int = 0): Unit = {
    if (enabled) ExpansionPort.setExpansionPort(new GeoRAM(size)) else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
  }

  protected def setREU(reu:Option[Int],reu16FileName:Option[String]) : Unit = {
    reu match {
      case None =>
        ExpansionPort.getExpansionPort.eject
        ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
      case Some(REU.REU_16M) =>
        val reu = REU.getREU(REU.REU_16M,mmu,dmaSwitcher.setLine(Switcher.CRT,_),irqSwitcher.setLine(Switcher.CRT,_),reu16FileName map { new File(_) } )
        ExpansionPort.setExpansionPort(reu)
        reu16FileName match {
          case Some(file) => REU.attached16MFileName = file
          case None =>
        }
      case Some(reuSize) =>
        ExpansionPort.setExpansionPort(REU.getREU(reuSize,mmu,dmaSwitcher.setLine(Switcher.CRT,_),irqSwitcher.setLine(Switcher.CRT,_),None))
    }
  }

  protected def showKeyboardEditor(c64Mode:Boolean): Unit = {
    val source = configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE,java.awt.im.InputContext.getInstance().getLocale.getLanguage.toUpperCase())
    val kbef = new JFrame(s"Keyboard editor ($source)")
    val kbe = new KeyboardEditor(keyb,keyb.getKeyboardMapper,c64Mode)
    kbef.getContentPane.add("Center",kbe)
    kbef.pack
    kbef.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    kbef.setVisible(true)
  }

  protected def warpMode(warpOn:Boolean,play:Boolean = true): Unit = {
    maxSpeedItem.setSelected(warpOn)
    clock.maximumSpeed = warpOn
    if (play) clock.pause
    sid.setFullSpeed(warpOn)
    if (play) clock.play
  }

  protected def setLightPen(setting:Int): Unit = {
    lightPenButtonEmulation = setting
    vicChip.enableLightPen(setting != LIGHT_PEN_NO_BUTTON)
    controlPortB.setLightPenEmulation(setting != LIGHT_PEN_NO_BUTTON)
  }

  protected def choose16MREU: Unit = {
    import Preferences._
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
          preferences.updateWithoutNotify(PREF_REUTYPE,if (fc.getSelectedFile == null) REU.REU_16M.toString else s"${REU.REU_16M},${fc.getSelectedFile.toString}")
        }
        catch {
          case t:Throwable =>
            showError("REU loading error",t.toString)
        }
      case _ =>
    }
  }

  protected def setDualSID(address:Option[Int]): Unit = {
    DualSID.setDualSID(address,sid)
  }

  protected def setRemote(source:Option[JRadioButtonMenuItem]): Unit = {
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

              showError("Remoting init error",io.toString)
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

  protected def enableFlyer(enabled:Boolean): Unit = {
    if (enabled != isFlyerEnabled) flyerIEC.reset
    isFlyerEnabled = enabled
  }

  protected def chooseFlyerDir: Unit = {
    val fc = new JFileChooser
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.setCurrentDirectory(flyerIEC.getFloppyRepository)
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        flyerIEC.setFloppyRepository(fc.getSelectedFile)
      case _ =>
    }
  }

  protected def setDigiMax(on:Boolean,address:Option[Int]): Unit = {
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

  protected def chooseDigiMaxSampleRate: Unit = {
    Option(JOptionPane.showInputDialog(displayFrame,"DigiMax sample rate Hz",DigiMAX.getSampleRate.toString)) match {
      case None =>
      case Some(rate) =>
        DigiMAX.setSampleRate(rate.toInt)
    }
  }

  protected def chooseGMod2: Unit = {
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

  protected def enableCPMCart(enabled:Boolean): Unit = {
    import Preferences._
    ExpansionPort.getExpansionPort.eject
    if (enabled) {
      ExpansionPort.setExpansionPort(new ucesoft.cbm.expansion.cpm.CPMCartridge(mmu,dmaSwitcher.setLine(Switcher.CRT,_),setTraceListener _))
      detachCtrItem.setEnabled(true)
    }
    else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    preferences.updateWithoutNotify(PREF_CPMCARTENABLED,enabled)
  }

  protected def manageRS232 : Unit = {
    RS232ConfigPanel.RS232ConfigDialog.setVisible(true)
  }

  protected def makeDisk : Unit = {
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
            showError("Disk making error",t.toString)
        }
      case _ =>
    }
  }

  def play(file:File) = {
    //ExpansionPort.getExpansionPort.eject
    //ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    handleDND(file,true,true)
  }

  protected def showAbout  : Unit = {
    val about = new AboutCanvas(getCharROM,ucesoft.cbm.Version.VERSION.toUpperCase + " (" + ucesoft.cbm.Version.BUILD_DATE.toUpperCase + ")")
    JOptionPane.showMessageDialog(displayFrame,about,"About",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  protected def showSettings : Unit = {
    val settingsPanel = new SettingsPanel(preferences)
    JOptionPane.showMessageDialog(displayFrame,settingsPanel,"Settings",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  protected def trace(cpu:Boolean,on:Boolean) : Unit = {
    if (traceDialog == null) return

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

  protected def ejectDisk(driveID:Int) : Unit = {
    drives(driveID).getFloppy.close
    driveLeds(driveID).setToolTipText("")
    if (traceDialog != null && !traceDialog.isTracing) clock.pause
    if (drives(driveID).driveType == DriveType._1581) drives(driveID).setDriveReader(D1581.MFMEmptyFloppy,true)
    else drives(driveID).setDriveReader(EmptyFloppy,true)
    loadFileItems(driveID).setEnabled(false)
    preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_FILE(driveID),"")
    clock.play
  }

  protected def showPrinterPreview : Unit = {
    printerGraphicsDriver.checkSize
    printerDialog.setVisible(true)
  }

  protected def configureJoystick : Unit = {
    def getControlPortFor(id:String) = configuration.getProperty(id) match {
      case CONFIGURATION_KEYPAD_VALUE => keypadControlPort
      case CONFIGURATION_JOYSTICK_VALUE => gameControlPort
      case CONFIGURATION_KEYBOARD_VALUE => keyboardControlPort
      case _ => controlport.ControlPort.emptyControlPort
    }

    controlPortA.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_2)
    controlPortB.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_1)
    keyboardControlPort.updateConfiguration
  }

  protected def joySettings : Unit = {
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

  protected def swapJoysticks : Unit = {
    val j1 = configuration.getProperty(CONFIGURATION_JOY_PORT_1)
    val j2 = configuration.getProperty(CONFIGURATION_JOY_PORT_2)
    if (j2 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_1,j2) else configuration.remove(CONFIGURATION_JOY_PORT_1)
    if (j1 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_2,j1) else configuration.remove(CONFIGURATION_JOY_PORT_2)
    configureJoystick
  }

  protected def setVicFullScreen: Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(displayFrame,
      display,
      vicChip.SCREEN_WIDTH,
      vicChip.SCREEN_HEIGHT,
      keypadControlPort,
      keyb,
      keypadControlPort,
      keyboardControlPort)
  }

  protected def openGIFRecorder : Unit = gifRecorder.setVisible(true)

  protected def vicZoom(f:Int) : Unit = {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f,vicChip.VISIBLE_SCREEN_HEIGHT * f)
    vicZoomFactor = f
    updateVICScreenDimension(dim)
  }

  protected def updateVICScreenDimension(dim:Dimension): Unit = {
    display.setPreferredSize(dim)
    display.invalidate
    display.repaint()
    displayFrame.pack
    if (vicChip.VISIBLE_SCREEN_WIDTH == dim.width && vicChip.VISIBLE_SCREEN_HEIGHT == dim.height) vicZoomFactor = 1
    else
    if (vicChip.VISIBLE_SCREEN_WIDTH * 2 == dim.width && vicChip.VISIBLE_SCREEN_HEIGHT * 2 == dim.height) vicZoomFactor = 2
    else vicZoomFactor = 0 // undefined
  }

  protected def setVICModel(model:VICType.Value,preserveDisplayDim:Boolean = false,resetFlag:Boolean,play:Boolean = true) : Unit = {
    if (play) clock.pause
    val vicType = model match {
      case VICType.PAL => VIC_PAL
      case VICType.NTSC => VIC_NTSC
    }
    vicChip.setVICModel(vicType)
    clock.setClockHz(vicType.CPU_FREQ)
    display.setNewResolution(vicChip.SCREEN_HEIGHT,vicChip.SCREEN_WIDTH)
    vicChip.setDisplay(display)
    if (!preserveDisplayDim) {
      if (vicZoomFactor > 0) vicZoom(vicZoomFactor)
      display.invalidate()
      displayFrame.pack
    }

    if (resetFlag) reset()
    if (play) clock.play
  }

  // -------------------------------------------------------------------
  protected def setMenu : Unit = {

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

    setFileMenu(fileMenu)
    setEditMenu(editMenu)
    setStateMenu(stateMenu)
    setTraceMenu(traceMenu)
    setSettingsMenu(optionMenu)
    setGameMenu(gamesMenu)
    setHelpMenu(helpMenu)

    val cartInfoItem = new JMenuItem("Cart info ...")
    cartInfoItem.addActionListener(_ => showCartInfo )
    cartMenu.add(cartInfoItem)
    val cartButtonItem = new JMenuItem("Press cartridge button...")
    cartButtonItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z,java.awt.event.InputEvent.ALT_DOWN_MASK))
    cartButtonItem.addActionListener(_ => cartButtonRequested = true )
    cartMenu.add(cartButtonItem)

    displayFrame.setJMenuBar(menuBar)

    setGlobalCommandLineOptions

  }

  protected def setGlobalCommandLineOptions : Unit = {
    import Preferences._
    // non-saveable settings
    preferences.add(PREF_WARP,"Run warp mode",false) { w =>
      val isAdjusting = preferences.get(PREF_WARP).get.isAdjusting
      warpMode(w,!isAdjusting)
    }
    preferences.add(PREF_HEADLESS,"Activate headless mode",false,Set(),false) { headless = _ }
    preferences.add(PREF_TESTCART,"Activate testcart mode",false,Set(),false) { TestCart.enabled = _ }
    preferences.add(PREF_LIMITCYCLES,"Run at most the number of cycles specified","",Set(),false) { cycles =>
      if (cycles != "" && cycles.toLong > 0) clock.limitCyclesTo(cycles.toLong)
    }
    preferences.add(PREF_RUNFILE,"Run the given file taken from the attached disk",null:String,Set(),false) { file => }
    preferences.add(PREF_SCREENSHOT,"Take a screenshot of VIC screen and save it on the given file path. Used with --testcart only.","") { file =>
      if (file != "") {
        TestCart.screenshotFile = Some(file)
        TestCart.screeshotHandler = display.waitFrameSaveSnapshot _
      }
    }
    preferences.add(PREF_CPUJAMCONTINUE,"On cpu jam continue execution",false,Set(),false) { cpujamContinue = _ }
    preferences.add(PREF_CIAMODEL,
         "Set the CIA model (both cia1 and cia2). 6526 for old cia, 8521 for the new one. Default is 6526.",
             "6526",Set("6526","8521")) { model =>
      val cm = if (model == "8521") CIA.CIA_MODEL_8521 else CIA.CIA_MODEL_6526
      cia1.setCIAModel(cm)
      cia2.setCIAModel(cm)
    }
    preferences.add(PREF_LOADSTATE,"Load a previous saved state.","",Set(),false) { file =>
      if (file != "") {
        try {
          loadStateFromOptions = true
          loadState(Some(file))
        }
        finally loadStateFromOptions = false
      }
    }
    preferences.add(PREF_SCREENDIM,"Zoom factor. Valued accepted are 1 and 2",0,Set(1,2),false) { dim =>
      vicZoom(dim)
      zoomOverride = true
    }
    preferences.add(PREF_FULLSCREEN,"Starts the emulator in full screen mode",false,Set(),false) { fullScreenAtBoot = _ }
    preferences.add(PREF_IGNORE_CONFIG_FILE,"Ignore configuration file and starts emulator with default configuration",false,Set(),false) { ignoreConfig = _ }
    preferences.add(PREF_KERNEL,"Set kernel rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.C64_KERNAL_ROM_PROP,file) }
    preferences.add(PREF_BASIC,"Set basic rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.C64_BASIC_ROM_PROP,file) }
    preferences.add(PREF_CHARROM,"Set char rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.C64_CHAR_ROM_PROP,file) }
    preferences.add(PREF_1541DOS,"Set 1541 dos rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.D1541_DOS_ROM_PROP,file) }
    preferences.add(PREF_1571DOS,"Set 1571 dos rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.D1571_DOS_ROM_PROP,file) }
    preferences.add(PREF_1581DOS,"Set 1581 dos rom path","",Set.empty,false) { file => if (file != "") reloadROM(ROM.D1581_DOS_ROM_PROP,file) }
    preferences.add(PREF_VICIINEW,"Set VICII new model",false) { vicChip.setNEWVICModel(_) }

    preferences.add(PREF_TRACE,"Starts the emulator in trace mode",false,Set(),false) { trace =>
      traceOption = trace
      if (trace && traceDialog != null) {
        traceDialog.forceTracing(true)
        traceDialog.setVisible(true)
        traceItem.setSelected(true)
      }
    }
  }

  protected def setFileMenu(fileMenu:JMenu) : Unit = {
    import Preferences._
    // WARP ON LOAD =======================================================================================
    val warpModeOnLoad = new JCheckBoxMenuItem("Warp mode on load")
    warpModeOnLoad.setSelected(false)
    warpModeOnLoad.addActionListener(_ => preferences(PREF_WARPONLOAD) = warpModeOnLoad.isSelected )
    fileMenu.add(warpModeOnLoad)
    // Setting ---------------------------
    preferences.add(PREF_WARPONLOAD,"Enabled/disable warp mode on load",false) { wpl =>
      ProgramLoader.loadingWithWarpEnabled = wpl
      warpModeOnLoad.setSelected(wpl)
    }
    // ====================================================================================================

    val zipItem = new JMenuItem("Attach zip ...")
    zipItem.addActionListener(_ => attachZip )
    fileMenu.add(zipItem)

    val tapeItem = new JMenuItem("Load file from tape ...")
    tapeItem.addActionListener(_ => loadFileFromTape )
    fileMenu.add(tapeItem)

    if (tapeAllowed) {
      val attachTapeItem = new JMenuItem("Attach tape ...")
      attachTapeItem.addActionListener(_ => attachTape)
      fileMenu.add(attachTapeItem)

      tapeMenu.setEnabled(false)
      fileMenu.add(tapeMenu)

      val tapePlayItem = new JMenuItem("Press play")
      tapePlayItem.addActionListener(_ => datassette.pressPlay)
      tapeMenu.add(tapePlayItem)

      val tapeStopItem = new JMenuItem("Press stop")
      tapeStopItem.addActionListener(_ => datassette.pressStop)
      tapeMenu.add(tapeStopItem)

      val tapeRecordItem = new JMenuItem("Press record & play")
      tapeRecordItem.addActionListener(_ => datassette.pressRecordAndPlay)
      tapeMenu.add(tapeRecordItem)

      val tapeRewindItem = new JMenuItem("Press rewind")
      tapeRewindItem.addActionListener(_ => datassette.pressRewind)
      tapeMenu.add(tapeRewindItem)

      val tapeForwardItem = new JMenuItem("Press forward")
      tapeForwardItem.addActionListener(_ => datassette.pressForward)
      tapeMenu.add(tapeForwardItem)

      val tapeResetItem = new JMenuItem("Reset")
      tapeResetItem.addActionListener(_ => datassette.resetToStart)
      tapeMenu.add(tapeResetItem)

      val tapeResetCounterItem = new JMenuItem("Reset counter")
      tapeResetCounterItem.addActionListener(_ => datassette.resetCounter)
      tapeMenu.add(tapeResetCounterItem)
    }

    fileMenu.addSeparator

    val makeDiskItem = new JMenuItem("Make empty disk ...")
    makeDiskItem.addActionListener(_ => makeDisk )
    fileMenu.add(makeDiskItem)

    val autorunDiskItem = new JMenuItem("Autorun disk ...")
    autorunDiskItem.addActionListener(_ => attachDisk(0,true,true) )
    fileMenu.add(autorunDiskItem)

    val attackDiskItem = new JMenu("Attach disk ...")
    fileMenu.add(attackDiskItem)
    for(d <- 0 until TOTAL_DRIVES) {
      val attachDisk0Item = new JMenuItem(s"Attach disk ${d + 8}...")
      val key = if (d > 1) java.awt.event.KeyEvent.VK_0 + (d - 2) else java.awt.event.KeyEvent.VK_8 + d
      attachDisk0Item.setAccelerator(KeyStroke.getKeyStroke(key,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
      attachDisk0Item.addActionListener(_ => attachDisk(d,false,isC64Mode) )
      attackDiskItem.add(attachDisk0Item)
    }

    val attackLocalDiskItem = new JMenu("Attach local directory as disk ...")
    fileMenu.add(attackLocalDiskItem)
    for(d <- 0 until TOTAL_DRIVES) {
      val attachDisk0Item = new JMenuItem(s"Attach local directory on disk ${d + 8}...")
      attachDisk0Item.addActionListener(_ => attachLocalDir(d) )
      attackLocalDiskItem.add(attachDisk0Item)
    }
    // For settings see below, after drive type

    val ejectMenu = new JMenu("Eject disk")
    fileMenu.add(ejectMenu)
    for(d <- 0 until TOTAL_DRIVES) {
      val ejectDisk0Item = new JMenuItem(s"Eject disk ${d + 8} ...")
      ejectDisk0Item.addActionListener(_ => ejectDisk(d) )
      ejectMenu.add(ejectDisk0Item)
    }

    val loadFileItem = new JMenu("Fast load from attached disk ...")
    fileMenu.add(loadFileItem)
    for(d <- 0 until TOTAL_DRIVES) {
      loadFileItems(d).setEnabled(false)
      loadFileItems(d).addActionListener(_ => loadFileFromAttachedFile(d,true,isC64Mode) )
      loadFileItem.add(loadFileItems(d))
    }
    fileMenu.addSeparator

    // LOAD PRG AS D64 ====================================================================================
    val prgAsDiskItem = new JCheckBoxMenuItem("Load PRG as D64")
    prgAsDiskItem.addActionListener(_ => preferences(PREF_PRGASDISK) = prgAsDiskItem.isSelected )
    fileMenu.add(prgAsDiskItem)
    // Setting ---------------------------
    preferences.add(PREF_PRGASDISK,"Load a PRG file as if inserted in a disk",false) { pad =>
      loadPRGasDisk = pad
      prgAsDiskItem.setSelected(loadPRGasDisk)
    }
    // ====================================================================================================
    val loadPrgItem = new JMenuItem("Load PRG file from local disk ...")
    loadPrgItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G,java.awt.event.InputEvent.ALT_DOWN_MASK))
    loadPrgItem.addActionListener(_ => loadPrg )
    fileMenu.add(loadPrgItem)

    val savePrgItem = new JMenuItem("Save PRG file to local disk ...")
    savePrgItem.addActionListener(_ => savePrg )
    fileMenu.add(savePrgItem)

    // DRIVEX-ENABLED =====================================================================================
    for(d <- 1 until TOTAL_DRIVES) {
      preferences.add(PREF_DRIVE_X_ENABLED(d),s"Enabled/disable driver ${8 + d}",false) { enableDrive(d, _, false) }
    }
    // ====================================================================================================

    // DRIVE12-LOCAL-PATH =================================================================================
    val localDriveItem = new JMenu("Drive on device 12 ...")
    fileMenu.add(localDriveItem)
    val group0 = new ButtonGroup
    val noLocalDriveItem = new JRadioButtonMenuItem("Disabled")
    noLocalDriveItem.setSelected(true)
    noLocalDriveItem.addActionListener(_ => enableDrive12(false,None) )
    group0.add(noLocalDriveItem)
    localDriveItem.add(noLocalDriveItem)
    val localDriveEnabled = new JRadioButtonMenuItem("Local drive ...")
    localDriveEnabled.addActionListener(_ => enableDrive12(true,None) )
    group0.add(localDriveEnabled)
    localDriveItem.add(localDriveEnabled)
    preferences.add(PREF_DRIVE12LOCALPATH,"Enabled driver 12 to the given local path","") { path =>
      val enabled = !path.isEmpty
      enableDrive12(enabled,if (enabled) Some(path) else None)
      localDriveEnabled.setSelected(enabled)
    }
    // ====================================================================================================

    // WRITE-ON-DISK ======================================================================================
    val writeOnDiskCheckItem = new JCheckBoxMenuItem("Write changes on disk")
    writeOnDiskCheckItem.setSelected(true)
    writeOnDiskCheckItem.addActionListener(_ => preferences(PREF_WRITEONDISK) = writeOnDiskCheckItem.isSelected )
    fileMenu.add(writeOnDiskCheckItem)
    preferences.add(PREF_WRITEONDISK,"Tells if the modifications made on disks must be written on file",true) { wod =>
      writeOnDiskCheckItem.setSelected(wod)
      writeOnDiskSetting(wod)
    }
    // ====================================================================================================

    fileMenu.addSeparator

    val resetMenu = new JMenu("Reset")
    fileMenu.add(resetMenu)
    val softResetItem = new JMenuItem("Soft Reset")
    softResetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK))
    softResetItem.addActionListener(_ => reset(true) )
    resetMenu.add(softResetItem)
    val hardResetItem = new JMenuItem("Hard Reset")
    hardResetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.SHIFT_DOWN_MASK))
    hardResetItem.addActionListener(_ => hardReset(true) )
    resetMenu.add(hardResetItem)
    val reset2Item = new JMenuItem("Reset and run last PRG")
    reset2Item.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.CTRL_DOWN_MASK))
    reset2Item.addActionListener(_ => reset(true,true) )
    resetMenu.add(reset2Item)

    fileMenu.addSeparator

    // CART ================================================================================================
    val attachCtrItem = new JMenuItem("Attach cartridge ...")
    attachCtrItem.addActionListener(_ => attachCtr )
    fileMenu.add(attachCtrItem)
    preferences.add(PREF_CART,"Attach the given cartridge","") { cart =>
      if (cart != "") loadCartridgeFile(new File(cart))
    }
    // ====================================================================================================

    detachCtrItem.setEnabled(false)
    detachCtrItem.addActionListener(_ => detachCtr )
    fileMenu.add(detachCtrItem)

    fileMenu.addSeparator

    // PREF-AUTO-SAVE ======================================================================================
    val autoSaveCheckItem = new JCheckBoxMenuItem("Autosave settings on exit")
    autoSaveCheckItem.addActionListener(e => preferences(PREF_PREFAUTOSAVE) = autoSaveCheckItem.isSelected )
    fileMenu.add(autoSaveCheckItem)
    preferences.add(PREF_PREFAUTOSAVE,"Auto save settings on exit",false) { auto =>
      autoSaveCheckItem.setSelected(auto)
    }
    // ====================================================================================================

    val saveSettingsCheckItem = new JMenuItem("Save settings")
    saveSettingsCheckItem.addActionListener(_ => saveSettings(true) )
    fileMenu.add(saveSettingsCheckItem)

    fileMenu.addSeparator

    val exitItem = new JMenuItem("Exit")
    exitItem.addActionListener(_ => turnOff )
    fileMenu.add(exitItem)
  }

  protected def setEditMenu(editMenu: JMenu) : Unit = {
    val pasteItem = new JMenuItem("Paste text")
    pasteItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.Event.CTRL_MASK))
    pasteItem.addActionListener(_ => paste )
    editMenu.add(pasteItem)
    val listItem = new JMenuItem("List BASIC to editor")
    listItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I,java.awt.Event.ALT_MASK))
    listItem.addActionListener(_ => ucesoft.cbm.misc.BasicListExplorer.list(mmu,0x801) )
    editMenu.add(listItem)
  }

  protected def setStateMenu(stateMenu: JMenu) : Unit = {
    val saveStateItem = new JMenuItem("Save state ...")
    saveStateItem.addActionListener(_ => saveState() )
    stateMenu.add(saveStateItem)
    val loadStateItem = new JMenuItem("Load state ...")
    loadStateItem.addActionListener(_ => loadState(None) )
    stateMenu.add(loadStateItem)
  }

  protected def setTraceMenu(traceMenu: JMenu) : Unit = {
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

  }

  protected def setGameMenu(gamesMenu: JMenu) : Unit = {
    val loader = ServiceLoader.load(classOf[ucesoft.cbm.game.GameProvider])
    var providers = loader.iterator
    try {
      if (!providers.hasNext) providers = java.util.Arrays.asList((new ucesoft.cbm.game.CSDBSpi).asInstanceOf[ucesoft.cbm.game.GameProvider],(new ucesoft.cbm.game.GameBaseSpi).asInstanceOf[ucesoft.cbm.game.GameProvider],(new ucesoft.cbm.game.PouetDemoSpi).asInstanceOf[ucesoft.cbm.game.GameProvider]).iterator
      val names = new collection.mutable.HashSet[String]
      while (providers.hasNext) {
        val provider = providers.next
        Log.info(s"Loaded ${provider.name} provider")
        if (!names.contains(provider.name)) {
          names += provider.name
          val item = new JCheckBoxMenuItem(provider.name)
          gamesMenu.add(item)
          item.addActionListener(new ActionListener {
            def actionPerformed(e:ActionEvent) : Unit = {
              try {
                val ui = GameUI.getUIFor(item,displayFrame,provider,cbmComputer)
                ui.setVisible(item.isSelected)
              }
              catch {
                case t:Throwable =>

                  showError(s"Error while contacting provider ${provider.name}'s server",t.toString)
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
  }

  protected def setHelpMenu(helpMenu: JMenu) : Unit = {
    val aboutItem = new JMenuItem("About")
    aboutItem.addActionListener(_ => showAbout )
    helpMenu.add(aboutItem)
    val settingsItem = new JMenuItem("Settings")
    settingsItem.addActionListener(_ => showSettings )
    helpMenu.add(settingsItem)
  }

  protected def setDriveMenu(parent:JMenu) : Unit = {
    val driveMenu = new JMenuItem("Drives ...")
    driveMenu.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    parent.add(driveMenu)
    driveMenu.addActionListener(_ => DrivesConfigPanel.getDriveConfigDialog.setVisible(true) )
  }

  protected def setVolumeSettings(parent:JMenu) : Unit = {
    val volumeItem = new JMenuItem("Volume settings ...")
    volumeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.event.InputEvent.ALT_DOWN_MASK))
    volumeItem.addActionListener(_ => volumeDialog.setVisible(true) )
    parent.add(volumeItem)
  }

  protected def setWarpModeSettings(parent:JMenu) : Unit = {
    maxSpeedItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W,java.awt.event.InputEvent.ALT_DOWN_MASK))
    maxSpeedItem.setSelected(clock.maximumSpeed)
    maxSpeedItem.addActionListener(e => warpMode(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    parent.add(maxSpeedItem)
  }

  protected def setRenderingSettings(parent:JMenu) : Unit = {
    import Preferences._
    // RENDERING-TYPE ======================================================================================
    val renderingItem = new JMenu("Rendering")
    parent.add(renderingItem)
    val groupR = new ButtonGroup
    val renderingDefault1Item = new JRadioButtonMenuItem("Default")
    renderingDefault1Item.setSelected(true)
    renderingDefault1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "default" )
    renderingItem.add(renderingDefault1Item)
    groupR.add(renderingDefault1Item)
    val renderingBilinear1Item = new JRadioButtonMenuItem("Bilinear")
    renderingBilinear1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "bilinear" )
    renderingItem.add(renderingBilinear1Item)
    groupR.add(renderingBilinear1Item)
    val renderingBicubic1Item = new JRadioButtonMenuItem("Bicubic")
    renderingBicubic1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "bicubic" )
    renderingItem.add(renderingBicubic1Item)
    groupR.add(renderingBicubic1Item)

    preferences.add(PREF_RENDERINGTYPE,"Set the rendering type (default,bilinear,bicubic)","",Set("default","bilinear","bicubic")) { rt =>
      rt match {
        case "bilinear" =>
          setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR)
          renderingBilinear1Item.setSelected(true)
        case "bicubic" =>
          setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_BICUBIC)
          renderingBicubic1Item.setSelected(true)
        case "default" | "" =>
          setDisplayRendering(java.awt.RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
          renderingDefault1Item.setSelected(true)
      }
    }
    // =====================================================================================================

    // VIC-PALETTE =========================================================================================
    val paletteItem = new JMenu("Palette")
    parent.add(paletteItem)
    val groupP = new ButtonGroup
    val vicePalItem = new JRadioButtonMenuItem("VICE")
    vicePalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "vice" )
    paletteItem.add(vicePalItem)
    groupP.add(vicePalItem)
    val brightPalItem = new JRadioButtonMenuItem("Bright")
    brightPalItem.setSelected(true)
    brightPalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "bright" )
    paletteItem.add(brightPalItem)
    groupP.add(brightPalItem)
    val peptoPalItem = new JRadioButtonMenuItem("Pepto")
    peptoPalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "pepto" )
    paletteItem.add(peptoPalItem)
    groupP.add(peptoPalItem)
    val colordorePalItem = new JRadioButtonMenuItem("Colodore")
    colordorePalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "colodore" )
    paletteItem.add(colordorePalItem)
    groupP.add(colordorePalItem)

    preferences.add(PREF_VICPALETTE,"Set the palette type (bright,vice,pepto,colodore)","",Set("bright","vice","pepto","colodore")) { pal =>
      pal match {
        case "bright"|"" =>
          Palette.setPalette(PaletteType.BRIGHT)
          brightPalItem.setSelected(true)
        case "vice" =>
          Palette.setPalette(PaletteType.VICE)
          vicePalItem.setSelected(true)
        case "pepto" =>
          Palette.setPalette(PaletteType.PEPTO)
          peptoPalItem.setSelected(true)
        case "colodore" =>
          Palette.setPalette(PaletteType.COLORDORE)
          colordorePalItem.setSelected(true)
        case _ =>
      }
    }
    // =====================================================================================================
  }

  protected def setFullScreenSettings(parent:JMenu) : Unit = {
    val fullScreenItem = new JMenuItem("Full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setVicFullScreen)
    parent.add(fullScreenItem)
  }

  protected def setJoysticsSettings(parent:JMenu) : Unit = {
    val joyAItem = new JMenuItem("Joystick...")
    joyAItem.addActionListener(_ => joySettings )
    parent.add(joyAItem)

    val swapJoyAItem = new JMenuItem("Swap joysticks")
    swapJoyAItem.addActionListener(_ => swapJoysticks )
    parent.add(swapJoyAItem)
  }

  protected def setLightPenSettings(parent:JMenu) : Unit = {
    val lightPenMenu = new JMenu("Light pen")
    parent.add(lightPenMenu)
    val group3 = new ButtonGroup
    val noPenItem = new JRadioButtonMenuItem("No light pen")
    noPenItem.setSelected(true)
    noPenItem.addActionListener(_ => setLightPen(LIGHT_PEN_NO_BUTTON) )
    // reset setting
    resetSettingsActions = (() => {
      noPenItem.setSelected(true)
      setLightPen(LIGHT_PEN_NO_BUTTON)
    }) :: resetSettingsActions
    group3.add(noPenItem)
    lightPenMenu.add(noPenItem)
    val penUp = new JRadioButtonMenuItem("Light pen with button up on control port 2")
    penUp.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_UP) )
    group3.add(penUp)
    lightPenMenu.add(penUp)
    val penLeft = new JRadioButtonMenuItem("Light pen with button left on control port 2")
    penLeft.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_LEFT) )
    group3.add(penLeft)
    lightPenMenu.add(penLeft)
  }

  protected def setMouseSettings(parent:JMenu) : Unit = {
    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled on port 1")
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M,java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.setSelected(false)
    mouseEnabledItem.addActionListener(e => enableMouse(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected,display) )
    parent.add(mouseEnabledItem)
    // reset setting
    resetSettingsActions = (() => {
      mouseEnabledItem.setSelected(false)
      enableMouse(false,display)
    }) :: resetSettingsActions
  }

  protected def setPauseSettings(parent:JMenu) : Unit = {
    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        display.setPaused
      } else clock.play )
    parent.add(pauseItem)
  }

  protected def setPrinterSettings(parent:JMenu) : Unit = {
    import Preferences._
    // PRINTER-ENABLED =====================================================================================
    val printerPreviewItem = new JMenuItem("Printer preview ...")
    printerPreviewItem.addActionListener(_ => showPrinterPreview )
    parent.add(printerPreviewItem)

    val printerEnabledItem = new JCheckBoxMenuItem("Printer enabled")
    printerEnabledItem.setSelected(false)
    printerEnabledItem.addActionListener(_ => preferences(PREF_PRINTERENABLED) = printerEnabledItem.isSelected )
    parent.add(printerEnabledItem)
    preferences.add(PREF_PRINTERENABLED,"Enable printer",false) { pe =>
      enablePrinter(pe)
      printerEnabledItem.setSelected(pe)
    }

    // reset setting
    resetSettingsActions = (() => {
      printerEnabledItem.setSelected(false)
      printerEnabled = false
      printer.setActive(false)
    }) :: resetSettingsActions
    // =====================================================================================================
  }

  protected def setSIDSettings(parent:JMenu) : Unit = {
    import Preferences._
    // SID-8580 ============================================================================================
    val sidItem = new JMenu("SID")
    parent.add(sidItem)
    val group7 = new ButtonGroup
    val sidTypeItem = new JMenu("SID Type")
    sidItem.add(sidTypeItem)
    val sid6581Item = new JRadioButtonMenuItem("MOS 6581")
    sid6581Item.setSelected(true)
    sid6581Item.addActionListener(_ => preferences(PREF_SID8580) = true )
    sidTypeItem.add(sid6581Item)
    group7.add(sid6581Item)
    val sid8580Item = new JRadioButtonMenuItem("MOS 8580")
    sid8580Item.setSelected(false)
    sid8580Item.addActionListener(_ => preferences(PREF_SID8580) = false )
    sidTypeItem.add(sid8580Item)
    group7.add(sid8580Item)
    preferences.add(PREF_SID8580,"Enable sid 8580 type",false) { sid8580 =>
      sid.setModel(!sid8580)
      sid8580Item.setSelected(sid8580)
    }
    // =====================================================================================================

    // DUAL-SID ============================================================================================
    val sid2Item = new JMenu("Dual SID")
    sidItem.add(sid2Item)
    val group8 = new ButtonGroup
    val nosid2Item = new JRadioButtonMenuItem("None")
    sid2Item.add(nosid2Item)
    nosid2Item.setSelected(true)
    nosid2Item.addActionListener(_ => setDualSID(None) )
    group8.add(nosid2Item)
    val addressMap = (for(adr <- DualSID.validAddresses(isC64Mode)) yield {
      val sid2AdrItem = new JRadioButtonMenuItem(adr)
      sid2Item.add(sid2AdrItem)
      sid2AdrItem.setSelected(false)
      sid2AdrItem.addActionListener(_ => preferences(PREF_DUALSID) = adr )
      group8.add(sid2AdrItem)
      adr -> sid2AdrItem
    }).toMap
    preferences.add(PREF_DUALSID,s"Enable dual sid on the given address. Valid addresses are: ${addressMap.keys.mkString(",")}","",addressMap.keySet) { adr =>
      addressMap get adr match {
        case Some(item) =>
          item.setSelected(true)
          setDualSID(Some(Integer.parseInt(adr,16)))
        case None =>
          throw new IllegalArgumentException(s"Invalid dual sid address: $adr")
      }
    }
    // =====================================================================================================

    // SID-CYCLE-EXACT =====================================================================================
    val sidCycleExactItem = new JCheckBoxMenuItem("SID cycle exact emulation")
    sidCycleExactItem.setSelected(false)
    sidCycleExactItem.addActionListener(_ => preferences(PREF_SIDCYCLEEXACT) = sidCycleExactItem.isSelected )
    sidItem.add(sidCycleExactItem)
    preferences.add(PREF_SIDCYCLEEXACT,"With this option enabled the SID emulation is more accurate with a decrease of performance",false) { sce =>
      clock.pause
      sidCycleExact = sce
      sid.setCycleExact(sidCycleExact)
      sidCycleExactItem.setSelected(sidCycleExact)
      clock.play
    }
    // =====================================================================================================
    // reset setting
    resetSettingsActions = (() => {
      sid6581Item.setSelected(true)
      sid.setModel(true)
      nosid2Item.setSelected(true)
      setDualSID(None)
    }) :: resetSettingsActions
  }

  protected def setDrivesSettings : Unit = {
    import Preferences._
    for(drive <- 0 until TOTAL_DRIVES) {
      // DRIVE-X-TYPE ========================================================================================
      preferences.add(PREF_DRIVE_X_TYPE(drive),"Set the driver's type (1541,1571,1581)","",Set("1541","1571","1581")) { dt =>
        dt match {
          case "1541" =>
            setDriveType(drive,DriveType._1541, true)
          case "1571" =>
            setDriveType(drive,DriveType._1571, true)
          case "1581" =>
            setDriveType(drive,DriveType._1581, true)
          case _ =>
        }
      }
      // =====================================================================================================

      // DRIVE-X-FILE ========================================================================================
      preferences.add(PREF_DRIVE_X_FILE(drive),s"Attach a file to drive ${drive + 8}","") { df =>
        if (df != "") attachDiskFile(drive, new File(df), false, None)
      }
      // =====================================================================================================
    }
  }

  protected def setRemotingSettings(parent:JMenu) : Unit = {
    val remoteItem = new JMenu("Remoting")
    parent.add(remoteItem)

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
  }

  protected def setREUSettings(parent:JMenu) : Unit = {
    import Preferences._
    // REU-TYPE ============================================================================================
    val reuItem = new JMenu("REU")
    val group5 = new ButtonGroup
    val noReuItem = new JRadioButtonMenuItem("None")
    noReuItem.setSelected(true)
    noReuItem.addActionListener(_ => preferences(PREF_REUTYPE) = "none" )
    group5.add(noReuItem)
    reuItem.add(noReuItem)
    val reu128Item = new JRadioButtonMenuItem("128K")
    reu128Item.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_1700.toString )
    group5.add(reu128Item)
    reuItem.add(reu128Item)
    val reu256Item = new JRadioButtonMenuItem("256K")
    reu256Item.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_1764.toString )
    group5.add(reu256Item)
    reuItem.add(reu256Item)
    val reu512Item = new JRadioButtonMenuItem("512K")
    reu512Item.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_1750.toString )
    group5.add(reu512Item)
    reuItem.add(reu512Item)
    val reu1MItem = new JRadioButtonMenuItem("1M")
    reu1MItem.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_1M.toString )
    group5.add(reu1MItem)
    reuItem.add(reu1MItem)
    val reu2MItem = new JRadioButtonMenuItem("2M")
    reu2MItem.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_2M.toString )
    group5.add(reu2MItem)
    reuItem.add(reu2MItem)
    val reu4MItem = new JRadioButtonMenuItem("4M")
    reu4MItem.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_4M.toString )
    group5.add(reu4MItem)
    reuItem.add(reu4MItem)
    val reu16MItem = new JRadioButtonMenuItem("16M ...")
    reu16MItem.addActionListener(_ => choose16MREU )
    group5.add(reu16MItem)
    reuItem.add(reu16MItem)
    parent.add(reuItem)

    preferences.add(PREF_REUTYPE,"Set the reu type (none,128,256,512,1024,2048,4096,16384). With 16384 the syntax is: 16384[,<reu file to load>]","none") { reu =>
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
          case REU.REU_1M =>
            setREU(Some(REU.REU_1M),None)
            reu1MItem.setSelected(true)
          case REU.REU_2M =>
            setREU(Some(REU.REU_2M),None)
            reu2MItem.setSelected(true)
          case REU.REU_4M =>
            setREU(Some(REU.REU_4M),None)
            reu4MItem.setSelected(true)
          case REU.REU_16M =>
            setREU(Some(REU.REU_16M),if (reuPars.length == 2 && reuPars(1) != "null") Some(reuPars(1)) else None)
            reu16MItem.setSelected(true)
        }
    }

    // reset setting
    resetSettingsActions = (() => {
      noReuItem.setSelected(true)
      setREU(None,None)
    }) :: resetSettingsActions
  }

  protected def setGEORamSettings(parent:JMenu) : Unit = {
    import Preferences._
    // GEO-RAM =============================================================================================
    val geoItem = new JMenu("GeoRAM")
    val groupgeo = new ButtonGroup
    val noGeoItem = new JRadioButtonMenuItem("None")
    noGeoItem.setSelected(true)
    noGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "none" )
    groupgeo.add(noGeoItem)
    geoItem.add(noGeoItem)
    val _256kGeoItem = new JRadioButtonMenuItem("256K")
    _256kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "256" )
    groupgeo.add(_256kGeoItem)
    geoItem.add(_256kGeoItem)
    val _512kGeoItem = new JRadioButtonMenuItem("512K")
    _512kGeoItem.addActionListener(_ => preferences(PREF_GEORAM) = "512" )
    groupgeo.add(_512kGeoItem)
    geoItem.add(_512kGeoItem)

    parent.add(geoItem)
    preferences.add(PREF_GEORAM,"Set the georam size (none,256,512)","none",Set("none","256","512")) { geo =>
      if (geo == "512") {
        _512kGeoItem.setSelected(true)
        setGeoRAM(true,512)
      }
      else
      if (geo == "256") {
        _256kGeoItem.setSelected(true)
        setGeoRAM(true,256)
      }
      else setGeoRAM(false)
    }

    // reset setting
    resetSettingsActions = (() => {
      noGeoItem.setSelected(true)
      setGeoRAM(false)
    }) :: resetSettingsActions
  }

  protected def setDigiMAXSettings(parent:JMenu) : Unit = {
    // DIGI-MAX ============================================================================================
    val digimaxItem = new JMenu("DigiMAX")
    parent.add(digimaxItem)
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
    // TODO: preferences
    // reset setting
    resetSettingsActions = (() => {
      digimaxDisabledItem.setSelected(true)
      setDigiMax(false,None)
    }) :: resetSettingsActions
  }

  protected def setCPMSettings(parent:JMenu) : Unit = {
    import Preferences._
    // CPM64-ENABLED =======================================================================================
    val cpmItem = new JCheckBoxMenuItem("CP/M Cartridge")
    cpmItem.addActionListener(e => preferences(PREF_CPMCARTENABLED) = cpmItem.isSelected )
    parent.add(cpmItem)
    preferences.add(PREF_CPMCARTENABLED,"Attach the CP/M cart",false) { cpm =>
      enableCPMCart(cpm)
      cpmItem.setSelected(cpm)
    }

    // reset setting
    resetSettingsActions = (() => {
      cpmItem.setSelected(false)
      enableCPMCart(false)
    }) :: resetSettingsActions
  }

  protected def setFlyerSettings(parent:JMenu) : Unit = {
    // FLYED-ENABLED =======================================================================================
    val flyerItem = new JMenu("Flyer internet modem")
    parent.add(flyerItem)
    val fylerEnabledItem = new JCheckBoxMenuItem("Flyer enabled on 7")
    fylerEnabledItem.setSelected(false)
    flyerItem.add(fylerEnabledItem)
    fylerEnabledItem.addActionListener(e => enableFlyer(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) )
    val flyerDirectoryItem = new JMenuItem("Set disks repository ...")
    flyerItem.add(flyerDirectoryItem)
    flyerDirectoryItem.addActionListener(_ => chooseFlyerDir )
    // TODO : preferences
    // reset setting
    resetSettingsActions = (() => {
      fylerEnabledItem.setSelected(false)
      enableFlyer(false)
    }) :: resetSettingsActions
  }

  protected def setBeamRacerSettings(parent:JMenu) : Unit = {
    import Preferences._
    // BEAM-RACER-ENABLED ==================================================================================
    val brItem = new JCheckBoxMenuItem("Beam Racer installed")
    brItem.setSelected(false)
    parent.add(brItem)
    brItem.addActionListener( _ => preferences(PREF_BEAMRACERENABLED) = brItem.isSelected )

    preferences.add(PREF_BEAMRACERENABLED,"Install Beam Racer VIC's coprocessor",false) { br =>
      vicChip.setCoprocessor(if (br) new VASYL(vicChip,cpu,dmaSwitcher.setLine(Switcher.EXT,_)) else null)
      brItem.setSelected(true)
    }
  }

  protected def setVICModel(parent:JMenu) : Unit = {
    import Preferences._
    // NTSC ================================================================================================
    val vicModelItem = new JMenu("VIC model")
    val group = new ButtonGroup
    val palItem = new JRadioButtonMenuItem("PAL (6569)")
    val ntscItem = new JRadioButtonMenuItem("NTSC (6567R8)")
    group.add(palItem)
    group.add(ntscItem)
    vicModelItem.add(palItem)
    vicModelItem.add(ntscItem)
    palItem.addActionListener( _ => preferences(PREF_NTSC) = false )
    palItem.setSelected(true)
    ntscItem.addActionListener( _ => preferences(PREF_NTSC) = true )
    parent.add(vicModelItem)

    preferences.add(PREF_NTSC,"Set ntsc video standard",false) { ntsc =>
      val adjusting = preferences.get(PREF_NTSC).get.isAdjusting
      if (ntsc) setVICModel(VICType.NTSC,false,!adjusting) else setVICModel(VICType.PAL,false,!adjusting)
      ntscItem.setSelected(ntsc)
    }
  }

  protected def setVICBorderMode(parent:JMenu) : Unit = {
    import Preferences._
    // VIC-BORDER-OFF ======================================================================================
    val borderItem = new JMenu("Border mode")
    val borderOnItem = new JCheckBoxMenuItem("Draw border")
    borderOnItem.setSelected(true)
    borderItem.add(borderOnItem)
    borderOnItem.addActionListener( _ => preferences(PREF_VICBORDEROFF) = !borderOnItem.isSelected )
    parent.add(borderItem)
    borderOnItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B,java.awt.event.InputEvent.ALT_DOWN_MASK))

    preferences.add(PREF_VICBORDEROFF,"Disable VIC's borders",false) { borderoff =>
      vicChip.setDrawBorder(!borderoff)
      borderOnItem.setSelected(!borderoff)
    }
  }

  protected def setOneFrameMode(parent:JMenu,display: => Display,shortKey:Int,maskKey:Int = java.awt.event.InputEvent.ALT_DOWN_MASK) : Unit = {
    val sfmItem = new JMenu("Single frame mode")
    val sfmEnableItem = new JCheckBoxMenuItem("Enabled")
    val sfmNextFrameItem = new JMenuItem("Advance to next frame")
    sfmNextFrameItem.setEnabled(false)
    sfmItem.add(sfmEnableItem)
    sfmEnableItem.addActionListener(_ => {
      display.setSingleFrameMode(sfmEnableItem.isSelected)
      sfmNextFrameItem.setEnabled(sfmEnableItem.isSelected)
    }
    )
    sfmNextFrameItem.setAccelerator(KeyStroke.getKeyStroke(shortKey,maskKey))
    sfmNextFrameItem.addActionListener(_ => display.advanceOneFrame )
    sfmItem.add(sfmNextFrameItem)
    parent.add(sfmItem)
  }

  protected def setEasyFlashSettings(parent:JMenu) : Unit = {
    val easyFlashMenu = new JMenu("EasyFlash")
    val jumperItem = new JCheckBoxMenuItem("Easy Flash jumper on")
    jumperItem.addActionListener( _ => EasyFlash.jumper = jumperItem.isSelected )
    easyFlashMenu.add(jumperItem)
    easyFlashWriteChangesItem.addActionListener(_ => easyFlashWriteChanges )
    easyFlashWriteChangesItem.setEnabled(false)
    easyFlashMenu.add(easyFlashWriteChangesItem)
    parent.add(easyFlashMenu)
  }

  protected def setGMOD3FlashSettings(parent:JMenu) : Unit = {
    val gmodMenu = new JMenu("GMOD ...")
    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2 )
    gmodMenu.add(gmod2Item)
    GMOD3WriteChangesItem.setEnabled(false)
    gmodMenu.add(GMOD3WriteChangesItem)
    GMOD3WriteChangesItem.addActionListener(_ => GMOD3WriteChanges )
    parent.add(gmodMenu)
  }

  protected def easyFlashWriteChanges : Unit = {
    ExpansionPort.getInternalExpansionPort match {
      case ef:EasyFlash =>
        ef.createCRT
      case _ =>
    }
  }

  protected def GMOD3WriteChanges : Unit = {
    ExpansionPort.getInternalExpansionPort match {
      case ef:GMOD3 =>
        ef.createCRT
      case _ =>
    }
  }


  protected def reloadROM(resource:String,location:String) : Unit = {
    val oldLocation = configuration.getProperty(resource)
    try {
      configuration.setProperty(resource, location)
      ROM.reload(resource)
    }
    finally {
      configuration.setProperty(resource,if (oldLocation == null) "" else oldLocation)
    }
  }

  protected def showCartInfo : Unit = {
    ExpansionPort.getExpansionPort.getCRT match {
      case Some(crt) =>
        val cols : Array[Object] = Array("Property","Value")
        val data : Array[Array[Object]] = Array(
          Array("Name",crt.name),
          Array("Type",crt.ctrType.toString),
          Array("EXROM",crt.EXROM.toString),
          Array("GAME",crt.GAME.toString),
          Array("Banks",crt.chips.length.toString),
          Array("Size",s"${crt.kbSize}Kb")
        )
        val banks : Array[Array[Object]] = for(b <- crt.chips) yield {
          Array("Bank %2d at %4X".format(b.bankNumber,b.startingLoadAddress),"%sKb".format(b.romSize / 1024))
        }
        val table = new JTable(data ++ banks,cols)
        val sp = new JScrollPane(table)
        val panel = new JPanel(new BorderLayout)
        panel.add("Center",sp)
        JOptionPane.showMessageDialog(displayFrame,panel,"Cart info",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
      case None =>
    }
  }

  protected def setDefaultProperties(configuration:Properties) : Unit = {
    import Preferences._
    configuration.setProperty(PREF_RENDERINGTYPE,"default")
    configuration.setProperty(PREF_WRITEONDISK,"true")
    configuration.setProperty(PREF_VICPALETTE,"bright")
  }

  protected def saveConfigurationFile : Unit = {
    try {
      val kernalConfigHome = System.getProperty("kernal.config",scala.util.Properties.userHome)
      val propsFile = new File(new File(kernalConfigHome), CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "C64 configuration file")
      out.close
    }
    catch {
      case _: IOException =>
    }
  }
}
