package ucesoft.cbm

import ucesoft.cbm.cpu.{CPU65xx, Memory, ROM}
import ucesoft.cbm.formats.{Diskette, TAP}
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.drive.{D1581, Drive, DriveIDMismatch, DriveType, EmptyFloppy}
import ucesoft.cbm.peripheral.keyboard
import ucesoft.cbm.peripheral.keyboard.Keyboard
import ucesoft.cbm.peripheral.printer.{MPS803GFXDriver, MPS803ROM, Printer}
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.trace.{InspectPanelDialog, TraceListener, Tracer, TracerGUI}

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, FlowLayout}
import java.io._
import java.util.Properties
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.swing._
import javax.swing.filechooser.FileFilter

object CBMComputer {
  def turnOn(computer : => CBMComputer, args:Array[String]) : Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

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
        sys.exit(1)
    }
  }
}

abstract class CBMComputer extends CBMComponent {
  protected val cbmModel : CBMComputerModel

  protected val TOTAL_DRIVES: Int = Preferences.TOTALDRIVES
  protected val ALLOWED_DRIVE_TYPES = DrivesConfigPanel.ALL_DRIVES_ALLOWED
  protected val APPLICATION_NAME : String
  protected val CONFIGURATION_FILENAME : String
  protected val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  protected val CONFIGURATION_FRAME_XY = "frame.xy"
  protected val CONFIGURATION_FRAME_DIM = "frame.dim"
  protected val CONFIGURATION_KEYB_MAP_FILE = "keyb.map.file"

  protected val DEFAULT_KEYBOARD_RESOURCE_NAME : String

  protected def PRG_LOAD_ADDRESS() = 0x801
  protected def PRG_RUN_DELAY_CYCLES = 2500000
  protected var lastLoadedPrg : Option[File] = None
  protected var headless = false // used with --testcart command options
  protected var cpujamContinue = false // used with --cpujam-continue
  protected var zoomOverride = false // used with --screen-dim
  protected var sidCycleExact = false // used with --sid-cycle-exact
  protected var loadStateFromOptions = false // used with --load-state
  protected var traceOption = false // used with --trace
  protected var fullScreenAtBoot = false // used with --fullscreen
  protected var ignoreConfig = false // used with --ignore-config-file

  protected val cartMenu = new JMenu("Cartridge")
  protected var cartButtonRequested = false

  protected lazy val keybMapper : keyboard.KeyboardMapper = keyboard.KeyboardMapperStore.loadMapper(Option(configuration.getProperty(CONFIGURATION_KEYB_MAP_FILE)),DEFAULT_KEYBOARD_RESOURCE_NAME,cbmModel)
  protected val keyb : Keyboard

  protected var display : Display = _
  protected var gifRecorder : JDialog = _

  protected lazy val displayFrame: JFrame = {
    val f = new JFrame(s"$APPLICATION_NAME " + ucesoft.cbm.Version.VERSION)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = turnOff
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    f.setFocusTraversalKeysEnabled(false) // to enable TAB key
    f
  }

  protected def isC64Mode : Boolean = false

  // memory & main cpu
  protected val mmu : Memory
  protected lazy val cpu: CPU65xx = CPU65xx.make(mmu)
  // main chips
  protected val clock: Clock = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)
  // -------------------- TAPE -----------------
  protected val tapeAllowed = true
  protected var datassette : Datassette = _
  // -------------------- DISK -----------------
  protected val drives : Array[Drive with TraceListener] = Array.ofDim(TOTAL_DRIVES)
  protected var device12Drive : Drive = _
  protected var device12DriveEnabled = false
  protected var canWriteOnDisk = true
  protected val drivesRunning: Array[Boolean] = Array.fill[Boolean](TOTAL_DRIVES)(true)
  protected val drivesEnabled: Array[Boolean] = Array.fill[Boolean](TOTAL_DRIVES)(true)
  protected lazy val diskFlusher = new FloppyFlushUI(displayFrame)
  protected val driveLeds: Array[DriveLed] = (for(d <- 0 until TOTAL_DRIVES) yield {
    val led = new DriveLed(d + 8)
    led.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = attachDisk(d,false,isC64Mode)
    })
    led
  }).toArray
  protected val floppyComponents: Array[FloppyComponent] = Array.ofDim[FloppyComponent](TOTAL_DRIVES)
  protected val driveLedListeners: Array[AbstractDriveLedListener] = {
    (for(d <- 0 until TOTAL_DRIVES) yield {
      new AbstractDriveLedListener(driveLeds(d)) {
        if (d > 0) driveLeds(d).setVisible(false)
      }
    }).toArray
  }
  // -------------- AUDIO ----------------------
  protected val volumeDialog: VolumeSettingsPanel.VolumeDialog
  // -------------------- PRINTER --------------
  protected var printerEnabled = false
  protected val printerGraphicsDriver = new MPS803GFXDriver(new MPS803ROM)
  protected val printer : Printer
  protected lazy val printerDialog: JDialog = {
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
    dialog.pack()
    dialog
  }
  // -------------- MENU ITEMS -----------------
  protected val maxSpeedItem = new JCheckBoxMenuItem("Warp mode")
  protected val loadFileItems: IndexedSeq[JMenuItem] = for(d <- 0 until TOTAL_DRIVES) yield new JMenuItem(s"Load file from attached disk ${d + 8} ...")
  protected val tapeMenu = new JMenu("Tape control...")

  // -------------------- TRACE ----------------
  protected var inspectDialog : InspectPanelDialog = _
  protected var traceItem : JCheckBoxMenuItem = _
  protected var tracer : Tracer = new TracerGUI(traceOpened => traceItem.setSelected(traceOpened))

  // ------------------------------------ Drag and Drop ----------------------------
  protected val DNDHandler = new DNDHandler(handleDND(_,true,true))

  protected val preferences = new Preferences

  protected lazy val configuration: Properties = {
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
    else {
      setDefaultProperties(props)
    }

    configurationLoaded(props)
    props
  }

  def turnOn(args:Array[String]) : Unit

  def turnOff() : Unit = {
    if (!headless) saveSettings(preferences[Boolean](Preferences.PREF_PREFAUTOSAVE).getOrElse(false))
    for (d <- drives)
      d.getFloppy.close
    shutdownComponent
    sys.exit(0)
  }

  protected def reset(play:Boolean,loadAndRunLastPrg:Boolean = false) : Unit = {
    tracer.enableTracing(false)
    if (Thread.currentThread != Clock.systemClock) clock.pause
    resetComponent
    if (loadAndRunLastPrg) lastLoadedPrg.foreach( f =>
      clock.schedule(new ClockEvent("RESET_PRG",clock.currentCycles + PRG_RUN_DELAY_CYCLES,_ => loadPRGFile(f,true)))
    )

    if (play) clock.play
  }

  protected def hardReset(play:Boolean=true) : Unit = {
    tracer.enableTracing(false)
    if (Thread.currentThread != Clock.systemClock) clock.pause
    hardResetComponent

    if (play) clock.play
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
            tracer.enableTracing(true)
            trace(true,true)
          case JOptionPane.CANCEL_OPTION => // continue
          case _ =>
            reset(true)
        }
      case _:CPUJammedException => // continue
      case _ =>
        if (headless) {
          println(s"Fatal error occurred on cycle ${clock.currentCycles}: $cpu\n${CPU65xx.disassemble(mmu, cpu.getCurrentInstructionPC)}")
          t.printStackTrace()
          sys.exit(1)
        } // exit if headless

        Log.info("Fatal error occurred: " + cpu + "-" + t)
        try Log.info(CPU65xx.disassemble(mmu,cpu.getCurrentInstructionPC).toString) catch { case _:Throwable => }
        t.printStackTrace(Log.getOut)
        t.printStackTrace()
        JOptionPane.showMessageDialog(displayFrame,t.toString + " [PC=" + Integer.toHexString(cpu.getCurrentInstructionPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
        //trace(true,true)
        reset(true)
    }
  }

  protected def trace(cpu:Boolean,on:Boolean) : Unit = {
    tracer.setVisible(on)
  }

  protected def setDisplayRendering(hints:java.lang.Object) : Unit = {
    display.setRenderingHints(hints)
  }

  protected def setRenderingSettings(parent: JMenu,includePalette:Boolean = false): Unit = {
    import Preferences._
    // RENDERING-TYPE ======================================================================================
    val renderingItem = new JMenu("Rendering")
    parent.add(renderingItem)
    val groupR = new ButtonGroup
    val renderingDefault1Item = new JRadioButtonMenuItem("Default")
    renderingDefault1Item.setSelected(true)
    renderingDefault1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "default")
    renderingItem.add(renderingDefault1Item)
    groupR.add(renderingDefault1Item)
    val renderingBilinear1Item = new JRadioButtonMenuItem("Bilinear")
    renderingBilinear1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "bilinear")
    renderingItem.add(renderingBilinear1Item)
    groupR.add(renderingBilinear1Item)
    val renderingBicubic1Item = new JRadioButtonMenuItem("Bicubic")
    renderingBicubic1Item.addActionListener(_ => preferences(PREF_RENDERINGTYPE) = "bicubic")
    renderingItem.add(renderingBicubic1Item)
    groupR.add(renderingBicubic1Item)

    preferences.add(PREF_RENDERINGTYPE, "Set the rendering type (default,bilinear,bicubic)", "", Set("default", "bilinear", "bicubic")) { rt =>
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
  }

  protected def warpMode(warpOn:Boolean,play:Boolean = true): Unit = {
    maxSpeedItem.setSelected(warpOn)
    clock.maximumSpeed = warpOn
  }

  protected def configurationLoaded(properties: Properties): Unit = {
    ROM.props = properties
  }

  protected def setDefaultProperties(configuration:Properties) : Unit = {
    import Preferences._
    configuration.setProperty(PREF_RENDERINGTYPE,"default")
    configuration.setProperty(PREF_WRITEONDISK,"true")
  }

  protected def saveConfigurationFile() : Unit = {
    try {
      val kernalConfigHome = System.getProperty("kernal.config",scala.util.Properties.userHome)
      val propsFile = new File(new File(kernalConfigHome), CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "Kernal64 configuration file")
      out.close()
    }
    catch {
      case _: IOException =>
    }
  }

  protected def showKeyboardEditor(): Unit = {
    val editor = KeyboardConfigEditor.getEditor(displayFrame,keyb.getKeyboardMapper,configuration,CONFIGURATION_KEYB_MAP_FILE)
    editor.setVisible(true)
  }

  protected def showAbout()  : Unit = {
    val about = new AboutCanvas(getCharROM,ucesoft.cbm.Version.VERSION.toUpperCase + " (" + ucesoft.cbm.Version.BUILD_DATE.toUpperCase + ")")
    JOptionPane.showMessageDialog(displayFrame,about,"About",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  protected def showSettings() : Unit = {
    val settingsPanel = new SettingsPanel(preferences)
    JOptionPane.showMessageDialog(displayFrame,settingsPanel,"Settings",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/commodore_file.png")))
  }

  protected def ejectDisk(driveID:Int) : Unit = {
    drives(driveID).getFloppy.close
    driveLeds(driveID).setToolTipText("")
    if (!tracer.isTracing()) clock.pause
    if (drives(driveID).driveType == DriveType._1581) drives(driveID).setDriveReader(D1581.MFMEmptyFloppy,true)
    else drives(driveID).setDriveReader(EmptyFloppy,true)
    loadFileItems(driveID).setEnabled(false)
    preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_FILE(driveID),"")
    clock.play
  }

  protected def enablePrinter(enable:Boolean) : Unit = {
    printerEnabled = enable
    printer.setActive(enable)
  }

  protected def showPrinterPreview() : Unit = {
    printerGraphicsDriver.checkSize
    printerDialog.setVisible(true)
  }

  protected def printerSaveImage()  : Unit = {
    val fc = new JFileChooser
    fc.showSaveDialog(printerDialog) match {
      case JFileChooser.APPROVE_OPTION =>
        val file = if (fc.getSelectedFile.getName.toUpperCase.endsWith(".PNG")) fc.getSelectedFile else new File(fc.getSelectedFile.toString + ".png")
        printerGraphicsDriver.saveAsPNG(file)
      case _ =>
    }
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

  protected def initializedDrives(defaultDriveType:DriveType.Value) : Unit = {
    for(d <- 0 until TOTAL_DRIVES) {
      initDrive(d,defaultDriveType)
      if (d > 0) drivesEnabled(d) = false
      floppyComponents(d) = new FloppyComponent(8 + d,drives(d),driveLeds(d))
      add(floppyComponents(d))
      add(driveLeds(d))
    }
  }
  protected def setDriveMenu(parent: JMenu): Unit = {
    val driveMenu = new JMenuItem("Drives ...")
    driveMenu.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.ALT_DOWN_MASK))
    parent.add(driveMenu)
    driveMenu.addActionListener(_ => DrivesConfigPanel.getDriveConfigDialog.setVisible(true))
  }

  protected def setWarpModeSettings(parent: JMenu): Unit = {
    maxSpeedItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.ALT_DOWN_MASK))
    maxSpeedItem.setSelected(clock.maximumSpeed)
    maxSpeedItem.addActionListener(e => warpMode(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected))
    parent.add(maxSpeedItem)
  }

  protected def setPauseSettings(parent: JMenu): Unit = {
    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(e =>
      if (e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected) {
        clock.pause
        display.setPaused
      } else clock.play)
    parent.add(pauseItem)
  }

  protected def writeOnDiskSetting(enabled:Boolean) : Unit = {
    canWriteOnDisk = enabled
    for(d <- 0 until TOTAL_DRIVES) drives(d).getFloppy.canWriteOnDisk = canWriteOnDisk
  }

  protected def reloadROM(resource: String, location: String): Unit = {
    val oldLocation = configuration.getProperty(resource)
    try {
      configuration.setProperty(resource, location)
      ROM.reload(resource)
    }
    finally {
      configuration.setProperty(resource, if (oldLocation == null) "" else oldLocation)
    }
  }

  protected def setDriveType(drive:Int,dt:DriveType.Value,dontPlay:Boolean = false) : Unit = {
    clock.pause
    initDrive(drive,dt)
    val driveType = if (dt == DriveType.LOCAL) s"$dt=${DrivesConfigPanel.getLocalPathFor(drive)}" else dt.toString
    preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_TYPE(drive),driveType)
    if (!dontPlay) clock.play
  }

  protected def enableDrive(id: Int, enabled: Boolean, updateFrame: Boolean): Unit = {
    drivesEnabled(id) = enabled
    drives(id).setActive(enabled)
    val dim = display.getPreferredSize
    driveLeds(id).setVisible(enabled)
    display.setPreferredSize(dim)
    displayFrame.pack()
    preferences(Preferences.PREF_DRIVE_X_ENABLED(id)) = enabled
  }

  protected def makeDisk() : Unit = {
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

  protected def attachTape(): Unit = {
    val fc = new JFileChooser
    val canvas = new TAPCanvas(fc, getCharROM, isC64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP")

      def getDescription = "TAP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val tapFile = canvas.selectedObject.asInstanceOf[Option[TAP.TAPHeader]]
        attachTapeFile(fc.getSelectedFile, tapFile, tapFile.isDefined)
      case _ =>
    }
  }

  protected def attachLocalDir(driveID: Int): Unit = {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc, getCharROM, isC64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setDialogTitle(s"Attach local directory to drive ${driveID + 8}")
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID, fc.getSelectedFile, false, canvas.selectedFile)
      case _ =>
    }
  }

  protected def attachTapeFile(file: File, tapFile: Option[TAP.TAPHeader], autorun: Boolean): Unit

  protected def openGIFRecorder() : Unit = gifRecorder.setVisible(true)

  protected def setMenu(): Unit = setMenu(true,true)

  protected def setMenu(enableCarts:Boolean,enableGames:Boolean) : Unit = {
    val menuBar = new JMenuBar
    val fileMenu = new JMenu("File")
    val editMenu = new JMenu("Edit")
    val stateMenu = new JMenu("State")
    val traceMenu = new JMenu("Trace")
    val optionMenu = new JMenu("Settings")
    val gamesMenu = new JMenu("Games")
    val helpMenu = new JMenu("Help")

    if (enableCarts) cartMenu.setVisible(false)

    menuBar.add(fileMenu)
    menuBar.add(editMenu)
    menuBar.add(stateMenu)
    menuBar.add(traceMenu)
    menuBar.add(optionMenu)
    if (enableCarts) menuBar.add(cartMenu)
    if (enableGames) menuBar.add(gamesMenu)
    menuBar.add(helpMenu)

    setFileMenu(fileMenu)
    setEditMenu(editMenu)
    setStateMenu(stateMenu)
    setTraceMenu(traceMenu)
    setSettingsMenu(optionMenu)
    if (enableGames) setGameMenu(gamesMenu)
    setHelpMenu(helpMenu)

    if (enableCarts) {
      val cartInfoItem = new JMenuItem("Cart info ...")
      cartInfoItem.addActionListener(_ => showCartInfo)
      cartMenu.add(cartInfoItem)
      val cartButtonItem = new JMenuItem("Press cartridge button...")
      cartButtonItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.event.InputEvent.ALT_DOWN_MASK))
      cartButtonItem.addActionListener(_ => cartButtonRequested = true)
      cartMenu.add(cartButtonItem)
    }

    displayFrame.setJMenuBar(menuBar)

    setGlobalCommandLineOptions

  }


  protected def swing(f: => Unit) : Unit = SwingUtilities.invokeAndWait(() => f)

  // Abstract methods
  protected def setFileMenu(menu: JMenu): Unit

  protected def setEditMenu(editMenu: JMenu): Unit = {
    val pasteItem = new JMenuItem("Paste text")
    pasteItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, java.awt.Event.CTRL_MASK))
    pasteItem.addActionListener(_ => paste)
    editMenu.add(pasteItem)
    val listItem = new JMenuItem("List BASIC to editor")
    listItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I, java.awt.Event.ALT_MASK))
    listItem.addActionListener(_ => listBASIC() )
    editMenu.add(listItem)
  }

  protected def listBASIC(): Unit

  protected def setStateMenu(stateMenu: JMenu): Unit = {
    val saveStateItem = new JMenuItem("Save state ...")
    saveStateItem.addActionListener(_ => saveState())
    stateMenu.add(saveStateItem)
    val loadStateItem = new JMenuItem("Load state ...")
    loadStateItem.addActionListener(_ => loadState(None))
    stateMenu.add(loadStateItem)
  }

  protected def setTraceMenu(traceMenu: JMenu): Unit = {
    traceItem = new JCheckBoxMenuItem("Trace devices ...")
    traceItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.ALT_DOWN_MASK))
    traceItem.setSelected(false)
    traceItem.addActionListener(e => trace(true, traceItem.isSelected))
    traceMenu.add(traceItem)

    val inspectItem = new JCheckBoxMenuItem("Inspect components ...")
    inspectItem.setSelected(false)
    inspectItem.addActionListener(e => inspectDialog.setVisible(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected))
    traceMenu.add(inspectItem)

  }
  protected def setGameMenu(menu: JMenu): Unit = {}

  protected def setHelpMenu(helpMenu: JMenu): Unit = {
    val aboutItem = new JMenuItem("About")
    aboutItem.addActionListener(_ => showAbout)
    helpMenu.add(aboutItem)
    val settingsItem = new JMenuItem("Settings")
    settingsItem.addActionListener(_ => showSettings)
    helpMenu.add(settingsItem)
  }
  protected def showCartInfo() : Unit = {}

  protected def setGlobalCommandLineOptions() : Unit = {}

  protected def getCharROM : Memory

  protected def initComputer() : Unit

  protected def attachDevice(file:File,autorun:Boolean,fileToLoad:Option[String] = None,emulateInserting:Boolean = true) : Unit

  protected def handleDND(file:File,_reset:Boolean,autorun:Boolean) : Unit

  protected def loadPRGFile(file:File,autorun:Boolean) : Unit

  protected def saveSettings(save:Boolean) : Unit

  protected def setSettingsMenu(optionsMenu:JMenu) : Unit

  protected def paste() : Unit

  protected def mainLoop(cycles:Long) : Unit

  protected def initDrive(id:Int,driveType:DriveType.Value) : Unit

  protected def attachDisk(driveID: Int, autorun: Boolean, c64Mode: Boolean): Unit = {
    val fc = new JFileChooser
    val canvas = new D64Canvas(fc, getCharROM, c64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setDialogTitle(s"Attach disk to drive ${driveID + 8}")
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR, "./")))
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File): Boolean = f.isDirectory || drives(driveID).formatExtList.exists { ext => try {
        f.toString.toUpperCase.endsWith(ext)
      } catch {
        case _: Throwable => false
      }
      }

      def getDescription = s"${drives(driveID).formatExtList.mkString(",")} files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachDiskFile(driveID, fc.getSelectedFile, autorun, canvas.selectedFile)
      case _ =>
    }
  }

  protected def attachDiskFile(driveID: Int, file: File, autorun: Boolean, fileToLoad: Option[String], emulateInserting: Boolean = true): Unit

  protected def delayedAutorun(fn:String): Unit

  protected def loadSettings(args: Array[String]): Unit = {
    // AUTOPLAY
    preferences.parseAndLoad(args, configuration) match {
      case None =>
        // run the given file name
        preferences[String](Preferences.PREF_RUNFILE) match {
          case Some(fn) if fn != null && fn != "" =>
            if (!new File(fn).exists()) throw new FileNotFoundException(fn)
            delayedAutorun(fn)
          case _ =>
        }
      case Some(f) =>
        preferences[String](Preferences.PREF_DRIVE_X_FILE(0)) match {
          case None =>
            if (!new File(f).exists()) throw new FileNotFoundException(f)
            handleDND(new File(f), false, true)
          case Some(_) =>
            // here we have both drive8 and PRG set: we load the given PRG file from disk 8
            val fn = new File(f).getName
            val dot = fn.indexOf('.')
            val cbmFile = if (dot > 0) fn.substring(0, dot) else f
            delayedAutorun(cbmFile)
        }
    }
    DrivesConfigPanel.registerDrives(displayFrame, drives, setDriveType(_, _, false), enableDrive(_, _, true), attachDisk(_, _, isC64Mode), attachDiskFile(_, _, _, None), drivesEnabled,ALLOWED_DRIVE_TYPES)
  }

  protected def resetSettings() : Unit = {}

  protected def setVolumeSettings(parent: JMenu): Unit = {
    val volumeItem = new JMenuItem("Volume settings ...")
    volumeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, java.awt.event.InputEvent.ALT_DOWN_MASK))
    volumeItem.addActionListener(_ => volumeDialog.setVisible(true))
    parent.add(volumeItem)
  }

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
            def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
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
        val model = in.readObject().asInstanceOf[CBMComputerModel]
        if (model != cbmModel) throw new IOException(s"Invalid state file. Found state saved for model ${model.modelName}, expected ${cbmModel.modelName}")
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
          in.close()
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
        t.printStackTrace()
        reset(false)
    }
    finally {
      if (in != null) in.close()
      clock.play
    }
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
        def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".K64")
        def getDescription = "Kernal64 state files"
      })
      val fn = fc.showSaveDialog(getActiveFrame.get) match {
        case JFileChooser.APPROVE_OPTION =>
          if (fc.getSelectedFile.getName.toUpperCase.endsWith(".K64")) fc.getSelectedFile.toString else fc.getSelectedFile.toString + ".k64"
        case _ =>
          return
      }
      out = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(fn)))
      out.writeObject(cbmModel)
      out.writeObject(ucesoft.cbm.Version.VERSION)
      out.writeLong(System.currentTimeMillis)
      save(out)
      out.close()
    }
    catch {
      case t:Throwable =>

        showError("State saving error","Can't save state. Unexpected error occurred: " + t)
        t.printStackTrace()
    }
    finally {
      if (out != null) out.close()
      clock.play
    }
  }
}
