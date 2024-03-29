package ucesoft.cbm

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.{Memory, ROM}
import ucesoft.cbm.expansion._
import ucesoft.cbm.formats._
import ucesoft.cbm.formats.cart.{EasyFlash, GMOD3}
import ucesoft.cbm.game.{GamePlayer, GameUI}
import ucesoft.cbm.misc._
import ucesoft.cbm.peripheral.bus.{IECBus, IEEE488Bus}
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.controlport.Joysticks._
import ucesoft.cbm.peripheral.controlport.{ControlPort, JoystickSettingDialog}
import ucesoft.cbm.peripheral.drive._
import ucesoft.cbm.peripheral.keyboard.HomeKeyboard
import ucesoft.cbm.peripheral.printer.{MPS803, Printer}
import ucesoft.cbm.peripheral.rs232._
import ucesoft.cbm.peripheral.vic.Palette.PaletteType
import ucesoft.cbm.peripheral.vic._
import ucesoft.cbm.peripheral.vic.coprocessor.VASYL
import ucesoft.cbm.peripheral.{controlport, keyboard, vic}
import ucesoft.cbm.remote.RemoteC64
import ucesoft.cbm.trace.{InspectPanel, Tracer}

import java.awt.datatransfer.DataFlavor
import java.awt.event._
import java.awt.{BorderLayout, Cursor, Dimension, Toolkit}
import java.io._
import java.util.{Properties, ServiceLoader}
import javax.swing._
import javax.swing.filechooser.FileFilter
import scala.util.{Failure, Success}

abstract class CBMHomeComputer extends CBMComputer with GamePlayer with KeyListener { cbmComputer =>
  override protected val ALLOWED_DRIVE_TYPES = DrivesConfigPanel.ALL_DRIVES_ALLOWED
  protected val CONFIGURATION_GMOD2_FILE = "gmod2.file"

  protected val DEFAULT_GAME_PROVIDERS = java.util.Arrays.asList((new ucesoft.cbm.game.CSDBSpi).asInstanceOf[ucesoft.cbm.game.GameProvider],(new ucesoft.cbm.game.GameBaseSpi).asInstanceOf[ucesoft.cbm.game.GameProvider],(new ucesoft.cbm.game.PouetDemoSpi).asInstanceOf[ucesoft.cbm.game.GameProvider])

  protected val detachCtrItem = new JMenuItem("Detach cartridge")
  protected val easyFlashWriteChangesItem = new JMenuItem("Write changes")
  protected val GMOD3WriteChangesItem = new JMenuItem("GMOD3 Write changes")

  protected var loadPRGasDisk = false // used with --prg-as-disk
  protected var disk8LoadedAsPRG = false

  protected var vicChip : vic.VIC = _
  protected var vicZoomFactor : Int = 1
  protected var cia1,cia2 : CIA = _
  protected val cia12Running: Array[Boolean] = Array(true,true)
  protected val sid = new ucesoft.cbm.peripheral.sid.SID
  protected val nmiSwitcher = new Switcher("NMI",cpu.nmiRequest _)//new NMISwitcher(cpu.nmiRequest _)
  protected val irqSwitcher = new Switcher("IRQ",cpu.irqRequest _)//new IRQSwitcher(cpu.irqRequest _)
  protected val dmaSwitcher = new Switcher("DMA",setDMA _)
  override protected lazy val keyb = new keyboard.HomeKeyboard(keybMapper,nmiSwitcher.setLine(Switcher.KB,_),cbmModel)	// key listener

  protected val bus = new IECBus
  protected val ieee488Bus = new IEEE488Bus
  protected var dma = false
  protected val expansionPort: ExpansionPort = ExpansionPort.getExpansionPort

  protected var resetSettingsActions : List[() => Unit] = Nil

  // ----------------- REMOTE ------------------
  protected var remote : Option[RemoteC64] = None

  protected val flyerIEC = new FlyerIEC(bus,file => attachDiskFile(0,file,false,None))
  protected var isFlyerEnabled = false

  // ----------------- RS-232 ------------------
  protected val rs232: BridgeRS232.type = BridgeRS232
  protected val AVAILABLE_RS232 : Array[RS232] = Array(//UP9600,
    TelnetRS232,
    TCPRS232,
    FileRS232,
    SwiftLink.getSL(nmiSwitcher.setLine(Switcher.CRT,_),None),
    SwiftLink.getSL(nmiSwitcher.setLine(Switcher.CRT,_),Some(REU.getREU(REU.REU_1750,mmu,dmaSwitcher.setLine(Switcher.CRT,_),irqSwitcher.setLine(Switcher.CRT,_),None))),
    ProcessRS232)
  override protected val printer : Printer = new MPS803(bus,printerGraphicsDriver)
  // -------------- AUDIO ----------------------
  override protected lazy val volumeDialog : VolumeSettingsPanel.VolumeDialog = VolumeSettingsPanel.getDialog(displayFrame,sid.getDriver)
  // ------------ Control Port -----------------------
  protected lazy val gameControlPort = new controlport.GamePadControlPort(configuration)
  protected val keypadControlPort: ControlPort with MouseListener with KeyListener = controlport.ControlPort.keypadControlPort
  protected lazy val keyboardControlPort: ControlPort with MouseListener with KeyListener = controlport.ControlPort.userDefinedKeyControlPort(configuration)
  protected val controlPortA = new controlport.ControlPortBridge(keypadControlPort,"Control Port 1")
  protected lazy val controlPortB = new controlport.ControlPortBridge(gameControlPort,"Control port 2")
  // -------------- Light Pen -------------------------
  protected val LIGHT_PEN_NO_BUTTON = 0
  protected val LIGHT_PEN_BUTTON_UP = 1
  protected val LIGHT_PEN_BUTTON_LEFT = 2
  protected val LIGHT_GUN_BUTTON_POTY = 4
  protected var lightPenButtonEmulation: Int = LIGHT_PEN_NO_BUTTON

  protected class LightPenButtonListener extends MouseAdapter with CBMComponent {
    val componentID = "Light pen"
    val componentType: Type = CBMComponentType.INPUT_DEVICE

    override def mousePressed(e:MouseEvent) : Unit = {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case LIGHT_PEN_BUTTON_UP => controlPortB.emulateUp()
        case LIGHT_PEN_BUTTON_LEFT => controlPortB.emulateLeft()
        case LIGHT_GUN_BUTTON_POTY => sid.setLightGunEnabled(true,potx = false,0)
      }
    }
    override def mouseReleased(e:MouseEvent) : Unit = {
      lightPenButtonEmulation match {
        case LIGHT_PEN_NO_BUTTON =>
        case LIGHT_GUN_BUTTON_POTY => sid.setLightGunEnabled(true,potx = false)
        case _ => controlPortB.releaseEmulated()
      }
    }

    def init()  : Unit = {}
    def reset()  : Unit = {}
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {}
    protected def loadState(in:ObjectInputStream) : Unit = {}
    protected def allowsStateRestoring : Boolean = true
  }
  // ----------------------WiC64 ---------------------------------------------------
  protected var wic64Panel : WiC64Panel = _
  // ------------------- INITIALIZATION --------------------------------------------
  initComputer()

  override protected def initComputer() : Unit = {
    ExpansionPort.setExpansionPortStateHandler(expansionPortStateHandler _)
  }

  override def afterInitHook() : Unit = {
    inspectDialog = InspectPanel.getInspectDialog(displayFrame, this,cbmModel)
    // deactivate drives > 8
    for(d <- 1 until TOTAL_DRIVES) {
      drives(d).setActive(false)
      driveLeds(d).setVisible(false)
    }
  }

  // ---------------- KEY LISTENER -----------------------------
  override def keyTyped(e: KeyEvent): Unit = {}
  private def isKeyForJoystick(e:KeyEvent): Boolean = controlPortA.isConnected && controlPortA.consumeKey(e) || controlPortB.isConnected && controlPortB.consumeKey(e)
  override def keyPressed(e: KeyEvent): Unit = {
    keypadControlPort.keyPressed(e)
    keyboardControlPort.keyPressed(e)
    // check if the key pressed is in charge of joysticks: in this case it does not forward the event to keyboard
    if (!isKeyForJoystick(e)) keyb.keyPressed(e)
  }
  override def keyReleased(e: KeyEvent): Unit = {
    keypadControlPort.keyReleased(e)
    keyboardControlPort.keyReleased(e)
    // check if the key pressed is in charge of joysticks: in this case it does not forward the event to keyboard
    if (!isKeyForJoystick(e)) keyb.keyReleased(e)
  }
  // -----------------------------------------------------------

  override protected def isC64Mode : Boolean = true

  protected def getRAM : Memory

  protected def setDMA(dma:Boolean) : Unit

  override protected def paste() : Unit = {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      HomeKeyboard.insertTextIntoKeyboardBuffer(str,mmu,isC64Mode)
    }
  }

  protected def savePrg() : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
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

  override protected def delayedAutorun(fn:String): Unit = {
    val cmd = if (isC64Mode) s"""LOAD"$fn",8,1""" + 13.toChar + "RUN" + 13.toChar else s"""RUN"$fn"""" + 13.toChar
    clock.schedule(new ClockEvent("Loading", clock.currentCycles + PRG_RUN_DELAY_CYCLES, _ => HomeKeyboard.insertTextIntoKeyboardBuffer(cmd, mmu, isC64Mode)))
  }

  override protected def attachDiskFile(driveID:Int,file:File,autorun:Boolean,fileToLoad:Option[String],emulateInserting:Boolean = true) : Unit = {
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
      drives(driveID).getFloppy.close()
      if (!tracer.isTracing()) clock.pause()
      drives(driveID).setDriveReader(disk,emulateInserting)
      preferences.updateWithoutNotify(Preferences.PREF_DRIVE_X_FILE(driveID),file.toString)
      clock.play()

      loadFileItems(driveID).setEnabled(isD64)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      val drive = driveID + 8
      fileToLoad match {
        case Some(fn) =>
          val cmd = s"""LOAD"$fn",$drive,1""" + 13.toChar + (if (autorun) "RUN" + 13.toChar else "")
          HomeKeyboard.insertTextIntoKeyboardBuffer(cmd,mmu,isC64Mode)
        case None if autorun =>
          //HomeKeyboard.insertSmallTextIntoKeyboardBuffer(s"""LOAD"*",$drive,1""" + 13.toChar + "RUN" + 13.toChar,mmu,isC64Mode)
          HomeKeyboard.insertTextIntoKeyboardBuffer(s"""LOAD"*",$drive,1""" + 13.toChar + "RUN" + 13.toChar,mmu,isC64Mode)
        case _ =>
      }
      driveLeds(driveID).setToolTipText(disk.toString)
    }
    catch {
      case t:Throwable =>
        t.printStackTrace()

        showError("Disk attaching error",t.toString)
    }
  }

  override protected def resetSettings() : Unit = {
    for(a <- resetSettingsActions) a()
  }

  def expansionPortStateHandler(in:ObjectInputStream,portType:ExpansionPortType.Value) : Unit = {
    import ExpansionPortType._
    import Preferences._

    portType match {
      case CRT =>
        val (crtFile,_) = Cartridge.createCRTFileFromState(in)
        loadCartridgeFile(crtFile,true)
      case CPM =>
        preferences(PREF_CPMCARTENABLED) = true
      case GEORAM =>
        preferences(PREF_GEORAM) = in.readInt.toString
      case REU =>
        preferences(PREF_REUTYPE) = in.readInt.toString
      case DUALSID =>
        preferences(PREF_DUALSID) = in.readInt.toString
      case RAMCART =>
        preferences(PREF_RAMCART) = in.readInt.toString
      case ISEPIC =>
        preferences(PREF_ISEPIC) = true
      case _ =>
    }
  }

  override protected def handleDND(file:File,_reset:Boolean,autorun:Boolean) : Unit = {
    val name = file.getName.toUpperCase
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
    else {
      if (_reset) reset(false)
      if (autorun) {
        clock.schedule(new ClockEvent("Loading",clock.currentCycles + PRG_RUN_DELAY_CYCLES,(cycles) => { attachDevice(file,true,None,false) }))
        clock.play()
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
    d64.rename("AUTOSTART","  ")
    val in = new java.io.FileInputStream(file)
    val prg = in.readAllBytes()
    in.close()
    val startAddress = prg(0) | prg(1) << 8
    val content = prg.drop(2)
    val prgName = name.dropRight(4)
    d64.addPRG(content,prgName,startAddress)
    d64.close()
    disk8LoadedAsPRG = true
    attachDiskFile(0,disk,autorun,Some(prgName),false)
  }

  override protected def attachDevice(file:File,autorun:Boolean,fileToLoad:Option[String] = None,emulateInserting:Boolean = true) : Unit = {
    val name = file.getName.toUpperCase

    if (name.endsWith(".PRG") && loadPRGasDisk) attachPRGAsDisk(file)
    else
    if (name.endsWith(".PRG")) {
      loadPRGFile(file,autorun)
      lastLoadedPrg = Some(file)
    }
    else
    if (name.endsWith(".D64") || name.endsWith(".G64") || name.endsWith(".D71") || name.endsWith(".D81") || name.endsWith(".G71")) attachDiskFile(0,file,autorun,fileToLoad,emulateInserting)
    else
    if (tapeAllowed && name.endsWith(".TAP")) attachTapeFile(file,None,autorun)
    else
    if (name.endsWith(".T64")) attachT64File(file,autorun)
    else
    if (name.endsWith(".ZIP")) attachZIPFile(file,autorun)
    else
    if (name.endsWith(".CRT")) loadCartridgeFile(file)
  }

  protected def attachZip()  : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".ZIP")
      def getDescription = "ZIP files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        attachZIPFile(fc.getSelectedFile,false)
      case _ =>
    }
  }

  protected def loadCartridgeFile(file:File,stateLoading : Boolean = false) : Option[Cartridge] = {
    try {
      if (!stateLoading && Thread.currentThread != Clock.systemClock) clock.pause()
      ExpansionPort.getExpansionPort.eject()
      val ep = ExpansionPortFactory.loadExpansionPort(file.toString,irqSwitcher.setLine(Switcher.CRT,_),nmiSwitcher.setLine(Switcher.CRT,_),getRAM,mmu,() => reset(true),configuration)
      println(ep)
      cartMenu.setVisible(true)
      ExpansionPort.setExpansionPort(ep)
      ExpansionPort.currentCartFileName = file.toString
      Log.info(s"Attached cartridge ${ExpansionPort.getExpansionPort.name}")
      preferences.updateWithoutNotify(Preferences.PREF_CART,file.toString)
      if (!stateLoading) hardReset(false)
      configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
      detachCtrItem.setEnabled(true)
      // easyflash
      easyFlashWriteChangesItem.setEnabled(ep.isInstanceOf[EasyFlash])
      // GMOD3
      GMOD3WriteChangesItem.setEnabled(ep.isInstanceOf[GMOD3])
    }
    catch {
      case t:Throwable =>
        Log.info(t.toString)
        showError("Cartridge loading error",t.toString)
    }
    finally {
      if (!stateLoading) clock.play()
    }

    None
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

  protected def loadFileFromTape()  : Unit = {
    val fc = new JFileChooser
    val canvas = new T64Canvas(fc,getCharROM,isC64Mode)
    val sp = new javax.swing.JScrollPane(canvas)
    canvas.sp = sp
    fc.setAccessory(sp)
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".T64")
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
            HomeKeyboard.insertSmallTextIntoKeyboardBuffer("RUN" + 13.toChar,mmu,true)
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

  override protected def attachTapeFile(file:File,tapFile:Option[TAP.TAPHeader],autorun:Boolean) : Unit = {
    val tap = new TAP(file.toString)
    datassette.setTAP(Some(tap),tapFile.map(_.tapOffset.toInt))
    tapeMenu.setEnabled(true)
    configuration.setProperty(CONFIGURATION_LASTDISKDIR,file.getParentFile.toString)
    if (autorun) {
      datassette.pressPlay()
      HomeKeyboard.insertSmallTextIntoKeyboardBuffer("LOAD" + 13.toChar + "RUN" + 13.toChar,mmu,isC64Mode)
    }
  }

  protected def loadPrg()  : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".PRG")
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

  protected def detachCtr()  : Unit = {
    if (ExpansionPort.getExpansionPort.isEmpty) showError("Detach error","No cartridge attached!")
    else {
      if (Thread.currentThread != Clock.systemClock) clock.pause()
      ExpansionPort.getExpansionPort.eject()
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

  protected def attachCtr()  : Unit = {
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new FileFilter {
      def accept(f:File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".CRT")
      def getDescription = "CRT files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        loadCartridgeFile(fc.getSelectedFile)
      case _ =>
    }
  }

  protected def _1571mode(_1571Mode:Boolean) : Unit = {}

  override protected def initDrive(id:Int,driveType:DriveType.Value) : Unit = {
    val old = Option(drives(id))
    old match {
      case Some(od) if od.driveType == driveType && driveType != DriveType.LOCAL => return
      case _ =>
    }
    drives(id) = driveType match {
      case DriveType._1571 =>
        driveLedListeners(id).setPowerLedMode(false)
        new D1571(id,bus,driveLedListeners(id),_1571mode _)
      case DriveType._1541 =>
        driveLedListeners(id).setPowerLedMode(false)
        new C1541(id,bus,driveLedListeners(id))
      case DriveType._1581 =>
        driveLedListeners(id).setPowerLedMode(true)
        new D1581(id,bus,driveLedListeners(id))
      case DriveType._8050 =>
        driveLedListeners(id).setPowerLedMode(false)
        new IEEE488Drive(s"IEEE488Drive_${id + 8}",id + 8,ieee488Bus,driveLedListeners(id))
      case DriveType.LOCAL =>
        val local = new LocalDrive(bus,id + 8)
        import Preferences._
        val dir  = DrivesConfigPanel.getLocalPathFor(id)
        preferences.updateWithoutNotify(PREF_DRIVE_X_TYPE(id),s"local=${dir}")
        local.setCurrentDir(new File(dir))
        local
    }

    old match {
      case None =>
        add(drives(id))
      case Some(c) =>
        floppyComponents(id).drive = drives(id)
        c.getFloppy.close()
        c.disconnect()
        drivesRunning(id) = true
        drives(id).initComponent()
        change(c,drives(id))
        inspectDialog.updateRoot()

        tracer.removeDevice(Tracer.TracedDevice(c.componentID,c.getMem,c))
        /*
        if (id == 0) {
          diskTraceDialog.mem = drives(id).getMem
          diskTraceDialog.traceListener = drives(id)
        }
         */
    }

    tracer.addDevice(Tracer.TracedDevice(drives(id).componentID,drives(id).getMem,drives(id)))

    drives(id).runningListener = running => {
      drivesRunning(id) = running
    }
  }

  protected def enableMouse(mouseEnabled:Boolean,display:vic.Display) : Unit = {
    controlPortA.setMouse1351Emulation(mouseEnabled)
    sid.setMouseEnabled(mouseEnabled)
    if (mouseEnabled) MouseCage.enableMouseCageOn(display) else MouseCage.disableMouseCage()
  }

  protected def setGeoRAM(enabled:Boolean,size:Int = 0): Unit = {
    if (enabled) ExpansionPort.setExpansionPort(new GeoRAM(size)) else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
  }

  protected def setRAMCart(enabled:Boolean, size:Int): Unit = {
    if (enabled) ExpansionPort.setExpansionPort(new RAMCart(size,getRAM))
    else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
  }

  protected def setIsepic(enabled:Boolean): Unit = {
    if (enabled) ExpansionPort.setExpansionPort(new Isepic(nmiSwitcher.setLine(Switcher.CRT,_)))
    else {
      ExpansionPort.getExpansionPort.eject()
      ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    }
  }

  protected def setREU(reu:Option[Int],reu16FileName:Option[String]) : Unit = {
    reu match {
      case None =>
        ExpansionPort.getExpansionPort.eject()
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

  override protected def warpMode(warpOn:Boolean,play:Boolean = true): Unit = {
    super.warpMode(warpOn, play)
    if (play) clock.pause()
    sid.setFullSpeed(warpOn)
    if (play) clock.play()
  }

  protected def setLightPen(setting:Int): Unit = {
    lightPenButtonEmulation = setting
    controlPortB.setLightPenEmulation(setting != LIGHT_PEN_NO_BUTTON)
    if (setting == LIGHT_GUN_BUTTON_POTY) {
      sid.setLightGunEnabled(true,false)
      vicChip.enableLightPen(true,16,-15)
    }
    else vicChip.enableLightPen(setting != LIGHT_PEN_NO_BUTTON,0,0
    )
    if (setting == LIGHT_PEN_NO_BUTTON) display.setCursor(Cursor.getDefaultCursor)
    else display.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR))
  }

  protected def choose16MREU(): Unit = {
    import Preferences._
    val fc = new JFileChooser
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileView(new C64FileView)
    fc.setFileFilter(new javax.swing.filechooser.FileFilter {
      def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase.endsWith(".REU")
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
        if (remote.isDefined) remote.get.stopRemoting()
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
            remote.start()
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
            rem.stopRemoting()
            display.setRemote(None)
            remote = None
          case None =>
        }
    }
  }

  protected def enableFlyer(enabled:Boolean): Unit = {
    if (enabled != isFlyerEnabled) flyerIEC.reset()
    isFlyerEnabled = enabled
  }

  protected def chooseFlyerDir(): Unit = {
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
          ExpansionPort.getExpansionPort.eject()
          ExpansionPort.setExpansionPort(new DigiMaxCart(a))
      }
    }
    else {
      DigiMAX.enabled(false,false)
      if (ExpansionPort.getExpansionPort.isInstanceOf[DigiMaxCart]) ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    }
  }

  protected def chooseDigiMaxSampleRate(): Unit = {
    Option(JOptionPane.showInputDialog(displayFrame,"DigiMax sample rate Hz",DigiMAX.getSampleRate.toString)) match {
      case None =>
      case Some(rate) =>
        DigiMAX.setSampleRate(rate.toInt)
    }
  }

  protected def chooseGMod2(): Unit = {
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
    ExpansionPort.getExpansionPort.eject()
    if (enabled) {
      ExpansionPort.setExpansionPort(new ucesoft.cbm.expansion.cpm.CPMCartridge(mmu,dmaSwitcher.setLine(Switcher.CRT,_),tracer))
      detachCtrItem.setEnabled(true)
    }
    else ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    preferences.updateWithoutNotify(PREF_CPMCARTENABLED,enabled)
  }

  protected def manageRS232() : Unit = {
    RS232ConfigPanel.RS232ConfigDialog.setVisible(true)
  }

  def play(file:File): Unit = {
    //ExpansionPort.getExpansionPort.eject
    //ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    handleDND(file,true,true)
  }

  protected def configureJoystick() : Unit = {
    def getControlPortFor(id:String) = configuration.getProperty(id) match {
      case CONFIGURATION_KEYPAD_VALUE => keypadControlPort
      case CONFIGURATION_JOYSTICK_VALUE => gameControlPort
      case CONFIGURATION_KEYBOARD_VALUE => keyboardControlPort
      case _ => controlport.ControlPort.emptyControlPort
    }

    controlPortA.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_2)
    controlPortB.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_1)
    keyboardControlPort.updateConfiguration()
  }

  protected def joySettings() : Unit = {
    Clock.systemClock.pause()
    try {
      val dialog = new JoystickSettingDialog(displayFrame,configuration,gameControlPort)
      dialog.setVisible(true)
      configureJoystick()
    }
    finally {
      Clock.systemClock.play()
    }
  }

  protected def swapJoysticks() : Unit = {
    val j1 = configuration.getProperty(CONFIGURATION_JOY_PORT_1)
    val j2 = configuration.getProperty(CONFIGURATION_JOY_PORT_2)
    if (j2 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_1,j2) else configuration.remove(CONFIGURATION_JOY_PORT_1)
    if (j1 != null) configuration.setProperty(CONFIGURATION_JOY_PORT_2,j1) else configuration.remove(CONFIGURATION_JOY_PORT_2)
    configureJoystick()
  }

  protected def setVicFullScreen(): Unit = {
    ucesoft.cbm.misc.FullScreenMode.goFullScreen(displayFrame,
      display,
      vicChip.SCREEN_WIDTH,
      vicChip.SCREEN_HEIGHT,
      keypadControlPort,
      keyb,
      keypadControlPort,
      keyboardControlPort)
  }

  protected def vicZoom(f:Int) : Unit = {
    val dim = new Dimension(vicChip.VISIBLE_SCREEN_WIDTH * f,vicChip.VISIBLE_SCREEN_HEIGHT * f)
    vicZoomFactor = f
    updateVICScreenDimension(dim)
  }

  protected def updateVICScreenDimension(dim:Dimension): Unit = {
    display.setPreferredSize(dim)
    display.invalidate()
    display.repaint()
    displayFrame.pack()
    if (vicChip.VISIBLE_SCREEN_WIDTH == dim.width && vicChip.VISIBLE_SCREEN_HEIGHT == dim.height) vicZoomFactor = 1
    else
    if (vicChip.VISIBLE_SCREEN_WIDTH * 2 == dim.width && vicChip.VISIBLE_SCREEN_HEIGHT * 2 == dim.height) vicZoomFactor = 2
    else
    if (vicChip.VISIBLE_SCREEN_WIDTH * 3 == dim.width && vicChip.VISIBLE_SCREEN_HEIGHT * 3 == dim.height) vicZoomFactor = 3
    else vicZoomFactor = 0 // undefined
  }

  protected def setVICModel(model:VICType.Value,preserveDisplayDim:Boolean = false,resetFlag:Boolean,play:Boolean = true) : Unit = {
    if (play) clock.pause()
    val vicType = cbmModel match {
      case VIC20Model =>
        val vicType = model match {
          case VICType.PAL => VIC_I_PAL
          case VICType.NTSC => VIC_I_NTSC
        }
        vicChip.asInstanceOf[vic.VIC_I].setVICModel(vicType)
        vicType
      case _ =>
        val vicType = model match {
          case VICType.PAL => VIC_II_PAL
          case VICType.NTSC => VIC_II_NTSC
        }
        vicChip.asInstanceOf[vic.VIC_II].setVICModel(vicType)
        vicType
    }

    clock.setClockHz(vicType.CPU_FREQ)
    display.setNewResolution(vicChip.SCREEN_HEIGHT,vicChip.SCREEN_WIDTH)
    vicChip.setDisplay(display)
    if (!preserveDisplayDim) {
      if (vicZoomFactor > 0) vicZoom(vicZoomFactor)
      display.invalidate()
      displayFrame.pack()
    }

    if (resetFlag) reset(false)
    if (play) clock.play()
  }

  // -------------------------------------------------------------------

  override protected def setGlobalCommandLineOptions() : Unit = {
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
    preferences.add(PREF_SCREENDIM,"Zoom factor. Valued accepted are 1,2,3",0,Set(1,2,3),false) { dim =>
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

    preferences.add(PREF_TRACE,"Starts the emulator in trace mode",false,Set(),false) { trace =>
      traceOption = trace
      tracer.enableTracing(trace)
    }
    // WiC64
    preferences.add(PREF_WIC64_NETWORK,"Sets the network interface of WiC64","") { wic64Panel.setNetwork(_) }
    preferences.add(PREF_WIC64_ENABLED,"Enables/disables WiC64 at startup",false) { enabled =>
      wic64Panel.setWiC64Enabled(enabled)
      if (!headless) wic64Panel.dialog.setVisible(true)
    }
    preferences.add(PREF_MOUSE_DELAY_MILLIS,"Sets the mouse delay parameter in millis",20) { delay =>
      MouseCage.setRatioMillis(delay)
    }
  }

  protected def setCartMenu(fileMenu:JMenu) : Unit = {
    import Preferences._
    val attachCtrItem = new JMenuItem("Attach cartridge ...")
    attachCtrItem.addActionListener(_ => attachCtr())
    fileMenu.add(attachCtrItem)
    preferences.add(PREF_CART, "Attach the given cartridge", "") { cart =>
      if (cart != "") loadCartridgeFile(new File(cart))
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
    zipItem.addActionListener(_ => attachZip() )
    fileMenu.add(zipItem)

    val tapeItem = new JMenuItem("Load file from tape ...")
    tapeItem.addActionListener(_ => loadFileFromTape() )
    fileMenu.add(tapeItem)

    if (tapeAllowed) {
      val attachTapeItem = new JMenuItem("Attach tape ...")
      attachTapeItem.addActionListener(_ => attachTape())
      fileMenu.add(attachTapeItem)

      tapeMenu.setEnabled(false)
      fileMenu.add(tapeMenu)

      val tapePlayItem = new JMenuItem("Press play")
      tapePlayItem.addActionListener(_ => datassette.pressPlay())
      tapeMenu.add(tapePlayItem)

      val tapeStopItem = new JMenuItem("Press stop")
      tapeStopItem.addActionListener(_ => datassette.pressStop())
      tapeMenu.add(tapeStopItem)

      val tapeRecordItem = new JMenuItem("Press record & play")
      tapeRecordItem.addActionListener(_ => datassette.pressRecordAndPlay())
      tapeMenu.add(tapeRecordItem)

      val tapeRewindItem = new JMenuItem("Press rewind")
      tapeRewindItem.addActionListener(_ => datassette.pressRewind())
      tapeMenu.add(tapeRewindItem)

      val tapeForwardItem = new JMenuItem("Press forward")
      tapeForwardItem.addActionListener(_ => datassette.pressForward())
      tapeMenu.add(tapeForwardItem)

      val tapeResetItem = new JMenuItem("Reset")
      tapeResetItem.addActionListener(_ => datassette.resetToStart())
      tapeMenu.add(tapeResetItem)

      val tapeResetCounterItem = new JMenuItem("Reset counter")
      tapeResetCounterItem.addActionListener(_ => datassette.resetCounter())
      tapeMenu.add(tapeResetCounterItem)
    }

    fileMenu.addSeparator()

    val makeDiskItem = new JMenuItem("Make empty disk ...")
    makeDiskItem.addActionListener(_ => makeDisk() )
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
    fileMenu.addSeparator()

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
    loadPrgItem.addActionListener(_ => loadPrg() )
    fileMenu.add(loadPrgItem)

    val savePrgItem = new JMenuItem("Save PRG file to local disk ...")
    savePrgItem.addActionListener(_ => savePrg() )
    fileMenu.add(savePrgItem)

    // DRIVEX-ENABLED =====================================================================================
    for(d <- 1 until TOTAL_DRIVES) {
      preferences.add(PREF_DRIVE_X_ENABLED(d),s"Enabled/disable drive ${8 + d}",false) { enableDrive(d, _, false) }
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

    fileMenu.addSeparator()

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
    for(d <- 0 until TOTAL_DRIVES) {
      val resetDrive = new JMenuItem(s"Reset drive ${8 + d}")
      resetMenu.add(resetDrive)
      resetDrive.addActionListener(_ => drives(d).resetComponent() )
    }

    fileMenu.addSeparator()

    // CART ================================================================================================
    setCartMenu(fileMenu)
    // ====================================================================================================

    detachCtrItem.setEnabled(false)
    detachCtrItem.addActionListener(_ => detachCtr() )
    fileMenu.add(detachCtrItem)

    fileMenu.addSeparator()

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

    fileMenu.addSeparator()

    val exitItem = new JMenuItem("Exit")
    exitItem.addActionListener(_ => turnOff() )
    fileMenu.add(exitItem)
  }

  override protected def setGameMenu(gamesMenu: JMenu) : Unit = {
    val loader = ServiceLoader.load(classOf[ucesoft.cbm.game.GameProvider])
    var providers = loader.iterator
    try {
      if (!providers.hasNext) providers = DEFAULT_GAME_PROVIDERS.iterator
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
        t.printStackTrace()
    }
  }

  override protected def setRenderingSettings(parent:JMenu,includePalette:Boolean = true) : Unit = {
    super.setRenderingSettings(parent,false)
    if (includePalette) setPaletteSettings(parent)
  }

  protected def loadPaletteFromFile(prefItem:String): Option[String] = {
    import Preferences._
    val fc = new JFileChooser()
    fc.setDialogTitle("Load VPL palette file")
    fc.setFileFilter(new FileFilter {
      override def accept(f: File): Boolean = f.isDirectory || f.getName.toUpperCase().endsWith("VPL")
      override def getDescription: String = "VPL palette file"
    })
    preferences[String](prefItem) match {
      case None =>
      case Some(file) =>
        fc.setSelectedFile(new File(file))
    }
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        Some(fc.getSelectedFile.toString)
      case _ =>
        None
    }
  }

  protected def setPaletteSettings(parent:JMenu): Unit = {
    import Preferences._
    // VIC-PALETTE =========================================================================================
    val paletteItem = new JMenu("Palette")
    parent.add(paletteItem)
    val groupP = new ButtonGroup
    val vicePalItem = new JRadioButtonMenuItem("VICE")
    vicePalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "vice")
    paletteItem.add(vicePalItem)
    groupP.add(vicePalItem)
    val brightPalItem = new JRadioButtonMenuItem("Bright")
    brightPalItem.setSelected(true)
    brightPalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "bright")
    paletteItem.add(brightPalItem)
    groupP.add(brightPalItem)
    val peptoPalItem = new JRadioButtonMenuItem("Pepto")
    peptoPalItem.addActionListener(_ => preferences(PREF_VICPALETTE) = "pepto")
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
        preferences.updateWithoutNotify(PREF_VICPALETTE,"")
        filePalItem.setSelected(true)
      }
    }

    preferences.add(PREF_VICPALETTE, "Set the VIC's palette type (bright,vice,pepto,colodore)", "", Set("bright", "vice", "pepto", "colodore","")) { pal =>
      pal match {
        case "bright" | "" =>
          Palette.setVICPalette(PaletteType.BRIGHT)
          brightPalItem.setSelected(true)
        case "vice" =>
          Palette.setVICPalette(PaletteType.VICE)
          vicePalItem.setSelected(true)
        case "pepto" =>
          Palette.setVICPalette(PaletteType.PEPTO)
          peptoPalItem.setSelected(true)
        case "colodore" =>
          Palette.setVICPalette(PaletteType.COLORDORE)
          colordorePalItem.setSelected(true)
        case _ =>
      }
    }
    // =====================================================================================================
  }

  protected def setFullScreenSettings(parent:JMenu) : Unit = {
    val fullScreenItem = new JMenuItem("Full screen")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => setVicFullScreen())
    parent.add(fullScreenItem)
  }

  protected def setJoysticsSettings(parent:JMenu) : Unit = {
    val joyAItem = new JMenuItem("Joystick...")
    joyAItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_J, java.awt.event.InputEvent.ALT_DOWN_MASK))
    joyAItem.addActionListener(_ => joySettings() )
    parent.add(joyAItem)

    val swapJoyAItem = new JCheckBoxMenuItem("Swap joysticks")
    swapJoyAItem.addActionListener(_ => swapJoysticks() )
    parent.add(swapJoyAItem)
  }

  protected def setLightPenSettings(parent:JMenu,port:String = "1") : Unit = {
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
    val penUp = new JRadioButtonMenuItem(s"Light pen with button up on control port $port")
    penUp.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_UP) )
    group3.add(penUp)
    lightPenMenu.add(penUp)
    val penLeft = new JRadioButtonMenuItem(s"Light pen with button left on control port $port")
    penLeft.addActionListener(_ => setLightPen(LIGHT_PEN_BUTTON_LEFT) )
    group3.add(penLeft)
    lightPenMenu.add(penLeft)
    val gunPotY = new JRadioButtonMenuItem(s"Light gun 'Magnum Light Phaser' on control port $port")
    gunPotY.addActionListener(_ => setLightPen(LIGHT_GUN_BUTTON_POTY))
    group3.add(gunPotY)
    lightPenMenu.add(gunPotY)
  }

  protected def setMouseSettings(parent:JMenu) : Unit = {
    val mouseMenu = new JMenu("Mouse")
    parent.add(mouseMenu)
    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled on port 1")
    mouseMenu.setEnabled(MouseCage.isMouseSupported)
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M,java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.setSelected(false)
    mouseEnabledItem.addActionListener(e => enableMouse(e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected,display) )
    mouseMenu.add(mouseEnabledItem)
    val mouseRatioItem = new JMenuItem("Mouse calibration settings ...")
    mouseRatioItem.addActionListener(_ => {
      Option(JOptionPane.showInputDialog(displayFrame,"Select mouse delay in millis",MouseCage.getRatioMillis.toString)) match {
        case None =>
        case Some(mr) =>
          try {
            MouseCage.setRatioMillis(mr.toInt)
            preferences.update(Preferences.PREF_MOUSE_DELAY_MILLIS,mr.toInt)
          }
          catch {
            case _:NumberFormatException =>
              JOptionPane.showMessageDialog(displayFrame,s"Invalid integer value: $mr","Invalid input",JOptionPane.ERROR_MESSAGE)
          }
      }
    })
    mouseMenu.add(mouseRatioItem)
    // reset setting
    resetSettingsActions = (() => {
      mouseEnabledItem.setSelected(false)
      enableMouse(false,display)
    }) :: resetSettingsActions
  }

  protected def setPrinterSettings(parent:JMenu) : Unit = {
    import Preferences._
    // PRINTER-ENABLED =====================================================================================
    val printerPreviewItem = new JMenuItem("Printer preview ...")
    printerPreviewItem.addActionListener(_ => showPrinterPreview() )
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
      clock.pause()
      sidCycleExact = sce
      sid.setCycleExact(sidCycleExact)
      sidCycleExactItem.setSelected(sidCycleExact)
      clock.play()
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

  protected def setDrivesSettings() : Unit = {
    import Preferences._
    for(drive <- 0 until TOTAL_DRIVES) {
      // DRIVE-X-TYPE ========================================================================================
      preferences.add(PREF_DRIVE_X_TYPE(drive),"Set the driver's type (1541,1571,1581,8050,local=<directory path>)","") { dt =>
        dt match {
          case "1541" =>
            setDriveType(drive,DriveType._1541, true)
          case "1571" =>
            setDriveType(drive,DriveType._1571, true)
          case "1581" =>
            setDriveType(drive,DriveType._1581, true)
          case "8050" =>
            setDriveType(drive, DriveType._8050, true)
          case _ =>
            if (dt.toUpperCase().startsWith("LOCAL=")) {
              val dir = new File(dt.substring(6))
              if (!dir.exists() || !dir.isDirectory) throw new IllegalArgumentException(s"Bad ${PREF_DRIVE_X_TYPE(drive)} option value: invalid directory $dir")
              DrivesConfigPanel.setLocalPathFor(drive,dir.toString)
              setDriveType(drive, DriveType.LOCAL, true)
            }
            else
              throw new IllegalArgumentException(s"Bad ${PREF_DRIVE_X_TYPE(drive)} option value: $dt")
        }
      }
      // =====================================================================================================

      // DRIVE-X-FILE ========================================================================================
      preferences.add(PREF_DRIVE_X_FILE(drive),s"Attach a file to drive ${drive + 8}","",Set.empty,false) { df =>
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
    val reu8MItem = new JRadioButtonMenuItem("8M")
    reu8MItem.addActionListener(_ => preferences(PREF_REUTYPE) = REU.REU_8M.toString )
    group5.add(reu8MItem)
    reuItem.add(reu8MItem)
    val reu16MItem = new JRadioButtonMenuItem("16M ...")
    reu16MItem.addActionListener(_ => choose16MREU() )
    group5.add(reu16MItem)
    reuItem.add(reu16MItem)
    parent.add(reuItem)

    preferences.add(PREF_REUTYPE,"Set the reu type (none,128,256,512,1024,2048,4096,8192,16384). With 16384 the syntax is: 16384[,<reu file to load>]","none") { reu =>
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
          case REU.REU_8M =>
            setREU(Some(REU.REU_8M),None)
            reu8MItem.setSelected(true)
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

  protected def setIsepicSettings(parent: JMenu): Unit = {
    import Preferences._

    val isepicItem = new JMenu("Isepic")
    val enabled = new JCheckBoxMenuItem("Enabled")
    isepicItem.add(enabled)
    val switch = new JCheckBoxMenuItem("Switch enabled")
    isepicItem.add(switch)
    parent.add(isepicItem)
    enabled.addActionListener(_ => {
      preferences(PREF_ISEPIC) = enabled.isSelected
      switch.setSelected(enabled.isSelected)
    })
    switch.addActionListener(_ => {
      ExpansionPort.getExpansionPort.TYPE match {
        case ExpansionPortType.ISEPIC =>
          clock.pause()
          ExpansionPort.getInternalExpansionPort.asInstanceOf[Isepic].setSwitch(switch.isSelected)
          clock.play()
        case _ =>
      }
    })

    preferences.add(PREF_ISEPIC,"Enable isepic cart",false) { isepicEnabled =>
      enabled.setSelected(isepicEnabled)
      setIsepic(isepicEnabled)
    }
  }

  protected def setRAMCartSettings(parent: JMenu): Unit = {
    import Preferences._

    val rcItem = new JMenu("Ram cart")
    val grouprc = new ButtonGroup
    val noRcItem = new JRadioButtonMenuItem("None")
    noRcItem.setSelected(true)
    noRcItem.addActionListener(_ => preferences(PREF_RAMCART) = "none")
    grouprc.add(noRcItem)
    rcItem.add(noRcItem)
    val _64kRcItem = new JRadioButtonMenuItem("64K")
    _64kRcItem.addActionListener(_ => preferences(PREF_RAMCART) = "64")
    grouprc.add(_64kRcItem)
    rcItem.add(_64kRcItem)
    val _128kRcItem = new JRadioButtonMenuItem("128K")
    _128kRcItem.addActionListener(_ => preferences(PREF_RAMCART) = "128")
    grouprc.add(_128kRcItem)
    rcItem.add(_128kRcItem)
    val readOnlyRC = new JCheckBoxMenuItem("Read only switch")
    rcItem.add(readOnlyRC)
    readOnlyRC.addActionListener(_ => {
      ExpansionPort.getExpansionPort.TYPE match {
        case ExpansionPortType.RAMCART =>
          ExpansionPort.getInternalExpansionPort.asInstanceOf[RAMCart].setReadOnly(readOnlyRC.isSelected)
        case _ =>
      }
    })

    preferences.add(PREF_RAMCART, "Enable ram-cart with the given size (none,64,128)", "none", Set("none", "64", "128")) {
      case "64" =>
        _64kRcItem.setSelected(true)
        setRAMCart(enabled = true,64)
      case "128" =>
        _128kRcItem.setSelected(true)
        setRAMCart(enabled = true,128)
      case "none" =>
        noRcItem.setSelected(true)
        setRAMCart(enabled = false,0)
    }

    parent.add(rcItem)

    // reset setting
    resetSettingsActions = (() => {
      noRcItem.setSelected(true)
      setRAMCart(enabled = false,0)
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

  protected def setDigiMAXSettings(parent:JMenu) : Unit = {
    // DIGI-MAX ============================================================================================
    val digimaxItem = new JMenu("DigiMAX")
    parent.add(digimaxItem)
    val digiMaxSampleRateItem  = new JMenuItem("DigiMax sample rate ...")
    digiMaxSampleRateItem.addActionListener(_ => chooseDigiMaxSampleRate() )
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

  protected def setWiC64Settings(parent:JMenu): Unit = {
    val wic64Item = new JMenuItem("WiC64 panel ...")
    parent.add(wic64Item)
    wic64Item.addActionListener(_ => {
      wic64Panel.dialog.setVisible(true)
    })
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
    flyerDirectoryItem.addActionListener(_ => chooseFlyerDir() )
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
      vicChip.setCoprocessor(if (br) new VASYL(vicChip.asInstanceOf[vic.VIC_II],cpu,dmaSwitcher.setLine(Switcher.EXT,_)) else null)
      brItem.setSelected(true)
    }
  }

  protected def setVICModel(parent:JMenu) : Unit = {
    import Preferences._
    // NTSC ================================================================================================
    val vicModelItem = new JMenu("VIC model")
    val group = new ButtonGroup
    val palItem = new JRadioButtonMenuItem("PAL")
    val ntscItem = new JRadioButtonMenuItem("NTSC")
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
    sfmNextFrameItem.addActionListener(_ => display.advanceOneFrame() )
    sfmItem.add(sfmNextFrameItem)
    parent.add(sfmItem)
  }

  protected def setEasyFlashSettings(parent:JMenu) : Unit = {
    val easyFlashMenu = new JMenu("EasyFlash")
    val jumperItem = new JCheckBoxMenuItem("Easy Flash jumper on")
    jumperItem.addActionListener( _ => EasyFlash.jumper = jumperItem.isSelected )
    easyFlashMenu.add(jumperItem)
    easyFlashWriteChangesItem.addActionListener(_ => easyFlashWriteChanges() )
    easyFlashWriteChangesItem.setEnabled(false)
    easyFlashMenu.add(easyFlashWriteChangesItem)
    parent.add(easyFlashMenu)
  }

  protected def setGMOD3FlashSettings(parent:JMenu) : Unit = {
    val gmodMenu = new JMenu("GMOD ...")
    val gmod2Item = new JMenuItem("GMOD2 eeprom file...")
    gmod2Item.addActionListener(_ => chooseGMod2() )
    gmodMenu.add(gmod2Item)
    GMOD3WriteChangesItem.setEnabled(false)
    gmodMenu.add(GMOD3WriteChangesItem)
    GMOD3WriteChangesItem.addActionListener(_ => GMOD3WriteChanges() )
    parent.add(gmodMenu)
  }

  protected def easyFlashWriteChanges() : Unit = {
    ExpansionPort.getInternalExpansionPort match {
      case ef:EasyFlash =>
        ef.createCRT()
      case _ =>
    }
  }

  protected def GMOD3WriteChanges() : Unit = {
    ExpansionPort.getInternalExpansionPort match {
      case ef:GMOD3 =>
        ef.createCRT()
      case _ =>
    }
  }

  override protected def showCartInfo() : Unit = {
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

  override protected def setDefaultProperties(configuration:Properties) : Unit = {
    import Preferences._
    configuration.setProperty(PREF_RENDERINGTYPE,"default")
    configuration.setProperty(PREF_WRITEONDISK,"true")
    configuration.setProperty(PREF_VICPALETTE,"bright")
  }

  override protected def listBASIC(): Unit = {
    BasicListExplorer.list(mmu, PRG_LOAD_ADDRESS())
  }
}
