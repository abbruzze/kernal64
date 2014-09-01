package ucesoft.c64

import cpu._
import peripheral._
import javax.swing._
import java.awt.event._
import java.awt._
import ucesoft.c64.expansion.ExpansionPort
import ucesoft.c64.formats._
import ucesoft.c64.peripheral.sid.SID
import ucesoft.c64.formats.ExpansionPortFactory
import ucesoft.c64.trace.TraceListener
import ucesoft.c64.trace.TraceDialog
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import ucesoft.c64.peripheral.bus.IECBus
import java.io._
import javax.swing.filechooser.FileFilter
import ucesoft.c64.peripheral.drive.C1541Emu
import ucesoft.c64.peripheral.drive.DriveLedListener
import java.util.Properties
import ucesoft.c64.peripheral.controlport.JoystickSettingDialog
import ucesoft.c64.peripheral.controlport.Joysticks._
import ucesoft.c64.peripheral.drive.C1541
import ucesoft.c64.peripheral.drive.Drive
import ucesoft.c64.peripheral.cia.CIA
import ucesoft.c64.trace.InspectPanel
import ucesoft.c64.util.D64Canvas
import ucesoft.c64.util.T64Canvas
import ucesoft.c64.util.C64FileView
import ucesoft.c64.peripheral.c2n.Datassette

object C64 extends App {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  val c64 = new C64
  c64.run
}

class C64 extends C64Component with ActionListener with DriveLedListener {
  val componentID = "Commodore 64"
  val componentType = C64ComponentType.INTERNAL
  
  private[this] val VERSION = "0.9.9"
  private[this] val CONFIGURATION_FILENAME = "C64.config"
  private[this] val CONFIGURATION_LASTDISKDIR = "lastDiskDirectory"
  private[this] val CONFIGURATION_FRAME_XY = "frame.xy"  
  private[this] val CONFIGURATION_FRAME_DIM = "frame.dim"
  private[this] val clock = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)
  private[this] val mem = new CPU6510Mems.MAIN_MEMORY
  private[this] val cpu = CPU6510.make(mem)  
  private[this] var vicChip : vic.VIC = _
  private[this] val sid = new SID
  private[this] var display : vic.Display = _
  private[this] val nmiSwitcher = new keyboard.NMISwitcher(cpu.nmiRequest _)
  private[this] val irqSwitcher = new IRQSwitcher
  private[this] val keyb = new keyboard.Keyboard(keyboard.DefaultKeyboardMapper,nmiSwitcher.keyboardNMIAction _)	// key listener
  private[this] val displayFrame = {
    val f = new JFrame("C64 emulator ver. " + VERSION)
    //f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) {
        close
      }
    })
    f.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)    
    f
  }
  private[this] val bus = new IECBus
  private[this] var baLowUntil = -1L
  private[this] var cpuWaitUntil = -1L
  // -------------------- TRACE ----------------
  private[this] var traceDialog : TraceDialog = _
  private[this] var diskTraceDialog : TraceDialog = _
  private[this] var inspectDialog : JDialog = _
  private[this] var traceItem,traceDiskItem : JCheckBoxMenuItem = _
  // -------------------- DISK -----------------
  private[this] var attachedDisk : Option[D64] = None
  private[this] val c1541 = new C1541Emu(bus,this)
  private[this] val c1541_real = new C1541(0x00,bus,this)
  private[this] var drive : Drive = c1541_real
  private[this] val driveLed = new DriveLed
  private[this] val progressPanel = new DriveLoadProgressPanel
  // -------------------- TAPE -----------------
  private[this] var datassette : Datassette = _
  // -------------------------------------------
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
    props
  }
  
  // ------------ Control Port -----------------------
  private[this] val gameControlPort = new controlport.GamePadControlPort(configuration)
  private[this] val keypadControlPort = controlport.ControlPort.keypadControlPort
  private[this] val controlPortA = new controlport.ControlPortBridge(keypadControlPort,"Control Port 1")  
  private[this] val controlPortB = new controlport.ControlPortBridge(gameControlPort,"Control port 2")
  // -------------- Light Pen -------------------------
  private[this] val LIGHT_PEN_NO_BUTTON = 0
  private[this] val LIGHT_PEN_BUTTON_UP = 1
  private[this] val LIGHT_PEN_BUTTON_LEFT = 2
  private[this] var lightPenButtonEmulation = LIGHT_PEN_NO_BUTTON
  // -------------- Mouse -----------------------------
  private[this] var mouseEnabled = true
  
  private[this] class LightPenButtonListener extends MouseAdapter with C64Component {
    val componentID = "Light pen"
    val componentType = C64ComponentType.INPUT_DEVICE 
    
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
  }
  
  private[this] class Mouse1351 extends MouseAdapter with C64Component {
    val componentID = "Mouse Commodore 1351"
    val componentType = C64ComponentType.INPUT_DEVICE
    private[this] var x,y = 0
    
    override def mouseMoved(e:MouseEvent) = if (mouseEnabled) {
      x = display.getMouseX & 0x7F
      y = 0x7F - display.getMouseY & 0x7F
      sid.write(0xD419,x << 1)
      sid.write(0xD41A,y << 1)
    }
    
    override def getProperties = {
      properties.setProperty("X",x.toString)
      properties.setProperty("Y",y.toString)
      properties
    }
    
    def init {}
    def reset {}
  }
  // -------------------------------------------------
  private class IRQSwitcher extends C64Component {
    val componentID = "IRQ Switcher (CIA,VIC)"
    val componentType = C64ComponentType.INTERNAL 
    
    private[this] var ciaIRQLow = false
    private[this] var vicIRQLow = false
    
    private def handleIRQ = {
      Log.debug(s"Handling IRQ ciaIRQ=${ciaIRQLow} vicIRQ=${vicIRQLow}")
      cpu.irqRequest(ciaIRQLow || vicIRQLow)
    }
    
    final def ciaIRQ(low:Boolean) {     
      Log.debug("CIA setting IRQ as " + low)
      ciaIRQLow = low
      handleIRQ
    }
    final def vicIRQ(low:Boolean) {  
      Log.debug("VIC setting IRQ as " + low)
      vicIRQLow = low
      handleIRQ
    }
    
    override def getProperties = {
      properties.setProperty("CIA1 IRQ",ciaIRQLow.toString)
      properties.setProperty("VIQ IRQ",vicIRQLow.toString)
      properties
    }
    
    def init {}
    
    def reset {
      ciaIRQLow = false
      vicIRQLow = false
    }
  }
  
  // DRIVE LED -------------------
  private[this] var driveLedOn = false
  
  private class DriveLed extends JComponent with C64Component {
    val componentID = "Drive led"
    val componentType = C64ComponentType.INTERNAL
    
    setPreferredSize(new Dimension(15,15))
    override def paint(g:Graphics) {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      g2.setColor(if (driveLedOn) Color.RED else Color.DARK_GRAY)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    }   
    
    override def getProperties = {
      properties.setProperty("Led on",driveLedOn.toString)
      properties
    }
    
    def init {}
    def reset {}
  }
  
  private class DriveLoadProgressPanel extends JProgressBar {
    setPreferredSize(new Dimension(150,15))
    setStringPainted(true)
    setString("")
    setVisible(false)
    
    def beginLoading(msg:String) {
      setVisible(true)
      setValue(0)
      setString(msg)
    }
    
    def updateValue(perc:Int) {
      setValue(perc)
    }
    
    def endLoading {
      setVisible(false)
    }
  }
  
  override def isOn = driveLedOn
  
  override def turnOn {
    if (!driveLedOn) {
      driveLedOn = true
      driveLed.repaint()
    }
  }
  override def turnOff {
    if (driveLedOn) {
      driveLedOn = false
      driveLed.repaint()
    }    
  }
  
  override def beginLoadingOf(fileName:String,indeterminate:Boolean=false) {
    progressPanel.setIndeterminate(indeterminate)
    progressPanel.beginLoading(fileName)
  }
  override def updateLoading(perc:Int) {
    progressPanel.updateValue(perc)
  }
  override def endLoading {
    progressPanel.endLoading
  }
  override def beginSavingOf(fileName:String) {
    progressPanel.beginLoading(fileName)
    progressPanel.setIndeterminate(true)
  }
  override def endSaving {
    progressPanel.endLoading
    progressPanel.setIndeterminate(false)
  }
    
  def reset {
    baLowUntil = 0
    cpuWaitUntil = 0
  }
  
  def init {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    
    Log.info("Building the system ...")
    ExpansionPort.addConfigurationListener(mem)
    // -----------------------    
    add(clock)
    add(mem)
    add(cpu)
    add(keyb)
    add(controlPortA)
    add(controlPortB)
    add(bus)
    add(ExpansionPort.getExpansionPort)
    // -----------------------
    val bankedMemory = new vic.BankedMemory(mem,mem.CHAR_ROM,mem.COLOR_RAM)    
    import cia._
    // control ports
    val cia1CP1 = new CIA1Connectors.PortAConnector(keyb,controlPortA)
    val cia1CP2 = new CIA1Connectors.PortBConnector(keyb,controlPortB,() => vicChip.triggerLightPen)
    add(cia1CP1)
    add(cia1CP2)
    add(irqSwitcher)    
    // CIAs
    val cia1 = new CIA("CIA1",
    				   0xDC00,
    				   cia1CP1,
    				   cia1CP2,
    				   irqSwitcher.ciaIRQ _)
    val cia2CP1 = new CIA2Connectors.PortAConnector(bankedMemory,bus)
    val cia2CP2 = CIA2Connectors.PortBConnector
    add(cia2CP1)
    add(cia2CP2)
    add(nmiSwitcher)    
    val cia2 = new CIA("CIA2",
    				   0xDD00,
    				   cia2CP1,
    				   cia2CP2,
    				   nmiSwitcher.cia2NMIAction _)
    vicChip = new vic.VIC(bankedMemory,irqSwitcher.vicIRQ _,baLow _)    
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
    display.addMouseListener(keypadControlPort)    
    val lightPen = new LightPenButtonListener
    add(lightPen)
    display.addMouseListener(lightPen)
    val mouse = new Mouse1351
    add(mouse)
    display.addMouseMotionListener(mouse)
    traceDialog = TraceDialog.getTraceDialog(displayFrame,mem,cpu,display,vicChip)
    diskTraceDialog = TraceDialog.getTraceDialog(displayFrame,c1541_real.getMem,c1541_real)
    // drive led
    add(driveLed)
    val ledPanel = new JPanel(new BorderLayout)
    val dummyPanel = new JPanel
    dummyPanel.add(progressPanel)
    dummyPanel.add(driveLed)
    ledPanel.add("East",dummyPanel)
    displayFrame.getContentPane.add("South",ledPanel)    
    if (configuration.getProperty(CONFIGURATION_FRAME_XY) != null) {
      val xy = configuration.getProperty(CONFIGURATION_FRAME_XY) split "," map { _.toInt }
      displayFrame.setLocation(xy(0),xy(1))
    }    
    configureJoystick
    // drive
    add(c1541)
    add(c1541_real)    
    Log.setOutput(traceDialog.logPanel.writer)
    // tape
    datassette = new Datassette(cia1.setFlagLow _)
    mem.setDatassette(datassette)
    add(datassette)
    Log.info(sw.toString)
  }
  
  override def afterInitHook {
	inspectDialog = InspectPanel.getInspectDialog(displayFrame,this)
  }
  
  private def errorHandler(t:Throwable) {
    Log.info("Fatal error occurred: " + cpu + "-" + t)
    Log.info(CPU6510.disassemble(mem,cpu.getCurrentInstructionPC).toString)
    t.printStackTrace(Log.getOut)
    t.printStackTrace
    JOptionPane.showMessageDialog(displayFrame,t.toString, "Fatal error",JOptionPane.ERROR_MESSAGE)
    trace(true,true)
  }
  private def mainLoop(cycles:Long) {
    // VIC
    vicChip.clock(cycles)
    //DRIVES
    drive.clock(cycles)
    // CPU
    val canExecCPU = cycles > cpuWaitUntil && cycles > baLowUntil
    if (canExecCPU) cpuWaitUntil = cycles + cpu.fetchAndExecute
  }
  
  private def baLow(cycles:Long) {
    if (cycles > baLowUntil) {
      baLowUntil = cycles
      //Log.fine(s"BA low until ${cycles}")
    }
  }
  
  // ---------------------------------- GUI HANDLING -------------------------------
    
  final def actionPerformed(e:ActionEvent) {
    e.getActionCommand match {
      case "TRACE" =>
      	val traceItem = e.getSource.asInstanceOf[JCheckBoxMenuItem]
      	trace(true,traceItem.isSelected)
      case "TRACE_DISK" =>
      	val traceItem = e.getSource.asInstanceOf[JCheckBoxMenuItem]
      	trace(false,traceItem.isSelected)
      case "INSPECT" =>
        val selected = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected
        inspectDialog.setVisible(selected)
      case "RESET" =>
        reset(true)
      case "ATTACH_CTR" => 
        attachCtr
      case "MAXSPEED" =>
        val maxSpeedItem = e.getSource.asInstanceOf[JCheckBoxMenuItem]
        clock.maximumSpeed = maxSpeedItem.isSelected
        sid.setFullSpeed(maxSpeedItem.isSelected)
      case "ADJUSTRATIO" =>
        val dim = display.asInstanceOf[java.awt.Component].getSize
        dim.height = (dim.width / vicChip.SCREEN_ASPECT_RATIO).toInt
        display.setPreferredSize(dim) 
        displayFrame.pack
      case "AUTORUN_DISK" =>
        attachDisk(true)
      case "ATTACH_DISK" =>
        attachDisk(false)
      case "LOAD_FILE" =>
        loadFileFromAttachedFile(true)
      case "JOY" =>
        joySettings
      case "PASTE" =>
        paste
      case "SNAPSHOT" =>
        takeSnapshot
      case "LOADPRG" =>
        loadPrg
      case "SAVEPRG" =>
        savePrg
      case "ZOOM1" =>
        zoom(1)
      case "ZOOM2" =>
        zoom(1.5)
      case "ZOOM3" =>
        zoom(2)
      case "PAUSE" =>
        Clock.systemClock.pause
      case "PLAY" =>
        Clock.systemClock.play
      case "SWAP_JOY" =>
        swapJoysticks
      case "DISK_TRUE_EMU" =>
        val trueEmu = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected
        bus.reset
        drive.setActive(false)
        drive = if (trueEmu) c1541_real else c1541
        drive.setActive(true)
      case "NO_PEN" =>
        lightPenButtonEmulation = LIGHT_PEN_NO_BUTTON
        keypadControlPort.setLightPenEmulation(false)
        vicChip.enableLightPen(false)
      case "PEN_UP" =>
        lightPenButtonEmulation = LIGHT_PEN_BUTTON_UP
        vicChip.enableLightPen(true)
        keypadControlPort.setLightPenEmulation(true)
      case "PEN_LEFT" =>
        lightPenButtonEmulation = LIGHT_PEN_BUTTON_LEFT
        vicChip.enableLightPen(true)
        keypadControlPort.setLightPenEmulation(true)
      case "MOUSE_ENABLED" =>
        mouseEnabled = e.getSource.asInstanceOf[JCheckBoxMenuItem].isSelected
      case "EXIT" => close
      case "TAPE" =>
        loadFileFromTape
      case "TAPE_PLAY" =>
        datassette.pressPlay
      case "TAPE_STOP" =>
        datassette.pressStop
      case "ATTACH_TAPE" =>
        attachTape
    }
  }
  
  private def zoom(f:Double) {
    val dim = new Dimension((vicChip.VISIBLE_SCREEN_WIDTH * f).toInt,(vicChip.VISIBLE_SCREEN_HEIGHT * f).toInt)
    display.setPreferredSize(dim) 
    displayFrame.invalidate
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
        val out = new FileOutputStream(fc.getSelectedFile)
        val start = mem.read(43) + mem.read(44) * 256
        val end = mem.read(45) + mem.read(46) * 256 - 1
	    out.write(mem.read(43))
	    out.write(mem.read(44))
        for (m <- start to end) out.write(mem.read(m))
        out.close
        Log.info(s"BASIC program saved from ${start} to ${end}")
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
        configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
        val in = new FileInputStream(fc.getSelectedFile)
        val start = in.read + in.read * 256
        var m = start
        var b = in.read
        var size = 0
        while (b != -1) {
          mem.write(m, b)
          m += 1
          size += 1
          b = in.read
        }
        in.close
        val end = start + size
        mem.write(45, end % 256)
        mem.write(46, end / 256)
        mem.write(0xAE,end % 256)
        mem.write(0xAF,end / 256)
        Log.info(s"BASIC program loaded from ${start} to ${end} size=${size}")
      case _ =>
    }
  }
  
  private def takeSnapshot {
    val fc = new JFileChooser
    fc.showSaveDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
      	display.saveSnapshot(fc.getSelectedFile)
      case _ =>
    }
  }
  
  private def paste {
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    val contents = clipboard.getContents(null)
    if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = contents.getTransferData(DataFlavor.stringFlavor).toString
      new Thread {
        override def run {
          val len = mem.read(649)
          var strpos = 0
          while (strpos < str.length) {
            val size = if (len < str.length - strpos) len else str.length - strpos            
            for(i <- 0 until size) {
              val c = str.charAt(strpos)
              mem.write(631 + i,if (c != '\n') c else 0x0D)
              strpos += 1
            }
            mem.write(198,size)
            while (mem.read(198) > 0) Thread.sleep(1)
          }
        }
      }.start
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
    clock.pause
    if (play) ExpansionPort.setExpansionPort(ExpansionPort.emptyExpansionPort)
    resetComponent
    if (play) clock.play
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
        try {          
          val ep = ExpansionPortFactory.loadExpansionPort(fc.getSelectedFile.toString)
          println(ep)
          ExpansionPort.setExpansionPort(ep)
          Log.info(s"Attached cartridge ${ExpansionPort.getExpansionPort.name}")
          reset(false)
          configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)          
        }
        catch {
          case t:Throwable =>
            t.printStackTrace(traceDialog.logPanel.writer)
            JOptionPane.showMessageDialog(displayFrame,t.toString, "Cartridge loading error",JOptionPane.ERROR_MESSAGE)
        }
        finally {
          clock.play
        }
      case _ =>
    }
  }
  
  private def attachDisk(autorun:Boolean) {
    val fc = new JFileChooser
    fc.setAccessory(new javax.swing.JScrollPane(new D64Canvas(fc,mem.CHAR_ROM)))
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".D64")
      def getDescription = "D64 files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        try {
          val disk = new D64(fc.getSelectedFile.toString)
          attachedDisk match {
            case Some(oldDisk) => oldDisk.close
            case None =>
          }
          attachedDisk = Some(disk)
          drive.setDriveReader(disk)
          configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
          if (autorun) {
            insertTextIntoKeyboardBuffer("LOAD\"*\",8,1" + 13.toChar + "RUN" + 13.toChar)
          }
        }
        catch {
          case t:Throwable =>
            t.printStackTrace
            JOptionPane.showMessageDialog(displayFrame,t.toString, "Disk attaching error",JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }
  
  private def loadFileFromAttachedFile(relocate:Boolean) {
    attachedDisk match {
      case None =>
        JOptionPane.showMessageDialog(displayFrame,"No disk attached!", "Loading error",JOptionPane.ERROR_MESSAGE)
      case Some(d64) =>
        Option(JOptionPane.showInputDialog(displayFrame,"Load file","*")) match {
          case None =>
          case Some(fileName) =>
            d64.loadInMemory(mem,fileName,relocate)
        }
    }
  }    
  
  private def loadFileFromTape {
    val fc = new JFileChooser
    fc.setAccessory(new javax.swing.JScrollPane(new T64Canvas(fc,mem.CHAR_ROM)))
    fc.setFileView(new C64FileView)
    fc.setCurrentDirectory(new File(configuration.getProperty(CONFIGURATION_LASTDISKDIR,"./")))
    fc.setFileFilter(new FileFilter {
      def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".T64")
      def getDescription = "T64 files"
    })
    fc.showOpenDialog(displayFrame) match {
      case JFileChooser.APPROVE_OPTION =>
        val tape = new T64(fc.getSelectedFile.toString)
        try {
          val values = tape.entries map { e => e.asInstanceOf[Object] }
          JOptionPane.showInputDialog(displayFrame,"Select file to open:","Open file in " + fc.getSelectedFile.getName,JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
            case null =>
            case entry:T64Entry =>
              tape.loadInMemory(mem,entry)
              configuration.setProperty(CONFIGURATION_LASTDISKDIR,fc.getSelectedFile.getParentFile.toString)
          }
        }
        finally {
          tape.close
        }
      case _ =>
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
        datassette.setTAP(Some(new TAP(fc.getSelectedFile.toString)))
      case _ =>
    }
  }
  
  private def insertTextIntoKeyboardBuffer(txt:String) {
    for(i <- 0 until txt.length) {
      mem.write(631 + i,txt.charAt(i))
    }
    mem.write(198,txt.length)
  }
  
  private def setMenu {
    val menuBar = new JMenuBar
    val fileMenu = new JMenu("File")
    val editMenu = new JMenu("Edit")
    val traceMenu = new JMenu("Trace")
    val optionMenu = new JMenu("Settings")
    menuBar.add(fileMenu)
    menuBar.add(editMenu)
    menuBar.add(traceMenu)
    menuBar.add(optionMenu)
    
    val tapeItem = new JMenuItem("Load file from tape ...")
    tapeItem.setActionCommand("TAPE")
    tapeItem.addActionListener(this)
    fileMenu.add(tapeItem)
    
    val attachTapeItem = new JMenuItem("Attach tape ...")
    attachTapeItem.setActionCommand("ATTACH_TAPE")
    attachTapeItem.addActionListener(this)
    fileMenu.add(attachTapeItem)
    
    val tapeMenu = new JMenu("Tape control...")
    fileMenu.add(tapeMenu)
    
    val tapePlayItem = new JMenuItem("Cassette press play")
    tapePlayItem.setActionCommand("TAPE_PLAY")
    tapePlayItem.addActionListener(this)
    tapeMenu.add(tapePlayItem)
    
    val tapeStopItem = new JMenuItem("Cassette press stop")
    tapeStopItem.setActionCommand("TAPE_STOP")
    tapeStopItem.addActionListener(this)
    tapeMenu.add(tapeStopItem)
    
    fileMenu.addSeparator
    
    val autorunDiskItem = new JMenuItem("Autorun disk ...")
    autorunDiskItem.setActionCommand("AUTORUN_DISK")
    autorunDiskItem.addActionListener(this)
    fileMenu.add(autorunDiskItem)
    
    val attachDiskItem = new JMenuItem("Attach disk ...")
    attachDiskItem.setActionCommand("ATTACH_DISK")
    attachDiskItem.addActionListener(this)
    fileMenu.add(attachDiskItem)
    
    val loadFileItem = new JMenuItem("Load file from attached disk ...")
    loadFileItem.setActionCommand("LOAD_FILE")
    loadFileItem.addActionListener(this)
    fileMenu.add(loadFileItem)    
    fileMenu.addSeparator
    
    val loadPrgItem = new JMenuItem("Load PRG file from local disk ...")
    loadPrgItem.setActionCommand("LOADPRG")
    loadPrgItem.addActionListener(this)
    fileMenu.add(loadPrgItem)
    
    val savePrgItem = new JMenuItem("Save PRG file to local disk ...")
    savePrgItem.setActionCommand("SAVEPRG")
    savePrgItem.addActionListener(this)
    fileMenu.add(savePrgItem)
    fileMenu.addSeparator
    
    val resetItem = new JMenuItem("Reset")
    resetItem.setActionCommand("RESET")
    resetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F12,0))
    resetItem.addActionListener(this)
    fileMenu.add(resetItem)
    
    fileMenu.addSeparator
    
    val attachCtrItem = new JMenuItem("Attach cartridge ...")
    attachCtrItem.setActionCommand("ATTACH_CTR")
    attachCtrItem.addActionListener(this)
    fileMenu.add(attachCtrItem)
    
    fileMenu.addSeparator
    
    val exitItem = new JMenuItem("Exit")
    exitItem.setActionCommand("EXIT")
    exitItem.addActionListener(this)
    fileMenu.add(exitItem)
        
    val pasteItem = new JMenuItem("Paste text")
    pasteItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V,java.awt.Event.CTRL_MASK))
    pasteItem.setActionCommand("PASTE")
    pasteItem.addActionListener(this)
    editMenu.add(pasteItem)
    
    traceItem = new JCheckBoxMenuItem("Trace CPU")
    traceItem.setSelected(false)
    traceItem.setActionCommand("TRACE")
    traceItem.addActionListener(this)    
    traceMenu.add(traceItem)  
    
    traceDiskItem = new JCheckBoxMenuItem("Trace Disk CPU")
    traceDiskItem.setSelected(false)
    traceDiskItem.setActionCommand("TRACE_DISK")
    traceDiskItem.addActionListener(this)    
    traceMenu.add(traceDiskItem)
    
    val inspectItem = new JCheckBoxMenuItem("Inspect components ...")
    inspectItem.setSelected(false)
    inspectItem.setActionCommand("INSPECT")
    inspectItem.addActionListener(this)    
    traceMenu.add(inspectItem)
    
    val maxSpeedItem = new JCheckBoxMenuItem("Maximum speed")
    maxSpeedItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F8,0))
    maxSpeedItem.setSelected(clock.maximumSpeed)
    maxSpeedItem.setActionCommand("MAXSPEED")
    maxSpeedItem.addActionListener(this)    
    optionMenu.add(maxSpeedItem)
    
    optionMenu.addSeparator
    
    val adjustRatioItem = new JMenuItem("Adjust display ratio")
    adjustRatioItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.Event.CTRL_MASK))
    adjustRatioItem.setActionCommand("ADJUSTRATIO")
    adjustRatioItem.addActionListener(this)
    optionMenu.add(adjustRatioItem)
    
    val zoomItem = new JMenu("Zoom")
    optionMenu.add(zoomItem)
    val zoom1Item = new JMenuItem("Zoom x 1")
    zoom1Item.setActionCommand("ZOOM1")
    zoom1Item.addActionListener(this)
    zoomItem.add(zoom1Item)
    val zoom2Item = new JMenuItem("Zoom x 1.5")
    zoom2Item.setActionCommand("ZOOM2")
    zoom2Item.addActionListener(this)
    zoomItem.add(zoom2Item)
    val zoom4Item = new JMenuItem("Zoom x 2")
    zoom4Item.setActionCommand("ZOOM3")
    zoom4Item.addActionListener(this)
    zoomItem.add(zoom4Item)
    
    optionMenu.addSeparator
        
    val joyAItem = new JMenuItem("Joystick...")
    joyAItem.setActionCommand("JOY")
    joyAItem.addActionListener(this)
    optionMenu.add(joyAItem)
    
    val swapJoyAItem = new JMenuItem("Swap joysticks")
    swapJoyAItem.setActionCommand("SWAP_JOY")
    swapJoyAItem.addActionListener(this)
    optionMenu.add(swapJoyAItem)
    
    val lightPenMenu = new JMenu("Light pen")
    optionMenu.add(lightPenMenu)
    val group3 = new ButtonGroup
    val noPenItem = new JRadioButtonMenuItem("No light pen")
    noPenItem.setSelected(true)
    noPenItem.setActionCommand("NO_PEN")
    noPenItem.addActionListener(this)
    group3.add(noPenItem)
    lightPenMenu.add(noPenItem)
    val penUp = new JRadioButtonMenuItem("Light pen with button up")
    penUp.setActionCommand("PEN_UP")
    penUp.addActionListener(this)
    group3.add(penUp)
    lightPenMenu.add(penUp)
    val penLeft = new JRadioButtonMenuItem("Light pen with button left")
    penLeft.setActionCommand("PEN_LEFT")
    penLeft.addActionListener(this)
    group3.add(penLeft)
    lightPenMenu.add(penLeft)
    
    val mouseEnabledItem = new JCheckBoxMenuItem("Mouse 1351 enabled")
    mouseEnabledItem.setSelected(true)
    mouseEnabledItem.setActionCommand("MOUSE_ENABLED")
    mouseEnabledItem.addActionListener(this)    
    optionMenu.add(mouseEnabledItem)
    
    optionMenu.addSeparator
    
    val snapshotItem = new JMenuItem("Take a snapshot...")
    snapshotItem.setActionCommand("SNAPSHOT")
    snapshotItem.addActionListener(this)
    optionMenu.add(snapshotItem)
    
    optionMenu.addSeparator
    
    val group2 = new ButtonGroup
    val pauseItem = new JRadioButtonMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F9,0))
    pauseItem.setActionCommand("PAUSE")
    pauseItem.addActionListener(this)
    optionMenu.add(pauseItem)
    group2.add(pauseItem)    
    val playItem = new JRadioButtonMenuItem("Play")
    playItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F11,0))
    playItem.setSelected(true)
    playItem.setActionCommand("PLAY")
    playItem.addActionListener(this)
    optionMenu.add(playItem)
    group2.add(playItem)
    
    optionMenu.addSeparator
    
    val diskTrueEmuItem = new JCheckBoxMenuItem("1541 True emulation")
    diskTrueEmuItem.setSelected(true)
    diskTrueEmuItem.setActionCommand("DISK_TRUE_EMU")
    diskTrueEmuItem.addActionListener(this)    
    optionMenu.add(diskTrueEmuItem)
    
    displayFrame.setJMenuBar(menuBar)
  }
  
  private def configureJoystick {
    def getControlPortFor(id:String) = configuration.getProperty(id) match {
      case CONFIGURATION_KEYPAD_VALUE => keypadControlPort
      case CONFIGURATION_JOYSTICK_VALUE => gameControlPort
      case _ => controlport.ControlPort.emptyControlPort
    }
    
    controlPortA.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_2)
    controlPortB.controlPort = getControlPortFor(CONFIGURATION_JOY_PORT_1)
  }
  
  private def joySettings {
    Clock.systemClock.pause
    try {
      // TODO
      val dialog = new JoystickSettingDialog(displayFrame,configuration)
      dialog.setVisible(true)
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
    configuration.setProperty(CONFIGURATION_FRAME_XY,displayFrame.getX + "," + displayFrame.getY)
    configuration.setProperty(CONFIGURATION_FRAME_DIM,displayFrame.getSize.width + "," + displayFrame.getSize.height)
    try {
      val propsFile = new File(new File(scala.util.Properties.userHome),CONFIGURATION_FILENAME)
      val out = new FileWriter(propsFile)
      configuration.store(out, "C64 configuration file")
      out.close
      attachedDisk match {
        case Some(d64) => d64.close
        case None =>
      }
    }
    catch {
      case io:IOException =>
    }
    sys.exit(0)
  }
  
  // -----------------------------------------------------------------------------------------
  
  def run {
    //build
    initComponent
    setMenu
    displayFrame.pack
    if (configuration.getProperty(CONFIGURATION_FRAME_DIM) != null) {
      val dim = configuration.getProperty(CONFIGURATION_FRAME_DIM) split "," map { _.toInt }
      displayFrame.setSize(dim(0),dim(1))
    }
    displayFrame.setVisible(true)
    clock.play
  }
}