package ucesoft.cbm.trace

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.cbm.{Clock, Log}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Dimension, FlowLayout}
import java.io.{BufferedOutputStream, FileOutputStream, PrintWriter, Writer}
import javax.swing.JSpinner.DefaultEditor
import javax.swing._
import javax.swing.table.AbstractTableModel
import javax.swing.text.DefaultCaret
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TracerGUI {
  def main(args:Array[String]): Unit = {
    val t = new TracerGUI(null)
    t.addDevice(Tracer.TracedDevice("Main 6502",null,null))
    t.setVisible(true)
    Log.setInfo()
    Log.info("Hello, world!")
  }
}

class TracerGUI(openCloseAction: Boolean => Unit) extends Tracer {
  import TraceListener._
  import Tracer._

  private class Register(val name:String) extends JPanel {
    private val value = new JLabel("")
    private val stdForegroundColor = value.getForeground

    setLayout(new FlowLayout(FlowLayout.LEFT))
    add(new JLabel(s"$name :"))
    add(value)
    setBorder(BorderFactory.createLineBorder(Color.BLACK))

    def setValue(v:String): Unit = {
      if (v != value.getText) value.setForeground(Color.RED) else value.setForeground(stdForegroundColor)
      value.setText(v)
    }
  }

  private case class AddressBreakType(read:Boolean,write:Boolean,execute:Boolean) {
    override def toString = {
      val r = if (read) "R" else ""
      val w = if (read) "W" else ""
      val x = if (read) "X" else ""
      s"$r$w$x"
    }
  }

  private sealed trait Break {
    var enabled = true
  }
  private case class AddressBreak(address:Int,breakType:AddressBreakType) extends Break
  private case class EventBreak(eventName:String) extends Break

  private final val BREAK_IRQ_EVENT = "IRQ"
  private final val BREAK_NMI_EVENT = "NMI"

  private class Breaks extends BreakType {
    val addressMap = new mutable.HashMap[Int,AddressBreak]()
    val eventMap = new mutable.HashMap[String,EventBreak]()

    override def isBreak(address: Int, irq: Boolean, nmi: Boolean): Boolean = {
      if (irq) {
        eventMap.get(BREAK_IRQ_EVENT) match {
          case Some(e) => return e.enabled
          case _ =>
        }
      }
      if (nmi) {
        eventMap.get(BREAK_NMI_EVENT) match {
          case Some(e) => return e.enabled
          case _ =>
        }
      }
      addressMap.get(address) match {
        case Some(e) =>
          e.enabled
        case _ =>
          false
      }
    }
  }

  private class BreaksTableModel extends AbstractTableModel {
    private val breaks = new ArrayBuffer[Break]
    override def getColumnName(column: Int): String = column match {
      case 0 => "Enabled"
      case 1 => "Address"
      case 2 => "Type"
    }
    override def isCellEditable(row: Int,col: Int): Boolean = false
    override def getRowCount: Int = breaks.size

    override def getColumnCount: Int = 3

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
      if (columnIndex == 0) return java.lang.Boolean.valueOf(breaks(rowIndex).enabled)

      breaks(rowIndex) match {
        case AddressBreak(address,breakType) =>
          columnIndex match {
            case 1 => address.toHexString.toUpperCase()
            case 2 => breakType.toString
          }
        case EventBreak(eventName) =>
          columnIndex match {
            case 1 => ""
            case 2 => eventName
          }
      }
    }

    override def getColumnClass(columnIndex: Int): Class[_] = columnIndex match {
      case 0 => classOf[java.lang.Boolean]
      case _ => classOf[String]
    }

    def contentChanged(breaks:List[Break]): Unit = {
      this.breaks.clear()
      this.breaks.addAll(breaks)
      fireTableDataChanged()
    }
  }

  private val breaksTableModel = new BreaksTableModel
  private val breaksTable = new JTable(breaksTableModel)
  private val breaks = new Breaks
  private var traceEnabled = false
  private val devices = new ArrayBuffer[TracedDevice]
  private var currentDevice : TracedDevice = _
  private var display : Tracer.TracedDisplay = _
  private val frame = new JFrame()
  private val deviceComboModel = new DefaultComboBoxModel[String]()
  private val deviceCombo = new JComboBox[String](deviceComboModel)
  private val tracePanel = new RSyntaxTextArea(30,100)
  private val logPanel = new RSyntaxTextArea(10,100)
  private val displayPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private val logWriter = new PrintWriter(new Writer {
    def write(chars: Array[Char], off: Int, len: Int): Unit = logPanel.append(new String(chars, off, len))
    def flush(): Unit = {}
    def close(): Unit = {}
  }, true)
  private var tracingFile : PrintWriter = _
  private val registerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private var registers : Map[String,Register] = Map.empty
  private val clockRegister = new Register("CLK")
  private val displayCycleRegister = new Register("LINE CYCLE")
  private val displayRasterRegister = new Register("RASTER LINE")

  private val cycleMode = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/cycleMode.png")))
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))
  private val traceOnFile = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/save.png")))

  private val tracingListeners = new ListBuffer[TracerListener]

  init()

  protected def init(): Unit = {
    Log.setOutput(logWriter)

    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    val toolBar = new JToolBar("Tracer")
    toolBar.setRollover(true)
    toolBar.add(new JLabel("Device:"))
    val comboPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    comboPanel.add(deviceCombo)
    toolBar.add(comboPanel)
    toolBar.addSeparator()

    onOffButton.setToolTipText("Enable tracing")
    onOffButton.addActionListener(_ => enableTracing(onOffButton.isSelected))
    cycleMode.setToolTipText("Cycle mode")
    cycleMode.addActionListener(_ => enableCycleMode(cycleMode.isSelected))
    val stepIn = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down.png")))
    stepIn.setToolTipText("Step in")
    stepIn.addActionListener( _ => step(StepIn))
    val stepOver = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down_left.png")))
    stepOver.addActionListener( _ => step(StepOver))
    stepOver.setToolTipText("Step over")
    val stepOut = new JButton(new ImageIcon(getClass.getResource("/resources/trace/up.png")))
    stepOut.addActionListener( _ => step(StepOut))
    stepOut.setToolTipText("Step out")

    val disa = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
    disa.addActionListener(_ => disassembleGUI())
    disa.setToolTipText("Disassemble")

    val read = new JButton(new ImageIcon(getClass.getResource("/resources/trace/read.png")))
    read.addActionListener(_ => readGUI())
    read.setToolTipText("Read memory")

    val write = new JButton(new ImageIcon(getClass.getResource("/resources/trace/write.png")))
    write.addActionListener(_ => writeGUI())
    write.setToolTipText("Fill memory")

    val clear = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    clear.addActionListener(_ => tracePanel.setText("") )
    clear.setToolTipText("Clear trace panel")

    traceOnFile.setToolTipText("Save tracing on file ...")
    traceOnFile.addActionListener(_ => openTraceOnFile())

    val build = new JButton(new ImageIcon(getClass.getResource("/resources/trace/build.png")))
    build.setToolTipText("Open Assembler ...")
    build.addActionListener(_ => openAssembler() )

    val rasterLineSpinner = new JSpinner(new SpinnerNumberModel(0,0,312,1))
    val showRasterCB = new JCheckBox("Show raster", false)
    showRasterCB.addActionListener(_ => display.enableDisplayRasterLine(showRasterCB.isSelected))
    rasterLineSpinner.addChangeListener(_ => {
      val r = rasterLineSpinner.getValue.asInstanceOf[Int]
      display.setDisplayRasterLine(r)
    })
    rasterLineSpinner.getEditor.asInstanceOf[DefaultEditor].getTextField.setEditable(true)
    rasterLineSpinner.setEditor(new JSpinner.NumberEditor(rasterLineSpinner, "###"))
    displayPanel.add(showRasterCB)
    displayPanel.add(rasterLineSpinner)
    displayPanel.setVisible(false)

    toolBar.add(onOffButton)
    toolBar.add(cycleMode)
    toolBar.add(stepIn)
    toolBar.add(stepOver)
    toolBar.add(stepOut)
    toolBar.add(disa)
    toolBar.add(read)
    toolBar.add(write)
    toolBar.add(clear)
    toolBar.add(traceOnFile)
    toolBar.add(build)
    toolBar.addSeparator()
    toolBar.add(displayPanel)

    // trace panel, register panel & log panel
    tracePanel.setEditable(true)
    tracePanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_ASSEMBLER_6502)
    tracePanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val tscroll = new RTextScrollPane(tracePanel)
    tscroll.setMinimumSize(new Dimension(0,400))
    tscroll.setBorder(BorderFactory.createTitledBorder("Trace panel"))
    tscroll.setLineNumbersEnabled(false)

    //val rscroll = new JScrollPane(registerPanel)
    registerPanel.setBorder(BorderFactory.createTitledBorder("Registers"))

    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    logPanel.setEditable(false)
    val lscroll = new RTextScrollPane(logPanel)
    lscroll.setMinimumSize(new Dimension(0,150))
    lscroll.setBorder(BorderFactory.createTitledBorder("Log panel"))

    val northPanel = new JPanel(new BorderLayout())
    northPanel.add("North",registerPanel)
    northPanel.add("Center",tscroll)
    val splitPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,northPanel,lscroll)
    splitPanel.setOneTouchExpandable(true)
    frame.getContentPane.add("Center",splitPanel)

    // device combo
    deviceCombo.addActionListener(e => {
      val sel = deviceCombo.getSelectedIndex
      if (sel != -1) selectDevice(deviceCombo.getSelectedItem.toString)
    })

    // breaks
    breaksTable.setAutoCreateRowSorter(true)
    breaksTable.setFillsViewportHeight(true)
    val breakPanel = new JPanel(new BorderLayout())
    breakPanel.setBorder(BorderFactory.createTitledBorder("Breakpoints"))
    val tableScroll = new JScrollPane(breaksTable)
    tableScroll.setPreferredSize(new Dimension(200,300))
    val tablePanel = new JPanel(new BorderLayout())
    tablePanel.add("Center",tableScroll)
    breakPanel.add("North",tablePanel)
    val breakButtonsPanel = new JToolBar()
    val addBreak = new JButton(new ImageIcon(getClass.getResource("/resources/trace/plus.png")))
    val removeBreak = new JButton(new ImageIcon(getClass.getResource("/resources/trace/minus.png")))
    val enableBreak = new JButton(new ImageIcon(getClass.getResource("/resources/trace/enable.png")))
    val disableBreak = new JButton(new ImageIcon(getClass.getResource("/resources/trace/disable.png")))
    addBreak.setToolTipText("Adds a breakpoint")
    removeBreak.setToolTipText("Remove selected breakpoints")
    enableBreak.setToolTipText("Enable selected breakpoints")
    disableBreak.setToolTipText("Disable selected breakpoints")
    removeBreak.setEnabled(false)
    enableBreak.setEnabled(false)
    disableBreak.setEnabled(false)
    breakButtonsPanel.add(addBreak)
    breakButtonsPanel.add(removeBreak)
    breakButtonsPanel.add(enableBreak)
    breakButtonsPanel.add(disableBreak)
    tablePanel.add("South",breakButtonsPanel)
    breaksTable.getSelectionModel.addListSelectionListener(e => {
        if (!e.getValueIsAdjusting) {
          val selected = breaksTable.getSelectedRowCount > 0
          removeBreak.setEnabled(selected)
          enableBreak.setEnabled(selected)
          disableBreak.setEnabled(selected)
        }
      }
    )
    breaksTableModel.contentChanged(List(AddressBreak(0xC000,AddressBreakType(true,true,false)),EventBreak("IRQ")))

    // Frame
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        enableTracing(false)
        openCloseAction(false)
      }
    })
    frame.getContentPane.add("North",toolBar)
    frame.getContentPane.add("East",breakPanel)
    frame.pack()
  }

  protected def stepInfoCallBack(cpuStepInfo: CpuStepInfo): Unit = {
    val same = cpuStepInfo.registers.length == registers.size && cpuStepInfo.registers.map(_.name).forall(registers.contains _)
    if (!same) {
      registerPanel.removeAll()
      registers = (for(r <- cpuStepInfo.registers) yield {
        val reg = new Register(r.name)
        registerPanel.add(reg)
        r.name -> reg
      }).toMap
      registerPanel.add(clockRegister)
      if (display != null) {
        registerPanel.add(displayCycleRegister)
        registerPanel.add(displayRasterRegister)
      }
      frame.getContentPane.validate()
      frame.pack()
    }
    for(r <- cpuStepInfo.registers) registers(r.name).setValue(r.value)
    clockRegister.setValue("%10d".format(Clock.systemClock.currentCycles))
    if (display != null) {
      val (rasterLine,cycle) = display.getRasterLineAndCycle()
      displayCycleRegister.setValue(cycle.toString)
      displayRasterRegister.setValue(rasterLine.toString)
    }

    if (traceEnabled) write(cpuStepInfo.disassembled)
    for(tl <- tracingListeners) tl.stepInto(cpuStepInfo.pc)
  }

  protected def setTitle(title:String): Unit = frame.setTitle(s"Tracer - $title")

  override def enableTracing(enabled: Boolean): Unit = {
    traceEnabled = enabled
    if (enabled) onOffButton.setToolTipText("Disable tracing")
    else onOffButton.setToolTipText("Enable tracing")

    currentDevice.listener.step(stepInfoCallBack _,StepIn)
    currentDevice.listener.setTrace(enabled)
  }

  override def setBrk(brk: TraceListener.BreakType): Unit = ???

  override def addListener(tl: Tracer.TracerListener): Unit = tracingListeners += tl

  override def removeListener(tl: Tracer.TracerListener): Unit = tracingListeners -= tl

  override def setVisible(on: Boolean): Unit = {
    frame.setVisible(on)
    openCloseAction(on)
  }

  override def addDevice(device: Tracer.TracedDevice): Unit = {
    val index = devices.map(_.id).indexOf(device.id)
    if (index == -1) devices += device
    else devices(index) = device

    deviceComboModel.removeAllElements()
    val sorted = devices.sortWith((a,b) => a.default && !b.default)
    import scala.jdk.CollectionConverters._
    deviceComboModel.addAll(sorted.map(_.id).asJava)
    deviceCombo.setSelectedIndex(0)
    selectCurrentDevice(sorted(0))
  }

  override def removeDevice(device:TracedDevice): Unit = {
    devices -= device
    val sorted = devices.sortWith((a,b) => a.default && !b.default)
    deviceComboModel.removeAllElements()
    import scala.jdk.CollectionConverters._
    deviceComboModel.addAll(sorted.map(_.id).asJava)
    deviceCombo.setSelectedIndex(0)
    selectCurrentDevice(sorted(0))
  }

  protected def selectCurrentDevice(device:TracedDevice): Unit = {
    currentDevice = device
    cycleMode.setEnabled(device.listener.cycleModeSupported)
  }

  override def selectDevice(id:String): Boolean = {
    devices.find(_.id == id) match {
      case None =>
        false
      case Some(dev) =>
        selectCurrentDevice(dev)
        setTitle(currentDevice.id)
        true
    }
  }

  override def step(stepType: StepType): Unit = {
    currentDevice.listener.step(stepInfoCallBack _,stepType)
  }

  protected def openAssembler(): Unit = {
    ucesoft.cbm.cpu.asm.Assembler.getAssemblerDialog(frame,currentDevice.mem,this).setVisible(true)
  }

  protected def openTraceOnFile(): Unit = {
    if (tracingFile != null && !traceOnFile.isSelected) {
      tracingFile.close()
      currentDevice.listener.setTraceOnFile(null,false)
    }
    else {
      val fc = new JFileChooser()
      fc.setDialogTitle("Select tracing output file")
      fc.showOpenDialog(frame) match {
        case JFileChooser.APPROVE_OPTION =>
          tracingFile = new PrintWriter(new BufferedOutputStream(new FileOutputStream(fc.getSelectedFile)))
          currentDevice.listener.setTraceOnFile(tracingFile, true)
        case _ =>
          traceOnFile.setSelected(false)
      }
    }
  }

  protected def disassembleGUI(): Unit = {
    val addressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val addressFrom = new JTextField(5)
    addressFrom.requestFocus()
    val addressTo = new JTextField(5)
    addressFrom.setToolTipText("Insert an hex address")
    addressTo.setToolTipText("Insert an hex address")
    addressPanel.add(new JLabel("From address:"))
    addressPanel.add(addressFrom)
    addressPanel.add(new JLabel("To address (optional):"))
    addressPanel.add(addressTo)
    JOptionPane.showConfirmDialog(frame,addressPanel,"Disassambler",JOptionPane.OK_CANCEL_OPTION) match {
      case JOptionPane.OK_OPTION =>
        try {
          disassemble(Integer.parseInt(addressFrom.getText,16),if (addressTo.getText.isEmpty) None else Some(Integer.parseInt(addressTo.getText,16)))
        }
        catch {
          case n:NumberFormatException =>
            JOptionPane.showMessageDialog(frame,s"Invalid address: ${n.getMessage}","Address error",JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }

  protected def readGUI(): Unit = {
    val addressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val addressFrom = new JTextField(5)
    val addressTo = new JTextField(5)
    addressFrom.setToolTipText("Insert an hex address")
    addressTo.setToolTipText("Insert an hex address")
    addressPanel.add(new JLabel("From address:"))
    addressPanel.add(addressFrom)
    addressPanel.add(new JLabel("To address (optional):"))
    addressPanel.add(addressTo)
    JOptionPane.showConfirmDialog(frame, addressPanel, "Read memory", JOptionPane.OK_CANCEL_OPTION) match {
      case JOptionPane.OK_OPTION =>
        try {
          readMemory(Integer.parseInt(addressFrom.getText, 16) & 0xFFFF, if (addressTo.getText.isEmpty) None else Some(Integer.parseInt(addressTo.getText, 16) & 0xFFFF))
        }
        catch {
          case n: NumberFormatException =>
            JOptionPane.showMessageDialog(frame, s"Invalid address: ${n.getMessage}", "Address error", JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }

  protected def writeGUI(): Unit = {
    val addressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val addressFrom = new JTextField(5)
    val addressTo = new JTextField(5)
    val stepAddress = new JTextField(5)
    val stepValue = new JTextField(5)
    val value = new JTextField(5)
    addressFrom.setToolTipText("Insert an hex address")
    addressTo.setToolTipText("Insert an hex address")
    stepAddress.setToolTipText("Insert an hex step for address")
    stepValue.setToolTipText("Insert an hex step for value")
    value.setToolTipText("Insert an hex value")
    stepAddress.setText("1")
    stepValue.setText("1")
    addressPanel.add(new JLabel("From address:"))
    addressPanel.add(addressFrom)
    addressPanel.add(new JLabel("To address:"))
    addressPanel.add(addressTo)
    addressPanel.add(new JLabel("Address step:"))
    addressPanel.add(stepAddress)
    addressPanel.add(new JLabel("Value step:"))
    addressPanel.add(stepValue)
    addressPanel.add(new JLabel("Value:"))
    addressPanel.add(value)
    JOptionPane.showConfirmDialog(frame, addressPanel, "Fill memory", JOptionPane.OK_CANCEL_OPTION) match {
      case JOptionPane.OK_OPTION =>
        try {
          fillMemory(Integer.parseInt(addressFrom.getText, 16) & 0xFFFF,
            Integer.parseInt(addressTo.getText, 16)  & 0xFFFF,
            Integer.parseInt(stepAddress.getText, 16) & 0xFFFF,
            Integer.parseInt(stepValue.getText, 16) & 0xFF,
            Integer.parseInt(value.getText, 16) & 0xFF
          )
        }
        catch {
          case n: NumberFormatException =>
            JOptionPane.showMessageDialog(frame, s"Invalid address/step/value: ${n.getMessage}", "Parameter error", JOptionPane.ERROR_MESSAGE)
        }
      case _ =>
    }
  }

  protected def fillMemory(fromAddress:Int,toAddress:Int,astep:Int,vstep:Int,value:Int): Unit = {
    var adr = fromAddress
    var v = value
    while (adr <= toAddress) {
      currentDevice.mem.write(adr,v)
      adr = (adr + astep) & 0xFFFF
      v = (v + vstep) & 0xFF
    }
  }

  protected def readMemory(fromAddress:Int,toAddress:Option[Int]): Unit = {
    var a = fromAddress
    val target = toAddress.getOrElse(fromAddress + 0x100)

    val sb = new StringBuilder
    val ascii = new StringBuilder
    var col = 0
    while (a <= target) {
      if (col == 0) sb.append("%04X: ".format(a))
      val c = currentDevice.mem.read(a)
      if (c > 32) ascii.append(c.toChar) else ascii.append('.')
      sb.append("%02X ".format(c))
      col += 1
      if (col == 16) {
        col = 0
        write(s"$sb  $ascii")
        sb.clear
        ascii.clear
      }
      a = (a + 1) & 0xFFFF
    }
    if (sb.length > 0) write(sb.toString)
  }

  protected def disassemble(fromAddress:Int,toAddress:Option[Int]): Unit = {
    var a = fromAddress
    val target = toAddress.getOrElse(fromAddress + 0x100)

    while (a <= target) {
      val DisassembleInfo(d,b) = currentDevice.listener.disassemble(a)
      write(d)
      a += b
    }
  }

  protected def enableCycleMode(enabled:Boolean): Unit = {
    currentDevice.listener.setCycleMode(enabled)
  }

  protected def write(s:String): Unit = {
    tracePanel.append(s"$s\n")
  }

  override def setDisplay(display: Tracer.TracedDisplay): Unit = {
    this.display = display
    displayPanel.setVisible(true)
    frame.getContentPane.invalidate()
  }

  override def executeCommand(cmd: String): String = ???

  override def isTracing(): Boolean = traceEnabled
}
