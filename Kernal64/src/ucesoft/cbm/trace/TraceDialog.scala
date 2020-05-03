package ucesoft.cbm.trace

import javax.swing._
import java.awt.event._

import ucesoft.cbm.cpu.Memory
import java.awt.{BorderLayout, Color}

import ucesoft.cbm.Log
import ucesoft.cbm.cpu.Assembler
import java.io.FileOutputStream

import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.peripheral.vic.VIC
import java.io._

import javax.swing.JSpinner.DefaultEditor

object TraceDialog {
  def getTraceDialog(displayFrame: JFrame, mem: Memory,traceListener: TraceListener, display: Display, vic: VIC): TraceDialog = {
    val dialog = new TraceDialog(displayFrame, mem, traceListener,Some(display),Some(vic))
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.pack
    dialog
  }
  def getTraceDialog(displayFrame: JFrame, mem: Memory, traceListener: TraceListener): TraceDialog = {
    val dialog = new TraceDialog(displayFrame, mem, traceListener,None,None)
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.pack
    dialog
  }
}

class TraceDialog private (displayFrame: JFrame,
  var mem: Memory,
  var traceListener: TraceListener,
  display: Option[Display],
  vic: Option[VIC]) extends JDialog(displayFrame, "Trace dialog") with ActionListener {
  private val notrace = new JButton("Tracing on")
  private val rasterLineSpinner = new JSpinner(new AbstractSpinnerModel {
    private var value = 0
    override def getValue = value

    override def setValue(value: Any) : Unit = {
      value match {
        case v:Integer if v >= 0 && v <= 312 =>
          this.value = v
          fireStateChanged()
        case _ =>
      }
    }

    override def getNextValue = if (value < 312) value + 1 else value

    override def getPreviousValue = if (value > 0) value - 1 else 0
  })
  private val traceSR = new JLabel
  val logPanel = Log.getLogPanel
  private[this] var tracing = false
  private[this] var tracingFile : PrintWriter = _
  
  private def s2a(address: String) = address.trim()(0) match {
    case '$' => Integer.parseInt(address.substring(1), 16)
    case '%' => Integer.parseInt(address.substring(1), 2)
    case _ => address.toInt
  }
  
  def isTracing = tracing

  private def updateRegs(_regs:String) : Unit = {
    val regs = vic match {
      case Some(vic) =>
        s"${_regs} rasterLine=${vic.getRasterLine} rasterCycle=${vic.getRasterCycle}"
      case None =>
        _regs
    }
    traceSR.setText(regs)
  }
  
  def forceTracing(on:Boolean) {
    tracing = on
    vic match {
      case Some(v) => v.setShowDebug(tracing)
      case None =>
    }
    traceListener.step(updateRegs _)
    traceListener.setTrace(tracing)
    if (!tracing) {
      traceListener.step(updateRegs _)
      Log.setInfo 
    }
    else Log.setDebug
    notrace.setText("Tracing " + (if (!tracing) "on" else "off"))    
  }
  
  def actionPerformed(e: ActionEvent) {
    try {
      handleAction(e)
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(this,t.toString,"Debug error",JOptionPane.ERROR_MESSAGE)
    }
  }

  private def handleAction(e: ActionEvent) {
    e.getActionCommand match {
      case "CYCLEMODE" =>
        traceListener.setCycleMode(e.getSource.asInstanceOf[JToggleButton].isSelected)
      case "JMP" =>
        Option(JOptionPane.showInputDialog(this, "Jump to address:")) match {
          case Some(address) =>
            traceListener.jmpTo(s2a(address))
          case _ =>
        }
      case "STEP" =>
        if (tracing) Log.setDebug
        Log.setOutput(logPanel.writer)
        traceListener.step(updateRegs _)
      case "GOTO" =>
        Log.setOutput(logPanel.writer)
        Option(JOptionPane.showInputDialog(this, "Break type:")) match {
          case Some(breakType) =>
            traceListener.setBreakAt(BreakType.makeBreak(breakType), (regs) => { updateRegs(regs) ; forceTracing(true) })
            Log.setInfo
            traceListener.step(updateRegs _)
          case _ =>
        }
      case "READ" =>
        Option(JOptionPane.showInputDialog(this, "Address to read:")) match {
          case Some(address) =>
            if (address.startsWith("find")) {
              val FINDRE = """find\s+(\d+)-(\d+)\s+'([^']+)'""".r
              address match {
                case FINDRE(from,to,what) =>
                  for(a <- s2a(from) to s2a(to) - what.length) {
                    var c = 0
                    while (c < what.length && mem.read(a + c) == what(c)) c += 1
                    if (c == what.length) {
                      Log.info("Found at address $" + a.toHexString)
                      return
                    }
                  }
                  Log.info(s"$what not found")
                case _ =>
                  Log.info("Bad find command")
              }
            }
            else
            if (address.contains("-")) {
              val addresses = address split "-" map { s => s2a(s.trim) }
              var col = 0
              val sb = new StringBuilder
              for (a <- addresses(0) to addresses(1)) {
                if (col == 0) sb.append("%04X: ".format(a))
                sb.append("%02X ".format(mem.read(a)))
                col += 1
                if (col == 16) {
                  col = 0
                  Log.info(sb.toString)
                  sb.clear
                }
              }
              if (sb.length > 0) Log.info(sb.toString)
            } else if (address.startsWith("w")) {
              val a = s2a(address.substring(1))
              val word = mem.read(a + 1) * 256 | mem.read(a)
              Log.info(s"ReadWord(${address.substring(1)})=${Integer.toHexString(word)}")
            } else Log.info(s"Read(${address})=${Integer.toHexString(mem.read(s2a(address)))}")
          case _ =>
        }
      case "WRITE" =>
        Option(JOptionPane.showInputDialog(this, "Address to write:")) match {
          case Some(address) =>
            val addressValue = address split " "
            mem.write(s2a(addressValue(0)), s2a(addressValue(1)))
            Log.info(s"Write(${addressValue(0)})=${Integer.toHexString(mem.read(s2a(addressValue(0))))}")
          case _ =>
        }
      case "NOTRACE" =>
        tracing = !tracing
        forceTracing(tracing)        
      case "DISA" =>
        Option(JOptionPane.showInputDialog(this, "Disassemble from address to address:")) match {
          case Some(address) =>
            val addresses = address split " " map s2a
            var a = addresses(0)
            while (a <= addresses(1)) {
              val (d,len) = traceListener.disassemble(mem, a)
              Log.info(d.toString)
              a += len
            }
          case _ =>
        }
      case "ASM" =>
        Assembler.getAssemblerDialog(displayFrame, mem).setVisible(true)
      case "SHOWRASTER" =>
        display.get.setRasterLineAt(rasterLineSpinner.getValue.asInstanceOf[Int])
        vic.get.setTraceRasterLineAt(rasterLineSpinner.getValue.asInstanceOf[Int])
        display.get.setDrawRasterLine(e.getSource.asInstanceOf[JCheckBox].isSelected)
        vic.get.enableTraceRasterLine(e.getSource.asInstanceOf[JCheckBox].isSelected)
      case "CHARVIEWER" =>
        FontPanel.getFontDialog(displayFrame, vic.get.getMemory).setVisible(true)
      case "CLEAR" =>
        logPanel.clear
      case "VICDUMP" =>
        Log.info("\n" + vic.get.dump)
      case "TRACEFILE" =>
        val button = e.getSource.asInstanceOf[JToggleButton]
        if (!button.isSelected) {
          tracingFile.close
          traceListener.setTraceOnFile(null,false)
        }
        else {
          val fc = new JFileChooser
          fc.showOpenDialog(displayFrame) match {
            case JFileChooser.APPROVE_OPTION =>
              tracingFile = new PrintWriter(new BufferedOutputStream(new FileOutputStream(fc.getSelectedFile)))
              traceListener.setTraceOnFile(tracingFile,true)
            case _ =>  
              button.setSelected(false)
          }
        }
    }
  }

  val tracePanel = new JPanel
  tracePanel.setLayout(new BorderLayout)
  val cycleMode = new JToggleButton("Cycle mode")
  cycleMode.setToolTipText("Enable/Disable cycle debug mode")
  cycleMode.setActionCommand("CYCLEMODE")
  cycleMode.addActionListener(this)
  val jmp = new JButton("Jmp")
  jmp.setToolTipText("Jump to the given PC address")
  jmp.setActionCommand("JMP")
  jmp.addActionListener(this)
  val step = new JButton("Go")
  step.setToolTipText("Step into istruction or cycle")
  step.setActionCommand("STEP")
  step.addActionListener(this)
  val goto = new JButton("Break")
  goto.setToolTipText("Set the breakpoint to the given address or 'irq'/'nmi'")
  goto.setActionCommand("GOTO")
  goto.addActionListener(this)
  val read = new JButton("R")
  read.setToolTipText("Read from a given address")
  read.setActionCommand("READ")
  read.addActionListener(this)
  val write = new JButton("W")
  write.setToolTipText("Write to a given address")
  write.setActionCommand("WRITE")
  write.addActionListener(this)
  notrace.setActionCommand("NOTRACE")
  notrace.setToolTipText("Enable/Disable tracing")
  notrace.addActionListener(this)
  val traceFile = new JToggleButton("TraceFile")
  traceFile.setToolTipText("Enable/Disable tracing on file")
  traceFile.setActionCommand("TRACEFILE")
  traceFile.addActionListener(this)
  val disa = new JButton("Dasm")
  disa.setToolTipText("Disassemble the given address range")
  disa.setActionCommand("DISA")
  disa.addActionListener(this)
  val asm = new JButton("Asm")
  asm.setToolTipText("Open the assembler")
  asm.setActionCommand("ASM")
  asm.addActionListener(this)
//  val char = new JButton("CViewer")
//  char.setActionCommand("CHARVIEWER")
//  char.addActionListener(this)
  val clear = new JButton("Clear")
  clear.setToolTipText("Clear debug area")
  clear.setActionCommand("CLEAR")
  clear.addActionListener(this)
  val vicDump = new JButton("VIC Dump")
  vicDump.setToolTipText("Print VIC info")
  vicDump.setActionCommand("VICDUMP")
  vicDump.addActionListener(this)
  val buttonPanel = new JPanel
  buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS))
  buttonPanel.add(cycleMode)
  buttonPanel.add(jmp)
  buttonPanel.add(step)
  buttonPanel.add(goto)
  buttonPanel.add(read)
  buttonPanel.add(write)
  buttonPanel.add(notrace)
  if (vic.isDefined) buttonPanel.add(traceFile)
  buttonPanel.add(disa)
  buttonPanel.add(asm)
  buttonPanel.add(clear)
  if (vic.isDefined) {
    //buttonPanel.add(char)
    buttonPanel.add(vicDump)
    val showRasterCB = new JCheckBox("Show raster", false)
    showRasterCB.setActionCommand("SHOWRASTER")
    showRasterCB.addActionListener(this)
    rasterLineSpinner.addChangeListener(new ChangeListener {
      def stateChanged(e: ChangeEvent) {
        val r = rasterLineSpinner.getValue.asInstanceOf[Int]
        display.get.setRasterLineAt(r)
        vic.get.setTraceRasterLineAt(r)
      }
    })
    rasterLineSpinner.getEditor.asInstanceOf[DefaultEditor].getTextField.setEditable(true)
    buttonPanel.add(showRasterCB)
    buttonPanel.add(rasterLineSpinner)
  }

  tracePanel.add("North", buttonPanel)
  val pcsrPanel = new JPanel
  pcsrPanel.add(traceSR)
  traceSR.setForeground(Color.BLUE)
  tracePanel.add("South", pcsrPanel)
  getContentPane.add("North", tracePanel)
  getContentPane.add("Center",logPanel)
}