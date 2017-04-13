package ucesoft.cbm.trace

import javax.swing._
import java.awt.event._
import ucesoft.cbm.cpu.Memory
import java.awt.BorderLayout
import ucesoft.cbm.Log
import ucesoft.cbm.cpu.Assembler
import ucesoft.cbm.cpu.CPU6510
import java.io.FileOutputStream
import java.io.FileInputStream
import java.awt.GridLayout
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.peripheral.vic.VIC
import ucesoft.cbm.cpu.CPU6510Mems
import java.io._

object TraceDialog {
  def getTraceDialog(displayFrame: JFrame, mem: Memory, traceListener: TraceListener, display: Display, vic: VIC): TraceDialog = {
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
  mem: Memory,
  traceListener: TraceListener,
  display: Option[Display],
  vic: Option[VIC]) extends JDialog(displayFrame, "Trace dialog") with ActionListener {
  private val notrace = new JButton("Tracing on")
  private val rasterLineSpinner = new JSpinner
  private val traceSR = new JLabel
  val logPanel = Log.getLogPanel
  private[this] var tracing = false
  private[this] var tracingFile : PrintWriter = _
  
  private def s2a(address: String) = address(0) match {
    case '$' => Integer.parseInt(address.substring(1), 16)
    case '%' => Integer.parseInt(address.substring(1), 2)
    case _ => address.toInt
  }
  
  def forceTracing(on:Boolean) {
    tracing = on
    vic match {
      case Some(v) => v.setShowDebug(tracing)
      case None =>
    }
    traceListener.step(regs => traceSR.setText(regs))
    traceListener.setTrace(tracing)
    if (!tracing) {
      traceListener.step(regs => traceSR.setText(regs))
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
      case "JMP" =>
        Option(JOptionPane.showInputDialog(this, "Jump to address:")) match {
          case Some(address) =>
            traceListener.jmpTo(s2a(address))
          case _ =>
        }
      case "STEP" =>
        if (tracing) Log.setDebug
        Log.setOutput(logPanel.writer)
        traceListener.step(regs => traceSR.setText(regs))
      case "GOTO" =>
        Log.setOutput(logPanel.writer)
        Option(JOptionPane.showInputDialog(this, "Break type:")) match {
          case Some(breakType) =>
            traceListener.setBreakAt(BreakType.makeBreak(breakType), (regs) => { traceSR.setText(regs) ; forceTracing(true) })
            Log.setInfo
            traceListener.step(regs => traceSR.setText(regs))
          case _ =>
        }
      case "READ" =>
        Option(JOptionPane.showInputDialog(this, "Address to read:")) match {
          case Some(address) =>
            if (address.contains("-")) {
              val addresses = address split "-" map s2a
              var col = 0
              var sb = new StringBuilder
              for (a <- addresses(0) until addresses(0) + addresses(1)) {
                if (col == 0) sb.append("%4X: ".format(a))
                sb.append("%2X ".format(mem.read(a)))
                col += 1
                if (col == 8) {
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
              val d = CPU6510.disassemble(mem, a)
              Log.info(d.toString)
              a += d.bytes.length
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
      case "PRINTRASTERINFO" =>
        Log.info(vic.get.getTraceRasterLineInfo)
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
  val jmp = new JButton("Jmp")
  jmp.setActionCommand("JMP")
  jmp.addActionListener(this)
  val step = new JButton("Go")
  step.setActionCommand("STEP")
  step.addActionListener(this)
  val goto = new JButton("Break")
  goto.setActionCommand("GOTO")
  goto.addActionListener(this)
  val read = new JButton("R")
  read.setActionCommand("READ")
  read.addActionListener(this)
  val write = new JButton("W")
  write.setActionCommand("WRITE")
  write.addActionListener(this)
  notrace.setActionCommand("NOTRACE")
  notrace.addActionListener(this)
  val traceFile = new JToggleButton("TraceFile")
  traceFile.setActionCommand("TRACEFILE")
  traceFile.addActionListener(this)
  val disa = new JButton("Dasm")
  disa.setActionCommand("DISA")
  disa.addActionListener(this)
  val asm = new JButton("Asm")
  asm.setActionCommand("ASM")
  asm.addActionListener(this)
//  val char = new JButton("CViewer")
//  char.setActionCommand("CHARVIEWER")
//  char.addActionListener(this)
  val clear = new JButton("Clear")
  clear.setActionCommand("CLEAR")
  clear.addActionListener(this)
  val vicDump = new JButton("VIC Dump")
  vicDump.setActionCommand("VICDUMP")
  vicDump.addActionListener(this)
  val buttonPanel = new JPanel
  buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS))
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
    val showRasterLineInfoButton = new JButton("Print raster info")
    showRasterLineInfoButton.setActionCommand("PRINTRASTERINFO")
    showRasterLineInfoButton.addActionListener(this)
    buttonPanel.add(showRasterCB)
    buttonPanel.add(rasterLineSpinner)
    buttonPanel.add(showRasterLineInfoButton)
  }

  tracePanel.add("North", buttonPanel)
  val pcsrPanel = new JPanel
  pcsrPanel.add(traceSR)
  tracePanel.add("South", pcsrPanel)
  getContentPane.add("North", tracePanel)
  getContentPane.add("Center",logPanel)
}