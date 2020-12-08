package ucesoft.cbm

import java.io.PrintWriter

import javax.swing.{JPanel, JScrollPane, JTextArea, SwingUtilities}
import java.io.Writer
import java.awt.{BorderLayout, Color, Font}

import javax.swing.text.DefaultCaret
import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane

object Log {
  private val FINE = 1
  private val DEBUG = 2
  private val INFO = 4
  private var severity = 0
  private var out : PrintWriter = new PrintWriter(System.out,true)
  
  def setInfo: Unit = severity = INFO
  def setDebug: Unit = severity = DEBUG | INFO
  def setFine : Unit= severity = FINE | DEBUG | INFO
  def setOutput(out:PrintWriter) : Unit = { this.out = out }
  def getOut : PrintWriter = out
  
  @inline private def format(m:String) = "[%10d]\t%s".format(if (Clock.isAvailable) Clock.systemClock.currentCycles else 0,m)
  @inline private def log(msg:String) : Unit = { out.println(format(msg)) }
  
  @inline final def fine(msg: => String): Unit = if ((severity & FINE) != 0) log(msg)
  @inline final def debug(msg: => String): Unit = if ((severity & DEBUG) != 0) log(msg)
  @inline final def info(msg: => String) : Unit = if ((severity & INFO) != 0) log(msg)

  def isDebug : Boolean = (severity & DEBUG) > 0
  
  def getLogPanel = new LogPanel
  
  class LogPanel extends JPanel {
    private val logPanel = new RSyntaxTextArea(30,50)
    setLayout(new BorderLayout)
    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_ASSEMBLER_6502)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val scroll = new RTextScrollPane(logPanel)
    scroll.setLineNumbersEnabled(false)
    add("Center",scroll)

    val writer = new PrintWriter(new Writer {
      def write(chars:Array[Char],off:Int,len:Int) : Unit = logPanel.append(new String(chars, off, len))
      def flush  : Unit = {}
      def close  : Unit = {}
    },true)
    
    def clear = logPanel.setText("")
  }
}