package ucesoft.cbm

import java.io.PrintWriter

import javax.swing.JPanel
import java.io.Writer
import java.awt.{BorderLayout, Color, Font}

import javax.swing.JTextArea
import javax.swing.JScrollPane

object Log {
  private val FINE = 1
  private val DEBUG = 2
  private val INFO = 4
  private var severity = 0
  private var out : PrintWriter = new PrintWriter(System.out,true)
  
  def setInfo = severity = INFO
  def setDebug = severity = DEBUG | INFO
  def setFine = severity = FINE | DEBUG | INFO
  def setOutput(out:PrintWriter) { this.out = out }
  def getOut = out
  
  @inline private def format(m:String) = "[%10d]\t%s".format(if (Clock.isAvailable) Clock.systemClock.currentCycles else 0,m)
  @inline private def log(msg:String) { out.println(format(msg)) }
  
  @inline final def fine(msg: => String) = if ((severity & FINE) != 0) log(msg)
  @inline final def debug(msg: => String) = if ((severity & DEBUG) != 0) log(msg)
  @inline final def info(msg: => String) = if ((severity & INFO) != 0) log(msg)
  
  def getLogPanel = new LogPanel
  
  class LogPanel extends JPanel {
    private val logPanel = new JTextArea(30,50)
    setLayout(new BorderLayout)
    logPanel.setEditable(false)
    add("Center",new JScrollPane(logPanel))
    logPanel.setFont(new Font(Font.MONOSPACED,Font.BOLD,12))
    logPanel.setForeground(Color.WHITE)
    logPanel.setBackground(Color.BLACK)
    val writer = new PrintWriter(new Writer {
      def write(chars:Array[Char],off:Int,len:Int) {
        val str = new String(chars,off,len)        
        logPanel.append(str)
        logPanel.setCaretPosition(logPanel.getText.length)
      }
      def flush {}
      def close {}
    },true)
    
    def clear = logPanel.setText("")
  }
}