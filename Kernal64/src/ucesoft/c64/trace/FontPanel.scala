package ucesoft.c64.trace

import javax.swing._
import ucesoft.c64.cpu.Memory
import java.awt.event.ActionListener
import java.awt.BorderLayout
import java.awt.GridLayout
import java.awt.event.ActionEvent

object FontPanel {
  def getFontDialog(displayFrame:JFrame,mem:Memory) = {
    val dialog = new JDialog(displayFrame,"Character viewer")
    dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    dialog.getContentPane.add("Center",new FontPanel(mem))
    dialog.pack
    dialog
  }  
}

class FontPanel(mem:Memory) extends JPanel with ActionListener {
  private val addressTextField = new JTextField("0",10)
  private val matrix = Array.fill(8,8)(new JLabel(" "))
  
  init
  
  private def init {
    setLayout(new BorderLayout)
    val northPanel = new JPanel
    northPanel.add(new JLabel("Address:"))
    northPanel.add(addressTextField)
    val up = new JButton("+")
    val down = new JButton("-")
    up.addActionListener(this)
    up.setActionCommand("UP")
    down.addActionListener(this)
    down.setActionCommand("DOWN")
    addressTextField.setActionCommand("ADDRESS")
    addressTextField.addActionListener(this)
    northPanel.add(up)
    northPanel.add(down)
    add("North",northPanel)
    val matrixPanel = new JPanel(new GridLayout(8,8,0,0))
    for(r <- 0 to 7;c <- 0 to 7) matrixPanel.add(matrix(r)(c))
    add("Center",matrixPanel)
  }
  
  private def updateMatrix {
    val address = addressTextField.getText.toInt
    for(r <- 0 to 7) {      
      val ch = mem.read(address + r)
      for(c <- 0 to 7) {
        val mask = 1 << c
        val set = if ((ch & mask) == mask) "X" else " "
        matrix(r)(7 - c).setText(set)
      }
    }    
    repaint()
  }
  
  def actionPerformed(e:ActionEvent) {
    e.getActionCommand match {
      case "UP" =>
        addressTextField.setText((addressTextField.getText.toInt + 8).toString)
        updateMatrix
      case "DOWN" =>
        addressTextField.setText((addressTextField.getText.toInt - 8).toString)
        updateMatrix
      case "ADDRESS" =>
        updateMatrix
    }
  }
}