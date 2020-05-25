package ucesoft.cbm.misc

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Container, FlowLayout, GridBagConstraints, GridBagLayout, GridLayout, Insets}
import java.io.{File, FileOutputStream}

import javax.swing.filechooser.FileFilter
import javax.swing.{JButton, JCheckBox, JComboBox, JComponent, JDialog, JFileChooser, JFrame, JLabel, JList, JPanel, JTextField, SwingUtilities, WindowConstants}
import ucesoft.cbm.peripheral.vic.Display

class GIFPanel(display:Array[Display],displayName:Array[String]) extends JPanel with Runnable {
  private var delayInMillis = 0
  private var out : FileOutputStream = _
  @volatile private var recording = false
  private val fileTextField = new JTextField(20)
  private val delayTextField = new JTextField("1000",10)
  private val startStopButton = new JButton("Start")
  private var selectedDisplayIndex = 0
  private val frameCheckbox = new JCheckBox("Include frame border")
  private val frameRecordedLabel = new JLabel("0")

  private def init : Unit = {
    setLayout(new GridBagLayout)
    val displaySelector = new JComboBox(displayName)
    displaySelector.addActionListener(_ => selectedDisplayIndex = displaySelector.getSelectedIndex )
    add(0,0,new JLabel("Display:"),GridBagConstraints.LINE_END)
    add(1,0,displaySelector,GridBagConstraints.LINE_START)
    add(0,1,new JLabel("GIF output file:"),GridBagConstraints.LINE_END)
    val browseButton = new JButton("Browse")
    browseButton.addActionListener(_ => {
      browse match {
        case Some(file) => fileTextField.setText(file)
        case _ =>
      }
    })
    add(1,1,fileTextField,GridBagConstraints.LINE_START)
    add(2,1,browseButton)
    add(0,2,new JLabel("Frame delay in millis:"),GridBagConstraints.LINE_END)
    add(1,2,delayTextField,GridBagConstraints.LINE_START)
    add(0,3,frameCheckbox,GridBagConstraints.LINE_END)
    add(1,3,new JLabel("Frame recorded:"),GridBagConstraints.LINE_END)
    add(2,3,frameRecordedLabel,GridBagConstraints.LINE_START)
    startStopButton.addActionListener(_ => startStop)
    add(1,4,startStopButton)
  }

  private def add(x:Int,y:Int,comp:JComponent,anchor:Int = GridBagConstraints.CENTER,gw:Int = 1,gh:Int = 1): Unit = {
    val c = new GridBagConstraints
    c.insets = new Insets(5,5,5,5)
    c.gridx = x
    c.gridy = y
    c.fill = GridBagConstraints.NONE
    c.gridwidth = gw
    c.gridheight = gh
    c.anchor = anchor
    add(comp,c)
  }

  private def checkDelay : Unit = {
    try {
      delayInMillis = delayTextField.getText.toInt
      if (delayInMillis < 100) {
        delayInMillis = 100
        delayTextField.setText("100")
      }
    }
    catch {
      case _:Exception =>
        delayTextField.setText("1000")
    }
  }

  private def startStop : Unit = {
    try {
      if (!recording) {
        out = new FileOutputStream(fileTextField.getText)
        val thread = new Thread(this,"GIFRecorder")
        recording = true
        checkDelay
        startStopButton.setText("Stop recording")
        thread.start
      }
      else stopRecording
    }
    catch {
      case t:Throwable =>
    }
  }

  private def browse : Option[String] = {
    val fc = new JFileChooser
    fc.setDialogTitle("Choose a file where to save GIF")
    fc.setFileFilter(new FileFilter {
      def accept(f: File) = f.isDirectory || f.getName.toUpperCase.endsWith(".GIF")
      def getDescription = "GIF files"
    })
    fc.showSaveDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        Some(fc.getSelectedFile.toString)
      case _ =>
        None
    }
  }

  def run : Unit = {
    val includeFrame = frameCheckbox.isSelected
    val component = if (includeFrame) SwingUtilities.getRoot(display(selectedDisplayIndex)) else display(selectedDisplayIndex)
    val image = createImage(component.getSize().width + 1,component.getSize().height + 1).asInstanceOf[BufferedImage]
    val writer = new AnimatedGIFWriter(true)
    writer.prepareForWrite(out,-1,-1)
    var frameRecorded = 0

    while (recording) {
      val ts = System.currentTimeMillis
      SwingUtilities.invokeAndWait( () => {
        component.paint(image.getGraphics)
        writer.writeFrame(out, image, delayInMillis)
        frameRecorded += 1
        frameRecordedLabel.setText(frameRecorded.toString)
      })
      val elapsed = System.currentTimeMillis - ts
      if (elapsed < delayInMillis) Thread.sleep(delayInMillis - elapsed)
    }
    writer.finishWrite(out)
    out.close
    stopRecording
  }

  def stopRecording : Unit = {
    recording = false
    startStopButton.setText("Start")
  }
}

object GIFPanel {
  def createGIFPanel(parent:JFrame,display:Array[Display],displayName:Array[String]) : JDialog = {
    val f = new JDialog(parent,s"GIF recording",false)
    val gifPanel = new GIFPanel(display,displayName)

    f.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = gifPanel.stopRecording
    })

    gifPanel.init
    f.getContentPane.add("Center",gifPanel)
    f.pack
    f
  }
}
