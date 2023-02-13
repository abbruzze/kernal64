package ucesoft.cbm.misc

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.cbm.peripheral.keyboard.{KeyboardMapper, KeyboardMapperStore}

import java.awt.BorderLayout
import java.io.{File, IOException}
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption
import java.util.Properties
import javax.swing.{ImageIcon, JButton, JDialog, JFileChooser, JFrame, JOptionPane, JPanel}

object KeyboardConfigEditor {
  private var editor : KeyboardConfigEditor = _

  def getEditor(frame:JFrame,keyMapper:KeyboardMapper,configuration:Properties,keyboardConfigurationEntry:String): JDialog = {
    if (editor != null) editor.dialog
    else {
      editor = new KeyboardConfigEditor(frame,keyMapper, configuration, keyboardConfigurationEntry)
      editor.dialog
    }
  }
}

class KeyboardConfigEditor(frame:JFrame,keyMapper:KeyboardMapper,configuration:Properties,keyboardConfigurationEntry:String) extends JPanel {
  private val config = new RSyntaxTextArea(30,100)

  init()

  val dialog = {
    val d = new JDialog(frame,s"Keyboard configuration - ${keyMapper.configuration.getOrElse(s"default ${keyMapper.locale.getOrElse("")} layout")}")
    d.getContentPane.add("Center",this)
    d.pack()
    d.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    d.setLocationRelativeTo(frame)
    d
  }

  private def init(): Unit = {
    config.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_UNIX_SHELL)

    setLayout(new BorderLayout())
    add("Center",new RTextScrollPane(config))

    val buttons = new JPanel()
    val close = new JButton("Close")
    close.addActionListener(_ => dialog.dispose())
    buttons.add(close)
    if (keyMapper.configuration.isDefined) {
      val save = new JButton("Save")
      buttons.add(save)
      save.addActionListener(_ => saveAction(false))
    }
    val saveAs = new JButton("Save as")
    buttons.add(saveAs)
    saveAs.addActionListener(_ => saveAction(true))

    val selectFile = new JButton("Select configuration file")
    buttons.add(selectFile)
    selectFile.addActionListener(_ => setLayout(true) )
    if (keyMapper.configuration.isDefined) {
      val restoreLocale = new JButton("Restore local detected configuration")
      buttons.add(restoreLocale)
      restoreLocale.addActionListener(_ => setLayout(false) )
    }
    val helper = new JButton("Keyboard layout helper")
    buttons.add(helper)
    helper.addActionListener(_ => {
      helper.setEnabled(false)
      val hd = KeyboardHelper.getDialog(frame, () => helper.setEnabled(true) )
      hd.setVisible(true)
    } )

    add("South",buttons)

    config.setText(keyMapper.content)
  }

  private def setLayout(onFile:Boolean): Unit = {
    onFile match {
      case true =>
        val fc = new JFileChooser()
        fc.setDialogTitle("Select keyboard configuration file")
        fc.showOpenDialog(dialog) match {
          case JFileChooser.APPROVE_OPTION =>
            configuration.setProperty(keyboardConfigurationEntry, fc.getSelectedFile.toString)
            JOptionPane.showMessageDialog(dialog, "Reboot the emulator to activate the new keyboard", "Keyboard..", JOptionPane.INFORMATION_MESSAGE)
          case _ =>
        }
      case false =>
        JOptionPane.showConfirmDialog(dialog,"Are you sure you want to restore local detected layout configuration ?","Confirm",JOptionPane.YES_NO_OPTION) match {
          case JOptionPane.YES_OPTION =>
            configuration.remove(keyboardConfigurationEntry)
            JOptionPane.showMessageDialog(dialog,"Reboot the emulator to activate the new keyboard", "Keyboard..",JOptionPane.INFORMATION_MESSAGE)
          case _ =>
        }
    }
  }

  private def saveAction(saveAs:Boolean): Unit = {
    try {
      import scala.jdk.CollectionConverters._
      if (!saveAs) java.nio.file.Files.write(new File(keyMapper.configuration.get).toPath,config.getText().split("\n").toList.asJava,Charset.defaultCharset(),StandardOpenOption.CREATE)
      else {
        val fc = new JFileChooser()
        fc.setDialogTitle("Save as ...")
        fc.showSaveDialog(dialog) match {
          case JFileChooser.APPROVE_OPTION =>
            java.nio.file.Files.write(fc.getSelectedFile.toPath,config.getText().split("\n").toList.asJava,Charset.defaultCharset(),StandardOpenOption.CREATE)
          case _ =>
        }
      }
    }
    catch {
      case io:IOException =>
        JOptionPane.showMessageDialog(dialog,s"Error while writing configuration: $io","Write error",JOptionPane.ERROR_MESSAGE)
    }
  }

}
