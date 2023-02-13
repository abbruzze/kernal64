package ucesoft.cbm.misc

import java.awt.datatransfer.StringSelection
import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, Toolkit}
import java.awt.event.{KeyEvent, KeyListener}
import javax.swing.{BorderFactory, JButton, JDialog, JFrame, JLabel, JPanel, WindowConstants}

object KeyboardHelper {
  def getDialog(frame:JFrame,closeAction: () => Unit): JDialog = {
    val dialog = new JDialog(frame,"Keyboard layout test",false)
    dialog.getContentPane.add(new KeyboardHelper(() => { dialog.dispose() ; closeAction() }))
    dialog.pack()
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.setLocationRelativeTo(frame)
    dialog
  }
}
class KeyboardHelper(closeEvent: () => Unit) extends JPanel with KeyListener {
  private val VK_MAP = getKeyEventMap()
  private val vkName = new JLabel()
  private val vkCode = new JLabel()
  private val position = new JLabel()
  private val modifiers = new JLabel()
  private val key = new JLabel()

  override def keyTyped(e: KeyEvent): Unit = {}
  override def keyPressed(e: KeyEvent): Unit = {
    VK_MAP.get(e.getExtendedKeyCode) match {
      case Some(vk) =>
        vkName.setText(vk.substring(3))
        vkCode.setText("")
        key.setText(vk.substring(3))
      case None =>
        vkName.setText("UNDEFINED")
        vkCode.setText("!" + e.getExtendedKeyCode)
        key.setText("!" + e.getExtendedKeyCode)
    }
    val pos = e.getKeyLocation() match {
      case KeyEvent.KEY_LOCATION_NUMPAD => "NUM PAD"
      case KeyEvent.KEY_LOCATION_LEFT => "LEFT"
      case KeyEvent.KEY_LOCATION_RIGHT => "RIGHT"
      case KeyEvent.KEY_LOCATION_STANDARD => "STANDARD"
      case KeyEvent.KEY_LOCATION_UNKNOWN => "UNKNOWN"
    }
    position.setText(pos)
    var mod : List[String]= Nil
    var modeS = ""
    if (e.isShiftDown) {
      mod ::= "Shift"
      if (e.getKeyCode != KeyEvent.VK_SHIFT) modeS += "+"
    }
    if (e.isAltDown) mod ::= "Alt"
    if (e.isAltGraphDown) {
      mod ::= "AltGr"
      modeS += "^"
    }
    if (e.isControlDown ) mod ::= "Ctrl"
    modifiers.setText(mod.mkString("+"))
    key.setText(modeS + key.getText())
  }
  override def keyReleased(e: KeyEvent): Unit = {}

  init()

  private def init(): Unit = {
    setLayout(new BorderLayout())
    setFocusTraversalKeysEnabled(false)

    val innerPanel = new JPanel()
    val stdFont = key.getFont()
    key.setFont(new Font(stdFont.getName,stdFont.getStyle,stdFont.getSize * 3))
    innerPanel.add(key)
    innerPanel.setPreferredSize(new Dimension(500,100))
    innerPanel.setBorder(BorderFactory.createTitledBorder("Press a key"))

    add("Center",innerPanel)
    val toolBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
    toolBar.add(new JLabel("VK_NAME:"))
    toolBar.add(vkName)
    toolBar.add(new JLabel("VK_CODE:"))
    toolBar.add(vkCode)
    toolBar.add(new JLabel("Modifiers:"))
    toolBar.add(modifiers)
    toolBar.add(new JLabel("Position:"))
    toolBar.add(position)
    add("North",toolBar)

    vkName.setForeground(Color.BLUE)
    vkCode.setForeground(Color.BLUE)
    position.setForeground(Color.BLUE)
    modifiers.setForeground(Color.BLUE)

    val closePanel = new JPanel()
    val close = new JButton("Close")
    close.addActionListener(_ => closeEvent() )
    val copyClip = new JButton("Copy into clipboard")
    copyClip.addActionListener(_ => Toolkit.getDefaultToolkit.getSystemClipboard().setContents(new StringSelection(key.getText),null))
    copyClip.setFocusable(false)
    closePanel.add(close)
    closePanel.add(copyClip)
    add("South",closePanel)

    setFocusable(true)
    addKeyListener(this)
  }

  private def getKeyEventMap(): Map[Int, String] = {
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields filter {
      _.getName.startsWith("VK_")
    } map { f => (f.get(null).asInstanceOf[Int], f.getName) } toMap
  }
}
