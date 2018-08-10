package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.keyboard.KeyboardMapper
import javax.swing._
import ucesoft.cbm.peripheral.keyboard.CKey
import ucesoft.cbm.peripheral.keyboard.KeyboardMapperStore
import java.awt.FlowLayout
import java.awt.GridLayout
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Font

object KeyboardEditor extends App {
  val ke = new KeyboardEditor(ucesoft.cbm.c128.C128KeyboardMapper,true)
  val f = new JFrame  
  f.getContentPane.add("Center",ke)
  f.pack
  f.setVisible(true)
}

class KeyboardEditor(keybm:KeyboardMapper,isC64:Boolean) extends JPanel with ActionListener with KeyListener {
  private val map = {
    val m = new collection.mutable.HashMap[CKey.Key,Int]
    for(kv <- keybm.map) m += ((kv._2,kv._1))
    m
  }
  private val cmap = {
    val m = new collection.mutable.HashMap[(CKey.Key,Boolean),Char]
    for(kv <- keybm.cmap) if (!kv._2._2) m += ((kv._2,kv._1))
    m
  }
  private val keypad_map = {
    val m = new collection.mutable.HashMap[CKey.Key,Int]
    for(kv <- keybm.keypad_map) m += ((kv._2,kv._1))
    m
  }
  
  private case class ButtonKey(key:CKey.Key,keyCode:Option[Int],keyChar:Option[Char]) {
    override def toString = keyCode match {
      case Some(k) => KeyboardMapperStore.KEY_EVENT_MAP(k)
      case None => keyChar match {
        case Some(c) => c.toString
        case None => "EMPTY"
      }
    }
    def getDesc : String = keyCode match {
      case Some(kc) => java.awt.event.KeyEvent.getKeyText(kc)
      case None => keyChar match {
        case Some(c) => c.toString
        case None => "Empty slot"
      }
    }
  }
  
  private val keys = CKey.values filter { k => if (isC64) !CKey.is128Key(k) else true } toArray
  private val maxKeyLen = keys map { _.toString.length } max
  private val keyButtons : Array[ButtonKey] = keys map { k =>
    findKeyCode(k) match {
      case Some(vk) => ButtonKey(k,Some(vk),None)
      case None => findKeyChar(k) match {
        case Some(c) => ButtonKey(k,None,Some(c))
        case None => ButtonKey(k,None,None)
      }        
    }
  }
  private val buttons = keyButtons map { k => 
    val b = new JButton(k.toString)
    b.setToolTipText(k.getDesc)
    b.setActionCommand(k.key.toString)
    b.addActionListener(this)
    b
  }
  private val tiles = for(k <- keys.zip(buttons)) yield new JPanel {
    setLayout(new FlowLayout(FlowLayout.LEFT))
    val lab = k._1.toString + (" " * (maxKeyLen - k._1.toString.length))
    val jlabel = new JLabel(lab)
    add(jlabel)
    val f = jlabel.getFont
    jlabel.setFont(new Font("Monospaced",f.getStyle,f.getSize))
    add(k._2)
  }
  private val statusLabel = new JLabel("Press a button to redefine a key...")
  private var waitingIndex = -1
  private val gridPanel = new JPanel(new GridLayout(0,5))
  private val statusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private val saveButton = new JButton("Save as ...")
  
  private val NON_SHIFTABLE_KEYS = Set(
      CKey.CTRL,
      CKey.CBM,
      CKey.L_ARROW,
      CKey.RESTORE,
      CKey.RETURN,
      CKey.EQUAL
  )
  
  setLayout(new BorderLayout)
  add("Center",new JScrollPane(gridPanel))
  add("South",statusPanel)
  for(t <- tiles) gridPanel.add(t)
  
  statusLabel.setForeground(Color.BLACK)
  statusPanel.add(saveButton)
  statusPanel.add(statusLabel)
  addKeyListener(this)
  setFocusTraversalKeysEnabled(false)
  saveButton.addActionListener(this)
  saveButton.setActionCommand("SAVE")
  
  private def findKeyCode(k:CKey.Key) : Option[Int] = {
    map get k match {
      case s@Some(vk) => s
      case None =>
        keypad_map get k
    }
  }
  
  private def findKeyChar(k:CKey.Key) : Option[Char] = {
    cmap get ((k,true)) match {
      case s@Some(c) => s
      case None =>
        cmap get ((k,false))
    }
  }  
  
  def actionPerformed(e:ActionEvent) {
    if (e.getActionCommand == "SAVE") {
      save
      return
    }
    val key = CKey.withName(e.getActionCommand)
    waitingIndex = keys.indexOf(key)
    for(b <- buttons) b.setEnabled(false)
    statusLabel.setText(s"Press a key to redefine C= key $key")
    statusLabel.setForeground(Color.RED)
    requestFocus
  }
  
  def keyPressed(e:KeyEvent) {
    val buttonKey = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) ButtonKey(keys(waitingIndex),Some(e.getKeyCode),None)
                    else ButtonKey(keys(waitingIndex),None,Some(e.getKeyChar))
    keyButtons(waitingIndex) = buttonKey
    buttons(waitingIndex).setText(buttonKey.toString)
    
    if (e.getKeyLocation == KeyEvent.KEY_LOCATION_NUMPAD) {
      if (isC64) JOptionPane.showMessageDialog(this,"Keypad must be used in C128 mode only","Error",JOptionPane.ERROR_MESSAGE,null)
      else keypad_map(keys(waitingIndex)) = e.getKeyCode
    }
    else {
      if (e.getKeyCode != KeyEvent.VK_UNDEFINED) map(keys(waitingIndex)) = e.getKeyCode
      else {
        cmap((keys(waitingIndex),false)) = e.getKeyChar
        if (!NON_SHIFTABLE_KEYS.contains(keys(waitingIndex))) {
          val shifted = JOptionPane.showInputDialog(this,s"Type the char for the shifted key ${keys(waitingIndex)}","Complete with the shifted version",JOptionPane.QUESTION_MESSAGE)
          if (shifted != null) cmap((keys(waitingIndex),true)) = shifted.charAt(0)
          else cmap -= ((keys(waitingIndex),true))
        }
      }
    }
    
    for(b <- buttons) b.setEnabled(true)
    statusLabel.setText("Press a button to redefine a key...")
    statusLabel.setForeground(Color.BLACK)
  }
  def keyReleased(e:KeyEvent) {}
  def keyTyped(e:KeyEvent) {}
  
  private def makeKeyboardMapper : KeyboardMapper = new KeyboardMapper {
    val map = KeyboardEditor.this.map map { kv => (kv._2,kv._1) } toMap
    val cmap = KeyboardEditor.this.cmap map { kv => (kv._2,kv._1) } toMap
    val keypad_map = KeyboardEditor.this.keypad_map map { kv => (kv._2,kv._1) } toMap
  }
  
  private def save {
    val fc = new JFileChooser
    fc.setDialogTitle("Choose where to save this keyboard configuration")
    val fn = fc.showSaveDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        val kbm = makeKeyboardMapper
        import java.io._
        val pw = new PrintWriter(new FileOutputStream(fc.getSelectedFile))
        KeyboardMapperStore.store(kbm,pw)
        pw.close
      case _ =>
    }
  }
}