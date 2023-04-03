package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.keyboard._
import ucesoft.cbm._

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, BorderLayout, CardLayout, Color, Component, Dimension, FlowLayout, Graphics, Graphics2D, GridLayout, Rectangle}
import java.io.{BufferedReader, File, FileReader, FileWriter, PrintWriter}
import javax.imageio.ImageIO
import javax.swing.border.BevelBorder
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing._

object KeybEditor {
  def getDialog(frame:JFrame,keyb:Keyboard,model:CBMComputerModel,closeAction:() => Unit): JDialog = {
    val d = new JDialog(frame,"")
    val editor = new KeybEditor(keyb,d,model)
    d.getContentPane.add("Center", editor)
    d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    d.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        if (editor.close()) {
          d.dispose()
          closeAction()
        }
      }
    })
    d.pack()
    d.setLocationRelativeTo(frame)
    d.setResizable(false)
    d
  }
}

class KeybEditor(keyb:Keyboard,dialog:JDialog,model:CBMComputerModel) extends JPanel with MouseMotionListener with MouseListener {
  private final val VIRTUAL_MODE = 0
  private final val EDIT_MODE = 1

  private case class KeyPos(key:CKey.Value,x:Int,y:Int,dx:Int,dy:Int) {
    def contains(xp:Int,yp:Int,scalex:Double,scaley:Double): Option[Rectangle] = {
      val scaled = getScaledBound(scalex, scaley)
      if (scaled.contains(xp,yp)) Some(scaled) else None
    }
    def getScaledBound(scalex:Double,scaley:Double): Rectangle = {
      val x0 = x * scalex
      val y0 = y * scaley
      val w = dx * scalex
      val h = dy * scaley

      new Rectangle(x0.toInt,y0.toInt,w.toInt,h.toInt)
    }
  }

  private class KeyLayoutPanel extends JPanel {
    override def paint(_g: Graphics): Unit = {
      val g = _g.asInstanceOf[Graphics2D]
      val size = getSize()
      scalex = size.width.toDouble / image.getWidth
      scaley = size.height.toDouble / image.getHeight
      g.drawImage(image, 0, 0, size.width, size.height, null)

      // non-mapped
      if (showNonMappedKeys) {
        for (k <- keys) {
          if (!CKey.isShift(k.key) && !currentEditMap.exists(kv => kv._2.contains(k.key)) && !currentEditKeypadMap.exists(kv => kv._2.contains(k.key))) {
            g.setColor(Color.RED)
            g.setStroke(new BasicStroke(2))
            val scaled = k.getScaledBound(scalex, scaley)
            g.drawRect(scaled.x, scaled.y, scaled.width, scaled.height)
          }
        }
      }

      if (selectedRect != null) {
        g.setColor(Color.WHITE)
        g.setStroke(new BasicStroke(2))
        g.drawRect(selectedRect.x, selectedRect.y, selectedRect.width, selectedRect.height)
      }
      // pinned
      for (k <- pinned) {
        g.setColor(Color.BLUE)
        g.setStroke(new BasicStroke(2))
        val scaled = k.getScaledBound(scalex, scaley)
        if (scaled != selectedRect)
          g.drawRect(scaled.x, scaled.y, scaled.width, scaled.height)
      }
    }
  }

  private class KeyMapCellRenderer(dup:Set[HostKey]) extends DefaultTableCellRenderer {
    private val defaultColor = new JLabel().getForeground
    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
      val label = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column).asInstanceOf[JLabel]
      if (column == 1) label.setToolTipText(table.getModel.getValueAt(row,column).toString) else label.setToolTipText("")
      val hk = table.getModel.getValueAt(row,0).asInstanceOf[HostKey]
      if (dup.contains(hk)) label.setForeground(Color.RED) else label.setForeground(defaultColor)
      label
    }
  }

  private class KeyMapTableModel(map:Map[HostKey,List[CKey.Value]]) extends AbstractTableModel {
    private val rows = {
      val ordered = map.keySet.toArray.sortWith((a,b) => a.code < b.code)
      new collection.mutable.ArrayBuffer[HostKey].addAll(ordered)
    }

    override def getColumnName(column: Int): String = column match {
      case 0 => "Host Key"
      case 1 => "Emulated keys"
    }
    override def isCellEditable(row: Int,col: Int): Boolean = false
    override def getRowCount: Int = rows.size
    override def getColumnCount: Int = 2
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
      columnIndex match {
        case 0 =>
          rows(rowIndex)
        case 1 =>
          map(rows(rowIndex)).mkString(",")
      }
    }

    def removeRows(rows: Array[Int]): Unit = {
      val orderedRows = rows.sortWith((r1, r2) => r1 > r2)
      for (r <- orderedRows) this.rows.remove(r)
      fireTableDataChanged()
    }

    def getMap(): Map[HostKey,List[CKey.Value]] = rows.map(k => k -> map(k)).toMap
    def hasBeenModified: Boolean = rows.size != map.size
  }

  private class KeyMapPanel(map:Map[HostKey,List[CKey.Value]],dup:Set[HostKey]) extends JPanel {
    private val model = new KeyMapTableModel(map)
    private val table = new JTable(model)

    init()

    private def init(): Unit = {
      table.setDefaultRenderer(classOf[Object],new KeyMapCellRenderer(dup))
      table.setAutoCreateRowSorter(true)
      table.setFillsViewportHeight(true)
      val tableScroll = new JScrollPane(table)
      tableScroll.setPreferredSize(new Dimension(250, 300))
      setLayout(new BorderLayout())
      add("Center",tableScroll)
      val buttonPanel = new JPanel(new FlowLayout())
      val deleteButton = new JButton("Delete")
      buttonPanel.add(deleteButton)
      add("South",buttonPanel)
      deleteButton.setEnabled(false)
      table.getSelectionModel.addListSelectionListener(e => {
        if (!e.getValueIsAdjusting) {
          val selected = table.getSelectedRowCount > 0
          deleteButton.setEnabled(selected)
        }
      })
      deleteButton.addActionListener(_ => {
        val rows = table.getSelectedRowCount
        JOptionPane.showConfirmDialog(this,s"Are you sure you want to delete $rows rows?","Confirmation",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            model.removeRows(table.getSelectedRows)
          case _ =>
        }
      })
    }

    def getMap(): Map[HostKey,List[CKey.Value]] = model.getMap()
    def hasBeenModified: Boolean = model.hasBeenModified
  }

  private case class KeyDuplicate(hostKey: HostKey,keys:List[CKey.Value])

  private val prefixName = model match {
    case C64Model => "c64"
    case C128Model => "c128"
    case VIC20Model => "vic20"
    case CBMIIModel => "cbm2"
  }
  private val layoutPanel = new KeyLayoutPanel
  private val image = loadImageLayout()
  private val (keys,scaleF) = readKeys()
  private var selectedKey : KeyPos = _
  private var selectedRect : Rectangle = _
  private var scalex, scaley : Double = 1
  private val pinned = new collection.mutable.HashSet[KeyPos]
  private var mode = VIRTUAL_MODE
  private val cardPanel = new JPanel(new CardLayout())
  private val vmodeHostKeysLabel,editModeHostKeysLabel = new JLabel()
  private val statusLabel = new JLabel()
  private val originalKeyMapper = keyb.getKeyboardMapper
  private var currentEditMap = new collection.mutable.HashMap[HostKey,List[CKey.Value]].addAll(originalKeyMapper.map)
  private var currentEditKeypadMap = new collection.mutable.HashMap[HostKey,List[CKey.Value]].addAll(originalKeyMapper.keypad_map)
  private var currentMapModified = false
  private var showNonMappedKeys = false
  private val restoreMappingButton = new JButton("Restore layout")
  private var mapperFile : Option[String] = None
  private val VIRTUAL_SHIFT = model match {
    case C64Model | C128Model => CKey.L_SHIFT
    case VIC20Model => CKey.VIC20_L_SHIFT
    case CBMIIModel => CKey.CBM2_SHIFT
  }

  init()

  private def loadImageLayout(): BufferedImage = ImageIO.read(getClass.getResourceAsStream(s"/resources/keyboardEditor/${prefixName}_layout.png"))

  private def readKeys(): (List[KeyPos], Double) = {
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(s"/resources/keyboardEditor/${prefixName}_layout.csv"))
    try {
      val keys = source.getLines()
      var scale = 1.0
      var offsetx = 0
      var offsety = 0
      val layout = keys.flatMap { k =>
        k.split(";") match {
          case Array(key, xy, dxdy) =>
            try {
              if (key == "OFFSET") {
                offsetx = xy.toInt
                offsety = dxdy.toInt
                None
              }
              else {
                val ckey = CKey.withName(key)
                val Array(x, y) = xy.split(" ")
                val Array(dx, dy) = dxdy.split(" ")
                Some(KeyPos(ckey, x.toInt + offsetx, y.toInt + offsety, dx.toInt, dy.toInt))
              }
            }
            catch {
              case _: Throwable =>
                None
            }
          case Array("SCALE", scaleF) =>
            scale = scaleF.toDouble
            None
          case _ =>
            None
        }
      }
      (layout.toList, scale)
    }
    finally {
      source.close()
    }
  }

  private def restoreKeyboardMapper(): Unit = {
    JOptionPane.showConfirmDialog(this, "Are you sure you want to restore initial layout ?", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
      case JOptionPane.YES_OPTION =>
        currentEditMap = new collection.mutable.HashMap[HostKey, List[CKey.Value]].addAll(originalKeyMapper.map)
        currentEditKeypadMap = new collection.mutable.HashMap[HostKey, List[CKey.Value]].addAll(originalKeyMapper.keypad_map)
        applyMapsToKeyboard()
        restoreMappingButton.setEnabled(false)
        currentMapModified = false
        checkMode()
      case _ =>
    }
  }

  private def checkDuplicates(key:Option[CKey.Value]): List[KeyDuplicate] = {
    def containsShift(keys:List[CKey.Value]): Boolean = keys.exists(CKey.isShift)
    def check(map:Map[HostKey,List[CKey.Value]]): List[KeyDuplicate] = {
      var noShiftKeys = map.values.flatten.filterNot(k => CKey.isShift(k)).toList
      key match {
        case Some(k) =>
          noShiftKeys = noShiftKeys.filter(_ == k)
        case None =>
      }
      val distinct = noShiftKeys.distinct
      val dups = distinct.filter(k => noShiftKeys.count(_ == k) > 1)
      val found = dups.map { k =>
        val dupKeysComb = map.filter(kv => kv._2.contains(k)).toList.combinations(2).filterNot {
          case List(a, b) if containsShift(a._2) && !containsShift(b._2) => true
          case List(a, b) if !containsShift(a._2) && containsShift(b._2) => true
          case _ => false
        }
        dupKeysComb.flatten.map(kk => KeyDuplicate(kk._1,kk._2)).toList
      }

      found.flatten
    }

    check(currentEditMap.toMap) ++ check(currentEditKeypadMap.toMap)
  }

  private def showKeyMapTable(): Unit = {
    val mapDialog = new JDialog(this.dialog,"Configuration Editor",true)
    mapDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    val tablesPanel = new JPanel(new GridLayout(1,2))
    val dup = checkDuplicates(None).map(_.hostKey).toSet
    val keyMapPanel = new KeyMapPanel(currentEditMap.toMap,dup)
    val keypadMapPanel = new KeyMapPanel(currentEditKeypadMap.toMap,dup)
    keyMapPanel.setBorder(BorderFactory.createTitledBorder("[map]"))
    keypadMapPanel.setBorder(BorderFactory.createTitledBorder("[keypad_map]"))
    tablesPanel.add(keyMapPanel)
    tablesPanel.add(keypadMapPanel)
    val buttonPanel = new JPanel(new FlowLayout())
    val applyButton = new JButton("Apply")
    val cancelButton = new JButton("Cancel")
    buttonPanel.add(applyButton)
    buttonPanel.add(cancelButton)
    mapDialog.getContentPane.add("Center",tablesPanel)
    mapDialog.getContentPane.add("South",buttonPanel)
    mapDialog.pack()

    cancelButton.addActionListener(_ => {
      if (keyMapPanel.hasBeenModified || keypadMapPanel.hasBeenModified) {
        JOptionPane.showConfirmDialog(this,"Are you sure you want to discard changes ?","Confirmation",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            mapDialog.dispose()
          case _ =>
        }
      }
      else
        mapDialog.dispose()
    })
    applyButton.addActionListener(_ => {
      if (keyMapPanel.hasBeenModified || keypadMapPanel.hasBeenModified) {
        JOptionPane.showConfirmDialog(this, "Are you sure you want to apply changes ?", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            currentEditMap.clear()
            currentEditMap.addAll(keyMapPanel.getMap())
            currentEditKeypadMap.clear()
            currentEditKeypadMap.addAll(keypadMapPanel.getMap())
            currentMapModified = true
            checkMode()
            applyMapsToKeyboard()
            mapDialog.dispose()
          case _ =>
        }
      }
    })

    mapDialog.setLocationRelativeTo(dialog)
    mapDialog.setVisible(true)
  }

  private def applyMapsToKeyboard(): Unit = {
    keyb.setKeyboardMapper(KeyboardMapper.makeInternal(currentEditMap.toMap,currentEditKeypadMap.toMap))
    restoreMappingButton.setEnabled(true)
    layoutPanel.repaint()
  }

  private def checkMode(): Unit = {
    val info = s"${mapperFile.map(f => s" - $f").getOrElse("")}${if (currentMapModified) " - modified" else ""}"
    mode match {
      case VIRTUAL_MODE =>
        dialog.setTitle(s"Virtual Keyboard $info")
        statusLabel.setText("Click on keys to press/release on emulated keyboard. Click with shift pressed to lock/unlock key")
      case EDIT_MODE =>
        dialog.setTitle(s"Keyboard Editor $info")
        statusLabel.setText("Double click on a key to edit it")
    }
  }

  private def saveConfig(): Unit = {
    val fc = new JFileChooser()
    fc.setDialogTitle("Configuration saving")
    fc.showSaveDialog(dialog) match {
      case JFileChooser.APPROVE_OPTION =>
        var out : PrintWriter = null
        try {
          out = new PrintWriter(new FileWriter(fc.getSelectedFile))
          KeyboardMapperStore.store(KeyboardMapper.makeInternal(currentEditMap.toMap,currentEditKeypadMap.toMap),out,model)
          currentMapModified = false
          mapperFile = Some(fc.getSelectedFile.toString)
          checkMode()
        }
        catch {
          case t:Throwable =>
            JOptionPane.showMessageDialog(this,s"Error while saving configuration: $t","Saving error",JOptionPane.ERROR_MESSAGE)
        }
        finally {
          if (out != null) out.close()
        }
    }
  }
  private def loadConfig(): Unit = {
    if (currentMapModified) {
      JOptionPane.showConfirmDialog(this, "Are you sure you want to discard changes ?", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
        case JOptionPane.NO_OPTION =>
          return
        case _ =>
      }
    }
    val fc = new JFileChooser()
    fc.setDialogTitle("Configuration loading")
    fc.showOpenDialog(dialog) match {
      case JFileChooser.APPROVE_OPTION =>
        var in : BufferedReader = null
        try {
          in = new BufferedReader(new FileReader(fc.getSelectedFile))
          val mapper = KeyboardMapperStore.load(in,model,Some(fc.getSelectedFile.toString),None)
          currentEditMap = new collection.mutable.HashMap[HostKey, List[CKey.Value]].addAll(mapper.map)
          currentEditKeypadMap = new collection.mutable.HashMap[HostKey, List[CKey.Value]].addAll(mapper.keypad_map)
          restoreMappingButton.setEnabled(true)
          currentMapModified = false
          mapperFile = Some(fc.getSelectedFile.toString)
          checkMode()
        }
        catch {
          case _:IllegalArgumentException =>
            JOptionPane.showMessageDialog(this,s"Error while loading configuration: bad format","Loading error",JOptionPane.ERROR_MESSAGE)
          case t:Throwable =>
            JOptionPane.showMessageDialog(this,s"Error while loading configuration: $t","Loading error",JOptionPane.ERROR_MESSAGE)
        }
        finally {
          if (in != null) in.close()
        }
    }
  }

  private def init(): Unit = {
    layoutPanel.setPreferredSize(new Dimension((image.getWidth / scaleF).toInt, (image.getHeight() / scaleF).toInt))
    layoutPanel.addMouseListener(this)
    layoutPanel.addMouseMotionListener(this)

    setLayout(new BorderLayout())
    val dummyPanel = new JPanel
    dummyPanel.add(layoutPanel)
    add("Center",dummyPanel)
    val modePanel = new JPanel(new FlowLayout(FlowLayout.LEFT,0,5))
    modePanel.setBorder(BorderFactory.createTitledBorder("Mode"))
    val vmode = new JToggleButton("Virtual Mode")
    val editMode = new JToggleButton("Edit Mode")
    modePanel.add(vmode)
    modePanel.add(editMode)
    vmode.setSelected(true)
    editMode.setSelected(false)
    vmode.addActionListener(_ => {
      editMode.setSelected(false)
      setMode(VIRTUAL_MODE)
      cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,VIRTUAL_MODE.toString)
    })
    editMode.addActionListener(_ => {
      vmode.setSelected(false)
      setMode(EDIT_MODE)
      cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,EDIT_MODE.toString)
    })

    val southPanel = new JPanel(new BorderLayout())

    val virtualModePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val editModePanel = new JPanel(new BorderLayout())
    cardPanel.add(virtualModePanel,VIRTUAL_MODE.toString)
    cardPanel.add(editModePanel,EDIT_MODE.toString)
    virtualModePanel.add(new JLabel("Host keys:"))
    virtualModePanel.add(vmodeHostKeysLabel)

    val editModePanelNorthPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    editModePanelNorthPanel.add(new JLabel("Host keys:"))
    editModePanelNorthPanel.add(editModeHostKeysLabel)
    editModePanel.add("North",editModePanelNorthPanel)

    val showUnmappedKeysButton = new JToggleButton("Show unmapped")
    showUnmappedKeysButton.setToolTipText("Highlight in red keys not bound")
    val loadConfigButton = new JButton("Load config")
    loadConfigButton.setToolTipText("Load a new layout from disk")
    val clearConfigButton = new JButton("Clear config")
    clearConfigButton.setToolTipText("Start from scratch with an empty layout")
    val saveConfigButton = new JButton("Save config")
    saveConfigButton.setToolTipText("Save current configuration to disk")
    val editConfigButton = new JButton("Show/Edit configuration")
    editConfigButton.setToolTipText("Show/edit current key bindings")
    val editButtonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    editButtonsPanel.add(showUnmappedKeysButton)
    editButtonsPanel.add(loadConfigButton)
    editButtonsPanel.add(clearConfigButton)
    editButtonsPanel.add(saveConfigButton)
    editButtonsPanel.add(editConfigButton)
    editButtonsPanel.add(restoreMappingButton)
    editModePanel.add("South",editButtonsPanel)

    saveConfigButton.addActionListener(_ => saveConfig())
    loadConfigButton.addActionListener(_ => loadConfig())
    showUnmappedKeysButton.addActionListener(_ => {
      showNonMappedKeys = showUnmappedKeysButton.isSelected
      layoutPanel.repaint()
    })
    editConfigButton.addActionListener(_ => showKeyMapTable())
    clearConfigButton.addActionListener(_ => clearConfig())
    restoreMappingButton.addActionListener(_ => restoreKeyboardMapper())
    restoreMappingButton.setToolTipText("Restore keyboard layout")

    southPanel.add("Center",cardPanel)
    southPanel.add("North",modePanel)
    val statusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    statusPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
    statusPanel.add(statusLabel)
    southPanel.add("South",statusPanel)
    add("South",southPanel)

    checkMode()
  }

  private def clearConfig(): Unit = {
    JOptionPane.showConfirmDialog(this, "Are you sure you want to clear current configuration ?", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
      case JOptionPane.YES_OPTION =>
        val azAuto = JOptionPane.showConfirmDialog(this, "Do you want to auto map A-Z keys (shifted and unshifted) ?", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION => true
          case _ => false
        }
        currentEditMap.clear()
        currentEditKeypadMap.clear()
        currentMapModified = true
        mapperFile = None
        checkMode()
        if (azAuto) {
          for(k <- keys) {
            if (CKey.isLetterKey(k.key,model)) {
              KeyboardMapperStore.parseHostKey(k.key.toString) match {
                case Some(hk) =>
                  currentEditMap += hk -> List(k.key)
                  currentEditMap += hk.copy(shifted = true) -> List(VIRTUAL_SHIFT,k.key)
                case None =>
              }
            }
          }
        }
        applyMapsToKeyboard()
      case _ =>
    }
  }

  private def setMode(newMode:Int): Unit = {
    mode = newMode
    checkMode()
  }

  private def editKey(k:CKey.Value): Unit = {
    val dialog = new JDialog(this.dialog, s"Set host key for C= key $k", true)
    dialog.getContentPane.add(new KeyboardHelper(() => dialog.dispose(), Some(key => {
      KeyboardMapperStore.parseHostKey(key.key) match {
        case Some(hk) =>
          val map = if (key.isNumPad) currentEditKeypadMap else currentEditMap
          key.applyType match {
            case KeyboardHelper.APPLY =>
              map += hk -> List(k)
            case KeyboardHelper.APPLY_UNSHIFTED_SHIFTED =>
              map += hk -> List(k)
              map += hk.copy(shifted = true) -> List(VIRTUAL_SHIFT,k)
            case KeyboardHelper.APPLY_SHIFTED_ONLY =>
              map += hk -> List(VIRTUAL_SHIFT,k)
          }
          currentMapModified = true
          applyMapsToKeyboard()
          checkMode()
        case None =>
      }
    })))
    dialog.pack()
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.setLocationRelativeTo(dialog)
    dialog.setLocationRelativeTo(this.dialog)
    dialog.setVisible(true)
  }

  private def findKey(e:MouseEvent): Option[(KeyPos,Rectangle)] = {
    for (k <- keys) {
      k.contains(e.getX, e.getY, scalex, scaley) match {
        case Some(rec) =>
          return Some((k,rec))
        case None =>
      }
    }
    None
  }

  override def mouseDragged(e: MouseEvent): Unit = {}
  override def mouseMoved(e: MouseEvent): Unit = {
    val oldSelectedRec = selectedRect
    try {
      findKey(e) match {
        case Some((k,rec)) =>
          selectedRect = rec
          selectedKey = k
        case None =>
          selectedRect = null
          selectedKey = null
      }
    }
    finally {
      if (oldSelectedRec != selectedRect) {
        repaint()
        if (selectedKey != null) {
          val hostKeys = keyb.getKeyboardMapper.findHostKeys(selectedKey.key)
          mode match {
            case VIRTUAL_MODE =>
              vmodeHostKeysLabel.setText(hostKeys.map(e => s"${e._1} = ${e._2.mkString(" , ")}").mkString("  "))
            case EDIT_MODE =>
              editModeHostKeysLabel.setText(hostKeys.map(e => s"${e._1} = ${e._2.mkString(" , ")}").mkString("  "))
          }
        }
      }
    }
  }
  override def mouseClicked(e: MouseEvent): Unit = {
    mode match {
      case EDIT_MODE =>
        if (e.getClickCount == 2) {
          findKey(e) match {
            case Some((k, _)) =>
              editKey(k.key)
            case None =>
          }
        }
      case _ =>
    }
  }
  override def mousePressed(e: MouseEvent): Unit = {
    mode match {
      case VIRTUAL_MODE =>
        findKey(e) match {
          case Some((k,_)) =>
            if (e.isShiftDown) {
              if (pinned.contains(k)) {
                pinned -= k
                keyb.releaseKey(k.key)
              }
              else {
                pinned += k
                keyb.pressKey(k.key)
              }
              repaint()
            }
            else keyb.pressKey(k.key)
          case None =>
        }
      case EDIT_MODE =>
    }
  }
  override def mouseReleased(e: MouseEvent): Unit = {
    mode match {
      case VIRTUAL_MODE =>
        findKey(e) match {
          case Some((k, _)) =>
            if (!e.isShiftDown) keyb.releaseKey(k.key)
          case None =>
        }
      case EDIT_MODE =>
    }
  }
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}

  def close(): Boolean = true
}
