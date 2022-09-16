package ucesoft.cbm.misc

import ucesoft.cbm.{C128Model, C64Model, CBMComputerModel, VIC20Model}
import ucesoft.cbm.c128.FunctionROMType

import java.awt.{Container, GridBagConstraints, GridBagLayout, Insets}
import java.io.File
import java.util.Properties
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

class ROMPanel(prop:Properties, model:CBMComputerModel, scpu:Boolean = false) extends JPanel {
  import ucesoft.cbm.cpu.ROM._

  private val C64 = 1
  private val C128 = 2
  private val DRIVE = 4
  private val C128_I_F_ROM = 8
  private val C128_E_F_ROM = 16
  private val SCPU = 32
  private val VIC20 = 64
  private case class ROM(label:String,propName:String,romType:Int,var path:Option[String] = None,var item:Option[String] = None) {
    def apply(prop:Properties): Unit = {
      val value = if (!path.isDefined || path.get == "") "" else {
        if (item.isDefined) path.get + "," + item.get else path.get
      }
      prop.setProperty(propName,value)
      ucesoft.cbm.cpu.ROM.reload(propName)
    }
  }

  private val romList = List(
    ROM("SCPU ROM",SCPU64_ROM_PROP,SCPU),
    ROM("C64 Kernal",C64_KERNAL_ROM_PROP,C64),
    ROM("C64 Basic",C64_BASIC_ROM_PROP,C64),
    ROM("C64 Char",C64_CHAR_ROM_PROP,C64|SCPU),
    ROM("C128 Kernal",C128_KERNAL_ROM_PROP,C128),
    ROM("C128 Basic",C128_BASIC_ROM_PROP,C128),
    ROM("C128 Char",C128_CHAR_ROM_PROP,C128),
    ROM("C128 Internal Function",C128_INTERNAL_ROM_PROP,C128_I_F_ROM),
    ROM("C128 External Function",C128_EXTERNAL_ROM_PROP,C128_E_F_ROM),
    ROM("Drive 1541 Kernal",D1541_DOS_ROM_PROP,DRIVE),
    ROM("Drive 1571 Kernal",D1571_DOS_ROM_PROP,DRIVE),
    ROM("Drive 1581 Kernal",D1581_DOS_ROM_PROP,DRIVE),
    ROM("VIC20 Kernal", VIC20_KERNAL_ROM_PROP, VIC20),
    ROM("VIC20 Basic", VIC20_BASIC_ROM_PROP, VIC20),
    ROM("VIC20 Char", VIC20_CHAR_ROM_PROP, VIC20)
  )

  private val romMap : Map[String,ROM] = romList filter { r =>
    /*if (scpu) (r.romType & SCPU) == SCPU || r.romType == DRIVE
    else if (c64Only) r.romType == C64 || r.romType == DRIVE
    else true*/
    model match {
      case C64Model =>
        if (scpu) (r.romType & SCPU) == SCPU || r.romType == DRIVE
        else r.romType == C64 || r.romType == DRIVE
      case C128Model =>
        true
      case VIC20Model =>
        r.romType == VIC20 || r.romType == DRIVE
    }
  } map { r => r.propName -> r } toMap
  private var lastDir = "./"

  def applyUpdates() : Unit = {
    for(rom <- romList) {
      rom.apply(prop)
    }
  }

  private def makePanel(name:String,romType:Int) : JPanel = {
    val roms = romList filter { r => (r.romType & romType) > 0 }

    val p = new JPanel(new GridBagLayout)
    p.setBorder(BorderFactory.createTitledBorder(name))
    for((rom,y) <- roms.zipWithIndex) {
      val tf = new JTextField(30)
      val cb = new JCheckBox(if (rom.romType == C128_I_F_ROM || rom.romType == C128_E_F_ROM) "none" else "default")
      val button = new JButton("Browse ...")
      cb.setSelected(!rom.path.isDefined)
      tf.setEnabled(rom.path.isDefined)
      tf.setText(rom.path.getOrElse(""))
      tf.getDocument.addDocumentListener(new DocumentListener {
        override def removeUpdate(e: DocumentEvent): Unit = rom.path = Some(tf.getText)
        override def insertUpdate(e: DocumentEvent): Unit = rom.path = Some(tf.getText)
        override def changedUpdate(e: DocumentEvent): Unit = rom.path = Some(tf.getText)
      })
      button.setEnabled(rom.path.isDefined)
      cb.addActionListener(_ => {
        tf.setEnabled(!cb.isSelected)
        button.setEnabled(!cb.isSelected)
        tf.setText("")
      })
      button.addActionListener(_ => {
        val fc = new JFileChooser(if (tf.getText.isEmpty) lastDir else new File(tf.getText).getParent)
        fc.setDialogTitle("Choose ROM path")
        fc.showOpenDialog(p) match {
          case JFileChooser.APPROVE_OPTION =>
            tf.setText(fc.getSelectedFile.toString)
            tf.setToolTipText(fc.getSelectedFile.toString)
            lastDir = fc.getSelectedFile.toString
          case _ =>
        }
      })

      add(p,0,y,new JLabel(rom.label))
      if (rom.romType == C128_I_F_ROM) {
        val p1 = new JPanel
        p1.setLayout(new BoxLayout(p1,BoxLayout.Y_AXIS))
        p1.add(cb)
        val combo = new JComboBox(Array(FunctionROMType.NORMAL.toString,FunctionROMType.MEGABIT.toString,FunctionROMType.MAGICDESK128.toString))
        combo.addActionListener(_ => rom.item = Some(FunctionROMType.withName(combo.getSelectedItem.toString).toString) )
        rom.item match {
          case Some(rt) if rt == FunctionROMType.NORMAL.toString =>
            combo.setSelectedIndex(0)
          case Some(rt) if rt == FunctionROMType.MEGABIT.toString =>
            combo.setSelectedIndex(1)
          case Some(rt) if rt == FunctionROMType.MAGICDESK128.toString =>
            combo.setSelectedIndex(2)
          case None =>
            combo.setEnabled(false)
        }
        p1.add(combo)
        cb.addActionListener(_ => {
          combo.setEnabled(!cb.isSelected)
          combo.setSelectedIndex(0)
        })
        add(p,1,y,p1)
      }
      else add(p,1,y,cb)
      add(p,2,y,tf)
      add(p,3,y,button)
    }

    p
  }

  private def add(p:Container,x:Int,y:Int,comp:JComponent): Unit = {
    val c = new GridBagConstraints
    c.insets = new Insets(5,5,5,5)
    c.gridx = x
    c.gridy = y
    c.fill = GridBagConstraints.NONE
    c.gridwidth = 1
    c.gridheight = 1
    p.add(comp,c)
  }

  import scala.jdk.CollectionConverters._
  for(kv <- prop.asScala) {
    romMap get kv._1 match {
      case Some(rom) if !kv._2.isEmpty =>
        if (rom.romType == C128_I_F_ROM) {
          kv._2.split(",") match {
            case Array(p,t) =>
              rom.path = Some(p)
              rom.item = Some(t)
            case _ =>
              rom.path = Some(kv._2)
              rom.item = None
          }
        }
        else rom.path = Some(kv._2)
      case _ =>
    }
  }

  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))
  model match {
    case C64Model =>
      if (!scpu) add(makePanel("Commodore 64",C64))
      else add(makePanel("SCPU",SCPU))
    case C128Model =>
      add(makePanel("Commodore 128",C128 | C128_I_F_ROM | C128_E_F_ROM))
    case VIC20Model =>
      add(makePanel("Commodore VIC 20",VIC20))
  }

  add(makePanel("Drives",DRIVE))
}

object ROMPanel {
  def showROMPanel(parent:JFrame,prop:Properties,model:CBMComputerModel,scpu:Boolean = false,applyCallBack : () => Unit): Unit = {
    val f = new JDialog(parent,"System ROMs",true)
    f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    f.setLocationRelativeTo(parent)

    val contentPane = f.getContentPane
    val romPanel = new ROMPanel(prop,model,scpu)
    contentPane.add("Center",romPanel)
    val buttonPanel = new JPanel
    val okB = new JButton("Apply")
    okB.setToolTipText("Apply changes")
    val cancelB = new JButton("Cancel")
    cancelB.addActionListener(_ => f.dispose() )
    okB.addActionListener(_ => {
      romPanel.applyUpdates
      f.dispose()
      applyCallBack()
    })
    buttonPanel.add(okB)
    buttonPanel.add(cancelB)
    contentPane.add("South",buttonPanel)

    f.pack()
    f.setVisible(true)
  }
}
