package ucesoft.cbm.misc

import ucesoft.cbm.Version
import ucesoft.cbm.expansion.WiC64

import java.awt._
import java.awt.event.{WindowAdapter, WindowEvent}
import java.net.NetworkInterface
import javax.swing._
import javax.swing.text.{Style, StyleConstants, StyleContext}

class WiC64Panel(frame:JFrame,pref:Preferences) extends JPanel with WiC64.WiC64Listener {
  private class Led(colorOn:Color) extends JComponent {
    var on = false
    setPreferredSize(new Dimension(15,15))
    override def paint(g:Graphics) : Unit = {
      val size = getSize()
      if (on) {
        g.setColor(colorOn)
        g.fillOval(0,0,size.width,size.height)
      }
    }
  }

  private val greenLed = new Led(Color.GREEN)
  private val enabledCheck = new JCheckBox("Enabled")
  private val updateCmd = new JCheckBox("Update cmd")
  private val networks = {
    import scala.jdk.CollectionConverters._
    val nw = NetworkInterface.getNetworkInterfaces.asScala.toArray
    val combo = new JComboBox[String](nw.map(_.getName))
    combo.setEditable(false)
    combo
  }
  private val newFirmwareLabel = new JLabel(" ! ")
  private val textPane = new JTextPane()
  private final val COLS = 25
  private var regular,large : Style = _
  private var lastCmd = ""
  private val firmwareStats = {
    val d = Version.DATE
    val t = Version.TIME
    s"${d.substring(6,8)}/${d.substring(4,6)}/${d.substring(0,4)} ${t.substring(0,2)}:${t.substring(2)}:00"
  }
  private val logPanel = new JTextArea(10,30)
  private val logButton = new JToggleButton("Debug log")
  private val logDialog = {
    val d = new JDialog(frame,"WiC64 log panel")
    logPanel.setEditable(false)
    logPanel.setForeground(Color.BLACK)
    val sp = new JScrollPane(logPanel)
    d.getContentPane.add("Center",sp)
    val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val clear = new JButton("Clear")
    southPanel.add(clear)
    d.getContentPane.add("South",southPanel)
    clear.addActionListener(_ => logPanel.setText(""))
    d.pack()
    d.setLocationRelativeTo(frame)
    d.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = logButton.setSelected(false)
    })
    d
  }

  val dialog: JDialog = {
    init()
    val d = new JDialog(frame,"WiC64 panel")
    d.getContentPane.add("Center",this)
    d.pack()
    d.setLocationRelativeTo(frame)
    d.setResizable(false)
    d.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        logDialog.setVisible(false)
        logButton.setSelected(false)
      }
    })
    d.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    d
  }

  def setNetwork(n:String): Unit = {
    import scala.jdk.CollectionConverters._
    val nw = NetworkInterface.getNetworkInterfaces.asScala.toArray
    nw.zipWithIndex.find(z =>
      if (n.isEmpty) z._1.getName.startsWith("wlan") || z._1.getInetAddresses.asScala.map(_.getHostAddress).exists(_.startsWith("192.168."))
      else z._1.getName.startsWith(n)
    ) match {
      case None =>
        WiC64.setNetworkInterface(nw(0))
      case Some((n,i)) =>
        networks.setSelectedIndex(i)
        networks.repaint()
        WiC64.setNetworkInterface(n)
        pref.updateWithoutNotify(Preferences.PREF_WIC64_NETWORK,n.getName)
        update()
    }
  }

  private def init(): Unit = {
    newFirmwareLabel.setForeground(Color.RED)
    newFirmwareLabel.setVisible(false)
    newFirmwareLabel.setBorder(BorderFactory.createLineBorder(Color.RED))
    val font = newFirmwareLabel.getFont
    newFirmwareLabel.setFont(new Font(font.getName,Font.BOLD,font.getSize))
    textPane.setEnabled(false)
    networks.setEnabled(false)
    logButton.setEnabled(false)
    updateCmd.setEnabled(false)
    textPane.setBackground(Color.BLACK)
    textPane.setForeground(Color.WHITE)
    textPane.setEditable(false)

    val doc = textPane.getStyledDocument
    val df = StyleContext.getDefaultStyleContext.getStyle(StyleContext.DEFAULT_STYLE)
    regular = doc.addStyle("regular", df)
    StyleConstants.setFontFamily(regular,"Monospaced")
    StyleConstants.setBold(regular, true)
    StyleConstants.setFontSize(regular, 16)
    large = doc.addStyle("large", regular)
    StyleConstants.setFontSize(large, 24)

    enabledCheck.addActionListener(_ => setWiC64Enabled(enabledCheck.isSelected))
    networks.addActionListener(_ => {
      import scala.jdk.CollectionConverters._
      val nw = NetworkInterface.getNetworkInterfaces.asScala.toArray
      WiC64.setNetworkInterface(nw(networks.getSelectedIndex))
      pref.updateWithoutNotify(Preferences.PREF_WIC64_NETWORK,nw(networks.getSelectedIndex).getName)
      update()
    })

    val resetButton = new JButton("Reset")
    resetButton.addActionListener(_ => WiC64.resetWiC64())

    setLayout(new BorderLayout())
    val northPanel = new JPanel(new BorderLayout())
    var northDummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    northPanel.add("West",northDummyPanel)
    northDummyPanel.add(enabledCheck)
    northDummyPanel.add(updateCmd)
    northDummyPanel.add(resetButton)
    northDummyPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    northPanel.add("East",northDummyPanel)
    northDummyPanel.add(newFirmwareLabel)
    val southPanel = new JPanel(new BorderLayout())
    val netPanel = new JPanel
    netPanel.add(new JLabel("Local network:",SwingConstants.RIGHT))
    netPanel.add(networks)
    southPanel.add(logButton)
    logButton.addActionListener(_ => {
      logDialog.setVisible(logButton.isSelected)
      WiC64.setLogEnabled(logButton.isSelected)
    })
    southPanel.add("West",netPanel)
    val ledPanel = new JPanel
    ledPanel.add(greenLed)
    southPanel.add("East",ledPanel)
    add("Center",textPane)
    add("North",northPanel)
    add("South",southPanel)
    update()
  }

  def setWiC64Enabled(enabled:Boolean): Unit = {
    enabledCheck.setSelected(enabled)
    WiC64.enabled = enabled
    textPane.setEnabled(enabled)
    networks.setEnabled(enabled)
    logButton.setEnabled(enabled)
    updateCmd.setEnabled(enabled)
    enabledCheck.setSelected(enabled)
    pref.updateWithoutNotify(Preferences.PREF_WIC64_ENABLED,enabled)
  }

  private def update(): Unit = {
    val doc = textPane.getStyledDocument
    doc.remove(0,doc.getLength)
    doc.insertString(doc.getLength,WiC64.getIPAddress(),large)
    doc.insertString(doc.getLength,"\n\n",regular)
    doc.insertString(doc.getLength,fill(s"SSID:${WiC64.SSID} -30",COLS) + "\n",regular)
    doc.insertString(doc.getLength,fill(fill(lastCmd,16) + WiC64.getTotalCmdIssued.toHexString.toUpperCase() ,COLS) + "\n",regular)
    doc.insertString(doc.getLength,fill(" WiC64 emulator by",COLS) + "\n",regular)
    doc.insertString(doc.getLength,fill("Kernal64",COLS) + "\n",regular)
    doc.insertString(doc.getLength,fill(firmwareStats,COLS),regular)
  }

  def fill(s:String,size:Int,left:Boolean = false): String = {
    if (s.length < size) {
      if (left) (" " * (size - s.length)) + s else s + (" " * (size - s.length))
    } else s.substring(0,size)
  }

  override def onCMD(cmdDescr: String): Unit = {
    lastCmd = cmdDescr
    if (updateCmd.isSelected) update()
  }

  override def turnGreenLed(on: Boolean): Unit = {
    greenLed.on = on
    greenLed.repaint()
  }

  override def log(info: String): Unit = {
    logPanel.append(info + "\n")
    logPanel.setCaretPosition(logPanel.getText().length())
  }

  override def newFirmwareAvaiilable(ver:Int,current:Int): Unit = {
    newFirmwareLabel.setVisible(true)
    val maj = ver / 10
    val min = ver % 10
    val curMaj = current / 10
    val curMin = current % 10
    newFirmwareLabel.setToolTipText(s"<html>Warning: a new REAL firmware version is available: $maj.$min, current is $curMaj.$curMin</html>")
  }
}
