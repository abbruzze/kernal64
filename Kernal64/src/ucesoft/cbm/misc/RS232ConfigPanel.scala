package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.rs232.{BridgeRS232, RS232}

import java.awt.{Color, Component, FlowLayout}
import javax.swing._

object RS232ConfigPanel {
  private var AVAILABLE_RS232 : Array[RS232] = _
  private var activeRs232 : Option[RS232] = None
  private var configPanel : JDialog = _
  private var selectingRs232 : Option[RS232] = None
  private val connectedToLabel = new JLabel()
  private val RS232StatusPanel = new RS232StatusPanel {
    override def connectedTo(address: String): Unit = {
      super.connectedTo(address)
      connectedToLabel.setText(address)
    }

    override def disconnected(): Unit = {
      super.disconnected()
      connectedToLabel.setText("")
    }
  }

  def RS232ConfigDialog : JDialog = configPanel

  def registerAvailableRS232Drivers(parent:JFrame,rs232Drivers:Array[RS232]): Unit = {
    AVAILABLE_RS232 = rs232Drivers
    configPanel = initConfigPanel(parent)
    BridgeRS232.setRS232Listener(RS232StatusPanel)
  }

  private def initConfigPanel(parent:JFrame) : JDialog = {
    val dialog = new JDialog(parent,"RS232 Configuration panel")
    dialog.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
    val pane = dialog.getContentPane
    val group = new ButtonGroup
    val conf = new JTextField(30)
    val applyButton = new JButton("Apply")
    conf.addActionListener(_ => applySelected(dialog,conf.getText) )
    applyButton.addActionListener(_ => applySelected(dialog,conf.getText) )
    conf.setEnabled(false)
    val radios = AVAILABLE_RS232 map { r =>
      val radio = new JRadioButton(r.toString)
      group.add(radio)
      radio.addActionListener(_ => {
        if (radio.isSelected) {
          selectingRs232 = Some(r)
          conf.setToolTipText(r.getDescription)
          conf.setEnabled(true)
        }
      } )
      radio.setAlignmentX(Component.LEFT_ALIGNMENT)
      radio
    }
    val radioPanel = new JPanel
    radioPanel.setBorder(BorderFactory.createTitledBorder("RS-232 drivers"))
    radioPanel.setLayout(new BoxLayout(radioPanel,BoxLayout.Y_AXIS))
    val noneRadio = new JRadioButton("None")
    noneRadio.setSelected(true)
    group.add(noneRadio)
    noneRadio.addActionListener(_ => {
      conf.setToolTipText("")
      conf.setEnabled(false)
      selectingRs232 = None
    })
    radioPanel.add(noneRadio)
    for(r <- radios) radioPanel.add(r)
    val confPanel = new JPanel
    confPanel.add(new JLabel("Connection string:"))
    confPanel.add(conf)
    confPanel.add(applyButton)
    val connPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    connectedToLabel.setForeground(Color.BLUE)
    connPanel.add(new JLabel("Connected to:"))
    connPanel.add(connectedToLabel)
    connPanel.setAlignmentX(Component.LEFT_ALIGNMENT)

    confPanel.setAlignmentX(Component.LEFT_ALIGNMENT)
    radioPanel.add(confPanel)
    radioPanel.add(connPanel)
    pane.add("Center",radioPanel)
    pane.add("South",RS232StatusPanel)
    RS232StatusPanel.setVisible(true)
    val handshakePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    handshakePanel.setBorder(BorderFactory.createTitledBorder("RS-232 handshacking"))
    val hscb = new JCheckBox("RS-232 flow control enabled")
    hscb.addActionListener(_ => BridgeRS232.setFlowControlEnabled(hscb.isSelected) )
    handshakePanel.add(hscb)
    pane.add("North",handshakePanel)
    dialog.pack()
    dialog.setResizable(false)
    dialog.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)

    dialog
  }

  private def applySelected(parent:JDialog,conf:String): Unit = {
    selectingRs232 match {
      case None => // disable RS-232
        activeRs232 match {
          case Some(ars232) =>
            ars232.setEnabled(false)
            activeRs232 = None
            BridgeRS232.unsetRS232()
            JOptionPane.showMessageDialog(parent,"RS-232 disabled", "RS-232 configuration",JOptionPane.INFORMATION_MESSAGE)
          case None =>
        }
      case Some(ars232) =>
        try {
          ars232.setConfiguration(conf)
          activeRs232 foreach {
            _.setEnabled(false)
          }
          activeRs232 = Some(ars232)
          BridgeRS232.setRS232(ars232)
          BridgeRS232.setEnabled(true)
          JOptionPane.showMessageDialog(parent,"RS-232 enabled with new configuration", "RS-232 configuration",JOptionPane.INFORMATION_MESSAGE)
        }
        catch {
          case t:Throwable =>
            JOptionPane.showMessageDialog(parent,t.toString, "RS-232 configuration error",JOptionPane.ERROR_MESSAGE)
        }
    }
  }
}
