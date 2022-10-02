package ucesoft.cbm.expansion

import ucesoft.cbm.ChipID
import ucesoft.cbm.ChipID.ID
import ucesoft.cbm.expansion.VIC20ExpansionPort.{Signals, VIC20ExpansionPortStateHandler}
import ucesoft.cbm.misc.Preferences
import ucesoft.cbm.peripheral.bus.IEEE488Bus
import ucesoft.cbm.peripheral.drive.VIA

import java.awt.{BorderLayout, FlowLayout, GridLayout}
import java.io.{File, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import javax.swing._

object VIC201112IEEE488 extends VIC20ExpansionPortStateHandler {

  def showConfPanel(parent: JFrame, pref: Preferences, enableHandler: (Boolean, String) => Unit): Unit = {
    val panel = new VIC201112IEEE488Panel(parent, pref, enableHandler)
    panel.dialog.setVisible(true)
  }

  def make(romPath: String,
           signals:Signals): Either[Throwable, VIC201112IEEE488] = {
    try {
      val f = new File(romPath)
      if (f.length() != 2048) throw new IllegalArgumentException("VIC 1112-IEEE488 ROM length must be 2K")
      val rom = java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
      Right(new VIC201112IEEE488(rom, signals))
    }
    catch {
      case io: Throwable =>
        Left(io)
    }
  }

  override def load(in: ObjectInputStream,
                    signals:Signals): VIC20ExpansionPort = {
    import Preferences._
    val rom = in.readObject().asInstanceOf[Array[Int]]
    val tmpFile = File.createTempFile("vic1112", "")
    tmpFile.deleteOnExit()
    val romBytes = rom.map(_.toByte)
    val out = new FileOutputStream(tmpFile)
    out.write(romBytes)
    out.close()
    signals.pref.updateWithoutNotify(PREF_IEEE488_ROM, tmpFile.toString)
    new VIC201112IEEE488(rom, signals)
  }

  override def save(cart: VIC20ExpansionPort, out: ObjectOutputStream): Unit = {
    out.writeObject(cart.asInstanceOf[VIC201112IEEE488].rom)
    cart.save(out)
  }

  private class VIC201112IEEE488Panel(parent: JFrame, pref: Preferences, enableHandler: (Boolean, String) => Unit) extends JPanel {

    import ucesoft.cbm.misc.Preferences._

    val enabledCheck = new JCheckBox("Enabled")
    val browseButton = new JButton("Browse")
    val applyButton = new JButton("Apply")
    val cancelButton = new JButton("Cancel")
    val fileText = new JTextField(40)
    val label = new JLabel("ROM file path")

    val dialog = new JDialog(parent, "VIC 1112-IEEE488 cartridge configuration", true)

    init()

    def checkFile(): Boolean = {
      val f = new File(fileText.getText())
      if (!f.exists()) {
        JOptionPane.showMessageDialog(this, s"ROM file $f does not exists", "VIC 1112-IEEE488 configuration error", JOptionPane.ERROR_MESSAGE)
        false
      }
      else if (f.length() != 2048) {
        JOptionPane.showMessageDialog(this, s"ROM file $f length must be 2K", "VIC 1112-IEEE488 configuration error", JOptionPane.ERROR_MESSAGE)
        false
      }
      else true
    }

    def init(): Unit = {
      setLayout(new BorderLayout())
      val romFile = pref.get[String](PREF_IEEE488_ROM).get.value
      fileText.setText(romFile)
      enabledCheck.setSelected(!romFile.isEmpty)
      val wasEnabled = !romFile.isEmpty
      val wasText = romFile
      val panel = new JPanel(new GridLayout(3, 1))
      add("Center", panel)
      panel.add(enabledCheck)
      var dummy = new JPanel(new FlowLayout(FlowLayout.LEFT))
      dummy.add(label)
      dummy.add(fileText)
      dummy.add(browseButton)
      panel.add(dummy)
      dummy = new JPanel(new FlowLayout(FlowLayout.CENTER))
      dummy.add(applyButton)
      dummy.add(cancelButton)
      add("South", dummy)

      fileText.setEnabled(enabledCheck.isSelected)
      browseButton.setEnabled(enabledCheck.isSelected)
      label.setEnabled(enabledCheck.isSelected)

      dialog.getContentPane.add("Center", this)
      dialog.pack()
      dialog.setResizable(false)
      dialog.setLocationRelativeTo(parent)

      browseButton.addActionListener(_ => {
        val fc = new JFileChooser
        fc.showOpenDialog(this) match {
          case JFileChooser.APPROVE_OPTION =>
            fileText.setText(fc.getSelectedFile.toString)
          case _ =>
        }
      })

      cancelButton.addActionListener(_ => dialog.dispose())

      enabledCheck.addActionListener(_ => {
        fileText.setEnabled(enabledCheck.isSelected)
        browseButton.setEnabled(enabledCheck.isSelected)
        label.setEnabled(enabledCheck.isSelected)
        if (!enabledCheck.isSelected) {
          pref.updateWithoutNotify(PREF_IEEE488_ROM, "")
        }
      })

      applyButton.addActionListener(_ => {
        if (checkFile()) {
          dialog.dispose()
          val enabled = enabledCheck.isSelected
          if (enabled) {
            pref.updateWithoutNotify(PREF_IEEE488_ROM, fileText.getText)
            if (wasText != fileText.getText) enableHandler(true, fileText.getText)
          }
          else {
            pref.updateWithoutNotify(PREF_IEEE488_ROM, "")
            if (wasEnabled) enableHandler(false, "")
          }
        }
      })
    }
  }
}

class VIC201112IEEE488(val rom:Array[Int],
                       override val signals:Signals) extends VIC20ExpansionPort(signals) {
  import IEEE488Bus.LineType._
  import IEEE488Bus.LineValue._
  import IEEE488Bus._

  override val portType = VIC20ExpansionPort.VICExpansionPortType.IEEE488
  override val componentID: String = "VIC201112IEEE488"
  override val needsClock = true
  private val bus = signals.ieee488Bus

  private val busMaster = new LineListener {
    override val isController = true
    bus.registerListener(this)
  }
  private var viaU4IRQ, viaU5IRQ = false

  private val viaU4 = new VIA("VIA_U4",0x9800,low => { viaU4IRQ = low ; checkIRQ() }) {
    override def read(address: Int, chipID: ID): Int = {
      address & 0x0F match {
        case PA | PA2 =>
          super.read(address,chipID)
          0xFF
        case PB =>
          super.read(address,chipID)
          var byte = 0xFF
          if (bus.getLine(ATN) == PULLED) byte -= 0x80
          if (bus.getLine(NDAC) == PULLED) byte -= 0x40
          if (bus.getLine(NRFD) == PULLED) byte -= 0x20
          if (bus.getLine(DAV) == PULLED) byte -= 0x10
          if (bus.getLine(EOI) == PULLED) byte -= 0x8
          (byte & ~regs(DDRB)) | (regs(PB) & regs(DDRB))
        case _ =>
          super.read(address,chipID)
      }
    }

    override def write(address: Int, value: Int, chipID: ChipID.ID): Unit = {
      address & 0x0F match {
        case PA | PA2 =>
          super.write(address,value,chipID)
        case PB =>
          super.write(address,value,chipID)
          if ((value & 0x1) == 0) bus.pullLine(busMaster,DAV) else bus.releaseLine(busMaster,DAV)
          if ((value & 0x2) == 0) bus.pullLine(busMaster,NRFD) else bus.releaseLine(busMaster,NRFD)
          if ((value & 0x4) == 0) bus.pullLine(busMaster,NDAC) else bus.releaseLine(busMaster,NDAC)
        case _ =>
          super.write(address,value, chipID)
      }
    }
  }
  private val viaU5 = new VIA("VIA_U5",0x9810,low => { viaU5IRQ = low ; checkIRQ() }) {
    override def read(address: Int, chipID: ID): Int = {
      address & 0x0F match {
        case PA | PA2 =>
          super.read(address, chipID)
          0xFF
        case PB =>
          super.read(address, chipID)
          ((bus.getDIO() ^ 0xFF) & ~regs(DDRB)) | (regs(PB) & regs(DDRB))
        case _ =>
          super.read(address, chipID)
      }
    }

    override def write(address: Int, value: Int, chipID: ChipID.ID): Unit = {
      address & 0x0F match {
        case PA | PA2 =>
          super.write(address,value, chipID)
          bus.setDIO(value)
        case PB =>
          super.write(address,value, chipID)
        case _ =>
          super.write(address, value, chipID)
      }
    }

    override def CB2Out(state: Boolean): Unit = {
      if (!state) bus.pullLine(busMaster,EOI) else bus.releaseLine(busMaster,EOI)
    }

    override def CA2Out(state: Boolean): Unit = {
      if (!state) bus.pullLine(busMaster,ATN) else bus.releaseLine(busMaster,ATN)
    }
  }

  protected def checkIRQ(): Unit = signals.irqHandler(viaU4IRQ || viaU5IRQ)

  override def clock(cycles:Long): Unit = {
    viaU4.clock(cycles)
    viaU5.clock(cycles)
  }

  override def read(address: Int): Option[Int] = {
    if (address >= 0xB000 && address < 0xB800) Some(rom(address & 0x7FF))
    else if (address >= 0x9800 && address < 0x9810) Some(viaU4.read(address))
    else if (address >= 0x9810 && address < 0x9820) Some(viaU5.read(address))
    else None
  }

  override def write(address: Int, value: Int): Boolean = {
    if (address >= 0x9800 && address < 0x9810) {
      viaU4.write(address,value)
      true
    }
    else if (address >= 0x9810 && address < 0x9820) {
      viaU5.write(address,value)
      true
    }
    else false
  }

  override def eject(): Unit = {
    import ucesoft.cbm.misc.Preferences._
    signals.pref.update(PREF_IEEE488_ROM, "")
  }
}
