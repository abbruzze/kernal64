package ucesoft.cbm.expansion.vic20

import ucesoft.cbm.expansion.vic20.VIC20ExpansionPort.{Signals, VIC20ExpansionPortStateHandler}
import ucesoft.cbm.misc.{AMF29F040, Preferences}

import java.awt.{BorderLayout, FlowLayout, GridLayout}
import java.io.{BufferedOutputStream, File, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import javax.swing._

object VIC20Ultimem extends VIC20ExpansionPortStateHandler {
  private val _512K = 524_288
  private val _1M = _512K * 2
  private val _8M = _1M * 8

  private var switch0 = false
  private var imageWriteBack = false
  private var lastEnabledROMPath = ""

  def make(romPath:String,
           signals:Signals): Either[Throwable,VIC20Ultimem] = {
    try {
      val f = new File(romPath)
      if (f.length() != _512K && f.length() != _8M) throw new IllegalArgumentException("Ultimem ROM length must be 512K or 8M")
      val rom = java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF)
      Right(new VIC20Ultimem(rom, signals))
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
    imageWriteBack = in.readBoolean()
    val tmpFile = File.createTempFile("ultimem","")
    tmpFile.deleteOnExit()
    val romBytes = rom.map(_.toByte)
    val out = new FileOutputStream(tmpFile)
    out.write(romBytes)
    out.close()
    signals.pref.updateWithoutNotify(PREF_VIC20_ULTIMEM,tmpFile.toString)
    new VIC20Ultimem(rom, signals)
  }

  override def save(cart: VIC20ExpansionPort, out: ObjectOutputStream): Unit = {
    out.writeObject(cart.asInstanceOf[VIC20Ultimem].rom)
    out.writeBoolean(imageWriteBack)
    cart.save(out)
  }

  private def saveRomBackOnImage(rom:Array[Int]): Unit = {
    try {
      val out = new BufferedOutputStream(new FileOutputStream(lastEnabledROMPath))
      var i = 0
      while (i < rom.length) {
        out.write(rom(i))
        i += 1
      }
      out.close()
    }
    catch {
      case e:Exception =>
        println(s"ULTIMEM: Cannot write back to image: $e")
    }
  }

  def showConfPanel(parent: JFrame, pref: Preferences,enableHandler:(Boolean,String) => Unit): Unit = {
    val panel = new VIC20UltimemPanel(parent, pref,enableHandler)
    panel.dialog.setVisible(true)
  }

  private class VIC20UltimemPanel(parent: JFrame, pref: Preferences,enableHandler:(Boolean,String) => Unit) extends JPanel {

    import ucesoft.cbm.misc.Preferences._

    val enabledCheck = new JCheckBox("Enabled")
    val switch0Check = new JCheckBox("Switch 0")
    val writeCheck = new JCheckBox("Image write back (when cart is detached)")
    val browseButton = new JButton("Browse")
    val applyButton = new JButton("Apply")
    val cancelButton = new JButton("Cancel")
    val fileText = new JTextField(40)
    val label = new JLabel("ROM file path")

    val dialog = new JDialog(parent, "Ultimem cartridge configuration", true)

    init()

    def checkFile(): Boolean = {
      val f = new File(fileText.getText())
      if (!f.exists()) {
        JOptionPane.showMessageDialog(this, s"ROM file $f does not exists", "Ultimem configuration error", JOptionPane.ERROR_MESSAGE)
        false
      }
      else if (f.length() != _512K && f.length() != _8M) {
        JOptionPane.showMessageDialog(this, s"ROM file $f length must be 512K or 1M", "Ultimem configuration error", JOptionPane.ERROR_MESSAGE)
        false
      }
      else true
    }

    def init(): Unit = {
      setLayout(new BorderLayout())
      val romFile = pref.get[String](PREF_VIC20_ULTIMEM).get.value
      fileText.setText(romFile)
      enabledCheck.setSelected(!romFile.isEmpty)
      val wasEnabled = !romFile.isEmpty
      val wasText = romFile
      val panel = new JPanel(new GridLayout(4, 1))
      add("Center", panel)
      panel.add(enabledCheck)
      panel.add(switch0Check)
      panel.add(writeCheck)
      var dummy = new JPanel(new FlowLayout(FlowLayout.LEFT))
      dummy.add(label)
      dummy.add(fileText)
      dummy.add(browseButton)
      panel.add(dummy)
      dummy = new JPanel(new FlowLayout(FlowLayout.CENTER))
      dummy.add(applyButton)
      dummy.add(cancelButton)
      add("South", dummy)

      switch0Check.setSelected(switch0)
      writeCheck.setSelected(imageWriteBack)
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

      cancelButton.addActionListener(_ => dialog.dispose() )

      enabledCheck.addActionListener(_ => {
        fileText.setEnabled(enabledCheck.isSelected)
        browseButton.setEnabled(enabledCheck.isSelected)
        label.setEnabled(enabledCheck.isSelected)
        if (!enabledCheck.isSelected) {
          pref.updateWithoutNotify(PREF_VIC20_ULTIMEM,"")
        }
      })

      applyButton.addActionListener(_ => {
        if (checkFile()) {
          dialog.dispose()
          switch0 = switch0Check.isSelected
          imageWriteBack = writeCheck.isSelected
          val enabled = enabledCheck.isSelected
          if (enabled) {
            pref.updateWithoutNotify(PREF_VIC20_ULTIMEM,fileText.getText)
            if (wasText != fileText.getText) enableHandler(true,fileText.getText)
            lastEnabledROMPath = fileText.getText
          }
          else {
            pref.updateWithoutNotify(PREF_VIC20_ULTIMEM,"")
            if (wasEnabled) enableHandler(false,"")
          }
        }
      })
    }
  }
}

class VIC20Ultimem(val rom:Array[Int],
                   override val signals:Signals) extends VIC20ExpansionPort(signals) {
  import VIC20Ultimem._

  override val portType = VIC20ExpansionPort.VICExpansionPortType.ULTIMEM
  override val componentID: String = "VIC20Ultimem"
  protected final val ram =  Array.ofDim[Int](if (rom.length == _512K) _512K else _1M)
  protected val flash = new AMF29F040(if (rom.length == _512K) AMF29F040.AMF29F040TypeB else AMF29F040.AMF29F040Type064)
  protected final val ramSizeMask = ram.length - 1
  protected final val romSizeMask = rom.length - 1
  protected final val regs = Array.ofDim[Int](0x10)
  protected var unhideSequence = 0

  private final val NO_RAM_NO_ROM = 0
  private final val FLASH_ROM     = 1
  private final val RAM_RO        = 2
  private final val RAM_RW        = 3

  private final val BANK_RAM123_REG = 4
  private final val BANK_IO_REG     = 6
  private final val BANK_BLK1_REG   = 8
  private final val BANK_BLK2_REG   = 10
  private final val BANK_BLK3_REG   = 12
  private final val BANK_BLK5_REG   = 14

  @inline private def ram123_config(): Int = regs(1) & 3
  @inline private def io2_config(): Int = (regs(1) >> 2) & 3
  @inline private def io3_config(): Int = (regs(1) >> 4) & 3
  @inline private def blk1_config(): Int = regs(2) & 3
  @inline private def blk2_config(): Int = (regs(2) >> 2) & 3
  @inline private def blk3_config(): Int = (regs(2) >> 4) & 3
  @inline private def blk5_config(): Int = (regs(2) >> 6) & 3

  @inline private def set_ram123_config(v:Int): Unit = regs(1) = (regs(1) & 0xFC) | v & 3
  @inline private def set_io2_config(v:Int): Unit = regs(1) = (regs(1) & 0xF3) | (v & 3) << 2
  @inline private def set_io3_config(v:Int): Unit = regs(1) = (regs(1) & 0xCF) | (v & 3) << 4
  @inline private def set_blk1_config(v:Int): Unit = regs(2) = (regs(2) & 0xFC) | v & 3
  @inline private def set_blk2_config(v:Int): Unit = regs(2) = (regs(2) & 0xF3) | (v & 3) << 2
  @inline private def set_blk3_config(v:Int): Unit = regs(2) = (regs(2) & 0xCF) | (v & 3) << 4
  @inline private def set_blk5_config(v:Int): Unit = regs(2) = (regs(2) & 0x3F) | (v & 3) << 6

  reset()
  flash.setROMBank(rom)

  override def eject(): Unit = {
    import ucesoft.cbm.misc.Preferences._
    if (imageWriteBack && flash.hasBeenWritten()) saveRomBackOnImage(rom)
    signals.pref.update(PREF_VIC20_ULTIMEM, "")
  }

  final override def reset(): Unit = {
    if ((regs(0) & 0x40) == 0) {
      if (!switch0) set_blk5_config(FLASH_ROM) else set_blk5_config(NO_RAM_NO_ROM)
      set_ram123_config(NO_RAM_NO_ROM)
      set_io2_config(NO_RAM_NO_ROM)
      set_io3_config(NO_RAM_NO_ROM)
      set_blk1_config(NO_RAM_NO_ROM)
      set_blk2_config(NO_RAM_NO_ROM)
      set_blk3_config(NO_RAM_NO_ROM)

      regs(BANK_RAM123_REG) = 1
      regs(BANK_IO_REG) = 2
      regs(BANK_BLK1_REG) = 3
      regs(BANK_BLK2_REG) = 4
      regs(BANK_BLK3_REG) = 5
      regs(BANK_BLK5_REG) = 0
    }
    unhideSequence = 0
    flash.reset()
  }

  override def init(): Unit = {
    reset()
  }

  override def hardReset(): Unit = {
    java.util.Arrays.fill(regs,0)
    java.util.Arrays.fill(ram,0)
    reset()
  }

  @inline private def bankAddress(bank_reg:Int): Int = regs(bank_reg) << 13 | regs(bank_reg + 1) << 21

  protected def readBlock(address: Int, config: Int, bank_reg: Int): Option[Int] = {
    config match {
      case NO_RAM_NO_ROM =>
        None
      case RAM_RW | RAM_RO =>
        Some(ram((bankAddress(bank_reg) | (address & 0x1FFF)) & ramSizeMask))
      case FLASH_ROM =>
        Some(flash.read((bankAddress(bank_reg) | (address & 0x1FFF)) & romSizeMask))
    }
  }

  override def read(address: Int): Option[Int] = {
    if ((regs(0) & 0x80) > 0 && address == 0x9F55 && unhideSequence == 0) {
      unhideSequence += 1
      return None
    }
    else if ((regs(0) & 0x80) > 0 && address == 0x9FAA && unhideSequence == 1) {
      unhideSequence += 1
      return None
    }
    else if ((regs(0) & 0x80) > 0 && address == 0x9F01 && unhideSequence == 2) {
      unhideSequence = 0
      regs(0) &= 0x7F // unhide
      return None
    }
    else if (address >= 0x9FF0 && address < 0xA000 && (regs(0) & 0x80) == 0) return Some(readReg(address))

    if (address < 0x400) return None
    if (address < 0x1000) return readBlock(address,ram123_config(), BANK_RAM123_REG)
    else if (address >= 0x2000 && address < 0x4000) return readBlock(address, blk1_config(), BANK_BLK1_REG)
    else if (address >= 0x4000 && address < 0x6000) return readBlock(address, blk2_config(), BANK_BLK2_REG)
    else if (address >= 0x6000 && address < 0x8000) return readBlock(address, blk3_config(), BANK_BLK3_REG)
    else if (address >= 0xA000 && address < 0xC000) return readBlock(address, blk5_config(), BANK_BLK5_REG)
    else if (address >= 0x9800 && address < 0x9C00) return readBlock(address, io2_config(), BANK_IO_REG)
    else if (address >= 0x9C00 && address < 0xA000) return readBlock(address, io3_config(), BANK_IO_REG)

    None
  }

  protected def readReg(address:Int): Int = {
    unhideSequence = 0
    address & 0xF match {
      case 0 =>
        regs(0) | 4 | (if (switch0) 0 else 2)
      case 3 =>
        (if (rom.length == _512K) 2 else 1) | 1 << 4 // ID
      case reg =>
        regs(reg)
    }
  }

  protected def writeReg(address:Int,value:Int): Unit = {
    val r = address & 0xF
    regs(r) = value
    //println(s"Write reg $r = $value")

    if (r == 0) {
      if ((value & 0x40) > 0) signals.resetHandler()
    }
  }

  protected def writeBlock(address:Int,value:Int,config:Int,bank_reg:Int): Boolean = {
    config match {
      case NO_RAM_NO_ROM =>
        false
      case RAM_RW =>
        ram((bankAddress(bank_reg) | (address & 0x1FFF)) & ramSizeMask) = value
        true
      case FLASH_ROM =>
        flash.write((bankAddress(bank_reg) | (address & 0x1FFF)) & romSizeMask,value)
        true
      case RAM_RO =>
        true
    }
  }

  override def write(address: Int, value: Int): Boolean = {
    if (address >= 0x9FF0 && address < 0xA000 && (regs(0) & 0x80) == 0) {
      writeReg(address,value)
      return true
    }

    if (address < 0x400) return false
    if (address < 0x1000)                           return writeBlock(address,value,ram123_config(),BANK_RAM123_REG)
    else if (address >= 0x2000 && address < 0x4000) return writeBlock(address,value,blk1_config(),BANK_BLK1_REG)
    else if (address >= 0x4000 && address < 0x6000) return writeBlock(address,value,blk2_config(),BANK_BLK2_REG)
    else if (address >= 0x6000 && address < 0x8000) return writeBlock(address,value,blk3_config(),BANK_BLK3_REG)
    else if (address >= 0xA000 && address < 0xC000) return writeBlock(address,value,blk5_config(),BANK_BLK5_REG)
    else if (address >= 0x9800 && address < 0x9C00) return writeBlock(address,value,io2_config(),BANK_IO_REG)
    else if (address >= 0x9C00 && address < 0xA000) return writeBlock(address,value,io3_config(),BANK_IO_REG)

    false
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    // rom is saved by the handler
    out.writeObject(ram)
    out.writeObject(regs)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    // rom is loaded by the handler
    loadMemory(ram,in)
    loadMemory(regs,in)
  }
}
