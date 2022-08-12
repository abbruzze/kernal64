package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.{C1541Mems, Drive, DriveType, ParallelCable}

import java.awt.{BorderLayout, Component, GridLayout}
import java.io.File
import javax.swing._

object DrivesConfigPanel {
  val ALL_DRIVES_ALLOWED = Map(
    DriveType._1541 -> true,
    DriveType._1571 -> true,
    DriveType._1581 -> true,
    DriveType._8050 -> true
  )
  val ALL_IEC_DRIVES_ALLOWED = Map(
    DriveType._1541 -> true,
    DriveType._1571 -> true,
    DriveType._1581 -> true,
    DriveType._8050 -> false
  )
  val ALL_IEEE488_DRIVES_ALLOWED = Map(
    DriveType._1541 -> false,
    DriveType._1571 -> false,
    DriveType._1581 -> false,
    DriveType._8050 -> true
  )
  private var drives : Array[Drive] = _
  private var configPanel : JDialog = _
  private var changeDrive : (Int,DriveType.Value) => Unit = _
  private var enableDrive : (Int,Boolean) => Unit = _
  private var selectDisk : (Int,Boolean) => Unit = _
  private var attachDisk : (Int,File,Boolean) => Unit = _
  private var allowedDriveTypes : Map[DriveType.Value,Boolean] = _

  def registerDrives(parent:JFrame,
                     drives:Array[_ <: Drive],
                     changeDrive: (Int,DriveType.Value) => Unit,
                     enableDrive: (Int,Boolean) => Unit,
                     selectDisk: (Int,Boolean) => Unit,
                     attachDisk: (Int,File,Boolean) => Unit,
                     initialDrivesEnabled : Array[Boolean],
                     allowedDriveTypes:Map[DriveType.Value,Boolean] = ALL_DRIVES_ALLOWED) : Unit = {
    this.drives = drives.toArray
    this.allowedDriveTypes = allowedDriveTypes
    configPanel = initConfigPanel(parent,initialDrivesEnabled)
    this.changeDrive = changeDrive
    this.enableDrive = enableDrive
    this.attachDisk = attachDisk
    this.selectDisk = selectDisk
  }

  def getDriveConfigDialog : JDialog = configPanel

  private def initConfigPanel(parent:JFrame,initialDrivesEnabled:Array[Boolean]) : JDialog = {
    val dialog = new JDialog(parent,"Drives Configuration")
    val tabPane = new JTabbedPane
    dialog.getContentPane.add("Center",tabPane)

    for((drive,id) <- drives.zipWithIndex) {
      var toBeDisabled : List[JComponent] = Nil

      val drivePanel = new JPanel(new BorderLayout)
      val panel = new JPanel(new GridLayout(2,1))
      drivePanel.add("Center",panel)
      tabPane.addTab(s"Drive ${id + 8}",drivePanel)
      // 1541 exp.
      val _1541ParallelCableCB = new JCheckBox("1541 parallel cable")
      _1541ParallelCableCB.setAlignmentX(Component.LEFT_ALIGNMENT)
      toBeDisabled = _1541ParallelCableCB :: toBeDisabled
      _1541ParallelCableCB.addActionListener(_ => ParallelCable.enabled = _1541ParallelCableCB.isSelected )
      val _1541RamExpCombo = new JComboBox[String](Array("None","$2000-$3FFF","$4000-$5FFF","$6000-$7FFF","$8000-$9FFF","$A000-$BFFF"))
      _1541RamExpCombo.setAlignmentX(Component.LEFT_ALIGNMENT)
      toBeDisabled = _1541RamExpCombo :: toBeDisabled
      val _1541ExpMemory : Array[C1541Mems.EXP_RAM] = Array(C1541Mems.RAM_EXP_2000,C1541Mems.RAM_EXP_4000,C1541Mems.RAM_EXP_6000,C1541Mems.RAM_EXP_8000,C1541Mems.RAM_EXP_A000)
      _1541RamExpCombo.addActionListener(_ => _1541RamExpCombo.getSelectedIndex match {
        case 0 => for(m <- _1541ExpMemory) m.isActive = false
        case i => _1541ExpMemory(i).isActive = true
      })
      // types
      val driveTypePanel = new JPanel
      driveTypePanel.setLayout(new BoxLayout(driveTypePanel,BoxLayout.Y_AXIS))
      driveTypePanel.setBorder(BorderFactory.createTitledBorder("Type"))
      val group = new ButtonGroup
      if (allowedDriveTypes(DriveType._1541)) {
        val _1541CB = new JRadioButton("1541")
        toBeDisabled = _1541CB :: toBeDisabled
        _1541CB.setAlignmentX(Component.LEFT_ALIGNMENT)
        group.add(_1541CB)
        _1541CB.setSelected(drive.driveType == DriveType._1541)
        _1541ParallelCableCB.setEnabled(_1541CB.isSelected)
        _1541RamExpCombo.setEnabled(_1541CB.isSelected)
        _1541CB.addActionListener(_ => {
          changeDrive(id, DriveType._1541)
          _1541ParallelCableCB.setEnabled(true)
          _1541RamExpCombo.setEnabled(true)
        })
        driveTypePanel.add(_1541CB)
      }
      if (allowedDriveTypes(DriveType._1571)) {
        val _1571CB = new JRadioButton("1571")
        toBeDisabled = _1571CB :: toBeDisabled
        _1571CB.setAlignmentX(Component.LEFT_ALIGNMENT)
        group.add(_1571CB)
        _1571CB.setSelected(drive.driveType == DriveType._1571)
        _1571CB.addActionListener(_ => {
          changeDrive(id, DriveType._1571)
          _1541ParallelCableCB.setEnabled(false)
          _1541RamExpCombo.setEnabled(false)
        })
        driveTypePanel.add(_1571CB)
      }
      if (allowedDriveTypes(DriveType._1581)) {
        val _1581CB = new JRadioButton("1581")
        toBeDisabled = _1581CB :: toBeDisabled
        _1581CB.setAlignmentX(Component.LEFT_ALIGNMENT)
        group.add(_1581CB)
        _1581CB.setSelected(drive.driveType == DriveType._1581)
        _1581CB.addActionListener(_ => {
          changeDrive(id, DriveType._1581)
          _1541ParallelCableCB.setEnabled(false)
          _1541RamExpCombo.setEnabled(false)
        })
        driveTypePanel.add(_1581CB)
      }
      if (allowedDriveTypes(DriveType._8050)) {
        val _8050CB = new JRadioButton("8050")
        toBeDisabled = _8050CB :: toBeDisabled
        _8050CB.setAlignmentX(Component.LEFT_ALIGNMENT)
        group.add(_8050CB)
        _8050CB.setSelected(drive.driveType == DriveType._8050)
        _8050CB.addActionListener(_ => {
          changeDrive(id, DriveType._8050)
          _1541ParallelCableCB.setEnabled(false)
          _1541RamExpCombo.setEnabled(false)
        })
        driveTypePanel.add(_8050CB)
      }
      panel.add(driveTypePanel)
      // general
      val generalPanel = new JPanel
      generalPanel.setBorder(BorderFactory.createTitledBorder("General"))
      generalPanel.setLayout(new BoxLayout(generalPanel,BoxLayout.Y_AXIS))

      val enabledCB = new JCheckBox("Enabled")
      enabledCB.setAlignmentX(Component.LEFT_ALIGNMENT)
      enabledCB.setSelected(initialDrivesEnabled(id))
      driveTypePanel.setEnabled(initialDrivesEnabled(id))
      enabledCB.addActionListener(_ => {
        enableDrive(id,enabledCB.isSelected)
        for(c <- toBeDisabled) c.setEnabled(enabledCB.isSelected)
      } )
      generalPanel.add(enabledCB)
      val roCB = new JCheckBox("Read only")
      toBeDisabled = roCB :: toBeDisabled
      roCB.setAlignmentX(Component.LEFT_ALIGNMENT)
      roCB.setSelected(drive.isReadOnly)
      roCB.addActionListener(_ => drive.setReadOnly(roCB.isSelected) )
      generalPanel.add(roCB)
      val goSleepCB = new JCheckBox("Can go sleeping")
      toBeDisabled = goSleepCB :: toBeDisabled
      goSleepCB.setAlignmentX(Component.LEFT_ALIGNMENT)
      goSleepCB.setSelected(drive.canGoSleeping)
      goSleepCB.addActionListener(_ => drive.setCanSleep(goSleepCB.isSelected) )
      generalPanel.add(goSleepCB)
      val maxSpeedCB = new JCheckBox("Max speed")
      toBeDisabled = maxSpeedCB :: toBeDisabled
      maxSpeedCB.setAlignmentX(Component.LEFT_ALIGNMENT)
      maxSpeedCB.setSelected(drive.getSpeedHz == drive.MAX_SPEED_HZ)
      maxSpeedCB.addActionListener(_ => if (maxSpeedCB.isSelected) drive.setSpeedHz(drive.MAX_SPEED_HZ) else drive.setSpeedHz(drive.MIN_SPEED_HZ) )
      generalPanel.add(maxSpeedCB)

      if (id == 0 && allowedDriveTypes(DriveType._1541)) {
        generalPanel.add(_1541ParallelCableCB)
        val _1541RamExpLabel = new JLabel("1541 RAM Expansion")
        _1541RamExpLabel.setAlignmentX(Component.LEFT_ALIGNMENT)
        generalPanel.add(_1541RamExpLabel)
        generalPanel.add(_1541RamExpCombo)
      }
      panel.add(generalPanel)

      val loadPanel = new JPanel
      loadPanel.setBorder(BorderFactory.createTitledBorder("Attach or Drag & Drop"))
      val autorunCB = new JCheckBox("autorun")
      toBeDisabled = autorunCB :: toBeDisabled
      loadPanel.add(autorunCB)
      val attachButton = new JButton("Attach disk ...")
      toBeDisabled = attachButton :: toBeDisabled
      loadPanel.add(attachButton)
      attachButton.addActionListener( _ => selectDisk(id,autorunCB.isSelected) )
      loadPanel.setTransferHandler(new DNDHandler(handleDND(id,autorunCB.isSelected,_),Some(() => drives(id).driveType)))
      drivePanel.add("South",loadPanel)

      for(c <- toBeDisabled) c.setEnabled(initialDrivesEnabled(id))
    }
    dialog.setResizable(false)
    dialog.pack()
    dialog.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)

    dialog
  }

  private def handleDND(id:Int,autorun: => Boolean,file:File): Unit = {
    attachDisk(id,file,autorun)
  }
}
