package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.vic.Display

import java.awt.{GridBagConstraints, GridBagLayout, Insets}
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

class DisplayEffectPanel(display:Display) extends JPanel with ChangeListener {
  private val rotationSlider = new JSlider(-180,180,0)
  private val angleLabel = new JLabel("+000")

  private def init() : Unit = {
    val flipXCheck = new JCheckBox("Horizontal mirroring")
    val flipYCheck = new JCheckBox("Vertical mirroring")
    rotationSlider.setLabelTable(rotationSlider.createStandardLabels(45))
    rotationSlider.setPaintLabels(true)
    rotationSlider.setPaintTicks(true)
    rotationSlider.addChangeListener(this)

    setLayout(new GridBagLayout)
    add(0,0,flipXCheck,GridBagConstraints.LINE_START)
    add(0,1,flipYCheck,GridBagConstraints.LINE_START)
    add(0,2,new JLabel("Rotation angle:"),GridBagConstraints.LINE_START)
    add(1,2,angleLabel,GridBagConstraints.LINE_START)
    add(2,2,rotationSlider,GridBagConstraints.LINE_START)
    val resetButton = new JButton("Default")
    resetButton.addActionListener(_ => rotationSlider.setValue(0))
    add(3,2,resetButton,GridBagConstraints.LINE_START)

    flipXCheck.addActionListener(_ => display.setFlipXY(flipXCheck.isSelected,flipYCheck.isSelected) )
    flipYCheck.addActionListener(_ => display.setFlipXY(flipXCheck.isSelected,flipYCheck.isSelected) )
  }

  private def add(x:Int,y:Int,comp:JComponent,anchor:Int = GridBagConstraints.CENTER,gw:Int = 1,gh:Int = 1): Unit = {
    val c = new GridBagConstraints
    c.insets = new Insets(5,5,5,5)
    c.gridx = x
    c.gridy = y
    c.fill = GridBagConstraints.NONE
    c.gridwidth = gw
    c.gridheight = gh
    c.anchor = anchor
    add(comp,c)
  }

  def stateChanged(e:ChangeEvent): Unit = /*if (!rotationSlider.getValueIsAdjusting)*/ {
    display.setRotationAngle(rotationSlider.getValue)
    angleLabel.setText("%3d".format(rotationSlider.getValue))
  }
}

object DisplayEffectPanel {
  private val mapDialog : collection.mutable.Map[String,JDialog] = collection.mutable.HashMap.empty

  def createDisplayEffectPanel(parent:JFrame,display:Display,name:String) : JDialog = {
    mapDialog.get(name) match {
      case None =>
        val dialog = new JDialog(parent, s"$name's display effects", false)
        val dep = new DisplayEffectPanel(display)

        dep.init
        dialog.getContentPane.add("Center", dep)
        dialog.pack()
        mapDialog += name -> dialog
        dialog
      case Some(dialog) =>
        dialog
    }
  }
}

