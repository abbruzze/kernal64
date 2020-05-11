package ucesoft.cbm.scpu

import java.awt.{Color, FlowLayout}
import java.awt.event.{MouseAdapter, MouseEvent}

import javax.swing.JPanel
import ucesoft.cbm.c128.LabelledLed

class MMUStatusPanel extends JPanel{
  private val nativeMode = new LabelledLed("native",true)
  private val m20 = new LabelledLed("20Mhz",true)
  private val jiffydos = new LabelledLed("JIFFYDOS",true,Color.ORANGE)
  private val sys_m1 = new LabelledLed("SPEED",true,Color.ORANGE)
  private val simmUsage = new LabelledLed("SIMM usage",true,Color.YELLOW,Color.DARK_GRAY,2)

  private var jiffydosAction : Boolean => Unit = _
  private var sys_m1Action : Boolean => Unit = _

  setLayout(new FlowLayout(FlowLayout.LEFT,2,0))
  add(nativeMode)
  add(m20)
  add(jiffydos)
  add(sys_m1)
  add(simmUsage)

  simmUsage.perc = 0.0
  simmUsage.on = true

  jiffydos.setToolTipText("Click to set/reset Jiffy DOS")
  sys_m1.setToolTipText("Click to set/reset system 1Mhz")

  jiffydos.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      jiffydos.on = !jiffydos.on
      jiffydosAction(jiffydos.on)
      jiffydos.repaint()
    }
  })

  sys_m1.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      sys_m1.on = !sys_m1.on
      sys_m1Action(sys_m1.on)
      sys_m1.repaint()
    }
  })

  def setNativeMode(native:Boolean) : Unit = { nativeMode.on = native ; nativeMode.repaint() }
  def setCPU20Mhz(_20Mhz:Boolean) : Unit = { m20.on = _20Mhz ; m20.repaint() }
  def setSIMMUsage(usage:Float) : Unit = {
    simmUsage.perc = usage
    simmUsage.repaint()
    simmUsage.setToolTipText("%3.2f%%".format(usage * 100))
  }
  def setJiffyDosAction(action:Boolean => Unit) : Unit = jiffydosAction = action
  def enableJiffyDOS(enabled:Boolean) : Unit = { jiffydos.on = enabled ; jiffydos.repaint() }
  def setSys1MhzAction(action:Boolean => Unit) : Unit = sys_m1Action = action
}
