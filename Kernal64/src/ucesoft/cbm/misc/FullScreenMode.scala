package ucesoft.cbm.misc

import javax.swing.JFrame
import javax.swing.JComponent
import java.awt.GraphicsEnvironment
import javax.swing.JOptionPane
import java.awt.Color
import java.awt.Dimension
import java.awt.event.KeyListener
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.ImageIcon
import java.awt.event.MouseListener

object FullScreenMode {
  def goFullScreen(frame:JFrame,component:JComponent,width:Int,height:Int,mouseListener:MouseListener,keyListeners:KeyListener*) {
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    val device = env.getScreenDevices()(0)
    val conf = device.getDefaultConfiguration
    if (device.isFullScreenSupported) {
        val window = new JFrame(conf)
        window.getContentPane.setLayout(null)
        window.getContentPane.add(component)
        window.getContentPane.setBackground(Color.BLACK)
        frame.setVisible(false)
        window.setUndecorated(true)   
        window.setIconImage(new ImageIcon(getClass.getResource("/resources/commodore.png")).getImage)
        device.setFullScreenWindow(window)
        val size = conf.getBounds
        val windowWidthFactor = size.width / width.toDouble
        val windowHeightFactor = size.height / height.toDouble
        val factor = math.min(windowWidthFactor,windowHeightFactor)
        println("Factor=" + windowWidthFactor + "," + windowHeightFactor)
        component.setSize(new Dimension((width * factor).toInt,(height * factor).toInt))
        val vicSize = component.getSize()
        val winSize = window.getSize()
        component.setLocation((winSize.width - vicSize.width) / 2,(winSize.height - vicSize.height) / 2)
        component.invalidate
        window.validate
        window.addMouseListener(mouseListener)
        for(kl <- keyListeners) window.addKeyListener(kl)        
        window.addKeyListener(new KeyAdapter {
          override def keyPressed(e:KeyEvent) {
            e.getKeyCode match {
              // mouse
              case java.awt.event.KeyEvent.VK_ENTER if e.isAltDown =>
                window.dispose
                frame.setVisible(true)
                frame.getContentPane.add("Center",component)
                frame.pack
              case _ =>
            }
          }
        })
    }
    else {
      JOptionPane.showMessageDialog(frame,"Your display device does not support full screen mode","Full Screen Mode",JOptionPane.ERROR_MESSAGE)
    }
  }
}