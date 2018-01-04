package ucesoft.cbm.util

import java.awt.event.MouseAdapter
import java.awt.Robot
import scala.util.Try
import javax.swing.JComponent
import java.awt.event.MouseEvent
import java.awt.Point
import java.awt.MouseInfo

object MouseCage extends MouseAdapter {
  private val mousePointer = new Point
  private val robot : Robot = Try(new Robot).getOrElse(null)
  private var component : JComponent = _
  
  def x = if (robot != null) mousePointer.x else MouseInfo.getPointerInfo.getLocation.x
  def y = if (robot != null) mousePointer.y else MouseInfo.getPointerInfo.getLocation.y
  
  @inline private def robotMove(p:Point) {
    if (robot != null) robot.mouseMove(p.x, p.y)
  }
  
  private def center : Point = {
    val loc = component.getLocationOnScreen
    new Point(loc.x + component.getWidth / 2,loc.y + component.getHeight / 2)
  }
  
  def enableMouseCageOn(c:JComponent) {
    component = c
    val p = center
    mousePointer.x = p.x
    mousePointer.y = p.y
    val cursor = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    component.setCursor(component.getToolkit.createCustomCursor(cursor,new Point(0, 0),"null"))
    component.addMouseListener(this)
    component.addMouseMotionListener(this)
    robotMove(p)
  }
  
  def disableMouseCage {
    if (component != null) {
      component.removeMouseListener(this)
      component.removeMouseMotionListener(this)
      component.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
    }
  }
  
  override def mouseExited(e:MouseEvent) = robotMove(center)
  
  override def mouseMoved(e:MouseEvent) {
    val compCenter = center
    mousePointer.x += e.getXOnScreen - compCenter.x
    mousePointer.y += e.getYOnScreen - compCenter.y
    robotMove(compCenter)
  }
  
  override def mouseDragged(e:MouseEvent) = mouseMoved(e)  
}