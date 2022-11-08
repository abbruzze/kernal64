package ucesoft.cbm.peripheral.controlport

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.cbm.Log
import ucesoft.cbm.peripheral.controlport.Joysticks._

import java.util.Properties

object GamePadControlPort {
  private var controllers : Array[Controller] = Array()

  private def discoverControllers(): Unit = {
    val thread = new Thread {
      override def run(): Unit = {
        System.setProperty("jinput.loglevel","SEVERE")
        controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
        Log.info(s"JInput controllers discovery terminated [${controllers.length}]")
      }
    }
    thread.start()
    thread.join(1000)
  }

  def getControllers(): Array[Controller] = controllers
}

class GamePadControlPort(configuration:Properties) extends ControlPort {
  private[this] var controller : Option[Controller] = None
  private[this] var controllerName = ""
  private[this] var controllerFireName = ""
  private[this] var xAxisComponent : Option[Component] = None
  private[this] var yAxisComponent : Option[Component] = None
  private[this] var fireComponent : Option[Component] = None
  private[this] val dirThreshold = 0.5f

  GamePadControlPort.discoverControllers()
  findController
  
  def findController() : Unit = {
    controllerName = configuration.getProperty(CONFIG_CONTROLLER_NAME)
    controllerFireName = configuration.getProperty(CONFIG_CONTROLLER_FIRE_BUTTON,"1")
    val controllers = GamePadControlPort.getControllers()
    controller = controllers find { c => c.getName == controllerName  && (c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK)}
    controller match {
      case None =>
      case Some(comp) =>        
        xAxisComponent = Option(comp.getComponent(Component.Identifier.Axis.X))
        yAxisComponent = Option(comp.getComponent(Component.Identifier.Axis.Y))
        fireComponent = comp.getComponents find { c => c.getIdentifier.getName == controllerFireName}
        //println("Find controller: " + comp.getName + " " + xAxisComponent + " " + yAxisComponent + " " + fireComponent)
    }
  }
  
  protected def read : Int = {
    var mask = 0
    controller match {
      case None =>
        if (controllerName != null) findController
      case Some(c) if c.poll =>
        for(x <- xAxisComponent;
        	y <- yAxisComponent;
        	f <- fireComponent) {
          if (f.getPollData != 0.0f) mask |= 16
          val xData = x.getPollData
          if (xData < -dirThreshold) mask |= 4 // left
          else
          if (xData > dirThreshold) mask |= 8 // right
          val yData = y.getPollData
          if (yData < -dirThreshold) mask |= 1 // up
          else
          if (yData > dirThreshold) mask |= 2 // down
        }
        
      case _ => 
        // controller unplugged
        controller = None
    }
    ~mask & 0xFF
  }
}