package ucesoft.c64.peripheral.controlport

import java.util.Properties
import net.java.games.input.Controller
import net.java.games.input.ControllerEnvironment
import net.java.games.input.Component
import Joysticks._

class GamePadControlPort(configuration:Properties) extends ControlPort {
  private[this] var controller : Option[Controller] = None
  private[this] val controllerName = configuration.getProperty(CONFIG_CONTROLLER_NAME)
  private[this] val controllerFireName = configuration.getProperty(CONFIF_CONTROLLER_FIRE_BUTTON,"1")
  private[this] var xAxisComponent : Option[Component] = None
  private[this] var yAxisComponent : Option[Component] = None
  private[this] var fireComponent : Option[Component] = None
  
  findController
  
  private def findController {
    val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
    controller = controllers find { c => c.getName == controllerName  && (c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK)}
    controller match {
      case None =>
      case Some(comp) =>        
        xAxisComponent = Option(comp.getComponent(Component.Identifier.Axis.X))
        yAxisComponent = Option(comp.getComponent(Component.Identifier.Axis.Y))
        fireComponent = comp.getComponents find { c => c.getIdentifier.getName == controllerFireName}
        println("Find controller: " + comp.getName + " " + xAxisComponent + " " + yAxisComponent + " " + fireComponent)
    }
  }
  
  protected def read = {
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
          if (xData ==  -1.0f) mask |= 4 // left
          else
          if (xData ==  1.0f) mask |= 8 // right
          val yData = y.getPollData
          if (yData ==  -1.0f) mask |= 1 // up
          else
          if (yData ==  1.0f) mask |= 2 // down
        }
        
      case _ => 
        // controller unplugged
        controller = None
    }
    ~mask & 0xFF
  }
}