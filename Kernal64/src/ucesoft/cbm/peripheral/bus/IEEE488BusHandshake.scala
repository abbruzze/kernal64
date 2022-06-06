package ucesoft.cbm.peripheral.bus

import ucesoft.cbm.{CBMComponent, Clock, ClockEvent}

abstract class IEEE488BusHandshake(val name:String, bus: IEEE488Bus) extends CBMComponent {
  import IEEE488Bus._
  import LineType._
  import LineValue._

  override val componentID: String = name

  protected object Role extends Enumeration {
    val IDLE = Value
    val LISTENER = Value
    val TALKER = Value
    val TALKER_READY = Value
  }

  import Role._

  protected var atnLow = false
  protected var eoiLow = false
  protected var role = IDLE
  protected var sendScheduled = false

  protected val TALK_WAIT_ATN_TIMEOUT = 300 // 200 / 2000000
  protected val clk = Clock.systemClock

  protected class DeviceLineListener extends LineListener {
    override def ATNchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = {
      if (newValue == PULLED) {
        bus.pullLine(listener,NDAC)
        atnLow = true
        role = IDLE
        clk.cancel("IEEE488Talk")
      }
      else atnLow = false
    }

    override def EOIchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = eoiLow = newValue == PULLED

    override def NDACchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = {
      if (!atnLow && role == TALKER && newValue == RELEASED) {
        //println(s"NDAC RELEASED by $id")
        bus.releaseLine(this,DAV)
        bus.setDIO(0)
        if (eoiLow) {
          //println("Last byte sent, received")
          bus.releaseLine(listener,EOI)
          role = IDLE
          eoiLow = false
        }
      }
    }

    override def NRFDchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = {
      if (!atnLow && role == TALKER && newValue == RELEASED && bus.getLine(EOI) == RELEASED) {
        //println(s"NRFD released by $id NDAC is ${bus.getLine(NDAC)}")
        //println(s"Waiting $TALK_WAIT_ATN_TIMEOUT cycles to send other data")
        clk.schedule(new ClockEvent("IEEE488Talk",clk.currentCycles + TALK_WAIT_ATN_TIMEOUT,_ => {
          sendScheduledData()
        }))
        sendScheduled = true
      }
    }

    override def DAVchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = {
      if (role != TALKER) {
        newValue match {
          case PULLED => // DAV pulled, data available
            bus.releaseLine(this,NDAC)
            dataAvailable()
          case RELEASED => // DAV released, data not available
            bus.pullLine(this,NDAC)
        }
      }
    }

    override def IFCchanged(id:Long,newValue: IEEE488Bus.LineValue.Value): Unit = reset
  }

  override def reset: Unit = {
    bus.releaseLine(listener,NRFD)
    bus.pullLine(listener,NDAC)
    bus.releaseLine(listener,DAV)
    atnLow = false
    eoiLow = false
    role = IDLE
  }

  override def init: Unit = {
    bus.pullLine(listener,NDAC)
  }

  protected def sendScheduledData(): Unit = {
    //println("Scheduler: sending data")
    if (sendData()) bus.pullLine(listener, DAV)
    sendScheduled = false
  }

  protected def dataAvailable(): Unit = {}
  protected def sendData(): Boolean = false

  protected val listener : DeviceLineListener = new DeviceLineListener

  // BUS Registration
  bus.registerListener(listener)
}
