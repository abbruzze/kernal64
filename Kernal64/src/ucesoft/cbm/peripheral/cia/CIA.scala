package ucesoft.cbm.peripheral.cia

import ucesoft.cbm.ChipID
import ucesoft.cbm.Chip
import ucesoft.cbm.Clock
import ucesoft.cbm.ClockEvent
import ucesoft.cbm.Log
import ucesoft.cbm.peripheral.Connector
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.cpu.RAMComponent
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

object CIA {
  val PRA = 0
  val PRB = 1
  val DDRA = 2
  val DDRB = 3
  val TALO = 4
  val TAHI = 5
  val TBLO = 6
  val TBHI = 7
  val TOD_10THS = 8
  val TOD_SEC = 9
  val TOD_MIN = 10
  val TOD_HR = 11
  val SDR = 12
  val ICR = 13
  val CRA = 14
  val CRB = 15
  
  val IRQ_SRC_TA = "Timer A"
  val IRQ_SRC_TB = "Timer B"
  val IRQ_SRC_ALARM = "ALARM"    
  val IRQ_SERIAL = "SERIAL"
  val IRQ_FLAG = "FLAG"
}

/**
 * CIA2 irqAction must be wired with nmiAction of CPU
 */
class CIA(val name:String,
		  val startAddress:Int,
		  portAConnector:Connector,
		  portBConnector:Connector,
		  irqAction:(Boolean) => Unit,
		  autoClock:Boolean = true) extends Chip with RAMComponent {
  import CIA._
  
  override lazy val componentID = name
  val isRom = false
  val length = 0x100
  val isActive = true
  val id = ChipID.CIA
    
  private[this] val timerB = new CIATimerB2(name,IRQ_SRC_TB,irqHandling _,autoClock)
  private[this] val timerA = new CIATimerA2(name,IRQ_SRC_TA,irqHandling _,autoClock,Some(timerB))
  private[this] val tod = new TOD2//new TOD((timerB.readCR & 0x80) == 0)
  private[this] var icr = 0
  private[this] var sdr,shiftRegister = 0
  private[this] var sdrLoaded,sdrOut = false
  private[this] var SP = false
  private[this] var serialOUTTrigger : Boolean => Unit = _
  private[this] var sdrIndex = 0
  private[this] var icrMask = 0	// bits 0 - 4
  
  // ========================== TOD ================================================
  
  private class TOD2 extends CBMComponent {
    val componentID = name + " TOD"
    val componentType = CBMComponentType.CHIP 
    final private[this] val TICK_SUBID = 1
    
    case class Time(var h:Int,var m:Int,var s:Int,var ts:Int,var am:Boolean) {
      var freezed = false
      def reset(hh:Int,mm:Int,ss:Int,tss:Int,amm:Boolean,f:Boolean) {
        h = hh
        m = mm
        s = ss
        ts = tss
        am = amm
        freezed = f
      }
      
      def setFrom(t:Time) {
        h = t.h
        m = t.m
        s = t.s
        ts = t.ts
        am = t.am
      }
      
      private def incBCD(value: Int) = {
        var c1 = value & 0x0F
        var c2 = (value & 0xF0) >> 4
        if (c1 == 0xF) {
          c1 = 0
          c2 += 1
        }
        else
        if (c1 >= 0xA) {
          c1 += 1
        }
        else
        if (c1 < 9) c1 += 1
        else {
          c1 = 0
          c2 += 1
        }
        (c2 << 4) | c1
      }
      
      
      def tick {
        // ts
        ts = incBCD(ts)
        if (ts == 0x10) {
          ts = 0
          // s
          s = incBCD(s)
          if (s == 0x60) {
            s = 0
            // m
            m = incBCD(m)
            if (m == 0x60) {
              m = 0
              // h
              h = incBCD(h)
              if (h == 0x12) {
                //h = 0
                am = !am
              }
              else h = h % 0x12
            }
          }
        }
      }
      
      def saveState(out:ObjectOutputStream) {
        out.writeBoolean(freezed)
        out.writeInt(h)
        out.writeInt(m)
        out.writeInt(s)
        out.writeInt(ts)
        out.writeBoolean(am)
      }
      def loadState(in:ObjectInputStream) {
        freezed = in.readBoolean
        h = in.readInt
        m = in.readInt
        s = in.readInt
        ts = in.readInt
        am = in.readBoolean
      }
      
      @inline private def bcd2dec(value:Int) = "" + (((value & 0xF0) >> 4) + '0').toChar + ((value & 0x0F)  + '0').toChar
      override def toString = s"${bcd2dec(h)}:${bcd2dec(m)}:${bcd2dec(s)}:${bcd2dec(ts)} [$freezed]"
    }
    
    private val actualTime = Time(0,0,0,0,true)
    private val latchTime = Time(1,0,0,0,true)
    private val alarmTime = Time(0,0,0,0,true)
    
    override def getProperties = {
      properties.setProperty("Time",actualTime.toString)
      properties.setProperty("Latch",latchTime.toString)
      properties.setProperty("Alarm",alarmTime.toString)
      properties
    }
    
    def init = reset
    
    def reset {
      actualTime.reset(1,0,0,0,true,true)
      latchTime.reset(1,0,0,0,true,false)
      alarmTime.reset(0,0,0,0,true,false)
      reschedule
    }
    
    def tick(cycles:Long) { // every 1/10 seconds
      // increment actual time if not freezed
      if (!actualTime.freezed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
      if (!actualTime.freezed) actualTime.tick      
      // reschedule tick
      reschedule
    }
    
    private[this] val tickCallback = tick _
    
    @inline private def reschedule = {
      val clk = Clock.systemClock
      clk.cancel(componentID)
      clk.schedule(new ClockEvent(componentID,Clock.systemClock.currentCycles + 98524,tickCallback,TICK_SUBID))
    }
    
    def readHour = {
      val time = if (!latchTime.freezed) {
        latchTime.freezed = true
        latchTime.setFrom(actualTime)
        latchTime
      } else latchTime
      time.h | (if (time.am) 0 else 0x80)
    }
    def readMin = if (latchTime.freezed) latchTime.m else actualTime.m
    def readSec = if (latchTime.freezed) latchTime.s else actualTime.s
    def readTenthSec = {
      if (!latchTime.freezed) actualTime.ts
      else {
        latchTime.freezed = false
        latchTime.ts
      }
    }
    def writeHour(hour:Int) {
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        alarmTime.h = hour & 0x7F
        alarmTime.am = (hour & 0x80) == 0        
      }
      else {
        actualTime.freezed = true
        actualTime.h = hour & 0x7F
        actualTime.am = (hour & 0x80) == 0
        if (actualTime.h == 0x12) actualTime.am = !actualTime.am        
      }
    }
    def writeMin(min:Int) {
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        alarmTime.m = min & 0x7F
      }
      else 
      actualTime.m = min & 0x7F    
    }
    def writeSec(sec:Int) {
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        alarmTime.s = sec & 0x7F
      }
      else 
      actualTime.s = sec & 0x7F      
    }
    def writeTenthSec(tsec:Int) {
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        alarmTime.ts = tsec & 0x0F        
      }
      else {
        actualTime.ts = tsec  & 0x0F
        actualTime.freezed = false
        if (actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
        // reschedule tick
        reschedule
      }
      //if (actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      actualTime.saveState(out)
      latchTime.saveState(out)
      alarmTime.saveState(out)
      saveClockEvents(out)
    }
    protected def loadState(in:ObjectInputStream) {
      actualTime.loadState(in)
      latchTime.loadState(in)
      alarmTime.loadState(in)
      loadClockEvents(in) {
        case (TICK_SUBID,w) =>
          new ClockEvent(componentID,w,tickCallback,TICK_SUBID)        
      }
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
  
  // ===============================================================================
  
  /**
   * Manual clock
   */
  final def clock {
    timerA.clock
    timerB.clock
  }
      
  def init {
    timerA.setSerialCallBack(Some(sendSerial _))
    add(timerB)
    add(timerA)
    add(tod)
  }
  
  def reset {
    icr = 0
    sdr = 0
    icrMask = 0
    sdrIndex = 0
  }
  
  def setFlagLow = irqHandling(IRQ_FLAG)
  
  final def irqHandling(src:String) {
    val bit = src match {
      case IRQ_SRC_ALARM => 4   
      case IRQ_SRC_TA => 1
      case IRQ_SRC_TB => 2
      case IRQ_SERIAL => 8
      case IRQ_FLAG => 16
      case _ => 0
    }
    icr |= bit //0x80 | bit
    if ((icrMask & bit) > 0) {
      icr |= 0x80
      Log.debug(s"${name} is generating IRQ(${src}) icr=${icr}")
      irqAction(true)
    }
  }
  
  override def getProperties = {
    properties.setProperty("Interrupt control register",Integer.toHexString(icr))
    properties.setProperty("Interrupt mask register",Integer.toHexString(icrMask))
    super.getProperties
  }
  
  @inline private def decodeAddress(address:Int) = address & 0x0F //% 16
	
  final def read(address: Int, chipID: ChipID.ID): Int = decodeAddress(address) match {
    case PRA => portAConnector.read      
    case PRB => 
      var portB = portBConnector.read
      if (timerA.timerUnderflowOnPortB) {
        timerA.toggleMode match {
          case true => 
            if (timerA.flipFlop) portB |= 0x40 else portB &= 0xBF
          case false =>
            if (timerA.toggleValue) portB |= 0x40 else portB &= 0xBF
        }
      }
      if (timerB.timerUnderflowOnPortB) {
        timerB.toggleMode match {
          case true => 
            if (timerB.flipFlop) portB |= 0x80 else portB &= 0x7F
          case false =>
            if (timerB.toggleValue) portB |= 0x80 else portB &= 0x7F
        }
      }
      portB
    case DDRA => portAConnector.ddr
    case DDRB => portBConnector.ddr
    // timer A
    case TALO => timerA.readLo
    case TAHI => timerA.readHi
    // timer B
    case TBLO => timerB.readLo
    case TBHI => timerB.readHi
    // tod
    case TOD_10THS => tod.readTenthSec
    case TOD_SEC => tod.readSec
    case TOD_MIN => tod.readMin
    case TOD_HR => tod.readHour
    case SDR => sdr
    case ICR =>
      val lastIcr = icr
      icr = 0
      irqAction(false)
      lastIcr & 0x9F	// bit 5 & 6 always 0      
    case CRA => timerA.readCR
    case CRB => timerB.readCR
  }
  final def write(address: Int, value: Int, chipID: ChipID.ID) = decodeAddress(address) match {
    case PRA => 
      portAConnector.write(value)
      Log.debug(s"${name} set PRA to ${Integer.toBinaryString(value)}")
    case PRB => 
      portBConnector.write(value)
      Log.debug(s"${name} set PRB to ${Integer.toBinaryString(value)}")
    case DDRA => 
      portAConnector.ddr = value
      Log.debug(s"${name} set DDRA to ${Integer.toBinaryString(value)}")
    case DDRB => 
      portBConnector.ddr = value
      Log.debug(s"${name} set DDRB to ${Integer.toBinaryString(value)}")
    // timer A
    case TALO => timerA.writeLo(value)
    case TAHI => timerA.writeHi(value)
    // timer B
    case TBLO => timerB.writeLo(value)
    case TBHI => timerB.writeHi(value)
    // tod
    case TOD_10THS => tod.writeTenthSec(value)
    case TOD_SEC => tod.writeSec(value)
    case TOD_MIN => tod.writeMin(value)
    case TOD_HR => tod.writeHour(value)
    case SDR => // serial register
      sdr = value
      if ((timerA.readCR & 0x40) == 0x40) { // serial out
        sdrLoaded = true        
        //println(s"$name SDR=${Integer.toHexString(value)} '${value.toChar}'")
      }
    case ICR =>
      val mustSet = (value & 0x80) == 0x80 
      if (mustSet) icrMask |= value & 0x7f else icrMask &= ~value
      
      if ((icrMask & icr & 0x1f) != 0) {
        icr |= 0x80
        irqAction(true)
      }
      Log.debug(s"${name} ICR's value is ${Integer.toBinaryString(value)} => ICR = ${Integer.toBinaryString(icrMask)}")
    case CRA =>
      timerA.writeCR(value)
      if (!timerA.isStarted) {
        sdrIndex = 0
        sdrLoaded = false
        sdrOut = false
      }
    case CRB => timerB.writeCR(value)
  }
  
  @inline private def loadShiftRegister {
    shiftRegister = sdr // load the shift register
    sdrLoaded = false
    sdrOut = true
    //println("Shift register loaded with " + sdr)
  }
  
  private def sendSerial {
    if ((timerA.readCR & 0x40) == 0x40) { // serial out
      if (sdrIndex == 0 && sdrLoaded) loadShiftRegister
      if (sdrOut) {
        sdrIndex += 1
        if ((sdrIndex & 1) == 0) {
          //println("Sending serial " + sdrIndex + " at rate " + timerA.getLatch)
          val bit = (shiftRegister & 0x80) > 0
          shiftRegister = (shiftRegister << 1) & 0xFF
          if (serialOUTTrigger != null) serialOUTTrigger(bit)
          if (sdrIndex == 16) {
            irqHandling(IRQ_SERIAL)
            sdrIndex = 0
            sdrOut = false
          }
        }
      }
    }
  }
  
  /**
   * Used for serial
   */
  final def serialIN(sp:Boolean) {
    // only on rising edge
    if ((timerA.readCR & 0x40) == 0) { // serial input mode
      SP = sp
      shiftRegister <<= 1
      if (SP) shiftRegister |= 0x01 else shiftRegister &= 0xFE
      sdrIndex += 1
      if (sdrIndex == 8) {
        sdrIndex = 0
        irqHandling(IRQ_SERIAL)
        sdr = shiftRegister & 0xFF
        //println(s"$name Received=${Integer.toHexString(sdr)} '${sdr.toChar}'")
      }
    }
  }
  
  final def setSerialOUT(sot:Boolean => Unit) {
    serialOUTTrigger = sot
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) {
    out.writeInt(icr)
    out.writeInt(sdr)
    out.writeInt(sdrIndex)
    out.writeInt(icrMask)
  }
  protected def loadState(in:ObjectInputStream) {
    icr = in.readInt
    sdr = in.readInt
    sdrIndex = in.readInt
    icrMask = in.readInt
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}