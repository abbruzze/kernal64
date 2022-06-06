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
  final val PRA = 0
  final val PRB = 1
  final val DDRA = 2
  final val DDRB = 3
  final val TALO = 4
  final val TAHI = 5
  final val TBLO = 6
  final val TBHI = 7
  final val TOD_10THS = 8
  final val TOD_SEC = 9
  final val TOD_MIN = 10
  final val TOD_HR = 11
  final val SDR = 12
  final val ICR = 13
  final val CRA = 14
  final val CRB = 15
  // IRQ sources (bit)
  final val IRQ_SRC_ALARM = 4
  final val IRQ_SRC_TA = 1
  final val IRQ_SRC_TB = 2
  final val IRQ_SERIAL = 8
  final val IRQ_FLAG = 16

  final val CIA_MODEL_6526 = 0 // old
  final val CIA_MODEL_8521 = 1 // new
}

/**
 * CIA2 irqAction must be wired with nmiAction of CPU
 */
class CIA(val name:String,
		  val startAddress:Int,
		  portAConnector:Connector,
		  portBConnector:Connector,
		  irqAction:(Boolean) => Unit,
      idleAction:(Boolean) => Unit = null,
		  manualClockTODUpdate:Boolean = false) extends Chip with RAMComponent {
  import CIA._
  
  override lazy val componentID = name
  val isRom = false
  val length = 0x100
  val isActive = true
  val id = ChipID.CIA

  private[this] val timerABRunning = Array(true,true)
  private[this] val timerB = new Timer_B(name,IRQ_SRC_TB,irqHandling _, idle => timerIdleCallBack(1,idle))
  private[this] val timerA = new Timer_A(name,IRQ_SRC_TA,irqHandling _,timerB, idle => timerIdleCallBack(0,idle))
  private[this] val tod = new TOD
  private[this] var icr = 0
  private[this] var sdr,sdrlatch,shiftRegister = 0
  private[this] var sdrLoaded,sdrOut = false
  private[this] var SP = false
  private[this] var serialOUTTrigger : Boolean => Unit = _
  private[this] var sdrIndex = 0
  private[this] var icrMask = 0	// bits 0 - 4
  private[this] var ciaModel = CIA_MODEL_6526
  private[this] var ackCycle = false
  private[this] var irqOnNextClock = false
  private[this] var irqSrcOnNextClock = 0

  private[this] var lastIcrMask = 0
  private[this] var lastIcrMaskClock = 0L

  private[this] val clk = Clock.systemClock

  @inline private def timerIdleCallBack(id:Int,idle:Boolean): Unit = {
    val lastRunning = timerABRunning(0) || timerABRunning(1)
    timerABRunning(id) = !idle
    val nowRunning = timerABRunning(0) || timerABRunning(1)
    if (idleAction != null && lastRunning != nowRunning) idleAction(!nowRunning)
  }
  
  // ========================== TOD ================================================
  
  private class TOD extends CBMComponent {
    val componentID = name + " TOD"
    val componentType = CBMComponentType.CHIP 
    final private[this] val TICK_SUBID = 1
    final private val PAL_TICK = 98990
    final private val NTSC_TICK = 102273

    private[this] var TICK_CYCLES = PAL_TICK // PAL
    
    case class Time(var h:Int,var m:Int,var s:Int,var ts:Int,var am:Boolean) {
      var freezed = false
      def reset(hh:Int,mm:Int,ss:Int,tss:Int,amm:Boolean,f:Boolean) : Unit = {
        h = hh
        m = mm
        s = ss
        ts = tss
        am = amm
        freezed = f
      }
      
      def setFrom(t:Time) : Unit = {
        h = t.h
        m = t.m
        s = t.s
        ts = t.ts
        am = t.am
      }
      
      def tick  : Unit = {
        var sl = s & 0x0F
        var sh = (s & 0xF0) >> 4
        var ml = m & 0x0F
        var mh = (m & 0xF0) >> 4
        var hl = h & 0x0F
        var hh = (h & 0xF0) >> 4

        //tenth seconds
        ts = (ts + 1) & 0x0F
        if (ts == 10) {
          ts = 0
          // seconds
          sl = (sl + 1) & 0x0F
          if (sl == 10) {
            sl = 0
            sh = (sh + 1) & 0x07
            if (sh == 6) {
              sh = 0
              // minutes
              ml = (ml + 1) & 0x0F
              if (ml == 10) {
                ml = 0
                mh = (mh + 1) & 0x07
                if (mh == 6) {
                  mh = 0
                  // hours
                  hl = (hl + 1) & 0x0F
                  if (hh > 0) {
                    if (hl == 2) am ^= true
                    else
                    if (hl == 3) {
                      hl = 1
                      hh = 0
                    }
                  }
                  else
                  if (hl == 10) {
                    hl = 0
                    hh = 1
                  }
                }
              }
            }
          }
        }
        s = sl | (sh << 4)
        m = ml | (mh << 4)
        h = hl | (hh << 4)
      }
      
      def saveState(out:ObjectOutputStream) : Unit = {
        out.writeBoolean(freezed)
        out.writeInt(h)
        out.writeInt(m)
        out.writeInt(s)
        out.writeInt(ts)
        out.writeBoolean(am)
      }
      def loadState(in:ObjectInputStream) : Unit = {
        freezed = in.readBoolean
        h = in.readInt
        m = in.readInt
        s = in.readInt
        ts = in.readInt
        am = in.readBoolean
      }
      
      @inline private def bcd2dec(value:Int) = "" + (((value & 0xF0) >> 4) + '0').toChar + ((value & 0x0F)  + '0').toChar
      override def toString = s"${bcd2dec(h)}:${bcd2dec(m)}:${bcd2dec(s)}:${bcd2dec(ts)} ${if (am) "am" else "pm"} [$freezed]"
    }
    
    private val actualTime = Time(0,0,0,0,true)
    private val latchTime = Time(1,0,0,0,true)
    private val alarmTime = Time(0,0,0,0,true)
    private var resetSync = false
    
    override def getProperties = {
      properties.setProperty("Time",actualTime.toString)
      properties.setProperty("Latch",latchTime.toString)
      properties.setProperty("Alarm",alarmTime.toString)
      properties
    }
    
    def init : Unit = {
      clk.addChangeFrequencyListener(f => {
        f match {
          case clk.PAL_CLOCK_HZ =>
            TICK_CYCLES = PAL_TICK
          case clk.NTSC_CLOCK_HZ =>
            TICK_CYCLES = NTSC_TICK
          case f =>
            TICK_CYCLES = (f / 10).toInt
        }
      })
      reset
    }
    
    def reset  : Unit = {
      actualTime.reset(1,0,0,0,true,true)
      latchTime.reset(1,0,0,0,true,false)
      alarmTime.reset(0,0,0,0,true,false)
      resetSync = false
      reschedule
    }
    
    def tick(cycles:Long) : Unit = { // every 1/10 seconds
      // increment actual time if not freezed
      if (!actualTime.freezed) actualTime.tick
      if (!actualTime.freezed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
      // reschedule tick
      reschedule
    }
    
    private[this] val tickCallback = tick _

    @inline private def reschedule = {
      if (!manualClockTODUpdate) {
        clk.cancel(componentID)
        clk.schedule(new ClockEvent(componentID,clk.currentCycles + TICK_CYCLES,tickCallback,TICK_SUBID))
      }
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
    def writeHour(hour:Int) : Unit = {
      var changed = false
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        val oldH = alarmTime.h
        val oldAM = alarmTime.am
        alarmTime.h = hour & 0x1F
        alarmTime.am = (hour & 0x80) == 0
        changed = oldH != alarmTime.h || oldAM != alarmTime.am
      }
      else {
        actualTime.freezed = true
        val oldH = actualTime.h
        val oldAM = actualTime.am
        actualTime.h = hour & 0x1F
        actualTime.am = (hour & 0x80) == 0
        changed = oldH != actualTime.h || oldAM != actualTime.am
        if (actualTime.h == 0x12) actualTime.am = !actualTime.am        
      }
      if (changed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
      resetSync = true
    }
    def writeMin(min:Int) : Unit = {
      var changed = false
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        val oldM = alarmTime.m
        alarmTime.m = min & 0x7F
        changed = oldM != alarmTime.m
      }
      else {
        val oldM = actualTime.m
        actualTime.m = min & 0x7F
        changed = oldM != actualTime.m
      }
      if (changed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
    }
    def writeSec(sec:Int) : Unit = {
      var changed = false
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        val oldS = alarmTime.s
        alarmTime.s = sec & 0x7F
        changed = oldS != alarmTime.s
      }
      else {
        val oldS = actualTime.s
        actualTime.s = sec & 0x7F
        changed = oldS != actualTime.s
      }
      if (changed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
    }
    def writeTenthSec(tsec:Int) : Unit = {
      var changed = false
      if ((timerB.readCR & 0x80) > 0) { // set alarm
        val oldTS = alarmTime.ts
        alarmTime.ts = tsec & 0x0F
        changed = oldTS != alarmTime.ts
      }
      else {
        val oldTS = actualTime.ts
        actualTime.ts = tsec  & 0x0F
        changed = oldTS != actualTime.ts
        actualTime.freezed = false
        // reschedule tick
        //println("Actual time "+actualTime + "  alarm " + alarmTime + " IRQ " + (actualTime == alarmTime))
      }
      if (resetSync) {
        reschedule
        resetSync = false
      }
      if (changed && actualTime == alarmTime) irqHandling(IRQ_SRC_ALARM)
    }
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      actualTime.saveState(out)
      latchTime.saveState(out)
      alarmTime.saveState(out)
      saveClockEvents(out)
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      actualTime.loadState(in)
      latchTime.loadState(in)
      alarmTime.loadState(in)
      loadClockEvents(in) {
        case (TICK_SUBID,w) =>
          new ClockEvent(componentID,w,tickCallback,TICK_SUBID)        
      }
    }
    protected def allowsStateRestoring : Boolean = true
  }
  
  // ===============================================================================

  def setCIAModel(model:Int): Unit = ciaModel = model
  
  /**
   * Manual clock
   */
  final def clock(updateTOD:Boolean) : Unit = {
    if (irqOnNextClock) {
      irqOnNextClock = false
      setIRQ(irqSrcOnNextClock)
    }
    if (timerABRunning(1)) timerB.clock
    if (timerABRunning(0)) timerA.clock
    if (updateTOD) tod.tick(0)
    ackCycle = false

    if (!timerABRunning(0) && !timerABRunning(1)) idleAction(true)
  }
      
  def init  : Unit = {
    timerA.setSerialCallBack(Some(sendSerial _))
    add(timerB)
    add(timerA)
    add(tod)
  }
  
  def reset  : Unit = {
    icr = 0
    sdr = 0
    icrMask = 0
    sdrIndex = 0
    shiftRegister = 0
    sdrLoaded = false
    sdrOut = false
    SP = false
    timerABRunning(0) = true
    timerABRunning(1) = true
    ackCycle = false
  }
  
  def setFlagLow = {
    irqHandling(IRQ_FLAG)
    idleAction(false)
  }
  
  final def irqHandling(bit:Int) : Unit = {
    // handle TimerB bug for old cias when reading ICR "near" underflow: the bit is not set
    if (!(ciaModel == CIA_MODEL_6526 && bit == IRQ_SRC_TB && ackCycle)) icr |= bit
    if (bit == IRQ_SRC_ALARM || bit == IRQ_SERIAL) setIRQ(bit) else setIRQOnNextClock(bit)
  }

  @inline private def setIRQ(src:Int) = {
    val deltaClock = if (ciaModel == CIA_MODEL_8521) 0 else 1
    val currentIcrMask = if (clk.currentCycles == lastIcrMaskClock + deltaClock) lastIcrMask else icrMask
    if ((currentIcrMask & icr) > 0) {
      icr |= 0x80
      Log.debug(s"${name} is generating IRQ(${src}) icr=${icr}")
      irqAction(true)
    }
  }

  @inline private def setIRQOnNextClock(src:Int): Unit = {
    if (ciaModel == CIA_MODEL_8521) setIRQ(src) // set immediately
    else {
      irqOnNextClock = true
      irqSrcOnNextClock = src
    }
  }
  
  override def getProperties = {
    properties.setProperty("Interrupt control register",Integer.toHexString(icr))
    properties.setProperty("Interrupt mask register",Integer.toHexString(icrMask))
    properties.setProperty("Shift register",Integer.toHexString(shiftRegister))
    properties.setProperty("SDR index",Integer.toHexString(sdrIndex))
    properties.setProperty("SDR out",sdrOut.toString)
    properties.setProperty("Model", if (ciaModel == CIA_MODEL_8521) "8521" else "6526")
    properties.setProperty("Idle",(!timerABRunning(0) && !timerABRunning(1)).toString)
    super.getProperties
  }
  
  @inline private def decodeAddress(address:Int) = address & 0x0F //% 16
	
  final def read(address: Int, chipID: ChipID.ID): Int = decodeAddress(address) match {
    case PRA => portAConnector.read      
    case PRB => 
      var data = portBConnector.read
      if ((timerA.readCR & 0x02) != 0) {
        data &= 0xBF
        val check = if ((timerA.readCR & 0x04) != 0) timerA.getPbToggle else timerA.isStateOut
        if (check) data |= 0x40
      }
      if ((timerB.readCR & 0x02) != 0) {
        data &= 0x7F
        val check = if ((timerB.readCR & 0x04) != 0) timerB.getPbToggle else timerB.isStateOut
        if (check) data |= 0x80
      }
      data
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
      ackCycle = true
      lastIcr & 0x9F	// bit 5 & 6 always 0
    case CRA => timerA.readCR & 0xEE | timerA.getState & 0x01
    case CRB => timerB.readCR & 0xEE | timerB.getState & 0x01
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
      if (sdrLoaded) sdrlatch = value
      else {
        sdr = value
        sdrlatch = -1
      }
      if ((timerA.readCR & 0x40) == 0x40) { // serial out
        sdrLoaded = true
    //println(s"$name SDR=${Integer.toHexString(value)} '${value.toChar}'")
      }
    case ICR =>
      lastIcrMask = icrMask
      lastIcrMaskClock = clk.currentCycles
      val mustSet = (value & 0x80) == 0x80 
      if (mustSet) icrMask |= value & 0x7f else icrMask &= ~value
      
      if ((icrMask & icr & 0x1f) != 0) {
        val delay = if (ciaModel == CIA_MODEL_8521) 1 else 2
        clk.schedule(new ClockEvent(componentID + "_IRQ", clk.currentCycles + delay, _ => setIRQ(-1)))
      }
      Log.debug(s"${name} ICR's value is ${Integer.toBinaryString(value)} => ICR = ${Integer.toBinaryString(icrMask)}")
    case CRA =>
      val oldSerialOut = timerA.readCR & 0x40
      timerA.writeCR(value)
      val serialOut = value & 0x40
      if (oldSerialOut != serialOut) {
        sdrIndex = 0
        //sdrLoaded = false
        //sdrOut = false
      }
    case CRB => timerB.writeCR(value)
  }
  
  @inline private def loadShiftRegister  : Unit = {
    shiftRegister = sdr // load the shift register
    sdrLoaded = false
    sdrOut = true
    //println("Shift register loaded with " + sdr + " latch=" + sdrlatch)
  }
  
  private def sendSerial  : Unit = {
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
            if (sdrlatch != -1) {
              sdr = sdrlatch
              sdrlatch = -1
              sdrLoaded = true
            }
          }
        }
      }
    }
  }
  
  /**
   * Used for serial
   */
  final def serialIN(sp:Boolean) : Unit = {
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
  
  final def setSerialOUT(sot:Boolean => Unit) : Unit = {
    serialOUTTrigger = sot
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(icr)
    out.writeInt(sdr)
    out.writeInt(sdrIndex)
    out.writeInt(icrMask)
    out.writeInt(shiftRegister)
    out.writeBoolean(sdrLoaded)
    out.writeBoolean(sdrOut)
    out.writeBoolean(SP)
    out.writeInt(ciaModel)
    out.writeBoolean(ackCycle)
    out.writeBoolean(irqOnNextClock)
    out.writeInt(irqSrcOnNextClock)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    icr = in.readInt
    sdr = in.readInt
    sdrIndex = in.readInt
    icrMask = in.readInt
    shiftRegister = in.readInt
    sdrLoaded = in.readBoolean
    sdrOut = in.readBoolean
    SP = in.readBoolean
    ciaModel = in.readInt
    ackCycle = in.readBoolean
    irqOnNextClock = in.readBoolean
    irqSrcOnNextClock = in.readInt
  }
  protected def allowsStateRestoring : Boolean = true
}