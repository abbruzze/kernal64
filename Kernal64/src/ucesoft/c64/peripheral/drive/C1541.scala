package ucesoft.c64.peripheral.drive

import ucesoft.c64.peripheral.bus.IECBus
import ucesoft.c64.Log
import ucesoft.c64.cpu.CPU6510
import ucesoft.c64.ChipID
import ucesoft.c64.Clock
import ucesoft.c64.trace.TraceListener
import ucesoft.c64.formats.D64
import ucesoft.c64.C64ComponentType
import ucesoft.c64.C64Component
import ucesoft.c64.cpu.CPU6510_CE
import ucesoft.c64.trace.BreakType
import java.io.PrintWriter
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

class C1541(val jackID: Int, bus: IECBus, ledListener: DriveLedListener) extends TraceListener with Drive {
  val componentID = "C1541 Disk Drive " + jackID
  override val MIN_SPEED_HZ = 985248
  override val MAX_SPEED_HZ = 1000000
  
  private[this] val LOAD_ROUTINE = 0xD7B4
  private[this] val FORMAT_ROUTINE = 0xC8C6
  private[this] val FORMAT_ROUTINE_OK = 0xC8EF
  private[this] val FORMAT_ROUTINE_NOK = 0xC8E8
  private[this] val WAIT_LOOP_ROUTINE = 0xEC9B //0xEBFF//0xEC9B
  private[this] val WAIT_CYCLES_FOR_STOPPING = 2000000
  private[this] val GO_SLEEPING_MESSAGE_CYCLES = 3000000
  private[this] var CYCLE_ADJ = 0.0 //(MAX_SPEED_MHZ - MIN_SPEED_MHZ) / MIN_SPEED_MHZ.toDouble
  private[this] var currentSpeedHz = MIN_SPEED_HZ
  private[this] var cycleFrac = 0.0
  private[this] val mem = new C1541Mems.C1541_RAM
  private[this] var cpu = CPU6510.make(mem,true,ChipID.CPU_1541)
  private[this] var cpuExact = cpu.isExact
  private[this] val clk = Clock.systemClock
  private[this] val viaBus = new VIAIECBus(jackID, bus, IRQSwitcher.viaBusIRQ _, () => awake)
  private[this] val viaDisk = new VIADiskControl(cpu, IRQSwitcher.viaDiskIRQ _, ledListener)
  private[this] var cpuWaitUntil = 0L
  private[this] var running = true
  private[this] var awakeCycles = 0L
  private[this] var goSleepingCycles = 0L
  private[this] var tracing = false
  private[this] var canSleep = true
  private[this] var useTRAPFormat = false
  
  def getFloppy = viaDisk.getFloppy

  def setDriveReader(driveReader: Floppy,emulateInserting:Boolean) = {
    useTRAPFormat = !driveReader.isFormattable
    viaDisk.setDriveReader(driveReader,emulateInserting)
  }
  override def setActive(active: Boolean) = {
    viaBus.setEnabled(active)
    running = active
    isRunningListener(active)
  }
  override def setCanSleep(canSleep: Boolean) {
    this.canSleep = canSleep
    awake
  }
  private def awake {
    running = true
    isRunningListener(true)
    viaBus.setActive(true)
    viaDisk.setActive(true)
    awakeCycles = clk.currentCycles
    viaDisk.awake
  }
  
  def disconnect {
    bus.unregisterListener(viaBus)
  }
  
  override def setReadOnly(readOnly:Boolean) = viaDisk.setReadOnly(readOnly)
  
  override def setSpeedHz(speed:Int) {
    currentSpeedHz = speed
    CYCLE_ADJ = (speed - MIN_SPEED_HZ) / MIN_SPEED_HZ.toDouble
  }
  
  override def getSpeedHz = currentSpeedHz
  
  override def getProperties = {
    properties.setProperty("Speed Hz",currentSpeedHz.toString)
    properties.setProperty("Cycle adjustment",CYCLE_ADJ.toString)
    properties.setProperty("Running", running.toString)
    val channels = mem.getChannelsState
    for (i <- 0 to 15) {
      val opened = (channels & (1 << i)) > 0
      properties.setProperty("Channel " + i, if (opened) "open" else "inactive")
    }
    properties
  }

  def init {
    Log.info("Initializing C1541...")
    add(mem)
    add(cpu)
    add(IRQSwitcher)
    mem.addBridge(viaBus)
    mem.addBridge(viaDisk)
    if (ledListener != null) ledListener.turnOn
  }

  def reset {
    running = true
    isRunningListener(true)
    awakeCycles = 0
    goSleepingCycles = 0
    cycleFrac = 0.0
    cpuWaitUntil = 0
    if (ledListener != null) ledListener.turnOn
  }
  
  override def changeCPU(cycleExact:Boolean) {
    val oldCpu = cpu
    cpu = CPU6510.make(mem,cycleExact,ChipID.CPU_1541)
    viaDisk.cpu = cpu
    cpu.initComponent
    cpuExact = cycleExact
    change(oldCpu,cpu)
    reset
  }

  private object IRQSwitcher extends C64Component {
    val componentID = "IRQ Switcher (VIA1,VIA2)"
    val componentType = C64ComponentType.INTERNAL

    private[this] var viaBusIRQLow = false
    private[this] var viaDiskIRQLow = false

    private def handleIRQ = {
      Log.debug(s"Handling IRQ viaBusIRQ=${viaBusIRQLow} viaDiskIRQ=${viaDiskIRQLow}")
      cpu.irqRequest(viaBusIRQLow || viaDiskIRQLow)
    }

    override def getProperties = {
      properties.setProperty("ViaBus IRQ", viaBusIRQLow.toString)
      properties.setProperty("ViaDisk IRQ", viaDiskIRQLow.toString)

      properties
    }

    final def viaBusIRQ(low: Boolean) {
      Log.debug("VIABus setting IRQ as " + low)
      viaBusIRQLow = low
      handleIRQ
    }
    final def viaDiskIRQ(low: Boolean) {
      Log.debug("VIADisk setting IRQ as " + low)
      viaDiskIRQLow = low
      handleIRQ
    }

    def init {}

    def reset {
      viaBusIRQLow = false
      viaDiskIRQLow = false
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(viaBusIRQLow)
      out.writeBoolean(viaDiskIRQLow)
    }
    protected def loadState(in:ObjectInputStream) {
      viaBusIRQLow = in.readBoolean
      viaDiskIRQLow = in.readBoolean
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }

  def getMem = mem

  @inline private def checkPC(cycles: Long) {
    val pc = cpu.getPC
    if (pc == LOAD_ROUTINE) setFilename
    else 
    if (pc == FORMAT_ROUTINE && useTRAPFormat) {
      setFilename
      if (viaDisk.formatDisk) cpu.jmpTo(FORMAT_ROUTINE_OK) else cpu.jmpTo(FORMAT_ROUTINE_NOK)
    } 
    else 
    if (pc == WAIT_LOOP_ROUTINE && canSleep && !mem.isChannelActive && !viaDisk.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
      running = false      
      viaBus.setActive(false)
      viaDisk.setActive(false)
      goSleepingCycles = cycles
      //ledListener.beginLoadingOf("Disk go sleeping...")
    }
  }

  def clock(cycles: Long) {
    if (running) {
      if (cpuExact) {
        checkPC(cycles)
        cpu.fetchAndExecute
        if (CYCLE_ADJ > 0) {
          cycleFrac += CYCLE_ADJ
          if (cycleFrac >= 1) {
            cycleFrac -= 1
            cpu.fetchAndExecute
            viaDisk.clock(cycles)
            viaBus.clock(cycles)
          }
        }
      } else {
        if (cycles > cpuWaitUntil) {
          checkPC(cycles)
          cpuWaitUntil = cycles + cpu.fetchAndExecute
          val delta = cpuWaitUntil - cycles + 1
          cycleFrac += delta * CYCLE_ADJ
          if (cycleFrac >= 1) {
            cpuWaitUntil -= 1
            cycleFrac -= 1
          }
        }        
      }
      
      viaDisk.clock(cycles)
      viaBus.clock(cycles)
    } else if (cycles - goSleepingCycles > GO_SLEEPING_MESSAGE_CYCLES) {
      ledListener.endLoading
      goSleepingCycles = Long.MaxValue
      isRunningListener(false)
    }
  }

  private def setFilename {
    var adr = 0x200
    val sb = new StringBuilder
    var c = mem.read(adr)
    while (c != 0) {
      sb.append(c.toChar)
      adr += 1
      c = mem.read(adr)
    }
    viaDisk.setCurrentFilename(sb.toString)
  }

  // ------------ TRACING -----------
  def setTraceOnFile(out:PrintWriter,enabled:Boolean) {/* ignored */}
  def setTrace(traceOn: Boolean) = {
    tracing = traceOn
    if (tracing) awake
    cpu.setTrace(traceOn)
  }
  def step(updateRegisters: (String) => Unit) = cpu.step(updateRegisters)
  def setBreakAt(breakType:BreakType,callback:(String) => Unit) = cpu.setBreakAt(breakType,callback)
  def jmpTo(pc: Int) = cpu.jmpTo(pc)
  // state
  protected def saveState(out:ObjectOutputStream) {
    if (ledListener != null) out.writeBoolean(ledListener.isOn)
    out.writeDouble(CYCLE_ADJ)
    out.writeInt(currentSpeedHz)
    out.writeDouble(cycleFrac)
    out.writeBoolean(running)
    out.writeLong(awakeCycles)
    out.writeLong(goSleepingCycles)
    out.writeBoolean(canSleep)
    out.writeBoolean(useTRAPFormat)
  }
  protected def loadState(in:ObjectInputStream) {
    if (ledListener != null) {
      if (in.readBoolean) ledListener.turnOn else ledListener.turnOff
    }
    CYCLE_ADJ = in.readDouble
    currentSpeedHz = in.readInt
    cycleFrac = in.readDouble
    running = in.readBoolean
    awakeCycles = in.readLong
    goSleepingCycles = in.readLong
    canSleep = in.readBoolean
    useTRAPFormat = in.readBoolean
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}