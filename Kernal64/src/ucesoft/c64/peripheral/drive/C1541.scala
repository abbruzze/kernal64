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

class C1541(val jackID: Int, bus: IECBus, ledListener: DriveLedListener) extends TraceListener with Drive {
  val componentID = "C1541 Disk Drive"
  private[this] val LOAD_ROUTINE = 0xD7B4
  private[this] val FORMAT_ROUTINE = 0xC8C6
  private[this] val FORMAT_ROUTINE_OK = 0xC8EF
  private[this] val FORMAT_ROUTINE_NOK = 0xC8E8
  private[this] val WAIT_LOOP_ROUTINE = 0xEC9B //0xEBFF//0xEC9B
  private[this] val WAIT_CYCLES_FOR_STOPPING = 2000000
  private[this] val GO_SLEEPING_MESSAGE_CYCLES = 3000000
  private[this] val CYCLE_ADJ = (1000000.0 - 985248.0) / 985248.0
  private[this] var cycleFrac = 0.0
  private[this] val mem = new C1541Mems.C1541_RAM
  private[this] var cpu = CPU6510.make(mem,true,ChipID.CPU_1541)
  private[this] var cpuExact = cpu.isExact
  private[this] val clk = Clock.systemClock
  private[this] val viaBus = new VIAIECBus(jackID, bus, IRQSwitcher.viaBusIRQ _, () => awake)
  private[this] val viaDisk = new VIADiskControl(cpu, IRQSwitcher.viaDiskIRQ _, ledListener)
  private[this] var cpuWaitUntil = 0L
  private[this] var diskWaitUntil = 0L
  private[this] var running = true
  private[this] var awakeCycles = 0L
  private[this] var goSleepingCycles = 0L
  private[this] var tracing = false
  private[this] var canSleep = true

  def setDriveReader(driveReader: D64) = viaDisk.setDriveReader(driveReader)
  override def setActive(active: Boolean) = viaBus.setEnabled(active)
  override def setCanSleep(canSleep: Boolean) {
    this.canSleep = canSleep
    awake
  }
  private def awake {
    if (!running) {
      running = true
      viaBus.setActive(true)
      viaDisk.setActive(true)
      awakeCycles = clk.currentCycles
      viaDisk.awake
    }
  }

  override def getProperties = {
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
    awakeCycles = 0
    goSleepingCycles = 0
    if (ledListener != null) ledListener.turnOn
  }
  
  override def changeCPU(cycleExact:Boolean) {
    val oldCpu = cpu
    cpu = CPU6510.make(mem,cycleExact,ChipID.CPU_1541)
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
  }

  def getMem = mem

  @inline private def checkPC(cycles: Long) {
    val pc = cpu.getPC
    if (pc == LOAD_ROUTINE) setFilename
    else if (pc == FORMAT_ROUTINE) {
      setFilename
      if (viaDisk.formatDisk) cpu.jmpTo(FORMAT_ROUTINE_OK) else cpu.jmpTo(FORMAT_ROUTINE_NOK)
    } else if (pc == WAIT_LOOP_ROUTINE && canSleep && !mem.isChannelActive && !viaDisk.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
      running = false
      viaBus.setActive(false)
      viaDisk.setActive(false)
      goSleepingCycles = cycles
      ledListener.beginLoadingOf("Disk go sleeping...")
    }
  }

  def clock(cycles: Long) {
    if (running) {
      if (cpuExact) {
        checkPC(cycles)
        cpu.fetchAndExecute
        cycleFrac += CYCLE_ADJ
        if (cycleFrac > 1) {
          cycleFrac -= 1
          cpu.fetchAndExecute
        }
      } else {
        if (cycles > cpuWaitUntil) {
          checkPC(cycles)
          cpuWaitUntil = cycles + cpu.fetchAndExecute
          val delta = cpuWaitUntil - cycles + 1
          cycleFrac += delta * CYCLE_ADJ
          if (cycleFrac > 1) {
            cpuWaitUntil -= 1
            cycleFrac -= 1
          }
        }        
      }
      
      if (cycles > diskWaitUntil) {
        diskWaitUntil = cycles + viaDisk.clock(cycles)
      }
    } else if (cycles - goSleepingCycles > GO_SLEEPING_MESSAGE_CYCLES) {
      ledListener.endLoading
      goSleepingCycles = Long.MaxValue
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

  def setTrace(traceOn: Boolean) = {
    tracing = traceOn
    if (tracing) awake
    cpu.setTrace(traceOn)
  }
  def step(updateRegisters: (String) => Unit) = cpu.step(updateRegisters)
  def setBreakAt(address: Int, callback: (String) => Unit) = cpu.setBreakAt(address, callback)
  def jmpTo(pc: Int) = cpu.jmpTo(pc)
}