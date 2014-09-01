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
  private[this] val cpu = CPU6510.make(mem,ChipID.CPU_1541)
  private[this] val clk = Clock.systemClock
  private[this] val viaBus = new VIAIECBus(jackID, bus, IRQSwitcher.viaBusIRQ _, () => awake)
  private[this] val viaDisk = new VIADiskControl(cpu, IRQSwitcher.viaDiskIRQ _, ledListener)
  private[this] var cpuWaitUntil = 0L
  private[this] var diskWaitUntil = 0L
  private[this] var running = true
  private[this] var awakeCycles = 0L
  private[this] var goSleepingCycles = 0L
  private[this] var tracing = false

  def setDriveReader(driveReader: D64) = viaDisk.setDriveReader(driveReader)
  override def setActive(active: Boolean) = viaBus.setActive(active)
  
  private def awake {
    if (!running) {
      running = true
      awakeCycles = clk.currentCycles
      viaDisk.awake
    }
  }
  
  override def getProperties = {
    properties.setProperty("Running",running.toString)
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

  private object IRQSwitcher extends C64Component {
    val componentID = "IRQ Switcher (VIA1,VIA2)"
    val componentType = C64ComponentType.INTERNAL 
    
    private[this] var viaBusIRQLow = false
    private[this] var viaDiskIRQLow = false

    private def handleIRQ = {
      Log.debug(s"Handling IRQ viaBusIRQ=${viaBusIRQLow} viaDiskIRQ=${viaDiskIRQLow}")
      cpu.irqRequest(viaBusIRQLow || viaDiskIRQLow)
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
  
  def clock(cycles: Long) {
    if (running) {
      if (cycles > cpuWaitUntil) {
        val pc = cpu.getPC
        if (pc == LOAD_ROUTINE) setFilename
        else
        if (pc == FORMAT_ROUTINE) {
          setFilename
          if (viaDisk.formatDisk) cpu.jmpTo(FORMAT_ROUTINE_OK) else cpu.jmpTo(FORMAT_ROUTINE_NOK) 
        }
        else
        if (pc == WAIT_LOOP_ROUTINE && !viaDisk.isMotorOn && (cycles - awakeCycles) > WAIT_CYCLES_FOR_STOPPING && !tracing) {
          running = false
          goSleepingCycles = cycles
          ledListener.beginLoadingOf("Disk go sleeping...")
        }
        cpuWaitUntil = cycles + cpu.fetchAndExecute 
        val delta = cpuWaitUntil - cycles + 1
        cycleFrac += delta * CYCLE_ADJ
        if (cycleFrac > 1) {
          cpuWaitUntil -= 1
          cycleFrac -= 1
        }
      }
      if (cycles > diskWaitUntil) {
        diskWaitUntil = cycles + viaDisk.clock(cycles)
      }
    }
    else
    if (cycles - goSleepingCycles > GO_SLEEPING_MESSAGE_CYCLES) {
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
  def setBreakAt(address: Int, callback: () => Unit) = cpu.setBreakAt(address, callback)
  def jmpTo(pc: Int) = cpu.jmpTo(pc)
}