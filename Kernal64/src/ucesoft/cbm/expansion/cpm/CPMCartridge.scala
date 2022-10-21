package ucesoft.cbm.expansion.cpm

import ucesoft.cbm.{ChipID, Clock, ClockEvent, Log}
import ucesoft.cbm.cpu.{Memory, Z80}
import ucesoft.cbm.expansion.{ExpansionPort, ExpansionPortType}
import ucesoft.cbm.trace.{TraceListener, Tracer}
import ucesoft.cbm.trace.Tracer.TracedDevice

import java.io.{ObjectInputStream, ObjectOutputStream}

class CPMCartridge(mem:Memory,
                   setDMA: Boolean => Unit,
                   tracer:Tracer) extends ExpansionPort {
  val TYPE : ExpansionPortType.Value = ExpansionPortType.CPM
  override val name = "CP/M Cartridge"
  override val componentID = "CP/M"
  val EXROM = true
  val GAME = true
  val ROML: Memory = null
  val ROMH: Memory = null
  
  private[this] var z80Active = false
  private[this] val clk = Clock.systemClock
  private[this] val z80Memory = new Memory {
    val isRom = false
    val length = 0xFFFF
    val startAddress = 0
    val name = "Z80 Memory"
    
    def init  : Unit = {}
    val isActive = true
    @inline private def z80Address(address:Int) = (address + 0x1000) & 0xFFFF
    def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem.read(z80Address(address),chipID)
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU): Unit = mem.write(z80Address(address),value,chipID)
  }
  
  private[this] val z80 = new Z80(z80Memory)

  z80.init
  tracer.addDevice(TracedDevice("CP/M Z80",z80Memory,z80,false))
  
  override def eject  : Unit = {
    Log.debug("Ejecting CP/M cartridge...")
    turnZ80(false)
    tracer.removeDevice(TracedDevice("CP/M Z80",z80Memory,z80,false))
  }
  
  @inline private def turnZ80(on:Boolean) : Unit = {
    setDMA(on)
    if (on && !z80Active) {
      z80Active = true
      clk.schedule(new ClockEvent("Z80 clock",clk.nextCycles,z80ClockCallback))
    }
    else
    if (!on) {
      z80Active = false
    }
  }
  
  override def reset  : Unit = {
    z80.reset
    z80Active = false
  }
  
  override def setBaLow(baLow:Boolean) : Unit = {
    z80.requestBUS(baLow)
  }
  
  final override def read(address: Int, chipID: ChipID.ID) = 0
  final override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if ((address & 0xFF00) == 0xDE00) {
      turnZ80((value & 0x01) == 0)  
    }
  }
  
  private[this] val z80ClockCallback = z80Clock _
  
  private def z80Clock(cycles:Long) : Unit = {
    z80.clock(cycles,2)
    if (z80Active) clk.schedule(new ClockEvent("Z80 clock",clk.nextCycles,z80ClockCallback))
  }

  override def saveState(out: ObjectOutputStream): Unit = {
    super.saveState(out)
    out.writeBoolean(z80Active)
    z80.save(out)
  }

  override def loadState(in: ObjectInputStream): Unit = {
    super.loadState(in)
    turnZ80(in.readBoolean)
    z80.load(in)
  }
}