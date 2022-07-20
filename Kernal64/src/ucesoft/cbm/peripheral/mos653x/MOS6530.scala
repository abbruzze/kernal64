package ucesoft.cbm.peripheral.mos653x

import ucesoft.cbm.peripheral.mos653x.MOS653X._

class MOS6530(override val name:String,override val portA:MOS653X.Port,override val portB:MOS653X.Port,override val irqLow: Boolean => Unit,rom:Array[Int]) extends MOS6532(name,portA,portB,irqLow) {
  require(rom.length == 1024,"MOS6530 rom size must be 1024 bytes")

  override protected val ram: Array[Int] = Array.ofDim[Int](64)
  override protected def writeTimerOrEdgeDetectControl(address:Int,value:Int): Unit = writeTimer(address,value)
  override def pa7(value:Boolean): Unit = {}
  override protected def timerIRQOccurred(): Unit = {
    if ((regs(DDRB) & 0x80) == 0) { // TO BE CHECKED PB7 must be input
      super.timerIRQOccurred()
    }
  }

  def readROM(address:Int): Int = rom(address & 0x3FF)
}
