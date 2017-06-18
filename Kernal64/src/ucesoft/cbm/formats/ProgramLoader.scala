package ucesoft.cbm.formats

import ucesoft.cbm.cpu.Memory

object ProgramLoader {
  def updateBASICPointers(mem:Memory,startAddress:Int,endAddress:Int,c64Mode:Boolean) {
    c64Mode match {
      case true =>
        mem.write(45, endAddress & 0xFF)
        mem.write(46, endAddress >> 8)
        mem.write(0xAE,endAddress & 0xFF)
        mem.write(0xAF,endAddress >> 8)
      case false =>
        mem.write(0x2B,startAddress & 0xFF)
        mem.write(0xAC,startAddress & 0xFF)
        mem.write(0x2C,startAddress >> 8)
        mem.write(0xAD,startAddress >> 8)
        mem.write(0x1210,endAddress & 0xFF)
        mem.write(0x1211,endAddress >> 8)
    }
  }
  def startBASICAddress(mem:Memory,c64Mode:Boolean) : Int = c64Mode match {
    case true => mem.read(43) + mem.read(44) * 256
    case false => mem.read(0x2B) + mem.read(0x2C) * 256
  }
  
  def startAddress(mem:Memory,c64Mode:Boolean) : Int = c64Mode match {
    case true => mem.read(43) | mem.read(44) << 8
    case false => mem.read(0xAC) | mem.read(0xAD) << 8
  }
  
  def endAddress(mem:Memory,c64Mode:Boolean) : Int = c64Mode match {
    case true => mem.read(45) | mem.read(46) << 8
    case false => mem.read(0x1210) | mem.read(0x1211) << 8
  }
}