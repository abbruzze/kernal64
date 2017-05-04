package ucesoft.cbm.peripheral.vdc

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.Log

/**
  0/$00 Total number of horizontal character positions
  l/$01 Number of visible horizontal character positions
  2/$02 Horizontal sync position
  3/$03 Horizontal and vertical sync width
  4/$04 Total number of screen rows
  5/$05 Vertical fine adjustment
  6/$06 Number of visible screen rows
  7/$07 Vertical sync position
  8/$08 Interlace mode control register
  9/$09 Number of scan lines per character
  10/$OA Cursor mode control
  ll/$0B Ending scan line foT cursor
  12/$0C Screen memory starting address (high byte)
  13/$0D Screen memory starting address (low byte)
  14/$0E Cursor position address (high byte)
  15/$0F Cursor position address (low byte)
  16/$10 Light pen vertical position
  17/$11 Light pen horizontal position
  18/$12 Current memory address (high byte)
  19/$13 Current memory address (low byte)
  20/$14 Attribute memory starting address (high byte)
  21/$15 Attribute memory starting address {low byte)
  22/$16 Character horizontal size control register
  23/$17 Character vertical size control register
  24/$18 Vertical smooth scrolling and control register
  25/$19 Horizontal smooth scrolling and control register
  26/$lA Fore ground/background color register
  27/$lB Address increment per row
  28/$lC Character set address and memory type register
  29/$lD Underline scan-line-position register
  30/$lE Number of bytes for block write or copy
  31/$1F Memory read/write register
  32/$20 Block copy source address (high byte)
  33/$21 Block copy source address (low byte)
  34/$22 Beginning position for horizontal blanking
  35/$23 Ending position for horizontal blanking
  36/$24 Number of memory refresh cycles per scan line
 */
class VDC extends RAMComponent {
  val name = "C128 VDC"
  val componentID = "C128_VDC"
  val isRom = false
  val startAddress = 0xD600
  val length = 0x2
  val componentType = CBMComponentType.MEMORY
  val isActive = true
  
  final private[this] val RAM_SIZE = 0x10000 
  final private[this] val RAM_ADDR_MASK = RAM_SIZE - 1
  private[this] val ram = Array.ofDim[Int](RAM_SIZE)
  private[this] var address_reg = 0
  private[this] val regs = Array.ofDim[Int](37)
  final private[this] val VDC_VERSION = 0
  private[this] var vblank = 0
  
  final def init {
    
  }
  
  final def reset {
    
  }
  
  final def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    // TODO
    address & 1 match {
      case 0 => read_status
      case 1 => read_regs
    }
  }
  
  final def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    // TODO
    address & 1 match {
      case 0 => 
        address_reg = value & 0x3F
        Log.debug(s"VDC register set to ${Integer.toHexString(address_reg)}")
      case 1 => 
        write_regs(value)               
    }
  }
  
  @inline private[this] def read_status : Int = {
    0x80 | vblank << 5 | VDC_VERSION // always status = 1
  }
  
  @inline private[this] def currentMemoryAddress = regs(18) << 8 | regs(19)
  @inline private[this] def incCurrentMemoryAddress {
    regs(19) += 1
    if (regs(19) == 0x100) {
      regs(19) = 0
      regs(18) += 1
      if (regs(18) == 0x100) regs(18) = 0
    }
  }
  
  @inline private[this] def read_regs : Int = {
    if (address_reg > 36) return 0xFF
    
    address_reg match {  
      case 31 =>
        val addr = currentMemoryAddress
        incCurrentMemoryAddress
        ram(addr & RAM_ADDR_MASK)
      case _ => 
        regs(address_reg)
    }
  }
  
  @inline private[this] def write_regs(value:Int) : Unit = {
    Log.debug(s"VDC writing ${Integer.toHexString(value)} to register ${Integer.toHexString(address_reg)}")
    if (address_reg < 37) regs(address_reg) = value
    address_reg match {  
      case 30 =>
        copyorfill
      case 31 =>
        val addr = currentMemoryAddress
        incCurrentMemoryAddress
        ram(addr & RAM_ADDR_MASK) = value
        Log.debug(s"VDC writing ${Integer.toHexString(value)} to RAM address ${Integer.toHexString(addr)}")
      case _ =>         
    }
  }
  
  @inline private[this] def copyorfill {
    val length = if (regs(30) == 0) 0xFF else regs(30)    
    var address = regs(18) << 8 | regs(19)
    if ((regs(24) & 0x80) > 0) { // copy
      var source_copy_address = regs(32) << 8 | regs(33)
      Log.debug(s"VDC copying from ${Integer.toHexString(source_copy_address)} to ${Integer.toHexString(address)} length=${Integer.toHexString(length)}")
      var i = 0
      while (i < length) {
        ram(address & RAM_ADDR_MASK) = ram(source_copy_address & RAM_ADDR_MASK)
        i += 1
        address += 1
        source_copy_address += 1
      }
      regs(31) = ram((source_copy_address - 1) & RAM_ADDR_MASK) // from vdc-mem.c
      regs(32) = (source_copy_address >> 8) & 0xFF
      regs(33) = source_copy_address & 0xFF
    }
    else { // fill
      val value = regs(31)
      Log.debug(s"VDC filling from ${Integer.toHexString(address)} length=${Integer.toHexString(length)} with ${Integer.toHexString(value)}")
      var i = 0
      while (i < length) {
        ram(address & RAM_ADDR_MASK) = value
        i += 1
        address += 1
      }
    }
    
    regs(18) = (address >> 8) & 0xFF
    regs(19) = address & 0xFF
  }
  
  // state -----------------------------------------------  
  protected def saveState(out:ObjectOutputStream) {
    // TODO
  }
  protected def loadState(in:ObjectInputStream) {
    // TODO
  }
  protected def allowsStateRestoring(parent:JFrame) = {
    true
    // TODO
  }
}