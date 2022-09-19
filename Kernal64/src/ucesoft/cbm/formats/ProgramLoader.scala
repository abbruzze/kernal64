package ucesoft.cbm.formats

import ucesoft.cbm.{C128Model, C64Model, CBMComputerModel, VIC20Model}
import ucesoft.cbm.cbm2.CBM2MMU
import ucesoft.cbm.cpu.{CPU65xx, Memory}

import java.io.{BufferedInputStream, File, FileInputStream, FileOutputStream}
;

object ProgramLoader {
  var cpu : CPU65xx = _
  private[this] var loadingWithWarp = false
  var loadingWithWarpEnabled = false
  var warpModeListener : (Boolean) => Unit = _

  def checkLoadingInWarpMode(model:CBMComputerModel,c64Mode:Boolean): Unit = {
    model match {
      case C64Model|C128Model =>
        if (c64Mode) {
          if (loadingWithWarpEnabled) {
            val pc = cpu.getCurrentInstructionPC
            if (pc == 0xF4E0 && !loadingWithWarp) { // F4A5 is the load entry point routine, but we must skip error conditions
              loadingWithWarp = true
              if (warpModeListener != null) warpModeListener(true)
            }
            else if (loadingWithWarp && (pc == 0xF52B || pc == 0xF633)) {
              loadingWithWarp = false
              if (warpModeListener != null) warpModeListener(false)
            }
          }
        }
        else {
          if (loadingWithWarpEnabled) {
            val pc = cpu.getCurrentInstructionPC
            if ((pc == 0xF421 || pc == 0xF2CF) && !loadingWithWarp) {
              loadingWithWarp = true
              if (warpModeListener != null) warpModeListener(true)
            }
            else if (loadingWithWarp && (pc == 0xF4A6 || pc == 0xF498 || pc == 0xF48C || pc == 0xF39B || pc == 0xF323)) {
              loadingWithWarp = false
              if (warpModeListener != null) warpModeListener(false)
            }
          }
        }
      case VIC20Model =>
        if (loadingWithWarpEnabled) {
          val pc = cpu.getCurrentInstructionPC
          if (pc == 0xF5B5 && !loadingWithWarp) {
            loadingWithWarp = true
            if (warpModeListener != null) warpModeListener(true)
          }
          else if (loadingWithWarp && (pc == 0xF5BF || pc == 0xF595)) {
            loadingWithWarp = false
            if (warpModeListener != null) warpModeListener(false)
          }
        }
    }
  }

  def reset(): Unit = {
    loadingWithWarp = false
  }

  def updateBASICPointers(mem:Memory,startAddress:Int,endAddress:Int,c64Mode:Boolean,drive:Int,fileName:String) : Unit = {
    if (c64Mode) {
      val endOfBasic = mem.read(55) | mem.read(56) << 8
      mem.write(11, 76)
      mem.write(15, 2)
      mem.write(35, 8)
      for (adr <- 45 until 50 by 2) {
        mem.write(adr, endAddress & 0xFF)
        mem.write(adr + 1, endAddress >> 8)
      }
      mem.write(54, 160)
      mem.write(73, drive)
      mem.write(144, 64)
      mem.write(148, 64)
      mem.write(163, 64)
      mem.write(0xAE, endAddress & 0xFF)
      mem.write(0xAF, endAddress >> 8)
      mem.write(184, 1)
      mem.write(185, 96)
      mem.write(186, drive)
      mem.write(187, endOfBasic & 0xFF)
      mem.write(188, endOfBasic >> 8)
      mem.write(195, 1)
      mem.write(196, 8)
      mem.write(183, 0)
    } else {
      mem.write(0x2B, startAddress & 0xFF)
      mem.write(0xAC, startAddress & 0xFF)
      mem.write(0x2C, startAddress >> 8)
      mem.write(0xAD, startAddress >> 8)
      mem.write(0x1210, endAddress & 0xFF)
      mem.write(0x1211, endAddress >> 8)
    }
  }
  private def startAddress(mem:Memory,c64Mode:Boolean) : Int = if (c64Mode) {
    mem.read(43) | mem.read(44) << 8
  } else {
    mem.read(0xAC) | mem.read(0xAD) << 8
  }

  private def endAddress(mem:Memory,c64Mode:Boolean) : Int = if (c64Mode) {
    mem.read(45) | mem.read(46) << 8
  } else {
    mem.read(0x1210) | mem.read(0x1211) << 8
  }

  def loadCBMIIPRG(mem:CBM2MMU,file:File): (Int,Int) = {
    val in = new BufferedInputStream(new FileInputStream(file))
    val size = (file.length() - 2).toInt
    in.skip(2)
    var m = 3
    var b = in.read
    while (b != -1) {
      mem.writeBank(m,b,2)
      m += 1
      b = in.read
    }
    in.close()
    // update end of BASIC pointers
    mem.writeBank(47,(3 + size) & 0xFF,15)
    mem.writeBank(48,(3 + size) >> 8,15)
    // update filename related entries
    val dotprg = file.getName.lastIndexOf(".")
    val fileName = if (dotprg != -1) file.getName.substring(0, dotprg) else file.getName
    mem.writeBank(157,fileName.length,15)
    m = mem.readBank(144,15) | mem.readBank(145,15) << 8
    val fnBank = mem.readBank(146,15)
    for(i <- 0 until fileName.length) mem.writeBank(m + i,fileName.charAt(i),fnBank)
    (3,3 + size)
  }

  def loadPRG(mem:Memory,file:File,c64Mode:Boolean,drive:Int,startAddress:Option[Int] = None): (Int,Int) = {
    val in = new FileInputStream(file)
    var start = in.read | in.read << 8
    startAddress match {
      case Some(s) => start = s
      case None =>
    }
    var m = start
    var b = in.read
    var size = 0
    while (b != -1) {
      mem.write(m, b)
      m += 1
      size += 1
      b = in.read
    }
    in.close()
    val end = start + size
    val dotprg = file.getName.lastIndexOf(".")
    val fileName = if (dotprg != -1) file.getName.substring(0,dotprg) else file.getName
    updateBASICPointers(mem,start,end,c64Mode,drive,fileName)
    (start,end)
  }

  def loadPRG(mem:Memory,data:Array[Int],startAddress:Option[Int],c64Mode:Boolean,drive:Int,fileName:String): (Int,Int) = {
    val start = startAddress match {
      case Some(address) => address
      case None => this.startAddress(mem,c64Mode)
    }
    // load PRG into memory
    var memPtr = start
    var dataIndex = 0
    while (dataIndex < data.length) {
      mem.write(memPtr,data(dataIndex))
      memPtr += 1
      dataIndex += 1
    }
    val end = (start + data.length) & 0xFFFF
    updateBASICPointers(mem,start,end,c64Mode,drive,fileName)
    (start,end)
  }

  def savePRG(file:File,mem:Memory,c64Mode:Boolean): (Int,Int) = {
    val out = new FileOutputStream(file)
    val start = startAddress(mem,c64Mode)
    val end = endAddress(mem,c64Mode) - 1
    out.write(start & 0x0F)
    out.write(start >> 8)
    for (m <- start to end) out.write(mem.read(m))
    out.close()
    (start,end)
  }
}
