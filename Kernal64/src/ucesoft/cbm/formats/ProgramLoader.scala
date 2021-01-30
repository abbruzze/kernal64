package ucesoft.cbm.formats

import java.io.{File, FileInputStream, FileOutputStream}

import ucesoft.cbm.Clock
import ucesoft.cbm.cpu.{CPU65xx, Memory}
;

object ProgramLoader {
  var cpu : CPU65xx = _
  private[this] var loadingWithWarp = false
  var loadingWithWarpEnabled = true
  var warpModeListener : (Boolean) => Unit = _

  def checkLoadingInWarpMode(c64Mode:Boolean): Unit = {
    if (c64Mode) {
      if (loadingWithWarpEnabled) {
        val pc = cpu.getCurrentInstructionPC
        if (pc == 0xF4A5 && !loadingWithWarp) {
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
  }

  def reset: Unit = {
    loadingWithWarp = false
  }

  def updateBASICPointers(mem:Memory,startAddress:Int,endAddress:Int,c64Mode:Boolean,drive:Int,fileName:String) : Unit = {
    c64Mode match {
      case true =>
        val endOfBasic = mem.read(55) | mem.read(56) << 8
        mem.write(11, 76)
        mem.write(15, 2)
        mem.write(35, 8)
        for(adr <- 45 until 50 by 2) {
          mem.write(adr, endAddress & 0xFF)
          mem.write(adr + 1, endAddress >> 8)
        }
        mem.write(54, 160)
        mem.write(73, drive)
        mem.write(144, 64)
        mem.write(148, 64)
        mem.write(163, 64)
        mem.write(0xAE,endAddress & 0xFF)
        mem.write(0xAF,endAddress >> 8)
        mem.write(184, 1)
        mem.write(185, 96)
        mem.write(186, drive)
        mem.write(187,endOfBasic & 0xFF)
        mem.write(188,endOfBasic >> 8)
        mem.write(195, 1)
        mem.write(196, 8)
        mem.write(183, 0)

        // filename
        /* Ignored for now: breaks testbench decimalmode c128 in c64 mode
        var i = 0
        while (i < fileName.length && i < 16) {
          mem.write(0x9FF0,fileName.charAt(i).toUpper)
          i += 1
        }

        mem.write(183,if (fileName.length > 16) 16 else fileName.length)
         */
      case false =>
        mem.write(0x2B,startAddress & 0xFF)
        mem.write(0xAC,startAddress & 0xFF)
        mem.write(0x2C,startAddress >> 8)
        mem.write(0xAD,startAddress >> 8)
        mem.write(0x1210,endAddress & 0xFF)
        mem.write(0x1211,endAddress >> 8)
    }
  }
  private def startAddress(mem:Memory,c64Mode:Boolean) : Int = c64Mode match {
    case true => mem.read(43) | mem.read(44) << 8
    case false => mem.read(0xAC) | mem.read(0xAD) << 8
  }

  private def endAddress(mem:Memory,c64Mode:Boolean) : Int = c64Mode match {
    case true => mem.read(45) | mem.read(46) << 8
    case false => mem.read(0x1210) | mem.read(0x1211) << 8
  }

  def loadPRG(mem:Memory,file:File,c64Mode:Boolean,drive:Int): (Int,Int) = {
    val in = new FileInputStream(file)
    val start = in.read | in.read << 8
    var m = start
    var b = in.read
    var size = 0
    while (b != -1) {
      mem.write(m, b)
      m += 1
      size += 1
      b = in.read
    }
    in.close
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
    out.close
    (start,end)
  }
}
