package ucesoft.cbm.peripheral.vic.coprocessor

import ucesoft.cbm.cpu.CPU65xx.CPUPostponeReadException
import ucesoft.cbm.cpu.{CPU65xx, Memory}
import ucesoft.cbm.peripheral.vic.VICType
import ucesoft.cbm.{CBMComponentType, ChipID, Log}

import java.io.{ObjectInputStream, ObjectOutputStream}

class VASYL(vicCtx:VICContext,cpu:CPU65xx,dmaHandler:(Boolean) => Unit) extends VICCoprocessor with Memory {
  override val isRom = false
  override val length = 0x400
  override val startAddress = 0xD000
  override val name = "VASYL MEMORY"

  override val componentID: String = "VASYL"
  override val componentType: CBMComponentType.Type = CBMComponentType.INTERNAL

  private[this] final val VERSION = "00011000".b // 0-2 Video System PAL/NTSC to be filled using VIC model, 3-7 version

  private[this] val lastCPUMem : Memory = cpu.getMemory

  private[this] val INSTRUCTION_SET : Array[() => Unit] = buildInstructionSet
  private[this] val banks = Array.ofDim[Int](8,65536)
  private[this] var displayListFetchBank,portBank = 0
  private[this] var portMem,prgMem : Array[Int] = banks(0)
  private[this] val regs = Array.ofDim[Int](0x4D)

  private[this] var dlistOnPendingOnNextFrame,dlistOnPendingOnNextCycle,dlistExecutionActive = false
  private[this] var updatePCWithDLIST2LH = false
  private[this] var beamRacerActiveState = 0
  private[this] val addressPort = Array.ofDim[Int](2)
  private[this] val portWriteReps = Array.ofDim[Int](2)
  private[this] val portWriteRepsEnabled = Array.ofDim[Boolean](2)
  private[this] val lastPortWriteValue = Array.ofDim[Int](2)
  private[this] var portsBind = false

  private[this] val debug = false
  private[this] val trace = true
  private[this] var tracedInstr = ""
  private[this] var pc,oldPC = 0
  private[this] var x_arg,v_arg,p_arg,h_arg,r_arg,rasterCounter = 0 // opcode's arguments or counters
  private[this] var fetchOp = false
  private[this] var lastInstr : () => Unit = _
  private[this] var lastOpcode = 0
  private[this] val counters = Array.ofDim[Int](2)
  private[this] var maskh, maskv = "00111111".b
  private[this] var clearMaskhOnNext,clearMaskvOnNext = false
  private[this] var skipNextWait = false
  private[this] var lastExecuteNow,executeNextNow = false
  private[this] var rasterLine, rasterCycle = 0
  private[this] var badLineRequested = false
  private[this] var badLineValue = 0
  // ================== SEQUENCER ==================================
  private[this] var seq_bank = 0
  private[this] var seq_active = false
  private[this] var seq_update_mode = 0
  private[this] var seq_swizzle_mode = 0
  private[this] var seq_bitmap_pointer = 0
  private[this] var seq_cycle_start = 15
  private[this] var seq_cycle_stop = 55
  private[this] var seq_step, seq_padding = 0
  private[this] var seq_xor_value = 0
  // ================== CPU BUFFERING ==============================
  private[this] var lastOpAccessingVIC = false
  private[this] var cpuPostponedWrite,cpuPostponedRead = false
  private[this] var cpuPostponedAddress = 0
  private[this] var cpuPostponedValue = 0
  private[this] var cpuPostponedRMWCounter = 0

  override val readOffset = 0xD030
  override val controlRegisterMask = 0x60
  override val interruptMaskRegisterMask = 0xE0

  private implicit class IntToBase(val digits: String) {
    def base(b: Int): Int = Integer.parseInt(digits, b)
    def b: Int = base(2)
    def o: Int = base(8)
    def x: Int = base(16)
  }

  private def buildInstructionSet : Array[() => Unit] = {
    val istr = Array.ofDim[() => Unit](256)
    // BADLINE
    for(i <- 0 to 7) istr("10101000".b | i) = BADLINE _
    // BRA
    istr("10100011".b) = BRA _
    istr("10100000".b) = DEC(0) _  // DECA
    istr("10100001".b) = DEC(1) _ // DECB
    istr("10110000".b) = DELAYH _
    for(i <- 0 to 1) istr("10111000".b | i) = DELAYV _
    istr("10100010".b) = IRQ _
    for(i <- 0 to 1) istr("10110100".b | i << 1) = MASKH _
    for(i <- 0 to 3) istr("10111100".b | i) = MASKV _
    for(i <- 0 to 63) istr("11000000".b | i) = MOV(0) _
    for(i <- 0 to 31) istr("10000000".b | i) = MOV(0x40) _ // MOVI
    istr("10110010".b) = SET(0) _
    istr("10110011".b) = SET(1) _
    istr("10100110".b) = SKIP _
    istr("10100111".b) = VNOP _
    for(i <- 0 to 127) istr("00000000".b | i) = WAIT _
    istr("10100100".b) = WAITBAD _
    for(i <- 0 to 1) istr("10111010".b | i) = WAITREP _
    istr("10100101".b) = XFER _
    istr
  }

  override def reset: Unit = {
    displayListFetchBank = 0
    portBank = 0
    portMem = banks(0)
    prgMem = portMem
    java.util.Arrays.fill(regs,0)
    dlistOnPendingOnNextCycle = false
    dlistOnPendingOnNextFrame = false
    dlistExecutionActive = false
    beamRacerActiveState = 0
    addressPort(0) = 0
    addressPort(1) = 0
    portWriteReps(0) = 0
    portWriteReps(1) = 0
    portWriteRepsEnabled(0) = false
    portWriteRepsEnabled(1) = false
    lastPortWriteValue(0) = 0
    lastPortWriteValue(1) = 0
    pc = 0
    counters(0) = 0
    counters(1) = 0
    clearMaskvOnNext = false
    clearMaskhOnNext = false
    maskv = "00111111".b
    maskh = "00111111".b
    skipNextWait = false
    executeNextNow = false
    seq_bank = 0
    seq_active = false
    seq_update_mode = 0
    seq_swizzle_mode = 0
    seq_bitmap_pointer = 0
    seq_cycle_start = 15
    seq_cycle_stop = 55
    seq_padding = 0
    seq_step = 0
    seq_xor_value = 0
    portsBind = false
    badLineRequested = false
    lastOpAccessingVIC = false
    cpuPostponedWrite = false
    cpuPostponedRead = false
    cpuPostponedRMWCounter = 0
    updatePCWithDLIST2LH = false
  }

  override def hardReset: Unit = {
    reset
    for(b <- banks) java.util.Arrays.fill(b,0)
  }

  override def init: Unit = {}

  def disinstall : Unit = {
    cpu.setMemory(lastCPUMem)
  }
  def install : Unit = {
    cpu.setMemory(this)
  }

  // INSTRUCTIONS

  private def BADLINE() : Unit = {
    if (fetchOp) {
      h_arg = lastOpcode & 7
      if (trace) tracedInstr = s"BADLINE ${h_arg.toHexString}"
    }

    if (vicCtx.isAECAvailable) {
      lastOpAccessingVIC = true
      fetchOp = true
      val c = vicCtx.read(0xD011, ChipID.VIC_COP)
      badLineRequested = true // workaround
      badLineValue = (c & 0xF8) | ((rasterLine & 0x7) + h_arg) & 0x7
    }
    else fetchOp = false
  }

  private def BRA() : Unit = {
    val offset = prgMem(pc).toByte
    pc = (pc + 1 + offset) & 0xFFFF
    if (trace) tracedInstr = s"BRA ${offset.toHexString}"
  }

  private def DEC(c:Int)() : Unit = {
    if (counters(c) == 0) pc = (pc + 2) & 0xFFFF
    else counters(c) -= 1
    if (trace) tracedInstr = s"DEC ${'A' + c}"
  }

  private def DELAYH() : Unit = {
    if (fetchOp) {
      fetchOp = false
      val arg = prgMem(pc)
      pc = (pc + 1) & 0xFFFF
      h_arg = arg & 63
      v_arg = (arg >> 6) & 3
      if (trace) tracedInstr = s"DELAYH ${h_arg.toHexString}${if (v_arg != 0) s",${v_arg.toHexString}" else ""}"

      if (h_arg == 0) h_arg = 1
      h_arg = (h_arg + rasterCycle - 2) % (vicCtx.getVICModel.RASTER_CYCLES - 1) // 0-based, 1 cycle already consumed
      rasterCounter = rasterLine + v_arg
    }

    if ((v_arg == 0 || compareV(rasterCounter)) && compareH(h_arg)) {
      fetchOp = true
      clearMaskh
      clearMaskv
    }
  }

  private def DELAYV() : Unit = {
    if (fetchOp) {
      val arg = prgMem(pc)
      pc = (pc + 1) & 0xFFFF
      v_arg = arg | (lastOpcode & 1) << 8

      if (v_arg == 0) return

      fetchOp = false
      rasterCounter = rasterLine + v_arg
      if (trace) tracedInstr = s"DELAYV ${v_arg.toHexString}"
    }

    if (compareH(0) && compareV(rasterCounter)) {
      fetchOp = true
      clearMaskh
      clearMaskv
      executeNextNow = true
    }
  }

  private def IRQ() : Unit = {
    vicCtx.turnOnInterruptControlRegisterBits(16)
    if (trace) tracedInstr = "IRQ"
  }

  private def MASKH() : Unit = {
    val arg = prgMem(pc)
    pc = (pc + 1) & 0xFFFF
    maskh = arg & 63
    clearMaskhOnNext = (lastOpcode & 2) == 0
    if (trace) tracedInstr = s"MASKH ${maskh.toHexString}"
  }

  private def MASKV() : Unit = {
    val arg = prgMem(pc)
    pc = (pc + 1) & 0xFFFF
    maskv = arg & 63
    clearMaskvOnNext = (lastOpcode & 2) == 0
    if (trace) tracedInstr = s"MASKH ${maskv.toHexString}"
  }

  private def MOV(reg_offset:Int)() : Unit = {
    if (fetchOp) {
      x_arg = prgMem(pc)
      r_arg = (lastOpcode & 63) + reg_offset
      pc = (pc + 1) & 0xFFFF

      if (trace) tracedInstr = s"MOV ${r_arg.toHexString},${x_arg.toHexString}"

      if (r_arg > 0x30) { // VASYL registers, only accessible
        writeReg(r_arg,x_arg,true)
        return
      }

      fetchOp = false
    }

    if (vicCtx.isAECAvailable) {
      lastOpAccessingVIC = true
      fetchOp = true
      vicCtx.write(r_arg,x_arg,ChipID.VIC_COP)
    }
  }

  private def SET(counter:Int)() : Unit = {
    counters(counter) = prgMem(pc)
    pc = (pc + 1) & 0xFFFF
    if (trace) tracedInstr = s"SET${'A' + counter}"
  }

  private def SKIP() : Unit = {
    skipNextWait = true
    if (trace) tracedInstr = "SKIP"
  }

  private def VNOP() : Unit = {
    /* DO NOTHING */
    if (trace) tracedInstr = "VNOP"
  }

  private def WAIT() : Unit = {
    if (fetchOp) {
      v_arg = prgMem(pc) | (lastOpcode & 1) << 8
      h_arg = (lastOpcode >> 1) & 63
      pc = (pc + 1) & 0xFFFF

      if (trace) tracedInstr = if (v_arg == 0x1FF && h_arg == 0x3F) "END" else s"WAIT ${v_arg.toHexString},${h_arg.toHexString}"

      if (skipNextWait) {
        skipNextWait = false

        if (compareV(v_arg,true) || (compareV(v_arg) && compareH(h_arg,true))) {
          pc = (pc + 2) & 0xFFFF
          clearMaskv
          clearMaskh
        }
      }
      else
        if (compareV(v_arg,true) || (compareV(v_arg) && compareH(h_arg,true))) {
          clearMaskv
          clearMaskh
          executeNextNow = true
          return
        }
        else fetchOp = false
    }

    if (compareV(v_arg) && compareH(h_arg)) {
      fetchOp = true
      clearMaskh
      clearMaskv
      executeNextNow = true
    }
  }

  private def WAITBAD() : Unit = {
    val d011 = vicCtx.read(0xD011,ChipID.VIC_COP)
    val yscroll = d011 & 7
    val den = (d011 & 16) == 16
    val targetRaster = rasterLine + 1
    fetchOp = rasterCycle == 1 && (targetRaster >= 0x30 && targetRaster <= 0xF7 && ((targetRaster & 7) == yscroll) && den)
    if (trace) tracedInstr = "WAITBAD"
  }

  private def WAITREP() : Unit = {
    if (fetchOp) {
      p_arg = lastOpcode & 1
      if (!portWriteRepsEnabled(p_arg)) return

      fetchOp = false
      if (trace) tracedInstr = s"WAITREP $p_arg"
    }

    if (portWriteReps(p_arg) == 0) fetchOp = true
  }

  private def XFER() : Unit = {
    if (fetchOp) {
      r_arg = prgMem(pc) & 63
      p_arg = (prgMem(pc) >> 7) & 1
      pc = (pc + 1) & 0xFFFF

      if (r_arg > 0x30) { // VASYL registers
        writeReg(r_arg,readPort(p_arg),true)
        return
      }
      fetchOp = false
      if (trace) tracedInstr = s"XFER ${r_arg.toHexString},$p_arg"
    }

    if (vicCtx.isAECAvailable) {
      lastOpAccessingVIC = true
      vicCtx.write(r_arg,readPort(p_arg),ChipID.VIC_COP)
      fetchOp = true
    }
  }

  private def deactivate() : Unit = {
    portWriteRepsEnabled(0) = false
    portWriteRepsEnabled(1) = false
    seq_active = false
    dlistOnPendingOnNextCycle = false
    dlistOnPendingOnNextFrame = false
    dlistExecutionActive = false
    executeNextNow = false
    beamRacerActiveState = 0
  }
  // ----------------------------------------------------------------------

  final def writeReg(_reg:Int,value:Int): Unit = writeReg(_reg,value,false)

  final def writeReg(_reg:Int,value:Int,isVasylWriting:Boolean) : Unit = {
    val reg = _reg & 0xFF
    //if (debug && reg == 0x2E || (reg > 0x30) && (reg < 0x3F)) println(s"regs(${reg.toHexString}) = ${value.toHexString}")
    if (reg == 0x31) {
      beamRacerActiveState match {
        case 0 =>
          if (value == 0x42) beamRacerActiveState += 1
        case 1 =>
          if (value == 0x52) {
            beamRacerActiveState += 1
            if (debug) println("VASYL activated")
          }
          else beamRacerActiveState = 0
        case 2 =>
          regs(0x31) = value

          dlistOnPendingOnNextFrame = (value & 8) > 0

          portBank = value & 7
          portMem = banks(portBank)

          if (!dlistOnPendingOnNextFrame) {
            displayListFetchBank = portBank
            prgMem = banks(displayListFetchBank)
          }

          portsBind = (value & 0x40) > 0

          if (!dlistOnPendingOnNextFrame) dlistExecutionActive = false

          // TODO: grey dot kill

          if (debug) println(s"Control register: portBank = $portBank, dlistOnPendingOnNextFrame = $dlistOnPendingOnNextFrame, dlistExecutionActive = $dlistExecutionActive portsBind = $portsBind")
      }
    }
    else
      if (beamRacerActiveState == 2) { // beamracer active
        if ((reg > 0x30 && reg < 0x40) || (reg >= 0x40 && reg < 0x4D && isVasylWriting)) regs(reg) = value

        reg match {
          case 0x2E =>
            if ((value & 0x40) > 0) { // un-knock
              deactivate
            }
          case 0x32 =>
            if (debug) println(s"DLISTL = ${value.toHexString}, execution address = ${(value | regs(0x33) << 8).toHexString}")
          case 0x33 =>
            if (debug) println(s"DLISTH = ${value.toHexString}, execution address = ${(value << 8| regs(0x32)).toHexString}")
          case 0x34 =>
            addressPort(0) = addressPort(0) & 0xFF00 | value & 0xFF
            if (debug) println(s"ADR0L = ${value.toHexString}, address port 0 = ${addressPort(0).toHexString}")
          case 0x35 =>
            addressPort(0) = addressPort(0) & 0xFF | (value & 0xFF) << 8
            if (debug) println(s"ADR0H = ${value.toHexString}, address port 0 = ${addressPort(0).toHexString}")
          case 0x36 =>
            if (debug) println(s"STEP0 = ${value.toHexString}")
          case 0x37 =>
            writePort(0,value)
          case 0x38 =>
            addressPort(1) = addressPort(1) & 0xFF00 | value & 0xFF
            if (debug) println(s"ADR1L = ${value.toHexString}, address port 1 = ${addressPort(1).toHexString}")
          case 0x39 =>
            addressPort(1) = addressPort(1) & 0xFF | (value & 0xFF) << 8
            if (debug) println(s"ADR1H = ${value.toHexString}, address port 1 = ${addressPort(1).toHexString}")
          case 0x3A =>
            if (debug) println(s"STEP1 = ${value.toHexString}")
          case 0x3B =>
            writePort(1,value)
          case 0x3C =>
            if (debug) println(s"REP0 = ${value.toHexString}")
            portWriteRepsEnabled(0) = true
            portWriteReps(0) = value
          case 0x3D =>
            if (debug) println(s"REP1 = ${value.toHexString} ${if (portsBind) s" portsBind enabled from ${addressPort(0).toHexString} to ${addressPort(1).toHexString}" else ""}")
            portWriteRepsEnabled(1) = true
            portWriteReps(1) = value
          case 0x3E =>
            dlistOnPendingOnNextCycle = true
          // ========================= VASYL WRITE-ONLY REGISTERS ==========================
          case 0x40 if isVasylWriting => // PBS_CONTROL
            seq_bank = value & 7
            seq_active = (value & 8) > 0
            seq_update_mode = (value >> 4) & 3
            seq_swizzle_mode = (value >> 6) & 3
            if (debug) println(s"PBS CONTROL REGISTER: seq_bank=$seq_bank seq_active=$seq_active seq_update=$seq_update_mode seq_swizzle=$seq_swizzle_mode")
          case 0x41 if isVasylWriting =>
            if (debug) println(s"DLIST2L = ${value.toHexString}, execution address = ${(value | regs(0x42) << 8).toHexString}")
          case 0x42 if isVasylWriting =>
            if (debug) println(s"DLIST2H = ${value.toHexString}, execution address = ${(value << 8| regs(0x41)).toHexString}")
          case 0x43 if isVasylWriting =>
            dlistOnPendingOnNextCycle = true
            updatePCWithDLIST2LH = true
            if ((value & 0x8) > 0) {
              displayListFetchBank = value & 7
              prgMem = banks(displayListFetchBank)
            }
          case 0x44 if isVasylWriting =>
            if (debug) println(s"S_BASEL = ${value.toHexString}, bitmap pointer = ${(value | regs(0x45) << 8).toHexString}")
          case 0x45 if isVasylWriting =>
            seq_bitmap_pointer = value << 8| regs(0x44)
            if (debug) println(s"S_BASEH = ${value.toHexString}, bitmap pointer = ${seq_bitmap_pointer.toHexString}")
          case 0x46 if isVasylWriting =>
            seq_cycle_start = value
            if (debug) println(s"S_CYC_START = $seq_cycle_start")
          case 0x47 if isVasylWriting =>
            seq_cycle_stop = value
            if (debug) println(s"S_CYC_STOP = $seq_cycle_stop")
          case 0x48 if isVasylWriting =>
            seq_step = ((seq_step & 0xFF00) | value).toShort
            if (debug) println(s"S_STEP_L = $value, seq_step = $seq_step")
          case 0x49 if isVasylWriting =>
            seq_step = ((seq_step & 0x00FF) | (value << 8)).toShort
            if (debug) println(s"S_STEP_H = $value, seq_step = $seq_step")
          case 0x4A if isVasylWriting =>
            seq_padding = ((seq_padding & 0xFF00) | value).toShort
            if (debug) println(s"S_PADDING_L = $value, seq_step = $seq_padding")
          case 0x4B if isVasylWriting =>
            seq_padding = ((seq_padding & 0x00FF) | (value << 8)).toShort
            if (debug) println(s"S_PADDING_H = $value, seq_step = $seq_padding")
          case 0x4C if isVasylWriting =>
            if (debug) println(s"S_XORBYTE= $value")
            seq_xor_value = value
          case _ =>
        }
      }
  }

  private def writePort(port:Int,value:Int) : Unit = {
    portMem(addressPort(port)) = value
    if (debug) println(s"Writing $value to $portBank/${addressPort(port).toHexString}")
    incPortAddress(port)
    lastPortWriteValue(port) = value
  }

  private def readPort(port:Int) : Int = {
    val value = portMem(addressPort(port))
    if (debug) println(s"Reading from $portBank/${addressPort(port).toHexString} = $value")
    incPortAddress(port)
    value
  }

  private def incPortAddress(port:Int) : Unit = {
    addressPort(port) = (addressPort(port) + regs(if (port == 0) 0x36 else 0x3A).toByte) & 0xFFFF
  }

  final def readReg(_reg:Int) : Int = {
    val reg = _reg & 0xFF
    if (beamRacerActiveState == 2) {
      reg match {
        case 0x34 =>
          addressPort(0) & 0xFF
        case 0x35 =>
          addressPort(0) >> 8
        case 0x37 =>
          if ((regs(0x31) & 0x10) > 0) readPort(0) // PORT_READ_ENABLE
          else 0xFF
        case 0x38 =>
          addressPort(1) & 0xFF
        case 0x39 =>
          addressPort(1) >> 8
        case 0x3B =>
          if ((regs(0x31) & 0x10) > 0) readPort(1) // PORT_READ_ENABLE
          else 0xFF
        case 0x3C =>
          portWriteReps(0)
        case 0x3D =>
          portWriteReps(1)
        case 0x3E =>
          VERSION | (vicCtx.getVICModel.VIC_TYPE match {
            case VICType.PAL => "110".b
            case VICType.NTSC => "000".b
          })
        case _ =>
          regs(reg)
      }
    }
    else 0xFF
  }

  private def repeatWriteOnPort(port:Int) : Unit = {
    if (portsBind && port == 1) writePort(1,readPort(0))
    else writePort(port, lastPortWriteValue(port))

    portWriteReps(port) = (portWriteReps(port) - 1) & 0xFF
    if (portWriteReps(port) == 0) portWriteRepsEnabled(port) = false
    if (debug) println(s"Repeat on write port $port: remaining ${portWriteReps(port)}")
  }

  private def reloadPcFromRegs() : Unit = {
    // load pc with list address
    if (updatePCWithDLIST2LH) {
      pc = regs(0x41) | regs(0x42) << 8
      updatePCWithDLIST2LH = false
    }
    else pc = regs(0x32) | regs(0x33) << 8
    fetchOp = true
  }

  private def activateListExecution() : Unit = {
    dlistExecutionActive = true
    reloadPcFromRegs
    if (debug) println(s"VASYL list execution started at ${pc.toHexString}")
  }

  private def clearMaskh() : Unit = {
    if (clearMaskhOnNext) {
      clearMaskhOnNext = false
      maskh = "00111111".b
      //if (debug) println("Maskh cleared")
    }
  }

  private def clearMaskv() : Unit = {
    if (clearMaskvOnNext) {
      clearMaskvOnNext = false
      maskv = "111111111".b
      //if (debug) println("Maskv cleared")
    }
  }

  private def compareH(targetH:Int,greater:Boolean = false) : Boolean = {
    //println(s"Comparing cycle($greater): ${vicCtx.currentRasterCycle} with ${targetH + 1}")
    if (greater) (rasterCycle & maskh) > ((targetH + 1) & maskh) else (rasterCycle & maskh) == ((targetH + 1) & maskh)
  }

  private def compareV(targetV:Int,greater:Boolean = false) : Boolean = {
    //println(s"Comparing raster($greater): ${vicCtx.currentRasterLine} with $targetV")
    if (greater) (rasterLine & maskv) > (targetV & maskv) else (rasterLine & maskv) == (targetV & maskv)
  }

  final def isActive : Boolean = beamRacerActiveState == 2

  @inline private def adjustCurrentRasterCycle(rasterCycle:Int) : Int = if (rasterCycle == vicCtx.getVICModel.RASTER_CYCLES) 1 else rasterCycle + 1
  @inline private def adjustCurrentRasterLine(rasterCycle:Int,rasterLine:Int) : Int = if (rasterCycle == vicCtx.getVICModel.RASTER_CYCLES) (rasterLine + 1) % vicCtx.getVICModel.RASTER_LINES else rasterLine

  final def cycle(_rasterLine:Int,_rasterCycle:Int) : Unit = {
    rasterCycle = adjustCurrentRasterCycle(_rasterCycle)
    rasterLine = adjustCurrentRasterLine(_rasterCycle,_rasterLine)

    if (beamRacerActiveState != 2) return
    // active

    // check badline request: workaround
    if (badLineRequested) {
      badLineRequested = false
      vicCtx.write(0xD011,badLineValue, ChipID.VIC_COP)
    }
    //=========================================================

    // port 0 & 1 reps
    if (portWriteRepsEnabled(0)) repeatWriteOnPort(0)
    if (portWriteRepsEnabled(1)) repeatWriteOnPort(1)
    //=========================================================
    // sequencer stop check
    if (seq_active && rasterCycle == seq_cycle_stop + 1) {
      seq_bitmap_pointer = (seq_bitmap_pointer + seq_padding) & 0xFFFF
      if (seq_update_mode == 1) regs(0x44) = seq_bitmap_pointer & 0xFF // PBS_CONTROL_UPDATE_EOL
    }
    //=========================================================
    // frame begin
    if (rasterLine == 0 && rasterCycle == 1) {
      // masks reset
      maskh = "00111111".b
      maskv = "111111111".b
      clearMaskhOnNext = false
      clearMaskvOnNext = false
      // skip reset
      skipNextWait = false

      if (dlistExecutionActive) reloadPcFromRegs
      else
        if (dlistOnPendingOnNextFrame) {
          dlistOnPendingOnNextFrame = false
          activateListExecution
        }
    }
    //=========================================================
    if (dlistOnPendingOnNextCycle) {
      dlistOnPendingOnNextCycle = false
      activateListExecution
    }
    // execution
    do {
      lastExecuteNow = executeNextNow
      executeNextNow = false
      if (dlistExecutionActive) {
        if (fetchOp) {
          oldPC = pc
          lastOpAccessingVIC = false
          lastOpcode = prgMem(pc)
          pc = (pc + 1) & 0xFFFF
          lastInstr = INSTRUCTION_SET(lastOpcode)
          if (lastInstr == null) {
            // invalid opcode
            dlistExecutionActive = false
            println(s"VASYL invalid opcode: ${lastOpcode.toHexString}")
          }
          else lastInstr() // execute opcode
        }
        else lastInstr() // execute opcode
        if (trace && Log.isDebug) {
          val bytes = s"%02x".format(lastOpcode) + (if (pc - oldPC > 1) s" %02x".format(prgMem((oldPC + 1) & 0xFFFF)) else "")
          val disa = s"[VASYL/$rasterLine] %04x $displayListFetchBank/$bytes $tracedInstr".format(oldPC)
          Log.debug(disa)
          //println(disa)
        }
      }
    } while (executeNextNow && !lastExecuteNow)

    // check buffered write
    if (cpuPostponedWrite && !lastOpAccessingVIC) {
      cpuPostponedWrite = false
      lastCPUMem.write(cpuPostponedAddress,cpuPostponedValue)
      dmaHandler(false)
    }
    else // check for postponed read
      if (cpuPostponedRead && !lastOpAccessingVIC) {
        cpuPostponedRead = false
        dmaHandler(false)
      }
    //=========================================================
  }
  final def g_access(rasterCycle:Int) : Int = {
    if (beamRacerActiveState == 2 && seq_active && rasterCycle >= seq_cycle_start + 1 && rasterCycle < seq_cycle_stop + 1) {
      val gdata = banks(seq_bank)(seq_bitmap_pointer)
      seq_bitmap_pointer = (seq_bitmap_pointer + seq_step) & 0xFFFF
      if (seq_update_mode == 2) regs(0x44) = seq_bitmap_pointer & 0xFF // PBS_CONTROL_UPDATE_ALWAYS
      seq_xor_value ^ (seq_swizzle_mode match {
        case 0 =>
          gdata
        case 1 =>
          rev(gdata)
        case 2 =>
          revPairs(gdata)
        case 3 => // reserved
          gdata // TODO
      })
    }
    else -1
  }

  // ======================= CPU MEMORY BRIDGE ========================================

  override def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    if (beamRacerActiveState == 2 && lastOpAccessingVIC && address >= 0xD000 && address < 0xD400) { // concurrent VASYL write and CPU read
      dmaHandler(true)
      cpuPostponedRead = true
      throw new CPUPostponeReadException // force CPU to block this read as it had received a RDY signal
    }
    lastCPUMem.read(address, chipID)
  }

  override def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    if (beamRacerActiveState == 2 && lastOpAccessingVIC && address >= 0xD000 && address < 0xD400) { // concurrent VASYL & CPU I/O write
      val cpuOpCode = cpu.getCurrentOpCode
      val opRow = cpuOpCode >> 4
      val opCol = cpuOpCode & 0x0F

      if (debug) println(s"CPU writes to ${address.toHexString} $value [${cpuOpCode.toHexString}] VASYL = $tracedInstr $lastOpAccessingVIC")

      if (opCol == 0xE && (opRow < 0x8 || opRow > 0xB)) { // ASL, LSR, DEC, INC, ROL or ROR
        cpuPostponedRMWCounter += 1
        if (cpuPostponedRMWCounter == 1) return // ignore first write of RMW
      }

      cpuPostponedRMWCounter = 0
      cpuPostponedWrite = true
      cpuPostponedAddress = address
      cpuPostponedValue = value
      dmaHandler(true)
    }
    else lastCPUMem.write(address,value,chipID)
  }

  override def byteOnBUS : Int = lastCPUMem.byteOnBUS

  private def rev(i:Int) : Int = (i & 1) << 7 | (i & 2) << 5 | (i & 4) << 3 | (i & 8) << 1 | (i & 16) >> 1 | (i & 32) >> 3 | (i & 64) >> 5 | (i & 128) >> 7
  private def revPairs(i:Int) : Int = (i & 1) << 6 | (i & 2) << 6 | (i & 4) << 2 | (i & 8) << 2 | (i & 16) >> 2 | (i & 32) >> 2 | (i & 64) >> 6 | (i & 128) >> 6

  override protected def saveState(out: ObjectOutputStream): Unit = {
    out.writeInt(displayListFetchBank)
    out.writeInt(portBank)
    out.writeObject(banks)
    out.writeObject(regs)
    out.writeBoolean(dlistOnPendingOnNextCycle)
    out.writeBoolean(dlistOnPendingOnNextFrame)
    out.writeBoolean(dlistExecutionActive)
    out.writeInt(beamRacerActiveState)
    out.writeObject(addressPort)
    out.writeObject(portWriteReps)
    out.writeObject(portWriteRepsEnabled)
    out.writeObject(lastPortWriteValue)
    out.writeInt(pc)
    out.writeObject(counters)
    out.writeBoolean(clearMaskvOnNext)
    out.writeBoolean(clearMaskhOnNext)
    out.writeInt(maskv)
    out.writeInt(maskh)
    out.writeBoolean(skipNextWait)
    out.writeBoolean(executeNextNow)
    out.writeInt(seq_bank)
    out.writeBoolean(seq_active)
    out.writeInt(seq_update_mode)
    out.writeInt(seq_swizzle_mode)
    out.writeInt(seq_bitmap_pointer)
    out.writeInt(seq_cycle_start)
    out.writeInt(seq_cycle_stop)
    out.writeInt(seq_padding)
    out.writeInt(seq_step)
    out.writeInt(seq_xor_value)
    out.writeBoolean(portsBind)
    out.writeBoolean(badLineRequested)
    out.writeInt(badLineValue)
    out.writeBoolean(fetchOp)
    out.writeInt(lastOpcode)
    out.writeInt(x_arg)
    out.writeInt(v_arg)
    out.writeInt(h_arg)
    out.writeInt(r_arg)
    out.writeInt(rasterCounter)
    out.writeBoolean(lastOpAccessingVIC)
    out.writeBoolean(cpuPostponedWrite)
    out.writeInt(cpuPostponedAddress)
    out.writeInt(cpuPostponedValue)
    out.writeInt(cpuPostponedRMWCounter)
    out.writeBoolean(cpuPostponedRead)
    out.writeBoolean(updatePCWithDLIST2LH)
  }

  override protected def loadState(in: ObjectInputStream): Unit = {
    displayListFetchBank = in.readInt()
    portBank = in.readInt()
    loadMemory(banks,in)
    portMem = banks(portBank)
    prgMem = banks(displayListFetchBank)
    loadMemory(regs,in)
    dlistOnPendingOnNextCycle = in.readBoolean()
    dlistOnPendingOnNextFrame = in.readBoolean()
    dlistExecutionActive = in.readBoolean()
    beamRacerActiveState = in.readInt()
    loadMemory(addressPort,in)
    loadMemory(portWriteReps,in)
    loadMemory(portWriteRepsEnabled,in)
    loadMemory(lastPortWriteValue,in)
    pc = in.readInt()
    loadMemory(counters,in)
    clearMaskvOnNext = in.readBoolean()
    clearMaskhOnNext = in.readBoolean()
    maskv = in.readInt()
    maskh = in.readInt()
    skipNextWait = in.readBoolean()
    executeNextNow = in.readBoolean()
    seq_bank = in.readInt()
    seq_active = in.readBoolean()
    seq_update_mode = in.readInt()
    seq_swizzle_mode = in.readInt()
    seq_bitmap_pointer = in.readInt()
    seq_cycle_start = in.readInt()
    seq_cycle_stop = in.readInt()
    seq_padding = in.readInt()
    seq_step = in.readInt()
    seq_xor_value = in.readInt()
    portsBind = in.readBoolean()
    badLineRequested = in.readBoolean()
    badLineValue = in.readInt()
    fetchOp = in.readBoolean()
    lastOpcode = in.readInt()
    lastInstr = INSTRUCTION_SET(lastOpcode)
    x_arg = in.readInt()
    v_arg = in.readInt()
    h_arg = in.readInt()
    r_arg = in.readInt()
    rasterCounter = in.readInt()
    lastOpAccessingVIC = in.readBoolean()
    cpuPostponedWrite = in.readBoolean()
    cpuPostponedAddress = in.readInt()
    cpuPostponedValue = in.readInt()
    cpuPostponedRMWCounter = in.readInt()
    cpuPostponedRead = in.readBoolean()
    updatePCWithDLIST2LH = in.readBoolean()
  }

  override protected def allowsStateRestoring: Boolean = true
}
