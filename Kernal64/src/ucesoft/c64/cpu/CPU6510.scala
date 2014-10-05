package ucesoft.c64.cpu

import ucesoft.c64.ChipID
import ucesoft.c64.Chip
import ucesoft.c64.Log
import ucesoft.c64.trace.TraceListener
import ucesoft.c64.C64Component
import ucesoft.c64.C64ComponentType
import ucesoft.c64.ChipID

object CPU6510 {
  class CPUJammedException extends Exception
  
  object Instruction extends Enumeration {
    type CODE = Value
    val ORA, AND, EOR, ADC, STA, LDA, CMP, SBC = Value
    val ASL, ROL, LSR, ROR, STX, LDX, DEC, INC = Value
    val BIT, JMP, STY, LDY, CPY, CPX = Value
    val BPL, BMI, BVC, BVS, BCC, BCS, BNE, BEQ = Value
    val BRK, JSR, RTI, RTS = Value
    val PHP, PLP, PHA, PLA, DEY, TAY, INY, INX = Value
    val CLC, SEC, CLI, SEI, TYA, CLV, CLD, SED = Value
    val TXA, TXS, TAX, TSX, DEX, NOP = Value
    // UNDOC
    val KIL, SLO, ANC, RLA, SRE, ALR, RRA, ARR, SAX = Value
    val XAA, AHX, TAS, SHY, SHX, LAX, LAS, DCP, AXS = Value
    val ISC,LAXI = Value

    val MARK = Value // mark value for instructions counting
  }

  object Mode extends Enumeration {
    type MODE = Value
    val ZP, ZPX, ZPY, IZX, IZY = Value
    val ABS, ABX, ABY = Value
    val IMM, IND, REL, IMP = Value
    val UNKNOWN_ABS_OR_ZP = Value // just for assembler
    val UNKNOWN_ABX_OR_ZPX = Value // just for assembler
    val UNKNOWN_ABY_OR_ZPY = Value // just for assembler
  }
  
  private[cpu] type OperationCell = (Instruction.CODE, Mode.MODE, Int)
  import Instruction._
  import Mode._
  private val OP_MATRIX: Array[Array[OperationCell]] = Array(
    //  		x0			x1				x2				x3				x4			x5				x6				x7				x8			x9				xA				xB				xC				xD				xE			xF
    Array((BRK, IMP, 7), (ORA, IZX, 6), (KIL, IMP, 0), (SLO, IZX, 8), (NOP, ZP , 3), (ORA, ZP , 3), (ASL, ZP , 5), (SLO, ZP , 5), (PHP, IMP, 3), (ORA, IMM, 2), (ASL, IMP, 2), (ANC, IMM, 2), (NOP, ABS, 4), (ORA, ABS, 4), (ASL, ABS, 6), (SLO, ABS, 6)), // 0x
    Array((BPL, REL,-2), (ORA, IZY,-5), (KIL, IMP, 0), (SLO, IZY, 8), (NOP, ZPX, 4), (ORA, ZPX, 4), (ASL, ZPX, 6), (SLO, ZPX, 6), (CLC, IMP, 2), (ORA, ABY,-4), (NOP, IMP, 2), (SLO, ABY, 7), (NOP, ABX,-4), (ORA, ABX,-4), (ASL, ABX, 7), (SLO, ABX, 7)), // 1x
    Array((JSR, ABS, 6), (AND, IZX, 6), (KIL, IMP, 0), (RLA, IZX, 8), (BIT, ZP , 3), (AND, ZP , 3), (ROL, ZP , 5), (RLA, ZP , 5), (PLP, IMP, 4), (AND, IMM, 2), (ROL, IMP, 2), (ANC, IMM, 2), (BIT, ABS, 4), (AND, ABS, 4), (ROL, ABS, 6), (RLA, ABS, 6)), // 2x
    Array((BMI, REL,-2), (AND, IZY,-5), (KIL, IMP, 0), (RLA, IZY, 8), (NOP, ZPX, 4), (AND, ZPX, 4), (ROL, ZPX, 6), (RLA, ZPX, 6), (SEC, IMP, 2), (AND, ABY,-4), (NOP, IMP, 2), (RLA, ABY, 7), (NOP, ABX,-4), (AND, ABX,-4), (ROL, ABX, 7), (RLA, ABX, 7)), // 3x
    Array((RTI, IMP, 6), (EOR, IZX, 6), (KIL, IMP, 0), (SRE, IZX, 8), (NOP, ZP , 3), (EOR, ZP , 3), (LSR, ZP , 5), (SRE, ZP , 5), (PHA, IMP, 3), (EOR, IMM, 2), (LSR, IMP, 2), (ALR, IMM, 2), (JMP, ABS, 3), (EOR, ABS, 4), (LSR, ABS, 6), (SRE, ABS, 6)), // 4x
    Array((BVC, REL,-2), (EOR, IZY,-5), (KIL, IMP, 0), (SRE, IZY, 8), (NOP, ZPX, 4), (EOR, ZPX, 4), (LSR, ZPX, 6), (SRE, ZPX, 6), (CLI, IMP, 2), (EOR, ABY,-4), (NOP, IMP, 2), (SRE, ABY, 7), (NOP, ABX,-4), (EOR, ABX,-4), (LSR, ABX, 7), (SRE, ABX, 7)), // 5x
    Array((RTS, IMP, 6), (ADC, IZX, 6), (KIL, IMP, 0), (RRA, IZX, 8), (NOP, ZP , 3), (ADC, ZP , 3), (ROR, ZP , 5), (RRA, ZP , 5), (PLA, IMP, 4), (ADC, IMM, 2), (ROR, IMP, 2), (ARR, IMM, 2), (JMP, IND, 5), (ADC, ABS, 4), (ROR, ABS, 6), (RRA, ABS, 6)), // 6x
    Array((BVS, REL,-2), (ADC, IZY,-5), (KIL, IMP, 0), (RRA, IZY, 8), (NOP, ZPX, 4), (ADC, ZPX, 4), (ROR, ZPX, 6), (RRA, ZPX, 6), (SEI, IMP, 2), (ADC, ABY,-4), (NOP, IMP, 2), (RRA, ABY, 7), (NOP, ABX,-4), (ADC, ABX,-4), (ROR, ABX, 7), (RRA, ABX, 7)), // 7x
    Array((NOP, IMM, 2), (STA, IZX, 6), (NOP, IMM, 2), (SAX, IZX, 6), (STY, ZP , 3), (STA, ZP , 3), (STX, ZP , 3), (SAX, ZP , 3), (DEY, IMP, 2), (NOP, IMM, 2), (TXA, IMP, 2), (XAA, IMM, 2), (STY, ABS, 4), (STA, ABS, 4), (STX, ABS, 4), (SAX, ABS, 4)), // 8x
    Array((BCC, REL,-2), (STA, IZY, 6), (KIL, IMP, 0), (AHX, IZY, 6), (STY, ZPX, 4), (STA, ZPX, 4), (STX, ZPY, 4), (SAX, ZPY, 4), (TYA, IMP, 2), (STA, ABY, 5), (TXS, IMP, 2), (TAS, ABY, 5), (SHY, ABX, 5), (STA, ABX, 5), (SHX, ABY, 5), (AHX, ABY, 5)), // 9x
    Array((LDY, IMM, 2), (LDA, IZX, 6), (LDX, IMM, 2), (LAX, IZX, 6), (LDY, ZP , 3), (LDA, ZP , 3), (LDX, ZP , 3), (LAX, ZP , 3), (TAY, IMP, 2), (LDA, IMM, 2), (TAX, IMP, 2), (LAXI,IMM, 2), (LDY, ABS, 4), (LDA, ABS, 4), (LDX, ABS, 4), (LAX, ABS, 4)), // Ax
    Array((BCS, REL,-2), (LDA, IZY,-5), (KIL, IMP, 0), (LAX, IZY,-5), (LDY, ZPX, 4), (LDA, ZPX, 4), (LDX, ZPY, 4), (LAX, ZPY, 4), (CLV, IMP, 2), (LDA, ABY,-4), (TSX, IMP, 2), (LAS, ABY,-4), (LDY, ABX,-4), (LDA, ABX,-4), (LDX, ABY,-4), (LAX, ABY,-4)), // Bx
    Array((CPY, IMM, 2), (CMP, IZX, 6), (NOP, IMM, 2), (DCP, IZX, 8), (CPY, ZP , 3), (CMP, ZP , 3), (DEC, ZP , 5), (DCP, ZP , 5), (INY, IMP, 2), (CMP, IMM, 2), (DEX, IMP, 2), (AXS, IMM, 2), (CPY, ABS, 4), (CMP, ABS, 4), (DEC, ABS, 6), (DCP, ABS, 6)), // Cx
    Array((BNE, REL,-2), (CMP, IZY,-5), (KIL, IMP, 0), (DCP, IZY, 8), (NOP, ZPX, 4), (CMP, ZPX, 4), (DEC, ZPX, 6), (DCP, ZPX, 6), (CLD, IMP, 2), (CMP, ABY,-4), (NOP, IMP, 2), (DCP, ABY, 7), (NOP, ABX,-4), (CMP, ABX,-4), (DEC, ABX, 7), (DCP, ABX, 7)), // Dx
    Array((CPX, IMM, 2), (SBC, IZX, 6), (NOP, IMM, 2), (ISC, IZX, 8), (CPX, ZP , 3), (SBC, ZP , 3), (INC, ZP , 5), (ISC, ZP , 5), (INX, IMP, 2), (SBC, IMM, 2), (NOP, IMP, 2), (SBC, IMM, 2), (CPX, ABS, 4), (SBC, ABS, 4), (INC, ABS, 6), (ISC, ABS, 6)), // Ex
    Array((BEQ, REL,-2), (SBC, IZY,-5), (KIL, IMP, 0), (ISC, IZY, 8), (NOP, ZPX, 4), (SBC, ZPX, 4), (INC, ZPX, 6), (ISC, ZPX, 6), (SED, IMP, 2), (SBC, ABY,-4), (NOP, IMP, 2), (ISC, ABY, 7), (NOP, ABX,-4), (SBC, ABX,-4), (INC, ABX, 7), (ISC, ABX, 7))  // Fx
    )

  private[this] val OP_Map = { // K=OP_MNEMONIC V=Seq[(code,Mode)]
    val codes = for (
      r <- 0 to 0xf;
      c <- 0 to 0xf
    ) yield {
      val op = OP_MATRIX(r)(c)
      (op._1.toString.toUpperCase, op._2, (r << 4) | c)
    }
    codes.groupBy(_._1) map { case (k, v) => k -> (v map { e => (e._3, e._2) }) }
  }

  def findCode(op: String, mode: Mode.MODE) = {
    OP_Map get op match {
      case Some(list) =>
        list find { case (_, m) => m == mode } map { case (c, _) => c }
      case None => None
    }
  }
  
  @inline private[cpu] def opcode(mem:Memory,address: Int) = {
    val opcode = mem.read(address) & 0xff
    val col = opcode & 0x0f
    val row = (opcode & 0xf0) >> 4
    OP_MATRIX(row)(col)
  }
  
  case class DisassembledInfo(address: Int, op: String, ind: String, bytes: Array[Int]) {
    val len = bytes.length
    override def toString = {
      val addressString = hex4(address)
      val bytesString = bytes map hex2 mkString " "
      "%s  %-8s  %s %s".format(addressString, bytesString, op, ind)
    }
  }  
  
  def disassemble(mem:Memory,address: Int) = {
    val op = opcode(mem,address)
    val (ind, len) = op._2 match {
      case IMP => ("", 0)
      case IMM => ("#$" + hex2(mem.read(address + 1)), 1)
      case REL =>
        val target = address + 2 + mem.read(address + 1).asInstanceOf[Byte]
        ("$" + hex4(target), 1)
      case ABS => ("$" + hex4(readWordFrom(address + 1, mem)), 2)
      case ABX | ABY => ("$" + hex4(readWordFrom(address + 1, mem)) + "," + (if (op._2 == ABX) "X" else "Y"), 2)
      case IND => ("($" + hex4(readWordFrom(address + 1, mem)) + ")", 2)
      case ZP => ("$" + hex2(mem.read(address + 1)), 1)
      case ZPX | ZPY => ("$" + hex2(mem.read(address + 1)) + "," + (if (op._2 == ZPX) "X" else "Y"), 1)
      case IZX => ("($" + hex2(mem.read(address + 1)) + ",X)", 1)
      case IZY => ("($" + hex2(mem.read(address + 1)) + "),Y", 1)
    }
    val bytes = for (a <- address to address + len) yield mem.read(a)
    DisassembledInfo(address, op._1.toString, ind, bytes.toArray)
  }
  
  def make(mem:Memory,id:ChipID.ID = ChipID.CPU) : CPU6510 = new CPU6510Impl(mem,id)
}

trait CPU6510 extends Chip with TraceListener {
  override lazy val componentID = "6510"
  override val componentType = C64ComponentType.CPU
  
  def getPC : Int
  def getCurrentInstructionPC : Int
  def getMem(address:Int) : Int
  def setOverflowFlag  
  def fetchAndExecute : Int
  
  def nmiRequest(low: Boolean)
  def irqRequest(low: Boolean)
  
  //def setBaLow(cycles:Long)
  //def clock(cycles:Long)
}

private[cpu] class CPU6510Impl(mem: Memory,val id : ChipID.ID) extends CPU6510 {
  private[this] val BASE_STACK = 0x0100
  private[this] val IRQ_ROUTINE = 0xfffe
  private[this] val NMI_ROUTINE = 0xfffa
  private[this] val RESET_ROUTINE = 0xfffc
  private[this] val IRQ_CYCLES = 7
  
  // ------------- Tracing --------------------
  private[this] var tracing = false
  private[this] var breakAt = -1
  private[this] var breakCallBack : (String) => Unit = _
  private[this] var stepCallBack : (String) => Unit = _
  private[this] val syncObject = new Object
  // --------------- Registers ----------------
  private[this] val FLAG_# = "00100000".b
  private[this] val N_FLAG = "10000000".b
  private[this] val V_FLAG = "01000000".b
  private[this] val B_FLAG = "00010000".b
  private[this] val NOT_B_FLAG = "11101111".b
  private[this] val D_FLAG = "00001000".b
  private[this] val I_FLAG = "00000100".b
  private[this] val Z_FLAG = "00000010".b
  private[this] val C_FLAG = "00000001".b

  private[this] val A_REG = 0
  private[this] val X_REG = 1
  private[this] val Y_REG = 2
  private[this] val SP_REG = 3
  private[this] val SR_REG = 4
  private[this] val FLAGS = "CZIDB#VN"

  private[this] var PC= 0
  private[this] var CURRENT_OP_PC = 0
  private[this] var SP = 0
  private[this] var A = 0
  private[this] var X = 0
  private[this] var Y = 0
  private[this] var SREG = FLAG_#
  
  // -----------------------------------------
  
  def getPC = PC
  def getCurrentInstructionPC = CURRENT_OP_PC
  def getMem(address:Int) = mem.read(address)

  @inline private[this] def SR = SREG | FLAG_#
  @inline private[this] def SR_=(sr: Int) = SREG = (sr | FLAG_#) & NOT_B_FLAG

  @inline private[this] def sen = SREG |= N_FLAG
  @inline private[this] def cln = SREG &= (~N_FLAG & 0xFF)
  @inline private[this] def sev = SREG |= V_FLAG
  @inline private[this] def clv = SREG &= (~V_FLAG & 0xFF)
  @inline private[this] def seb = SREG |= B_FLAG
  @inline private[this] def clb = SREG &= (~B_FLAG & 0xFF)
  @inline private[this] def sed = SREG |= D_FLAG
  @inline private[this] def cld = SREG &= (~D_FLAG & 0xFF)
  @inline private[this] def sei = SREG |= I_FLAG
  @inline private[this] def cli = SREG &= (~I_FLAG & 0xFF)
  @inline private[this] def sez = SREG |= Z_FLAG
  @inline private[this] def clz = SREG &= (~Z_FLAG & 0xFF)
  @inline private[this] def sec = SREG |= C_FLAG
  @inline private[this] def clc = SREG &= (~C_FLAG & 0xFF)

  @inline private[this] def isNegative = (SREG & N_FLAG) == N_FLAG
  @inline private[this] def isOverflow = (SREG & V_FLAG) == V_FLAG
  @inline private[this] def isBreak = (SREG & B_FLAG) == B_FLAG
  @inline private[this] def isDecimal = (SREG & D_FLAG) == D_FLAG
  @inline private[this] def isInterrupt = (SREG & I_FLAG) == I_FLAG
  @inline private[this] def isZero = (SREG & Z_FLAG) == Z_FLAG
  @inline private[this] def isCarry = (SREG & C_FLAG) == C_FLAG
  
  /**
   * SO of 6502 used by 1541
   */
  def setOverflowFlag = sev
  
  override def getProperties = {
    properties.setProperty("PC",hex4(PC))
    properties.setProperty("A",hex2(A))
    properties.setProperty("X",hex2(X))
    properties.setProperty("Y",hex2(Y))
    properties.setProperty("S",hex2(SP))
    properties.setProperty("NV#BDIZC",sr2String)
    properties
  }

  override def toString = s"PC=${hex4(PC)} AC=${hex2(A)} XR=${hex2(X)} YR=${hex2(Y)} SP=${hex2(SP)} NV#BDIZC=${sr2String}"
  private[this] def sr2String = (for (b <- 7 to 0 by -1) yield {
    if (b == 5) '#'
    else {
      val m = 1 << b
      if ((SREG & m) == m) FLAGS(b) else '-'
    }
  }).mkString
  // -------------------------------------------
  import CPU6510._
  
  /*
  private[this] object Phase extends Enumeration {
    val FETCH = Value
    val EXECUTE = Value
    val WAIT = Value
  }
  */
  
  // cache for performance optimization
  private[this] object OpAddressType extends Enumeration {
    type Type = Value
    val IMPLICIT, IMMEDIATE, ADDRESS = Value
  }
  private[this] var opAdditionalCycles = 0
  private[this] var opLen = 0
  private[this] var opData = 0
  private[this] var opAddressType = OpAddressType.IMPLICIT
  private[this] var opInstruction : Instruction.CODE = null
  private[this] var opCycles = 0
  //private[this] var phase = Phase.FETCH
  //private[this] var waitCycles = 0L
  //private[this] var baLowUntil = 0L
  
  @inline private def setOp(opType:OpAddressType.Type,data:Int,len:Int,addCycles:Int) {
    opAddressType = opType
    opData = data
    opLen = len
    opAdditionalCycles = addCycles
  }
  @inline private def setOpInst(i:Instruction.CODE,c:Int) {
    opInstruction = i
    opCycles = c
  }
  // -------------------------------------------
 
  import Instruction._
  import Mode._
  
  /*
  def setBaLow(cycles:Long) = if (cycles > baLowUntil) baLowUntil = cycles      
  
  def clock(cycles:Long) {    
    
    phase match {
      case Phase.FETCH =>
        if (cycles <= baLowUntil) return
        phase = Phase.EXECUTE
      case Phase.EXECUTE =>
        if (cycles <= baLowUntil) return
        phase = Phase.WAIT
                
        if (breakAt != -1 && PC == breakAt) {
	      breakAt = -1
	      tracing = true
	      breakCallBack()
	    }
	    if (tracing) {
	      syncObject.synchronized {
	        syncObject.wait
	      }
	      stepCallBack(toString)
	    }
    
	    // check interrupts
	    if (nmiOnNegativeEdge) {
	      nmiOnNegativeEdge = false
	      waitCycles = cycles + handleInterrupt(NMI_ROUTINE) - 2
	    } 
	    else 
	    if (irqLow && !isInterrupt) {      
	      waitCycles = cycles + handleInterrupt(IRQ_ROUTINE) - 2
	    }
	    else {
	      if (tracing) Log.debug(formatDebug)
	      
	      val op = opcode(mem,PC)
          PC += 1
          decodeAddressMode(op)
          setOpInst(op._1,op._3)
        // ---------------------
	      CURRENT_OP_PC = PC
	      val operationCode = OPERATIONS(opInstruction.id)
	      if (operationCode == null) throw new IllegalArgumentException("Opcode " + opInstruction + " not implemented")
	      waitCycles = cycles + opCycles + operationCode.run(opAddressType,opData,opLen,opAdditionalCycles) - 2
	    }
      case Phase.WAIT =>
        if (cycles >= waitCycles) phase = Phase.FETCH
    }
  }
  */
  @inline private def fecthAndDecode {
    val op = opcode(mem,PC)
    PC += 1
    decodeAddressMode(op)
    setOpInst(op._1,math.abs(op._3))
  }    

  @inline private def decodeAddressMode(opCell: OperationCell) {
    import Mode._
    
    @inline def pageBoundaryCrossedCycles(address: Int, target: Int) = {
      val addhi = address & 0xff00
      val tahi = target & 0xff00
      if (opCell._3 < 0 && addhi != tahi) 1 else 0
    } 
    
    val mode = opCell._2
    mode match {
      case IMP => //Implicit
        setOp(opType = OpAddressType.IMPLICIT,data = 0,len = 0,addCycles = 0)
      case IMM | REL =>
        setOp(opType = OpAddressType.IMMEDIATE,data = mem.read(PC),len = 1,addCycles = 0)
      case ABS =>
        val address = readWordFrom(PC, mem)
        setOp(opType = OpAddressType.ADDRESS,data = address,len = 2,addCycles = 0)
      case ABX | ABY =>
        val address = readWordFrom(PC, mem)
        val offset = if (mode == ABX) X else Y
        // read from a bad address
        val ah = address & 0xFF00
        val al = address & 0x00FF
        val effectiveAddress = ah | ((al + offset) & 0xFF)
        mem.read(effectiveAddress)
        // good address
        val target = (address + offset) & 0xFFFF
        setOp(opType = OpAddressType.ADDRESS,data = target,len = 2,addCycles = pageBoundaryCrossedCycles(effectiveAddress, target))
      case IND =>
        val address = readWordFrom(PC, mem)
        setOp(opType = OpAddressType.ADDRESS,data = readWordFromWithBUG(address, mem),len = 2,addCycles = 0)
      case ZP =>
        val address = mem.read(PC)
        setOp(opType = OpAddressType.ADDRESS,data = address,len = 1,addCycles = 0)
      case ZPX | ZPY =>
        val address = mem.read(PC)
        val target = (address + (if (mode == ZPX) X else Y)) & 0xff
        setOp(opType = OpAddressType.ADDRESS,data = target,len = 1,addCycles = pageBoundaryCrossedCycles(address, target))
      case IZX =>
        val address = (mem.read(PC) + X) & 0xFF
        val address2 = readWordFromWithBUG(address, mem)
        setOp(opType = OpAddressType.ADDRESS,data = address2,len = 1,addCycles = 0)
      case IZY =>
        val address = mem.read(PC)
        // maybe read from a bad address
        val ah = mem.read(address + 1) << 8
        val al = mem.read(address)
        val effectiveAddress = ah | (al + Y) & 0xFF
        // good address
        //val target = Y + readWordFromWithBUG(address, mem)
        val target = Y + (if (address == 0xFF) (mem.read(0) << 8 | al) else ah | al)
        setOp(opType = OpAddressType.ADDRESS,data = target,len = 1,addCycles = pageBoundaryCrossedCycles(target, effectiveAddress))
    }
  }

  // -------------------------------------------
  private[this] var nmiOnNegativeEdge = false
  private[this] var irqLow = false
  private[this] var nmiLow = false

  private[this] val OPERATIONS = {
    val operations = Array.ofDim[Op](Instruction.MARK.id)
    val availableOperations = Array(
      new KIL,
      // Logical and arithmetic commands
      new ORA, new AND, new EOR, new ADC, new SBC, new CMP(A_REG), new CMP(X_REG), new CMP(Y_REG),
      new INC_DEC(true), new INC_DEC(false),
      new INC_DEXY(true, X_REG), new INC_DEXY(true, Y_REG), new INC_DEXY(false, X_REG), new INC_DEXY(false, Y_REG),
      new ASL_LSR(true), new ASL_LSR(false), new ROL_ROR(true), new ROL_ROR(false),
      // Move commands
      new LDAXY(A_REG), new LDAXY(X_REG), new LDAXY(Y_REG),
      new STAXY(A_REG), new STAXY(X_REG), new STAXY(Y_REG),
      new TAXYS(true, X_REG), new TAXYS(true, Y_REG), new TAXYS(true, SP_REG),
      new TAXYS(false, X_REG), new TAXYS(false, Y_REG), new TAXYS(false, SP_REG),
      new POP_PUSH(true, A_REG), new POP_PUSH(true, SR_REG), new POP_PUSH(false, A_REG), new POP_PUSH(false, SR_REG),
      // Jump/Flag commands
      new Branch(N_FLAG, true), new Branch(N_FLAG, false),
      new Branch(V_FLAG, true), new Branch(V_FLAG, false),
      new Branch(C_FLAG, true), new Branch(C_FLAG, false),
      new Branch(Z_FLAG, true), new Branch(Z_FLAG, false),
      new BRK, new RTI, new RTS, new JSR, new JMP, new BIT, new NOP,
      new SET_CLR_SR(true, C_FLAG), new SET_CLR_SR(false, C_FLAG),
      new SET_CLR_SR(true, D_FLAG), new SET_CLR_SR(false, D_FLAG),
      new SET_CLR_SR(true, I_FLAG), new SET_CLR_SR(false, I_FLAG), new SET_CLR_SR(false, V_FLAG),
      // -----------------
      new DCP,new LAX,new LAXI,new ANC,new SLO, new RLA,new SRE,new RRA,
      new SAX,new ISC,new ALR,new XAA,new AXS,new ARR,new AHX,new SHX,new SHY,new TAS,new LAS)
    for (op <- availableOperations) operations(op.code.id) = op
    operations
  }

  private[this] abstract class Op {
    val OP_R = 0 // read
    val OP_RW = 1 // read/write
    val N_BIT = "10000000".b
    val V_BIT = "01000000".b
    val DATA_IMPLICIT = 0x10000

    val code: Instruction.CODE
    val rw = OP_R

    @inline protected final def checkNZ(src: Int) = {
      val value = src & 0xff
      if (value >= 0x80) sen else cln
      if (value == 0) sez else clz
    }
    
    final def getData(opAddressType:OpAddressType.Type,opData:Int) = {
      import OpAddressType._
      opAddressType match {
        case IMPLICIT => DATA_IMPLICIT
        case IMMEDIATE => opData
        case ADDRESS => if (rw == OP_R) mem.read(opData) else opData
      }
    }

    def run(opAddressType:OpAddressType.Type,opData:Int,opLen:Int,addCycles:Int): Int = {
      val data = getData(opAddressType,opData)
      PC += opLen
      execute(data) + addCycles
    }

    def execute(data: Int): Int
  }     

  private[this] abstract class StdOp extends Op {
    final def execute(data: Int) = {
      exe(data)
      0
    }
    def exe(data: Int)
  }    
  
  private[this] abstract class CompositeOp(ops:StdOp*) extends StdOp {
    override final def run(opAddressType:OpAddressType.Type,opData:Int,opLen:Int,addCycles:Int): Int = {
      var i = 0
      while (i < ops.length) {
        val data = ops(i).getData(opAddressType,opData)
        ops(i).exe(data)
        i += 1
      }
      PC += opLen
      addCycles
    }
    
    def exe(data:Int) {}    
  }

  // --------------------------- Instructions implementation -------------------
  private[this] class KIL extends StdOp {
    val code = KIL
    final def exe(data: Int) = throw new CPUJammedException
  }
  // Logical and arithmetic commands
  private[this] class ADC extends StdOp {
    val code = ADC
    final def exe(data: Int) = {
      var tmp = A + data + (if (isCarry) 1 else 0)
      if ((tmp & 0xFF) == 0) sez else clz
      
      if (isDecimal) {
        tmp = (A & 0xf) + (data & 0xf) + (if (isCarry) 1 else 0)
        if (tmp > 0x9) tmp += 0x6
        if (tmp <= 0x0f) tmp = (tmp & 0xf) + (A & 0xf0) + (data & 0xf0)
        else tmp = (tmp & 0xf) + (A & 0xf0) + (data & 0xf0) + 0x10
        if ((((A ^ data) & 0x80) == 0) && (((A ^ tmp) & 0x80) != 0)) sev else clv
        if ((tmp & 0x80) > 0) sen else cln
        if ((tmp & 0x1f0) > 0x90) tmp += 0x60
        if (tmp > 0x99) sec else clc
        A = tmp & 0xff
      } else {
//        val tmp = A + data + (if (isCarry) 1 else 0)
        if ((((A ^ data) & 0x80) == 0) && (((A ^ tmp) & 0x80) != 0)) sev else clv
        if (tmp > 0xff) sec else clc
        A = tmp & 0xff
        checkNZ(A)
      }
    }
  }
  private[this] class SBC extends StdOp {
    val code = SBC
    final def exe(data: Int) = {
      var tmp = A - data - (if (isCarry) 0 else 1)
      val nextCarry = tmp >= 0
      tmp = tmp & 0x1ff
      checkNZ(tmp)
      if ((((A ^ tmp) & 0x80) != 0) && (((A ^ data) & 0x80) != 0)) sev else clv
      if (isDecimal) {
        tmp = (A & 0xf) - (data & 0xf) - (if (isCarry) 0 else 1)
        if ((tmp & 0x10) > 0) tmp = ((tmp - 6) & 0xf) | ((A & 0xf0) - (data & 0xf0) - 0x10)
        else tmp = (tmp & 0xf) | ((A & 0xf0) - (data & 0xf0))
        if ((tmp & 0x100) > 0) tmp -= 0x60
      }
      A = tmp & 0xFF
      if (nextCarry) sec else clc
    }
  }
  private[this] class ORA extends StdOp {
    val code = ORA
    final def exe(data: Int) = {
      A |= data
      checkNZ(A)
    }
  }
  private[this] class AND extends StdOp {
    val code = AND
    final def exe(data: Int) = {
      A &= data
      checkNZ(A)
    }
  }
  private[this] class EOR extends StdOp {
    val code = EOR
    final def exe(data: Int) = {
      A ^= data
      checkNZ(A)
    }
  }
  private[this] class CMP(reg: Int) extends StdOp {
    val code = reg match {
      case A_REG => CMP
      case X_REG => CPX
      case Y_REG => CPY
    }
    final def exe(data: Int) = {
      val R = reg match {
        case A_REG => A
        case X_REG => X
        case Y_REG => Y
      }
      val diff = R - data
      if (diff >= 0) sec else clc
      checkNZ(diff)
    }
  }
  private[this] class INC_DEC(isDec: Boolean) extends StdOp {
    val code = if (isDec) DEC else INC
    override val rw = OP_RW
    private[this] val delta = if (isDec) -1 else 1
    final def exe(data: Int) = {
      val d = (mem.read(data) + delta) & 0xff
      mem.write(data, d)
      checkNZ(d)
    }
  }
  private[this] class INC_DEXY(isDec: Boolean, reg: Int) extends StdOp {
    val code = (isDec, reg) match {
      case (true, X_REG) => DEX
      case (true, Y_REG) => DEY
      case (false, X_REG) => INX
      case (false, Y_REG) => INY
      case (_, _) => throw new IllegalArgumentException("Bad INC_DEXY configuration")
    }
    private[this] val delta = if (isDec) -1 else 1
    final def exe(data: Int) = {
      if (reg == X_REG) {
        X = (X + delta) & 0xff
        checkNZ(X)
      } else {
        Y = (Y + delta) & 0xff
        checkNZ(Y)
      }
    }
  }
  private[this] class ASL_LSR(isLeft: Boolean) extends StdOp {
    val code = if (isLeft) ASL else LSR
    override val rw = OP_RW
    final def exe(data: Int) = {
      var value = if (data == DATA_IMPLICIT) A else mem.read(data)
      if (isLeft) {
        if ((value & N_BIT) == N_BIT) sec else clc
        value = (value << 1) & 0xff
      } else {
        if ((value & 1) == 1) sec else clc
        value >>= 1
      }
      if (data == DATA_IMPLICIT) A = value else {
        mem.write(data, value)
      }
      checkNZ(value)
    }
  }
  private[this] class ROL_ROR(isLeft: Boolean) extends StdOp {
    val code = if (isLeft) ROL else ROR
    override val rw = OP_RW
    final def exe(data: Int) = {
      var value = if (data == DATA_IMPLICIT) A else mem.read(data)
      val oldC = if (isCarry) 1 else 0
      if (isLeft) {
        if ((value & N_BIT) == N_BIT) sec else clc
        value = ((value << 1) & 0xff) | oldC
      } else {
        if ((value & 1) == 1) sec else clc
        value = ((value >> 1) & 0xff) | (oldC << 7)
      }
      if (data == DATA_IMPLICIT) A = value else {
        mem.write(data, value)
      }
      checkNZ(value)
    }
  }
  // Move commands
  private[this] class LDAXY(reg: Int) extends StdOp {
    val code = reg match {
      case A_REG => LDA
      case X_REG => LDX
      case Y_REG => LDY
    }
    final def exe(data: Int) = {
      val r = reg match {
        case A_REG =>
          A = data; A
        case X_REG =>
          X = data; X
        case Y_REG => Y = data; Y
      }
      checkNZ(r)
    }
  }
  private[this] class STAXY(reg: Int) extends StdOp {
    override val rw = OP_RW
    val code = reg match {
      case A_REG => STA
      case X_REG => STX
      case Y_REG => STY
    }
    final def exe(data: Int) = {
      reg match {
        case A_REG => mem.write(data, A)
        case X_REG => mem.write(data, X)
        case Y_REG => mem.write(data, Y)
      }
    }
  }
  private[this] class TAXYS(flip: Boolean, reg: Int) extends StdOp {
    val code = (flip, reg) match {
      case (true, X_REG) => TAX
      case (false, X_REG) => TXA
      case (true, Y_REG) => TAY
      case (false, Y_REG) => TYA
      case (true, SP_REG) => TSX
      case (false, SP_REG) => TXS
      case (_, _) => throw new IllegalArgumentException("Bad configuration of TAXYS")
    }
    final def exe(data: Int) = {
      reg match {
        case X_REG =>
          val r = if (flip) { X = A; X } else { A = X; A }
          checkNZ(r)
        case Y_REG =>
          val r = if (flip) { Y = A; Y } else { A = Y; A }
          checkNZ(r)
        case SP_REG =>
          if (flip) {
            X = SP & 0xff
            checkNZ(X)
          }  
          else SP = X
      }
    }
  }
  private[this] class POP_PUSH(isPush: Boolean, reg: Int) extends StdOp {
    val code = (isPush, reg) match {
      case (true, A_REG) => PHA
      case (true, SR_REG) => PHP
      case (false, A_REG) => PLA
      case (false, SR_REG) => PLP
      case (_, _) => throw new IllegalArgumentException("Bad configuration of POP_PUSH")
    }
    final def exe(data: Int) = {
      if (isPush) {
        if (reg == A_REG) push(A) else {
          seb
          push(SR)
          clb
        }
      } else {
        if (reg == A_REG) {
          A = pop
          checkNZ(A)
        } 
        else {
          SR = pop
          clb
        }
      }
    }
  }
  // Jump/Flag commands
  private[this] class Branch(bit: Int, onZero: Boolean) extends Op {
    val code = (bit, onZero) match {
      case (N_FLAG, true) => BPL
      case (N_FLAG, false) => BMI
      case (V_FLAG, true) => BVC
      case (V_FLAG, false) => BVS
      case (C_FLAG, true) => BCC
      case (C_FLAG, false) => BCS
      case (Z_FLAG, false) => BEQ
      case (Z_FLAG, true) => BNE
      case (_, _) => throw new IllegalArgumentException("Bad configuration of Branch")
    }
    final def execute(data: Int) = {
      val flag = bit match {
        case N_FLAG => isNegative
        case V_FLAG => isOverflow
        case C_FLAG => isCarry
        case Z_FLAG => isZero
      }
      val jump = onZero ^ flag
      val from = PC
      val branchCycle = if (jump) {
        PC = PC + data.asInstanceOf[Byte]
        1
      } else 0
      branchCycle + (if (((from ^ PC) & 0xFF00) > 0) 1 else 0)
    }
  }
  private[this] class BRK extends StdOp {
    val code = BRK
    final def exe(data: Int) = {
      PC += 1
      push((PC >> 8) & 0xff)
      push(PC & 0xff)
      seb
      push(SR) // the B flag is just checked directly on the stack
      clb
      sei
      PC = readWordFrom(IRQ_ROUTINE, mem)
    }
  }
  private[this] class RTI extends StdOp {
    val code = RTI
    final def exe(data: Int) = {
      SR = pop
      clb
      val PC_L = pop
      val PC_H = pop
      PC = (PC_H << 8) | PC_L
    }
  }
  private[this] class RTS extends StdOp {
    val code = RTS
    final def exe(data: Int) = {
      val PC_L = pop
      val PC_H = pop
      PC = (PC_H << 8) | PC_L
      PC += 1
    }
  }
  private[this] class JSR extends StdOp {
    val code = JSR
    override val rw = OP_RW
    final def exe(data: Int) = {
      val pcToPush = PC - 1
      push((pcToPush >> 8) & 0xff)
      push(pcToPush & 0xff)
      PC = data
    }
  }
  private[this] class JMP extends StdOp {
    val code = JMP
    override val rw = OP_RW
    final def exe(data: Int) = PC = data
  }
  private[this] class BIT extends StdOp {
    val code = BIT
    final def exe(data: Int) = {
      if (data >= 0x80) sen else cln
      if ((data & V_BIT) == V_BIT) sev else clv
      if ((A & data) == 0) sez else clz
    }
  }
  private[this] class SET_CLR_SR(isSet: Boolean, bit: Int) extends StdOp {
    val code = (isSet, bit) match {
      case (true, C_FLAG) => SEC
      case (false, C_FLAG) => CLC
      case (true, D_FLAG) => SED
      case (false, D_FLAG) => CLD
      case (true, I_FLAG) => SEI
      case (false, I_FLAG) => CLI
      case (false, V_FLAG) => CLV
      case (_, _) => throw new IllegalArgumentException("Bad configuration of SET_CLR_SR")	    
    }
    final def exe(data: Int) = {
      bit match {
        case C_FLAG => if (isSet) sec else clc
        case D_FLAG => if (isSet) sed else cld
        case I_FLAG => if (isSet) sei else cli
        case V_FLAG => clv
      }
    }
  }
  private[this] class NOP extends StdOp {
    val code = NOP
    final def exe(data: Int) {}
  }
  // ----------------------- EXTENDED OPCODES ----------------------------------
  private[this] class DCP extends CompositeOp(new INC_DEC(true),new CMP(A_REG)) { val code = DCP }
  private[this] class LAX extends CompositeOp(new LDAXY(A_REG),new LDAXY(X_REG)) { val code = LAX }
  private[this] class SLO extends CompositeOp(new ASL_LSR(true),new ORA) { val code = SLO }
  private[this] class RLA extends CompositeOp(new ROL_ROR(true),new AND) { val code = RLA }
  private[this] class SRE extends CompositeOp(new ASL_LSR(false),new EOR) { val code = SRE }
  private[this] class RRA extends CompositeOp(new ROL_ROR(false),new ADC) { val code = RRA }
  private[this] class ISC extends CompositeOp(new INC_DEC(false),new SBC) { val code = ISC }  
  //private[this] class XAA extends CompositeOp(new TAXYS(false,X_REG),new AND) { val code = XAA }
  
  private[this] class LAXI extends StdOp {	// LAX imm
    val code = LAXI
    final def exe(data: Int) = {
      A = (A | 0xEE) & data
      X = A
      checkNZ(A)
    }
  }
  
  private[this] class XAA extends StdOp {	// ANE
    val code = XAA
    final def exe(data: Int) = {
      A = (A | 0xEE) & X & data
      checkNZ(A)
    }
  }
  
  private[this] class LAS extends StdOp {
    val code = LAS
    final def exe(data: Int) = {
      val tmp = data & SP
      A = tmp
      X = tmp
      SP = tmp
      checkNZ(A)
    }
  }
  private[this] class TAS extends StdOp {
    val code = TAS
    override val rw = OP_RW
    final def exe(data: Int) = {
      mem.write(data,A & X & (data >> 8 + 1))
      SP = A & X
    }
  }
  private[this] class AHX extends StdOp {
    val code = AHX
    override val rw = OP_RW
    final def exe(data: Int) = {
      mem.write(data,A & X & (data >> 8 + 1))      
    }
  }
  private[this] class SHX extends StdOp {
    val code = SHX
    override val rw = OP_RW
    final def exe(data: Int) = {
      mem.write(data,X & (data >> 8 + 1))
    }
  }
   private[this] class SHY extends StdOp {
    val code = SHY
    override val rw = OP_RW
    final def exe(data: Int) = {
      mem.write(data,Y & (data >> 8 + 1))
    }
  }
  private[this] class ARR extends StdOp {
    val code = ARR
    final def exe(_data: Int) = {
      val data = _data & A
      A = if (isCarry) (data >> 1) | 0x80 else data >> 1
      if (!isDecimal) {
        checkNZ(A)
        if ((A & 0x40) > 0) sec else clc
        if (((A & 0x40) ^ ((A & 0x20) << 1)) > 0) sev else clv
      }
      else {
        if (isCarry) sen else cln
        if (A == 0) sez else clz
        if (((data ^ A) & 0x40) != 0) sev else clv
        if ((data & 0x0F) + (data & 0x01) > 5) A = A & 0xF0 | (A + 6) & 0x0F
        if (((data + (data & 0x10)) & 0x1F0) > 0x50) {
          sec
          A += 0x60
        } 
        else clc
      }
    }
  }
  private[this] class AXS extends StdOp {
    val code = AXS
    final def exe(data: Int) = {
      val tmp = (A & X) - data
      if (tmp >= 0) sec else clc
      X = tmp & 0xFF
      checkNZ(X)
    }
  }
  private[this] class ANC extends StdOp {
    val code = ANC
    final def exe(data: Int) = {
      A &= data
      checkNZ(A)
      if ((A & 0x80) != 0) sec else clc
    }
  }
  private[this] class SAX extends StdOp {
    val code = SAX
    override val rw = OP_RW
    final def exe(data: Int) = {
      mem.write(data,A & X)
    }
  }
  private[this] class ALR extends StdOp {
    val code = ALR
    final def exe(data: Int) = {
      A = A & data
      if ((A & 1) == 1) sec else clc
      A >>= 1
      checkNZ(A)
    }
  }
  // ---------------------------------------------------------------------------
  def init = reset
  
  def reset {
    PC = readWordFrom(RESET_ROUTINE, mem)
    irqLow = false
    nmiLow = false
    nmiOnNegativeEdge = false
    A = 0
    X = 0
    Y = 0
    SR = 0
    SP = 0
    Log.info(s"CPU reset! PC = ${hex4(PC)}")
  }
  
  final def fetchAndExecute : Int = {
    if (breakAt != -1 && PC == breakAt) {
      breakAt = -1
      tracing = true
      breakCallBack(toString)
    }
    
    // check interrupts
    if (nmiOnNegativeEdge) {
      nmiOnNegativeEdge = false
      handleInterrupt(NMI_ROUTINE)
    } 
    else 
    if (irqLow && !isInterrupt) {      
      handleInterrupt(IRQ_ROUTINE)
    }
    else {
      if (tracing) Log.debug(formatDebug)
    
      CURRENT_OP_PC = PC
      fecthAndDecode    
      val opcode = OPERATIONS(opInstruction.id)
      if (opcode == null) throw new IllegalArgumentException("Opcode " + opInstruction + " not implemented")
      val totalCycles = opCycles + opcode.run(opAddressType,opData,opLen,opAdditionalCycles)
      if (tracing) {
        syncObject.synchronized { syncObject.wait }
        stepCallBack(toString)
      }
      totalCycles - 1 // 1 cycles has been consumed now
    }
  }
  
  protected def formatDebug = s"[${id.toString}] ${disassemble(mem,PC).toString}"

  @inline private def push(data: Int) {
    mem.write(BASE_STACK | SP, data)
    SP = (SP - 1) & 0xff
  }

  @inline private def pop = {
    SP = (SP + 1) & 0xff
    mem.read(BASE_STACK | SP)
  }

  @inline private def handleInterrupt(address: Int) = {
    push((PC >> 8) & 0xff)
    push(PC & 0xff)
    push(SR)
    sei
    PC = readWordFrom(address, mem)
    if (tracing) Log.debug(s"Handling interrupt. Jumping to ${hex4(address)}")
    IRQ_CYCLES
  }

  final def isIRQLow = irqLow
  final def isNMILow = nmiLow

  final def irqRequest(low: Boolean) {
    if (tracing) Log.debug(s"IRQ request low=${low}")
    irqLow = low
  }

  final def nmiRequest(low: Boolean) {
    if (!nmiLow && low) {
      nmiOnNegativeEdge = true
      if (tracing) Log.debug("NMI request on negative edge")
    }
    nmiLow = low
  }
  
  def dumpCurrentPC {
    Log.info(disassemble(mem, CURRENT_OP_PC).toString)
  }
  
  // TRACING ---------------------------------------------
  def setTrace(traceOn:Boolean) = tracing = traceOn
  
  def step(updateRegisters: (String) => Unit) {
    stepCallBack = updateRegisters
    syncObject.synchronized {
      syncObject.notify
    }
  }
  def setBreakAt(address:Int,callback:(String) => Unit) {
    tracing = false
    breakCallBack = callback
    breakAt = address
  }
  def jmpTo(pc:Int) {
    PC = pc
  }
}