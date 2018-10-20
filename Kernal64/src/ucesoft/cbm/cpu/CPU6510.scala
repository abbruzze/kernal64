package ucesoft.cbm.cpu

import ucesoft.cbm.ChipID
import ucesoft.cbm.Chip
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.ChipID

object CPU6510 {
  class CPUJammedException(val cpuID:ChipID.ID,val pcError:Int) extends Exception
  
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
  private[cpu] val OP_MATRIX: Array[Array[OperationCell]] = Array(
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
    OP_Map get op.toUpperCase match {
      case Some(list) =>
        list find { case (_, m) => m == mode } map { case (c, _) => c }
      case None => None
    }
  }
  
  def hasMode(op:String,m:Mode.MODE) : Boolean = {
    OP_Map get op.toUpperCase match {
      case None => false
      case Some(l) => l map { _._2 } exists { _ == m }
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
  
  def make(mem:Memory,id:ChipID.ID = ChipID.CPU) : CPU6510 = new CPU6510_CE(mem,id)
}

trait CPU6510 extends Chip with TraceListener {
  override lazy val componentID = "6510"
  override val componentType = CBMComponentType.CPU
  
  def getPC : Int
  def getCurrentInstructionPC : Int
  def getMem(address:Int) : Int
  def setOverflowFlag  
  def fetchAndExecute(cycles:Int)
  
  def nmiRequest(low: Boolean)
  def irqRequest(low: Boolean)
  
  def setBaLow(low:Boolean) {}
  def setDMA(dma:Boolean) {}
  def isFetchingInstruction : Boolean
}