package ucesoft.cbm.cpu

import scala.language.implicitConversions
import ucesoft.cbm.ChipID
import ucesoft.cbm.trace.TraceListener
import java.io.PrintWriter
import ucesoft.cbm.trace.BreakType
import ucesoft.cbm.Log
import ucesoft.cbm.Chip
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame

object Z80 {
  @inline def hex2(data: Int) = "%02X".format(data & 0xffff)
  @inline def hex4(data: Int) = "%04X".format(data & 0xffff)
  @inline def WORD(h:Int,l:Int) = ((h << 8) | l) & 0xFFFF
  
  trait IOMemory {
    def in(addressHI:Int,addressLO:Int) : Int
    def out(addressHI:Int,addressLO:Int,value:Int)
  }
  
  object EmptyIOMemory extends IOMemory {
    def in(addressHI:Int,addressLO:Int) = 0
    def out(addressHI:Int,addressLO:Int,value:Int) {}
  }
    
  class Context(val mem:Memory,val io:IOMemory) {    
    var A1,F1,H1,L1,D1,E1,B1,C1 = 0
    var halted = false
    var im = 0
    var A = 0
    var B = 0
    var C = 0
    var D = 0
    var E = 0
    var F = 0
    var H = 0
    var L = 0
    var I = 0
    var R = 0
    var IX = 0
    var IY = 0
    var IFF1 = 0
    var IFF2 = 0
    var PC = 0
    var SP = 0
    private[this] var delayInt = false
    private[this] var additionalClockCycles = 0
    
    // state
	  def saveState(out:ObjectOutputStream) {
      out.writeInt(im)
	  	out.writeInt(A1)
	  	out.writeInt(B1)
	  	out.writeInt(C1)
	  	out.writeInt(D1)
	  	out.writeInt(E1)
	  	out.writeInt(F1)
	  	out.writeInt(H1)
	  	out.writeInt(L1)
	  	out.writeInt(A)
	  	out.writeInt(B)
	  	out.writeInt(C)
	  	out.writeInt(D)
	  	out.writeInt(E)
	  	out.writeInt(F)
	  	out.writeInt(H)
	  	out.writeInt(L)
	  	out.writeInt(I)
	  	out.writeInt(R)
	  	out.writeInt(IX)
	  	out.writeInt(IY)
	  	out.writeInt(IFF1)
	  	out.writeInt(IFF2)
	  	out.writeInt(PC)
	  	out.writeInt(SP)
	  	out.writeBoolean(halted)
	  	out.writeBoolean(delayInt)
	  	out.writeInt(additionalClockCycles)
	  }
	  def loadState(in:ObjectInputStream) {
	    im = in.readInt
	  	A1 = in.readInt
	  	B1 = in.readInt
	  	C1 = in.readInt
	  	D1 = in.readInt
	  	E1 = in.readInt
	  	F1 = in.readInt
	  	H1 = in.readInt
	  	L1 = in.readInt
	  	A = in.readInt
	  	B = in.readInt
	  	C = in.readInt
	  	D = in.readInt
	  	E = in.readInt
	  	F = in.readInt
	  	H = in.readInt
	  	L = in.readInt
	  	I = in.readInt
	  	R = in.readInt
	  	IX = in.readInt
	  	IY = in.readInt
	  	IFF1 = in.readInt
	  	IFF2 = in.readInt
	  	PC = in.readInt
	  	SP = in.readInt
	  	halted = in.readBoolean
	  	delayInt = in.readBoolean
	  	additionalClockCycles = in.readInt
	  }
    
    def setAdditionalClockCycles(acs:Int) = additionalClockCycles = acs
    def getAdditionalClockSycles = {
      val acs = additionalClockCycles
      additionalClockCycles = 0
      acs
    }
    
    def mustDelayInt = {
      val di = delayInt
      delayInt = false
      di
    }    
    def setDelayInt = delayInt = true
    
    // ========================================= FLAGS =========================================================
    // pre calculated Sign, Zero and Parity flags for 0-255 values
    val SZP = Array(
        PFLAG | ZFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             PFLAG,               0,               0,          PFLAG,
             PFLAG,               0,               0,          PFLAG,
                  0,          PFLAG,          PFLAG,               0,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
             SFLAG, SFLAG | PFLAG, SFLAG | PFLAG,          SFLAG,
    SFLAG | PFLAG,          SFLAG,          SFLAG, SFLAG | PFLAG
    )
    /*
    | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |   
    |---|---|---|---|---|---|---|---|
    | S | Z |   | H |   |P/V| N | C |
    |---|---|---|---|---|---|---|---|
    */    
    final private[this] val CFLAG = 0x1
    final private[this] val NFLAG = 0x2
    final private[this] val PFLAG = 0x4
    final private[this] val HFLAG = 0x10
    final private[this] val ZFLAG = 0x40
    final private[this] val SFLAG = 0x80
    
    def zero = (F & ZFLAG)
    def sign = (F & SFLAG)
    def carry = (F & CFLAG)
    def negative = (F & NFLAG)
    def parity = (F & PFLAG)
    def half = (F & HFLAG)
    
    def setZero(set:Boolean) = if (set) F |= ZFLAG else F &= ~ZFLAG
    def setSign(set:Boolean) = if (set) F |= SFLAG else F &= ~SFLAG
    def setCarry(set:Boolean) = if (set) F |= CFLAG else F &= ~CFLAG
    def setNegative(set:Boolean) = if (set) F |= NFLAG else F &= ~NFLAG
    def setParity(set:Boolean) = if (set) F |= PFLAG else F &= ~PFLAG
    def setHalf(set:Boolean) = if (set) F |= HFLAG else F &= ~HFLAG
    
    // =========================================================================================================
    
    def AF1 = WORD(A1,F1)
    def HL1 = WORD(H1,L1)
    def DE1 = WORD(D1,E1)
    def BC1 = WORD(B1,C1)
    
    def AF = WORD(A,F)
    def HL = WORD(H,L)
    def BC = WORD(B,C)
    def DE = WORD(D,E)
    def AF_=(w:Int) = {
      A = (w >> 8) & 0xFF
      F = w & 0xFF
    }
    def HL_=(w:Int) = {
      H = (w >> 8) & 0xFF
      L = w & 0xFF
    }
    def BC_=(w:Int) {
      B = (w >> 8) & 0xFF
      C = w & 0xFF
    }
    def DE_=(w:Int) {
      D = (w >> 8) & 0xFF
      E = w & 0xFF
    }
    def AF1_=(w:Int) = {
      A1 = (w >> 8) & 0xFF
      F1 = w & 0xFF
    }
    def HL1_=(w:Int) = {
      H1 = (w >> 8) & 0xFF
      L1 = w & 0xFF
    }
    def BC1_=(w:Int) {
      B1 = (w >> 8) & 0xFF
      C1 = w & 0xFF
    }
    def DE1_=(w:Int) {
      D1 = (w >> 8) & 0xFF
      E1 = w & 0xFF
    }
    def incDecBC(inc:Boolean) {
      if (inc) {
        C += 1
        if (C == 0x100) {
          C = 0
          B = (B + 1) & 0xFF
        }
      }
      else {
        C -= 1
        if (C == -1) {
          C = 0xFF
          B = (B - 1) & 0xFF
        }
      }
    }
    def incDecDE(inc:Boolean) {
      if (inc) {
        E += 1
        if (E == 0x100) {
          E = 0
          D = (D + 1) & 0xFF
        }
      }
      else {
        E -= 1
        if (E == -1) {
          E = 0xFF
          D = (D - 1) & 0xFF
        }
      }
    }
    def incDecHL(inc:Boolean) {
      if (inc) {
        L += 1
        if (L == 0x100) {
          L = 0
          H = (H + 1) & 0xFF
        }
      }
      else {
        L -= 1
        if (L == -1) {
          L = 0xFF
          H = (H - 1) & 0xFF
        }
      }
    }
    
    def incDecSP(inc:Boolean) {
      if (inc) SP = (SP + 1) & 0xFFFF else SP = (SP - 1) & 0xFFFF
    }
    def incDecIX(inc:Boolean) {
      if (inc) IX = (IX + 1) & 0xFFFF else IX = (IX - 1) & 0xFFFF
    }
    def incDecIY(inc:Boolean) {
      if (inc) IY = (IY + 1) & 0xFFFF else IY = (IY - 1) & 0xFFFF
    }
    
    def IX_+(d:Int) = IX + d.asInstanceOf[Byte]
    def IY_+(d:Int) = IY + d.asInstanceOf[Byte]
         
    def EX_AF {
      var tmp = A
      A = A1
      A1 = tmp
      tmp = F
      F = F1
      F1 = tmp
    }
    
    def EX_DE_HL {
      var tmp = D
      D = H
      H = tmp
      tmp = E
      E = L
      L = tmp
    }
    
    def EXX {
      // BC <=> BC'
      var tmp = B
      B = B1
      B1 = tmp
      tmp = C
      C = C1
      C1 = tmp
      // DE <=> DE'
      tmp = D
      D = D1
      D1 = tmp
      tmp = E
      E = E1
      E1 = tmp
      // HL <=> HL'
      tmp = H
      H = H1
      H1 = tmp
      tmp = L
      L = L1
      L1 = tmp
    }
    
    def IXL = IX & 0xFF
    
    def push(w:Int) {
      SP = (SP - 1) & 0xFFFF
      mem.write(SP,(w >> 8) & 0xFF,ChipID.CPU)
      SP = (SP - 1) & 0xFFFF
      mem.write(SP,w & 0xFF,ChipID.CPU)
    }
    
    def pop = {
      var popped = mem.read(SP)
      SP = (SP + 1) & 0xFFFF
      popped |= mem.read(SP) << 8
      SP = (SP + 1) & 0xFFFF
      popped
    }
    
    def byte(offset:Int) = mem.read(PC + offset)
    def word(offset:Int) = mem.read(PC + offset + 1) << 8 | mem.read(PC + offset)
    
    def read(address:Int) = {val value = mem.read(address) ; if (value < 0) throw new IllegalArgumentException("R <0") ; value}
    def readW(address:Int) = mem.read(address) | mem.read(address + 1) << 8
    def write(address:Int,value:Int) = {mem.write(address,value,ChipID.CPU) ; if (value < 0) throw new IllegalArgumentException("W <0") }
    def writeW(address:Int,value:Int) = {
      mem.write(address,value & 0xFF,ChipID.CPU)
      mem.write(address + 1,(value >> 8) & 0xFF,ChipID.CPU)
    }
    
    def reset {
      AF = 0xFFFF
      SP = 0xFFFF
      PC = 0
      IFF1 = 0
      IFF2 = 0
      im = 0
      halted = false
      IX = 0
      IY = 0
      BC = 0
      DE = 0
      HL = 0
      AF1 = 0
      BC1 = 0
      DE1 = 0
      HL1 = 0
      I = 0
      R = 0
    }
    
    override def toString = s"PC=${hex4(PC)} AF=${hex4(AF)} BC=${hex4(BC)} DE=${hex4(DE)} HL=${hex4(HL)} IX=${hex4(IX)} IY=${hex4(IY)} I=${hex2(I)} im=$im SP=${hex2(SP)} SZ#H#PNC=${sr2String}"
    @inline private def sr2String = {
      val sb = new StringBuilder
      if (sign > 0) sb += 'S' else sb += '-'
      if (zero > 0) sb += 'Z' else sb += '-'
      sb += '#'
      if (half > 0) sb += 'H' else sb += '-'
      sb += '#'
      if (parity > 0) sb += 'P' else sb += '-'
      if (carry > 0) sb += 'C' else sb += '-'
      sb.toString
    }
  }
  
  implicit def int2Array(x:Int) = Array(x)
  implicit def tuple2Array(x:Tuple2[Int,Int]) = Array(x._1,x._2)
  implicit def tuple3Array(x:Tuple3[Int,Int,Int]) = Array(x._1,x._2,x._3)
  implicit def string2Mnem(s:String) = (m:Memory,p:Int) => s
  
  case class Opcode(opcodes:Array[Int],
                    cycles:Int,
                    size:Int,                    
                    getMnemonic : (Memory,Int) => String,
                    modifyPC:Boolean = false)(val executeFunction:(Context) => Unit) {
    def disassemble(mem:Memory,pc:Int) : String = {
      val sb = new StringBuilder(hex4(pc) + "  ")
      var s = pc
      while (s < pc + size) {
        sb.append(hex2(mem.read(s)) + " ")
        s += 1
      }
      var spaces = 20 - sb.length
      while (spaces > 0) {
        sb += ' '
        spaces -= 1
      }
      sb.toString + getMnemonic(mem,pc)
    }
  }
  
  val opcodes_1,
      opcodes_ed,
      opcodes_cb,
      opcodes_dd,
      opcodesFd,
//      opcodes_ddcb,
//      opcodesFdcb,
      opcodes_ddcb4,
      opcodesFdcb4 = Array.ofDim[Opcode](256)
  
  // ================================== LOAD 8 bit ===========================================================
  val LD_A_I = Opcode((0xED,0x57),9,2,"LD A,I") { ctx =>
    ctx.A = ctx.I
    ctx.F = ctx.SZP(ctx.A) | ctx.carry
    ctx.setParity(ctx.IFF2 > 0)
  }
  val LD_A_R = Opcode((0xED,0x5F),9,2,"LD A,R") { ctx =>
    ctx.A = ctx.R    
    ctx.F = ctx.SZP(ctx.A) | ctx.carry
    ctx.setParity(ctx.IFF2 > 0)
  }
  // *** LD r,r'
  // **************
  val LD_A_A = Opcode(0x7F,4,1,"LD A,A") { ctx => ctx.A = ctx.A }
  val LD_A_B = Opcode(0x78,4,1,"LD A,B") { ctx => ctx.A = ctx.B }
  val LD_A_C = Opcode(0x79,4,1,"LD A,C") { ctx => ctx.A = ctx.C }
  val LD_A_D = Opcode(0x7A,4,1,"LD A,D") { ctx => ctx.A = ctx.D }
  val LD_A_E = Opcode(0x7B,4,1,"LD A,E") { ctx => ctx.A = ctx.E }
  val LD_A_H = Opcode(0x7C,4,1,"LD A,F") { ctx => ctx.A = ctx.H }
  val LD_A_L = Opcode(0x7D,4,1,"LD A,L") { ctx => ctx.A = ctx.L }
  //
  val LD_B_A = Opcode(0x47,4,1,"LD B,A") { ctx => ctx.B = ctx.A }
  val LD_B_B = Opcode(0x40,4,1,"LD B,B") { ctx => ctx.B = ctx.B }
  val LD_B_C = Opcode(0x41,4,1,"LD B,C") { ctx => ctx.B = ctx.C }
  val LD_B_D = Opcode(0x42,4,1,"LD B,D") { ctx => ctx.B = ctx.D }
  val LD_B_E = Opcode(0x43,4,1,"LD B,E") { ctx => ctx.B = ctx.E }
  val LD_B_H = Opcode(0x44,4,1,"LD B,F") { ctx => ctx.B = ctx.H }
  val LD_B_L = Opcode(0x45,4,1,"LD B,L") { ctx => ctx.B = ctx.L }
  //
  val LD_C_A = Opcode(0x4F,4,1,"LD C,A") { ctx => ctx.C = ctx.A }
  val LD_C_B = Opcode(0x48,4,1,"LD C,B") { ctx => ctx.C = ctx.B }
  val LD_C_C = Opcode(0x49,4,1,"LD C,C") { ctx => ctx.C = ctx.C }
  val LD_C_D = Opcode(0x4A,4,1,"LD C,D") { ctx => ctx.C = ctx.D }
  val LD_C_E = Opcode(0x4B,4,1,"LD C,E") { ctx => ctx.C = ctx.E }
  val LD_C_H = Opcode(0x4C,4,1,"LD C,F") { ctx => ctx.C = ctx.H }
  val LD_C_L = Opcode(0x4D,4,1,"LD C,L") { ctx => ctx.C = ctx.L }
  //
  val LD_D_A = Opcode(0x57,4,1,"LD D,A") { ctx => ctx.D = ctx.A }
  val LD_D_B = Opcode(0x50,4,1,"LD D,B") { ctx => ctx.D = ctx.B }
  val LD_D_C = Opcode(0x51,4,1,"LD D,C") { ctx => ctx.D = ctx.C }
  val LD_D_D = Opcode(0x52,4,1,"LD D,D") { ctx => ctx.D = ctx.D }
  val LD_D_E = Opcode(0x53,4,1,"LD D,E") { ctx => ctx.D = ctx.E }
  val LD_D_H = Opcode(0x54,4,1,"LD D,F") { ctx => ctx.D = ctx.H }
  val LD_D_L = Opcode(0x55,4,1,"LD D,L") { ctx => ctx.D = ctx.L }
  //
  val LD_E_A = Opcode(0x5F,4,1,"LD E,A") { ctx => ctx.E = ctx.A }
  val LD_E_B = Opcode(0x58,4,1,"LD E,B") { ctx => ctx.E = ctx.B }
  val LD_E_C = Opcode(0x59,4,1,"LD E,C") { ctx => ctx.E = ctx.C }
  val LD_E_D = Opcode(0x5A,4,1,"LD E,D") { ctx => ctx.E = ctx.D }
  val LD_E_E = Opcode(0x5B,4,1,"LD E,E") { ctx => ctx.E = ctx.E }
  val LD_E_H = Opcode(0x5C,4,1,"LD E,F") { ctx => ctx.E = ctx.H }
  val LD_E_L = Opcode(0x5D,4,1,"LD E,L") { ctx => ctx.E = ctx.L }
  //
  val LD_H_A = Opcode(0x67,4,1,"LD H,A") { ctx => ctx.H = ctx.A }
  val LD_H_B = Opcode(0x60,4,1,"LD H,B") { ctx => ctx.H = ctx.B }
  val LD_H_C = Opcode(0x61,4,1,"LD H,C") { ctx => ctx.H = ctx.C }
  val LD_H_D = Opcode(0x62,4,1,"LD H,D") { ctx => ctx.H = ctx.D }
  val LD_H_E = Opcode(0x63,4,1,"LD H,E") { ctx => ctx.H = ctx.E }
  val LD_H_H = Opcode(0x64,4,1,"LD H,F") { ctx => ctx.H = ctx.H }
  val LD_H_L = Opcode(0x65,4,1,"LD H,L") { ctx => ctx.H = ctx.L }
  //
  val LD_L_A = Opcode(0x6F,4,1,"LD L,A") { ctx => ctx.L = ctx.A }
  val LD_L_B = Opcode(0x68,4,1,"LD L,B") { ctx => ctx.L = ctx.B }
  val LD_L_C = Opcode(0x69,4,1,"LD L,C") { ctx => ctx.L = ctx.C }
  val LD_L_D = Opcode(0x6A,4,1,"LD L,D") { ctx => ctx.L = ctx.D }
  val LD_L_E = Opcode(0x6B,4,1,"LD L,E") { ctx => ctx.L = ctx.E }
  val LD_L_H = Opcode(0x6C,4,1,"LD L,F") { ctx => ctx.L = ctx.H }
  val LD_L_L = Opcode(0x6D,4,1,"LD L,L") { ctx => ctx.L = ctx.L }
  // *** LD r,n
  // **************
  private def MNEMONIC_n(pattern:String) = (m:Memory,PC:Int) => pattern.format(hex2(m.read(PC + 1)))
  val LD_A_n = Opcode(0x3E,7,2,MNEMONIC_n("LD A,%s")) { ctx => ctx.A = ctx.byte(1) }
  val LD_B_n = Opcode(0x06,7,2,MNEMONIC_n("LD B,%s")) { ctx => ctx.B = ctx.byte(1) }
  val LD_C_n = Opcode(0x0E,7,2,MNEMONIC_n("LD C,%s")) { ctx => ctx.C = ctx.byte(1) }
  val LD_D_n = Opcode(0x16,7,2,MNEMONIC_n("LD D,%s")) { ctx => ctx.D = ctx.byte(1) }
  val LD_E_n = Opcode(0x1E,7,2,MNEMONIC_n("LD E,%s")) { ctx => ctx.E = ctx.byte(1) }
  val LD_H_n = Opcode(0x26,7,2,MNEMONIC_n("LD H,%s")) { ctx => ctx.H = ctx.byte(1) }
  val LD_L_n = Opcode(0x2E,7,2,MNEMONIC_n("LD L,%s")) { ctx => ctx.L = ctx.byte(1) }
  // *** LD r,(HL)
  // **************
  val LD_A_$HL$ = Opcode(0x7E,7,1,"LD A,(HL)") { ctx => ctx.A = ctx.read(ctx.HL) }
  val LD_B_$HL$ = Opcode(0x46,7,1,"LD B,(HL)") { ctx => ctx.B = ctx.read(ctx.HL) }
  val LD_C_$HL$ = Opcode(0x4E,7,1,"LD C,(HL)") { ctx => ctx.C = ctx.read(ctx.HL) }
  val LD_D_$HL$ = Opcode(0x56,7,1,"LD D,(HL)") { ctx => ctx.D = ctx.read(ctx.HL) }
  val LD_E_$HL$ = Opcode(0x5E,7,1,"LD E,(HL)") { ctx => ctx.E = ctx.read(ctx.HL) }
  val LD_H_$HL$ = Opcode(0x66,7,1,"LD H,(HL)") { ctx => ctx.H = ctx.read(ctx.HL) }
  val LD_L_$HL$ = Opcode(0x6E,7,1,"LD L,(HL)") { ctx => ctx.L = ctx.read(ctx.HL) }
  // *** LD A,(BC)
  // **************
  val LD_A_$BC$ = Opcode(0x0A,7,1,"LD A,(BC)") { ctx => ctx.A = ctx.read(ctx.BC) }
  // *** LD A,(DE)
  // **************
  val LD_A_$DE$ = Opcode(0x1A,7,1,"LD A,(DE)") { ctx => ctx.A = ctx.read(ctx.DE) }
  // *** LD r,(IX + d)
  // **************
  private def MNEMONIC_IXY_d(pattern:String) = (m:Memory,PC:Int) => {
    val ofs = m.read(PC + 2).asInstanceOf[Byte]
    if (ofs > 0) pattern.format(" + " + hex2(ofs)) else pattern.format(" - " + hex2(-ofs))
  }
  val LD_A_$IX_d$ = Opcode((0xDD,0x7E),19,3,MNEMONIC_IXY_d("LD A,(IX %s)")) { ctx => ctx.A = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_B_$IX_d$ = Opcode((0xDD,0x46),19,3,MNEMONIC_IXY_d("LD B,(IX %s)")) { ctx => ctx.B = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_C_$IX_d$ = Opcode((0xDD,0x4E),19,3,MNEMONIC_IXY_d("LD C,(IX %s)")) { ctx => ctx.C = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_D_$IX_d$ = Opcode((0xDD,0x56),19,3,MNEMONIC_IXY_d("LD D,(IX %s)")) { ctx => ctx.D = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_E_$IX_d$ = Opcode((0xDD,0x5E),19,3,MNEMONIC_IXY_d("LD E,(IX %s)")) { ctx => ctx.E = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_H_$IX_d$ = Opcode((0xDD,0x66),19,3,MNEMONIC_IXY_d("LD H,(IX %s)")) { ctx => ctx.H = ctx.read(ctx.IX_+(ctx.byte(2))) }
  val LD_L_$IX_d$ = Opcode((0xDD,0x6E),19,3,MNEMONIC_IXY_d("LD L,(IX %s)")) { ctx => ctx.L = ctx.read(ctx.IX_+(ctx.byte(2))) }
  // *** LD r,(IY + d)
  // **************
  val LD_A_$IY_d$ = Opcode((0xFD,0x7E),19,3,MNEMONIC_IXY_d("LD A,(IY %s)")) { ctx => ctx.A = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_B_$IY_d$ = Opcode((0xFD,0x46),19,3,MNEMONIC_IXY_d("LD B,(IY %s)")) { ctx => ctx.B = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_C_$IY_d$ = Opcode((0xFD,0x4E),19,3,MNEMONIC_IXY_d("LD C,(IY %s)")) { ctx => ctx.C = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_D_$IY_d$ = Opcode((0xFD,0x56),19,3,MNEMONIC_IXY_d("LD D,(IY %s)")) { ctx => ctx.D = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_E_$IY_d$ = Opcode((0xFD,0x5E),19,3,MNEMONIC_IXY_d("LD E,(IY %s)")) { ctx => ctx.E = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_H_$IY_d$ = Opcode((0xFD,0x66),19,3,MNEMONIC_IXY_d("LD H,(IY %s)")) { ctx => ctx.H = ctx.read(ctx.IY_+(ctx.byte(2))) }
  val LD_L_$IY_d$ = Opcode((0xFD,0x6E),19,3,MNEMONIC_IXY_d("LD L,(IY %s)")) { ctx => ctx.L = ctx.read(ctx.IY_+(ctx.byte(2))) }
  // *** LD A,(nn)
  // **************
  private def MNEMONIC_nn(pattern:String,ofs:Int = 1) = (m:Memory,PC:Int) => pattern.format(hex4(WORD(m.read(PC + ofs + 1),m.read(PC + ofs))))
  val LD_A_$nn$ = Opcode(0x3A,13,3,MNEMONIC_nn("LD A,(%s)")) { ctx => ctx.A = ctx.read(ctx.word(1)) }
  // *** LD (HL),r
  // **************
  val LD_$HL$_A = Opcode(0x77,7,1,"LD (HL),A") { ctx => ctx.write(ctx.HL,ctx.A) }
  val LD_$HL$_B = Opcode(0x70,7,1,"LD (HL),B") { ctx => ctx.write(ctx.HL,ctx.B) }
  val LD_$HL$_C = Opcode(0x71,7,1,"LD (HL),C") { ctx => ctx.write(ctx.HL,ctx.C) }
  val LD_$HL$_D = Opcode(0x72,7,1,"LD (HL),D") { ctx => ctx.write(ctx.HL,ctx.D) }
  val LD_$HL$_E = Opcode(0x73,7,1,"LD (HL),E") { ctx => ctx.write(ctx.HL,ctx.E) }
  val LD_$HL$_H = Opcode(0x74,7,1,"LD (HL),H") { ctx => ctx.write(ctx.HL,ctx.H) }
  val LD_$HL$_L = Opcode(0x75,7,1,"LD (HL),L") { ctx => ctx.write(ctx.HL,ctx.L) }
  // *** LD (HL),n
  // **************
  val LD_$HL$_n= Opcode(0x36,10,2,MNEMONIC_n("LD (HL),%s")) { ctx => ctx.write(ctx.HL,ctx.byte(1)) }
  // *** LD (BC),A
  // **************
  val LD_$BC$_A = Opcode(0x02,7,1,"LD (BC),A") { ctx => ctx.write(ctx.BC,ctx.A) }
  // *** LD (DE),A
  // **************
  val LD_$DE$_A = Opcode(0x12,7,1,"LD (DE),A") { ctx => ctx.write(ctx.DE,ctx.A) }
  // *** LD (IX + d),r
  // **************
  val LD_$IX_d$_A = Opcode((0xDD,0x77),19,3,MNEMONIC_IXY_d("LD (IX %s),A")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.A) }
  val LD_$IX_d$_B = Opcode((0xDD,0x70),19,3,MNEMONIC_IXY_d("LD (IX %s),B")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.B) }
  val LD_$IX_d$_C = Opcode((0xDD,0x71),19,3,MNEMONIC_IXY_d("LD (IX %s),C")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.C) }
  val LD_$IX_d$_D = Opcode((0xDD,0x72),19,3,MNEMONIC_IXY_d("LD (IX %s),D")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.D) }
  val LD_$IX_d$_E = Opcode((0xDD,0x73),19,3,MNEMONIC_IXY_d("LD (IX %s),E")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.E) }
  val LD_$IX_d$_H = Opcode((0xDD,0x74),19,3,MNEMONIC_IXY_d("LD (IX %s),H")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.H) }
  val LD_$IX_d$_L = Opcode((0xDD,0x75),19,3,MNEMONIC_IXY_d("LD (IX %s),L")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.L) }
  // *** LD (IY + d),r
  // **************
  val LD_$IY_d$_A = Opcode((0xFD,0x77),19,3,MNEMONIC_IXY_d("LD (IY %s),A")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.A) }
  val LD_$IY_d$_B = Opcode((0xFD,0x70),19,3,MNEMONIC_IXY_d("LD (IY %s),B")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.B) }
  val LD_$IY_d$_C = Opcode((0xFD,0x71),19,3,MNEMONIC_IXY_d("LD (IY %s),C")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.C) }
  val LD_$IY_d$_D = Opcode((0xFD,0x72),19,3,MNEMONIC_IXY_d("LD (IY %s),D")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.D) }
  val LD_$IY_d$_E = Opcode((0xFD,0x73),19,3,MNEMONIC_IXY_d("LD (IY %s),E")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.E) }
  val LD_$IY_d$_H = Opcode((0xFD,0x74),19,3,MNEMONIC_IXY_d("LD (IY %s),H")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.H) }
  val LD_$IY_d$_L = Opcode((0xFD,0x75),19,3,MNEMONIC_IXY_d("LD (IY %s),L")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.L) }
  // *** LD (IX + d),n
  // **************
  private def MNEMONIC_IXY_d_n(pattern:String) = (m:Memory,PC:Int) => {
    val ofs = m.read(PC + 2).asInstanceOf[Byte]
    val n = hex2(m.read(PC + 3))
    if (ofs > 0) pattern.format("+ " + hex2(ofs),n) else pattern.format("- " + hex2(-ofs),n)
  }
  val LD_$IX_d$_n = Opcode((0xDD,0x36),19,4,MNEMONIC_IXY_d_n("LD (IX %s),%s")) { ctx => ctx.write(ctx.IX_+(ctx.byte(2)),ctx.byte(3)) }
  // *** LD (IY + d),n
  // **************
  val LD_$IY_d$_n = Opcode((0xFD,0x36),19,4,MNEMONIC_IXY_d_n("LD (IY %s),%s")) { ctx => ctx.write(ctx.IY_+(ctx.byte(2)),ctx.byte(3)) }
  // *** LD (nn),A
  // **************
  val LD_$nn$_A = Opcode(0x32,13,3,MNEMONIC_nn("LD (%s),A")) { ctx => ctx.write(ctx.word(1),ctx.A) }
  // *** LD I,A
  // **************
  val LD_I_A = Opcode((0xED,0x47),9,2,"LD I,A") { ctx => ctx.I = ctx.A }
  // *** LD R,A
  // **************
  val LD_R_A = Opcode((0xED,0x4F),9,2,"LD R,A") { ctx => ctx.R = ctx.A }
  // =========================================== LOAD 16 bit =================================================
  // *** LD dd,nn
  // **************
  val LD_BC_nn = Opcode(0x01,10,3,MNEMONIC_nn("LD BC,%s")) { ctx => ctx.BC = ctx.word(1) }
  val LD_DE_nn = Opcode(0x11,10,3,MNEMONIC_nn("LD DE,%s")) { ctx => ctx.DE = ctx.word(1) }
  val LD_HL_nn = Opcode(0x21,10,3,MNEMONIC_nn("LD HL,%s")) { ctx => ctx.HL = ctx.word(1) }
  val LD_SP_nn = Opcode(0x31,10,3,MNEMONIC_nn("LD SP,%s")) { ctx => ctx.SP = ctx.word(1) }
  // *** LD IX,nn
  // **************
  val LD_IX_nn = Opcode((0xDD,0x21),14,4,MNEMONIC_nn("LD IX,%s",2)) { ctx => ctx.IX = ctx.word(2) }
  // *** LD IY,nn
  // **************
  val LD_IY_nn = Opcode((0xFD,0x21),14,4,MNEMONIC_nn("LD IY,%s",2)) { ctx => ctx.IY = ctx.word(2) }
  // *** LD dd,(nn)
  // **************
  val LD_BC_$nn$ = Opcode((0xED,0x4B),20,4,MNEMONIC_nn("LD BC,(%s)",2)) { ctx => ctx.BC = ctx.readW(ctx.word(2)) }
  val LD_DE_$nn$ = Opcode((0xED,0x5B),20,4,MNEMONIC_nn("LD DE,(%s)",2)) { ctx => ctx.DE = ctx.readW(ctx.word(2)) }
  val LD_HL_$nn$ = Opcode((0xED,0x6B),20,4,MNEMONIC_nn("LD HL,(%s)",2)) { ctx => ctx.HL = ctx.readW(ctx.word(2)) }  
  val LD_SP_$nn$ = Opcode((0xED,0x7B),20,4,MNEMONIC_nn("LD SP,(%s)",2)) { ctx => ctx.SP = ctx.readW(ctx.word(2)) }
  // *** LD HL,(nn)
  // **************
  val LD2_HL_$nn$ = Opcode(0x2A,16,3,MNEMONIC_nn("LD HL,(%s)")) { ctx => ctx.HL = ctx.readW(ctx.word(1)) } 
  // *** LD IX,(nn)
  // **************
  val LD_IX_$nn$ = Opcode((0xDD,0x2A),20,4,MNEMONIC_nn("LD IX,(%s)",2)) { ctx => ctx.IX = ctx.readW(ctx.word(2)) }
  // *** LD IY,(nn)
  // **************
  val LD_IY_$nn$ = Opcode((0xFD,0x2A),20,4,MNEMONIC_nn("LD IY,(%s)",2)) { ctx => ctx.IY = ctx.readW(ctx.word(2)) }
  // *** LD SP,HL
  // **************
  val LD_SP_HL = Opcode(0xF9,6,1,"LD SP,HL") { ctx => ctx.SP = ctx.HL }
  // *** LD SP,IX
  // **************
  val LD_SP_IX = Opcode((0xDD,0xF9),10,2,"LD SP,IX") { ctx => ctx.SP = ctx.IX }
  // *** LD SP,IY
  // **************
  val LD_SP_IY = Opcode((0xFD,0xF9),10,2,"LD SP,IY") { ctx => ctx.SP = ctx.IY }
  // *** LD (nn),dd
  // **************
  val LD_$nn$_BC = Opcode((0xED,0x43),20,4,MNEMONIC_nn("LD (%s),BC",2)) { ctx => ctx.writeW(ctx.word(2),ctx.BC) }
  val LD_$nn$_DE = Opcode((0xED,0x53),20,4,MNEMONIC_nn("LD (%s),DE",2)) { ctx => ctx.writeW(ctx.word(2),ctx.DE) }
  val LD_$nn$_HL = Opcode((0xED,0x63),20,4,MNEMONIC_nn("LD (%s),HL",2)) { ctx => ctx.writeW(ctx.word(2),ctx.HL) }
  val LD_$nn$_SP = Opcode((0xED,0x73),20,4,MNEMONIC_nn("LD (%s),SP",2)) { ctx => ctx.writeW(ctx.word(2),ctx.SP) }
  val LD_$nn$_IX = Opcode((0xDD,0x22),20,4,MNEMONIC_nn("LD (%s),IX",2)) { ctx => ctx.writeW(ctx.word(2),ctx.IX) }
  val LD_$nn$_IY = Opcode((0xFD,0x22),20,4,MNEMONIC_nn("LD (%s),IY",2)) { ctx => ctx.writeW(ctx.word(2),ctx.IY) }
  // *** LD (nn),HL
  // **************
  val LD2_$nn$_$BC$ = Opcode(0x22,16,3,MNEMONIC_nn("LD (%s),HL")) { ctx => ctx.writeW(ctx.word(1),ctx.HL) }
  // *** PUSH AF
  // **************
  val PUSH_AF = Opcode(0xF5,11,1,"PUSH AF") { ctx => ctx.push(ctx.AF) }
  // *** PUSH BC
  // **************
  val PUSH_BC = Opcode(0xC5,11,1,"PUSH BC") { ctx => ctx.push(ctx.BC) }
  // *** PUSH DE
  // **************
  val PUSH_DE = Opcode(0xD5,11,1,"PUSH DE") { ctx => ctx.push(ctx.DE) }
  // *** PUSH HL
  // **************
  val PUSH_HL = Opcode(0xE5,11,1,"PUSH HL") { ctx => ctx.push(ctx.HL) }
  // *** PUSH IX
  // **************
  val PUSH_IX = Opcode((0xDD,0xE5),15,2,"PUSH IX") { ctx => ctx.push(ctx.IX) }
  // *** PUSH IY
  // **************
  val PUSH_IY = Opcode((0xFD,0xE5),15,2,"PUSH IY") { ctx => ctx.push(ctx.IY) }
  // *** POP AF
  // **************
  val POP_AF = Opcode(0xF1,10,1,"POP AF") { ctx => ctx.AF = ctx.pop }
  // *** POP BC
  // **************
  val POP_BC = Opcode(0xC1,10,1,"POP BC") { ctx => ctx.BC = ctx.pop }
  // *** POP DE
  // **************
  val POP_DE = Opcode(0xD1,10,1,"POP DE") { ctx => ctx.DE = ctx.pop }
  // *** POP HL
  // **************
  val POP_HL = Opcode(0xE1,10,1,"POP HL") { ctx => ctx.HL = ctx.pop }
  // *** POP IX
  // **************
  val POP_IX = Opcode((0xDD,0xE1),14,2,"POP IX") { ctx => ctx.IX = ctx.pop }
  // *** POP IY
  // **************
  val POP_IY = Opcode((0xFD,0xE1),14,2,"POP IY") { ctx => ctx.IY = ctx.pop }
  // ======================================= Exchange ========================================================
  // *** EXX
  // **************
  val EXX = Opcode(0xD9,4,1,"EXX") { ctx => ctx.EXX }
  // *** EX DE,HL
  // **************
  val EX_DE_HL = Opcode(0xEB,4,1,"EX DE,HL") { ctx => ctx.EX_DE_HL }
  // *** EX AF,AF'
  // **************
  val EX_AF_AF1 = Opcode(0x08,4,1,"EX AF,AF'") { ctx => ctx.EX_AF }
  // *** EX (SP),HL
  // **************
  val EX_$SP$_HL = Opcode(0xE3,19,1,"EX (SP),HL") { ctx => val tmp = ctx.readW(ctx.SP) ; ctx.writeW(ctx.SP,ctx.HL) ; ctx.HL = tmp }
  // *** EX (SP),IX
  // **************
  val EX_$SP$_IX = Opcode((0xDD,0xE3),23,2,"EX (SP),IX") { ctx => val tmp = ctx.readW(ctx.SP) ; ctx.writeW(ctx.SP,ctx.IX) ; ctx.IX = tmp }
  // *** EX (SP),IY
  // **************
  val EX_$SP$_IY = Opcode((0xFD,0xE3),23,2,"EX (SP),IY") { ctx => val tmp = ctx.readW(ctx.SP) ; ctx.writeW(ctx.SP,ctx.IY) ; ctx.IY = tmp }
  // ======================================= Block Transfer ==================================================
  // *** LDI
  // **************
  val LDI = Opcode((0xED,0xA0),16,2,"LDI") { ctx => 
    ctx.write(ctx.DE,ctx.read(ctx.HL)) ; ctx.incDecDE(true) ; ctx.incDecHL(true) ; ctx.incDecBC(false)
    ctx.F = ctx.F & 0xC1
    ctx.setParity(ctx.BC != 0)
  }
  // *** LDIR
  // **************
  val LDIR = Opcode((0xED,0xB0),21,2,"LDIR",modifyPC = true) { ctx => 
    ctx.write(ctx.DE,ctx.read(ctx.HL)) ; ctx.incDecDE(true) ; ctx.incDecHL(true) ; ctx.incDecBC(false)
    ctx.F = ctx.F & 0xC1
    ctx.setParity(ctx.BC != 0)
    if (ctx.BC == 0) {
      ctx.PC += 2
      ctx.setAdditionalClockCycles(-5)
    }
  }
  // *** LDD
  // **************
  val LDD = Opcode((0xED,0xA8),16,2,"LDD") { ctx => 
    ctx.write(ctx.DE,ctx.read(ctx.HL)) ; ctx.incDecDE(false) ; ctx.incDecHL(false) ; ctx.incDecBC(false)
    ctx.F = ctx.F & 0xC1
    ctx.setParity(ctx.BC != 0)
  }
  // *** LDDR
  // **************
  val LDDR = Opcode((0xED,0xB8),21,2,"LDDR",modifyPC = true) { ctx => 
    ctx.write(ctx.DE,ctx.read(ctx.HL)) ; ctx.incDecDE(false) ; ctx.incDecHL(false) ; ctx.incDecBC(false)
    ctx.F = ctx.F & 0xC1
    ctx.setParity(ctx.BC != 0)
    if (ctx.BC == 0) {
      ctx.PC += 2
      ctx.setAdditionalClockCycles(-5)
    }
  }
  // *** CPI
  // **************
  val CPI = Opcode((0xED,0xA1),16,2,"CPI") { ctx =>
    val value = ctx.read(ctx.HL)
    val cmp = (ctx.A - value) & 0xFF
    ctx.incDecHL(true) ; ctx.incDecBC(false)
    ctx.F = ctx.carry | ctx.SZP(cmp)
    ctx.setParity(ctx.BC != 0)
    ctx.setNegative(true)
    ctx.setHalf(((ctx.A ^ value ^ cmp) & 0x10) > 0)
  }
  // *** CPIR
  // **************
  val CPIR = Opcode((0xED,0xB1),21,2,"CPIR",modifyPC = true) { ctx =>
    val value = ctx.read(ctx.HL)
    val cmp = (ctx.A - value) & 0xFF
    ctx.incDecHL(true) ; ctx.incDecBC(false)
    ctx.F = ctx.carry | ctx.SZP(cmp)
    ctx.setParity(ctx.BC != 0)
    ctx.setNegative(true)
    ctx.setHalf(((ctx.A ^ value ^ cmp) & 0x10) > 0)
    if (ctx.BC == 0 || cmp == 0) {
      ctx.PC += 2
      ctx.setAdditionalClockCycles(-5)
    }
  }
  // *** CPDR
  // **************
  val CPDR = Opcode((0xED,0xB9),21,2,"CPDR",modifyPC = true) { ctx =>
    val value = ctx.read(ctx.HL)
    val cmp = (ctx.A - value) & 0xFF
    ctx.incDecHL(false) ; ctx.incDecBC(false)
    ctx.F = ctx.carry | ctx.SZP(cmp)
    ctx.setParity(ctx.BC != 0)
    ctx.setNegative(true)
    ctx.setHalf(((ctx.A ^ value ^ cmp) & 0x10) > 0)
    if (ctx.BC == 0 || cmp == 0) {
      ctx.PC += 2
      ctx.setAdditionalClockCycles(-5)
    }
  }
  // *** CPD
  // **************
  val CPD = Opcode((0xED,0xA9),16,2,"CPD") { ctx =>
    val value = ctx.read(ctx.HL)
    val cmp = (ctx.A - value) & 0xFF
    ctx.incDecHL(false) ; ctx.incDecBC(false)
    ctx.F = ctx.carry | ctx.SZP(cmp)
    ctx.setParity(ctx.BC != 0)
    ctx.setNegative(true)
    ctx.setHalf(((ctx.A ^ value ^ cmp) & 0x10) > 0)
  }
  // ===================================== 8 bit arithmetic ==================================================
  // *** ADD A,r
  // **************
  @inline private def add(ctx:Context,value:Int) {
    val tmp = (ctx.A + value) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setCarry(((ctx.A + value) & 0x100) > 0)
    ctx.setHalf(((ctx.A ^ value ^ tmp) & 0x10) > 0)
    ctx.setParity(((~(ctx.A ^ value)) & (ctx.A ^ tmp) & 0x80) > 0)
    ctx.A = tmp
  }
  @inline private def adc(ctx:Context,value:Int) {
    val carry = ctx.carry
    val tmp = (ctx.A + value + carry) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setCarry(((ctx.A + value + carry) & 0x100) > 0)
    ctx.setHalf(((ctx.A ^ value ^ tmp) & 0x10) > 0)
    ctx.setParity(((~(ctx.A ^ value)) & (ctx.A ^ tmp) & 0x80) > 0)
    ctx.A = tmp
  }
  val ADD_A_A = Opcode(0x87,4,1,"ADD A,A") { ctx => add(ctx,ctx.A) }
  val ADD_A_B = Opcode(0x80,4,1,"ADD A,B") { ctx => add(ctx,ctx.B) }
  val ADD_A_C = Opcode(0x81,4,1,"ADD A,C") { ctx => add(ctx,ctx.C) }
  val ADD_A_D = Opcode(0x82,4,1,"ADD A,D") { ctx => add(ctx,ctx.D) }
  val ADD_A_E = Opcode(0x83,4,1,"ADD A,E") { ctx => add(ctx,ctx.E) }
  val ADD_A_H = Opcode(0x84,4,1,"ADD A,H") { ctx => add(ctx,ctx.H) }
  val ADD_A_L = Opcode(0x85,4,1,"ADD A,L") { ctx => add(ctx,ctx.L) }
  // *** ADD A,(HL)
  // **************
  val ADD_A_$HL$ = Opcode(0x86,7,1,"ADD A,(HL)") { ctx => add(ctx,ctx.read(ctx.HL)) }
  // *** ADD A,(IX + d)
  // **************
  val ADD_A_$IX_d$ = Opcode((0xDD,0x86),19,3,MNEMONIC_IXY_d("ADD A,(IX%s)")) { ctx => add(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** ADD A,(IY + d)
  // **************
  val ADD_A_$IY_d$ = Opcode((0xFD,0x86),19,3,MNEMONIC_IXY_d("ADD A,(IY%s)")) { ctx => add(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** ADD A,n
  // **************
  val ADD_A_n = Opcode(0xC6,7,2,MNEMONIC_n("ADD A,%s")) { ctx => add(ctx,ctx.byte(1)) }
  // *** ADC A,r
  // **************
  val ADC_A_A = Opcode(0x8F,4,1,"ADC A,A") { ctx => adc(ctx,ctx.A) }
  val ADC_A_B = Opcode(0x88,4,1,"ADC A,B") { ctx => adc(ctx,ctx.B) }
  val ADC_A_C = Opcode(0x89,4,1,"ADC A,C") { ctx => adc(ctx,ctx.C) }
  val ADC_A_D = Opcode(0x8A,4,1,"ADC A,D") { ctx => adc(ctx,ctx.D) }
  val ADC_A_E = Opcode(0x8B,4,1,"ADC A,E") { ctx => adc(ctx,ctx.E) }
  val ADC_A_H = Opcode(0x8C,4,1,"ADC A,H") { ctx => adc(ctx,ctx.H) }
  val ADC_A_L = Opcode(0x8D,4,1,"ADC A,L") { ctx => adc(ctx,ctx.L) }
  // *** ADC A,(HL)
  // **************
  val ADC_A_$HL$ = Opcode(0x8E,7,1,"ADC A,L") { ctx => adc(ctx,ctx.read(ctx.HL)) }
  // *** ADC A,(IX + d)
  // **************
  val ADC_A_$IX_d$ = Opcode((0xDD,0x8E),19,3,MNEMONIC_IXY_d("ADC A,(IX%s)")) { ctx => adc(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** ADC A,(IY + d)
  // **************
  val ADC_A_$IY_d$ = Opcode((0xFD,0x8E),19,3,MNEMONIC_IXY_d("ADC A,(IY%s)")) { ctx => adc(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** ADC A,n
  // **************
  val ADC_A_n = Opcode(0xCE,7,2,MNEMONIC_n("ADC A,%s")) { ctx => adc(ctx,ctx.byte(1)) }
  // *** ADC A,IXL
  // **************
  val ADC_A_IXL = Opcode((0xDD,0x8D),4,2,"ADD IXL") { ctx => add(ctx,ctx.IXL) }
  // *** SUB r
  // **************
  @inline private def sub(ctx:Context,value:Int) {
    val tmp = (ctx.A - value) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setNegative(true)
    ctx.setCarry(value > ctx.A)
    ctx.setHalf(((ctx.A ^ value ^ tmp) & 0x10) > 0)
    ctx.setParity(((ctx.A ^ value) & (ctx.A ^ tmp) & 0x80) > 0)
    ctx.A = tmp
  }
  val SUB_A = Opcode(0x97,4,1,"SUB A") { ctx => sub(ctx,ctx.A) }
  val SUB_B = Opcode(0x90,4,1,"SUB B") { ctx => sub(ctx,ctx.B) }
  val SUB_C = Opcode(0x91,4,1,"SUB C") { ctx => sub(ctx,ctx.C) }
  val SUB_D = Opcode(0x92,4,1,"SUB D") { ctx => sub(ctx,ctx.D) }
  val SUB_E = Opcode(0x93,4,1,"SUB E") { ctx => sub(ctx,ctx.E) }
  val SUB_H = Opcode(0x94,4,1,"SUB H") { ctx => sub(ctx,ctx.H) }
  val SUB_L = Opcode(0x95,4,1,"SUB L") { ctx => sub(ctx,ctx.L) }
  // *** SUB (HL)
  // **************
  val SUB_$HL$ = Opcode(0x96,7,1,"SUB (HL)") { ctx => sub(ctx,ctx.read(ctx.HL)) }
  // *** SUB (IX + d)
  // **************
  val SUB_$IX_d$ = Opcode((0xDD,0x96),19,3,MNEMONIC_IXY_d("SUB (IX%s)")) { ctx => sub(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** SUB (IY + d)
  // **************
  val SUB_$IY_d$ = Opcode((0xFD,0x96),19,3,MNEMONIC_IXY_d("SUB (IY%s)")) { ctx => sub(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** SUB n
  // **************
  val SUB_n = Opcode(0xD6,7,2,MNEMONIC_n("SUB %s")) { ctx => sub(ctx,ctx.byte(1)) }
  // *** SBC r
  // **************
  @inline private def sbc(ctx:Context,value:Int) {
    val carry = ctx.carry
    val tmp = (ctx.A - value - carry) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setNegative(true)
    ctx.setCarry(value + carry> ctx.A)
    ctx.setHalf(((ctx.A ^ value ^ tmp) & 0x10) > 0)
    ctx.setParity(((ctx.A ^ value) & (ctx.A ^ tmp) & 0x80) > 0)
    ctx.A = tmp
  }
  val SBC_A = Opcode(0x9F,4,1,"SBC A,A") { ctx => sbc(ctx,ctx.A) }
  val SBC_B = Opcode(0x98,4,1,"SBC A,B") { ctx => sbc(ctx,ctx.B) }
  val SBC_C = Opcode(0x99,4,1,"SBC A,C") { ctx => sbc(ctx,ctx.C) }
  val SBC_D = Opcode(0x9A,4,1,"SBC A,D") { ctx => sbc(ctx,ctx.D) }
  val SBC_E = Opcode(0x9B,4,1,"SBC A,E") { ctx => sbc(ctx,ctx.E) }
  val SBC_H = Opcode(0x9C,4,1,"SBC A,H") { ctx => sbc(ctx,ctx.H) }
  val SBC_L = Opcode(0x9D,4,1,"SBC A,L") { ctx => sbc(ctx,ctx.L) }
  // *** SBC A,(HL)
  // **************
  val SBC_A_$HL$ = Opcode(0x9E,7,1,"SBC A,(HL)") { ctx => sbc(ctx,ctx.read(ctx.HL)) }
  // *** SBC A,(IX + d)
  // **************
  val SBC_A_$IX_d$ = Opcode((0xDD,0x9E),19,3,MNEMONIC_IXY_d("SBC A,(IX%s)")) { ctx => sbc(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** SBC A,(IY + d)
  // **************
  val SBC_A_$IY_d$ = Opcode((0xFD,0x9E),19,3,MNEMONIC_IXY_d("SBC A,(IY%s)")) { ctx => sbc(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** SBC n
  // **************
  val SBC_n = Opcode(0xDE,7,2,MNEMONIC_n("SBC A,%s")) { ctx => sbc(ctx,ctx.byte(1)) }
  // *** AND r
  // **************
  @inline private def and(ctx:Context,value:Int) {
    ctx.A &= value
    ctx.F = ctx.SZP(ctx.A)
    ctx.setHalf(true)    
  }
  val AND_A = Opcode(0xA7,4,1,"AND A") { ctx => and(ctx,ctx.A) }
  val AND_B = Opcode(0xA0,4,1,"AND B") { ctx => and(ctx,ctx.B) }
  val AND_B_dd = Opcode((0xDD,0xA0),4,2,"AND B") { ctx => and(ctx,ctx.B) }
  val AND_C = Opcode(0xA1,4,1,"AND C") { ctx => and(ctx,ctx.C) }
  val AND_D = Opcode(0xA2,4,1,"AND D") { ctx => and(ctx,ctx.D) }
  val AND_E = Opcode(0xA3,4,1,"AND E") { ctx => and(ctx,ctx.E) }
  val AND_H = Opcode(0xA4,4,1,"AND H") { ctx => and(ctx,ctx.H) }
  val AND_L = Opcode(0xA5,4,1,"AND L") { ctx => and(ctx,ctx.L) }
  // *** AND (HL)
  // **************
  val AND_$HL$ = Opcode(0xA6,7,1,"AND (HL)") { ctx => and(ctx,ctx.read(ctx.HL)) }
  // *** AND (IX + d)
  // **************
  val AND_$IX_d$ = Opcode((0xDD,0xA6),19,3,MNEMONIC_IXY_d("AND (IX%s)")) { ctx => and(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** AND (IY + d)
  // **************
  val AND_$IY_d$ = Opcode((0xFD,0xA6),19,3,MNEMONIC_IXY_d("AND (IY%s)")) { ctx => and(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** AND n
  // **************
  val AND_n = Opcode(0xE6,7,2,MNEMONIC_n("AND %s")) { ctx => and(ctx,ctx.byte(1)) }
  // *** XOR r
  // **************
  @inline private def xor(ctx:Context,value:Int) {
    ctx.A ^= value
    ctx.F = ctx.SZP(ctx.A)
  }
  val XOR_A = Opcode(0xAF,4,1,"XOR A") { ctx => xor(ctx,ctx.A) }
  val XOR_B = Opcode(0xA8,4,1,"XOR B") { ctx => xor(ctx,ctx.B) }
  val XOR_C = Opcode(0xA9,4,1,"XOR C") { ctx => xor(ctx,ctx.C) }
  val XOR_C_dd = Opcode((0xDD,0xA9),4,2,"XOR C") { ctx => xor(ctx,ctx.C) }
  val XOR_D = Opcode(0xAA,4,1,"XOR D") { ctx => xor(ctx,ctx.D) }
  val XOR_E = Opcode(0xAB,4,1,"XOR E") { ctx => xor(ctx,ctx.E) }
  val XOR_H = Opcode(0xAC,4,1,"XOR H") { ctx => xor(ctx,ctx.H) }
  val XOR_L = Opcode(0xAD,4,1,"XOR L") { ctx => xor(ctx,ctx.L) }
  // *** XOR (HL)
  // **************
  val XOR_$HL$ = Opcode(0xAE,7,1,"XOR (HL)") { ctx => xor(ctx,ctx.read(ctx.HL)) }
  // *** XOR (IX + d)
  // **************
  val XOR_$IX_d$ = Opcode((0xDD,0xAE),19,3,MNEMONIC_IXY_d("XOR (IX%s)")) { ctx => xor(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** XOR (IY + d)
  // **************
  val XOR_$IY_d$ = Opcode((0xFD,0xAE),19,3,MNEMONIC_IXY_d("XOR (IY%s)")) { ctx => xor(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** XOR n
  // **************
  val XOR_n = Opcode(0xEE,7,2,MNEMONIC_n("XOR %s")) { ctx => xor(ctx,ctx.byte(1)) }
  // *** OR r
  // **************
  @inline private def or(ctx:Context,value:Int) {
    ctx.A |= value
    ctx.F = ctx.SZP(ctx.A)
  }
  val OR_A = Opcode(0xB7,4,1,"OR A") { ctx => or(ctx,ctx.A) }
  val OR_B = Opcode(0xB0,4,1,"OR B") { ctx => or(ctx,ctx.B) }
  val OR_C = Opcode(0xB1,4,1,"OR C") { ctx => or(ctx,ctx.C) }
  val OR_D = Opcode(0xB2,4,1,"OR D") { ctx => or(ctx,ctx.D) }
  val OR_E = Opcode(0xB3,4,1,"OR E") { ctx => or(ctx,ctx.E) }
  val OR_H = Opcode(0xB4,4,1,"OR H") { ctx => or(ctx,ctx.H) }
  val OR_L = Opcode(0xB5,4,1,"OR L") { ctx => or(ctx,ctx.L) }
  // *** OR (HL)
  // **************
  val OR_$HL$ = Opcode(0xB6,7,1,"OR (HL)") { ctx => or(ctx,ctx.read(ctx.HL)) }
  // *** OR (IX + d)
  // **************
  val OR_$IX_d$ = Opcode((0xDD,0xB6),19,3,MNEMONIC_IXY_d("OR (IX%s)")) { ctx => or(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** OR (IY + d)
  // **************
  val OR_$IY_d$ = Opcode((0xFD,0xB6),19,3,MNEMONIC_IXY_d("OR (IY%s)")) { ctx => or(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** OR n
  // **************
  val OR_n = Opcode(0xF6,7,2,MNEMONIC_n("OR %s")) { ctx => or(ctx,ctx.byte(1)) }
  // *** CP r
  // **************
  @inline private def cp(ctx:Context,value:Int) {
    val tmp = (ctx.A - value) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setNegative(true)
    ctx.setCarry(value > ctx.A)
    ctx.setHalf(((ctx.A ^ value ^ tmp) & 0x10) > 0)
    ctx.setParity(((ctx.A ^ value) & (ctx.A ^ tmp) & 0x80) > 0)
  }
  val CP_A = Opcode(0xBF,4,1,"CP A") { ctx => cp(ctx,ctx.A) }
  val CP_B = Opcode(0xB8,4,1,"CP B") { ctx => cp(ctx,ctx.B) }
  val CP_C = Opcode(0xB9,4,1,"CP C") { ctx => cp(ctx,ctx.C) }
  val CP_D = Opcode(0xBA,4,1,"CP D") { ctx => cp(ctx,ctx.D) }
  val CP_E = Opcode(0xBB,4,1,"CP E") { ctx => cp(ctx,ctx.E) }
  val CP_H = Opcode(0xBC,4,1,"CP H") { ctx => cp(ctx,ctx.H) }
  val CP_L = Opcode(0xBD,4,1,"CP L") { ctx => cp(ctx,ctx.L) }
  // *** CP (HL)
  // **************
  val CP_$HL$ = Opcode(0xBE,7,1,"CP (HL)") { ctx => cp(ctx,ctx.read(ctx.HL)) }
  // *** CP (IX + d)
  // **************
  val CP_$IX_d$ = Opcode((0xDD,0xBE),19,3,MNEMONIC_IXY_d("CP (IX%s)")) { ctx => cp(ctx,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** CP (IY + d)
  // **************
  val CP_$IY_d$ = Opcode((0xFD,0xBE),19,3,MNEMONIC_IXY_d("CP (IY%s)")) { ctx => cp(ctx,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** CP n
  // **************
  val CP_n = Opcode(0xFE,7,2,MNEMONIC_n("CP %s")) { ctx => cp(ctx,ctx.byte(1)) }
  // *** INC r
  // **************
  @inline def incdecPrologue(ctx:Context,regValueAfter:Int,inc:Boolean) {
    val carry = ctx.carry
    ctx.F = ctx.SZP(regValueAfter) | carry
    if (inc) {
      ctx.setParity(regValueAfter == 0x80)
      ctx.setHalf((regValueAfter & 0x0F) == 0)
    }
    else {
      ctx.setParity(regValueAfter == 0x7F)
      ctx.setHalf((regValueAfter & 0x0F) == 0x0F)
      ctx.setNegative(true)
    }
  }
  val INC_A = Opcode(0x3C,4,1,"INC A") { ctx => ctx.A = (ctx.A + 1) & 0xFF ; incdecPrologue(ctx,ctx.A,true) }
  val INC_B = Opcode(0x04,4,1,"INC B") { ctx => ctx.B = (ctx.B + 1) & 0xFF ; incdecPrologue(ctx,ctx.B,true) }
  val INC_C = Opcode(0x0C,4,1,"INC C") { ctx => ctx.C = (ctx.C + 1) & 0xFF ; incdecPrologue(ctx,ctx.C,true) }
  val INC_D = Opcode(0x14,4,1,"INC D") { ctx => ctx.D = (ctx.D + 1) & 0xFF ; incdecPrologue(ctx,ctx.D,true) }
  val INC_E = Opcode(0x1C,4,1,"INC E") { ctx => ctx.E = (ctx.E + 1) & 0xFF ; incdecPrologue(ctx,ctx.E,true) }
  val INC_H = Opcode(0x24,4,1,"INC H") { ctx => ctx.H = (ctx.H + 1) & 0xFF ; incdecPrologue(ctx,ctx.H,true) }
  val INC_L = Opcode(0x2C,4,1,"INC L") { ctx => ctx.L = (ctx.L + 1) & 0xFF ; incdecPrologue(ctx,ctx.L,true) }
  // *** INC (HL)
  // **************
  val INC_$HL$ = Opcode(0x34,11,1,"INC (HL)") { ctx => val tmp = (ctx.read(ctx.HL) + 1) & 0xFF ; ctx.write(ctx.HL,tmp) ; incdecPrologue(ctx,tmp,true) }
  // *** INC (IX + d)
  // **************
  val INC_$IX_d$ = Opcode((0xDD,0x34),23,3,MNEMONIC_IXY_d("INC (IX%s)")) { ctx => 
    val addr = ctx.IX_+(ctx.byte(2))
    val tmp = (ctx.read(addr) + 1) & 0xFF
    ctx.write(addr,tmp) ; incdecPrologue(ctx,tmp,true)
  }
  // *** INC (IY + d)
  // **************
  val INC_$IY_d$ = Opcode((0xFD,0x34),23,3,MNEMONIC_IXY_d("INC (IY%s)")) { ctx =>
    val addr = ctx.IY_+(ctx.byte(2))
    val tmp = (ctx.read(addr) + 1) & 0xFF
    ctx.write(addr,tmp) ; incdecPrologue(ctx,tmp,true)
  }
  // *** DEC r
  // **************
  val DEC_A = Opcode(0x3D,4,1,"DEC A") { ctx => ctx.A = (ctx.A - 1) & 0xFF ; incdecPrologue(ctx,ctx.A,false) }
  val DEC_B = Opcode(0x05,4,1,"DEC B") { ctx => ctx.B = (ctx.B - 1) & 0xFF ; incdecPrologue(ctx,ctx.B,false) }
  val DEC_C = Opcode(0x0D,4,1,"DEC C") { ctx => ctx.C = (ctx.C - 1) & 0xFF ; incdecPrologue(ctx,ctx.C,false) }
  val DEC_D = Opcode(0x15,4,1,"DEC D") { ctx => ctx.D = (ctx.D - 1) & 0xFF ; incdecPrologue(ctx,ctx.D,false) }
  val DEC_E = Opcode(0x1D,4,1,"DEC E") { ctx => ctx.E = (ctx.E - 1) & 0xFF ; incdecPrologue(ctx,ctx.E,false) }
  val DEC_H = Opcode(0x25,4,1,"DEC H") { ctx => ctx.H = (ctx.H - 1) & 0xFF ; incdecPrologue(ctx,ctx.H,false) }
  val DEC_L = Opcode(0x2D,4,1,"DEC L") { ctx => ctx.L = (ctx.L - 1) & 0xFF ; incdecPrologue(ctx,ctx.L,false) }
  // *** DEC (HL)
  // **************
  val DEC_$HL$ = Opcode(0x35,11,1,"DEC (HL)") { ctx => val tmp = (ctx.read(ctx.HL) - 1) & 0xFF ; ctx.write(ctx.HL,tmp) ; incdecPrologue(ctx,tmp,false) }
  // *** DEC (IX + d)
  // **************
  val DEC_$IX_d$ = Opcode((0xDD,0x35),23,3,MNEMONIC_IXY_d("DEC (IX%s)")) { ctx => 
    val addr = ctx.IX_+(ctx.byte(2))
    val tmp = (ctx.read(addr) - 1) & 0xFF
    ctx.write(addr,tmp) ; incdecPrologue(ctx,tmp,false)
  }
  // *** INC (IY + d)
  // **************
  val DEC_$IY_d$ = Opcode((0xFD,0x35),23,3,MNEMONIC_IXY_d("DEC (IY%s)")) { ctx =>
    val addr = ctx.IY_+(ctx.byte(2))
    val tmp = (ctx.read(addr) - 1) & 0xFF
    ctx.write(addr,tmp) ; incdecPrologue(ctx,tmp,false)
  }
  // ==================================== 16 bit arithmetic ==================================================
  // *** ADD HL,rr
  // **************
  val ADD_HL_BC = Opcode(0x09,11,1,"ADD HL,BC") { ctx =>
    val tmp = ctx.HL + ctx.BC
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ ctx.B) & 0x10) > 0)
    ctx.HL = tmp
  }
  val ADD_HL_DE = Opcode(0x19,11,1,"ADD HL,DE") { ctx =>
    val tmp = ctx.HL + ctx.DE
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ ctx.D) & 0x10) > 0)
    ctx.HL = tmp
  }
  val ADD_HL_HL = Opcode(0x29,11,1,"ADD HL,HL") { ctx =>
    val tmp = ctx.HL + ctx.HL
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ ctx.H) & 0x10) > 0)
    ctx.HL = tmp
  }
  val ADD_HL_SP = Opcode(0x39,11,1,"ADD HL,SP") { ctx =>
    val tmp = ctx.HL + ctx.SP
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ ((ctx.SP >> 8) & 0xFF)) & 0x10) > 0)
    ctx.HL = tmp
  }
  val ADD_IX_BC = Opcode((0xDD,0x09),15,2,"ADD IX,BC") { ctx =>
    val tmp = ctx.IX + ctx.BC
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IX >> 8) & 0xFF) ^ ctx.B) & 0x10) > 0)
    ctx.IX = tmp & 0xFFFF
  }
  val ADD_IX_DE = Opcode((0xDD,0x19),15,2,"ADD IX,DE") { ctx =>
    val tmp = ctx.IX + ctx.DE
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IX >> 8) & 0xFF) ^ ctx.D) & 0x10) > 0)
    ctx.IX = tmp & 0xFFFF
  }
  val ADD_IX_SP = Opcode((0xDD,0x39),15,2,"ADD IX,SP") { ctx =>
    val tmp = ctx.IX + ctx.SP
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IX >> 8) & 0xFF) ^ ((ctx.SP >> 8) & 0xFF)) & 0x10) > 0)
    ctx.IX = tmp & 0xFFFF
  }
  val ADD_IX_IX = Opcode((0xDD,0x29),15,2,"ADD IX,IX") { ctx =>
    val tmp = ctx.IX + ctx.IX
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IX >> 8) & 0xFF) ^ ((ctx.IX >> 8) & 0xFF)) & 0x10) > 0)
    ctx.IX = tmp & 0xFFFF
  }
  val ADD_IY_BC = Opcode((0xFD,0x09),15,2,"ADD IY,BC") { ctx =>
    val tmp = ctx.IY + ctx.BC
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IY >> 8) & 0xFF) ^ ctx.B) & 0x10) > 0)
    ctx.IY = tmp & 0xFFFF
  }
  val ADD_IY_DE = Opcode((0xFD,0x19),15,2,"ADD IY,DE") { ctx =>
    val tmp = ctx.IY + ctx.DE
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IY >> 8) & 0xFF) ^ ctx.D) & 0x10) > 0)
    ctx.IY = tmp & 0xFFFF
  }
  val ADD_IY_SP = Opcode((0xFD,0x39),15,2,"ADD IY,SP") { ctx =>
    val tmp = ctx.IY + ctx.SP
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IY >> 8) & 0xFF) ^ ((ctx.SP >> 8) & 0xFF)) & 0x10) > 0)
    ctx.IY = tmp & 0xFFFF
  }
  val ADD_IY_IX = Opcode((0xFD,0x29),15,2,"ADD IY,IY") { ctx =>
    val tmp = ctx.IY + ctx.IY
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ((ctx.IY >> 8) & 0xFF) ^ ((ctx.IY >> 8) & 0xFF)) & 0x10) > 0)
    ctx.IY = tmp & 0xFFFF
  }
  // *** ADC HL,rr
  // **************
  @inline private def adcHL(ctx:Context,value:Int) {
    val valueH = (value >> 8) & 0xFF
    val tmp = ctx.HL + value + ctx.carry
    ctx.setZero((tmp & 0xFFFF) == 0)
    ctx.setSign((tmp & 0x8000) > 0)
    ctx.setNegative(false)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ valueH) & 0x10) > 0)
    ctx.setParity(((~(ctx.H ^ valueH)) & (valueH ^ (tmp >> 8)) & 0x80) > 0)
    ctx.HL = tmp
  }
  val ADC_HL_BC = Opcode((0xED,0x4A),15,2,"ADC HL,BC") { ctx => adcHL(ctx,ctx.BC) }
  val ADC_HL_DE = Opcode((0xED,0x5A),15,2,"ADC HL,DE") { ctx => adcHL(ctx,ctx.DE) }
  val ADC_HL_HL = Opcode((0xED,0x6A),15,2,"ADC HL,HL") { ctx => adcHL(ctx,ctx.HL) }
  val ADC_HL_SP = Opcode((0xED,0x7A),15,2,"ADC HL,SP") { ctx => adcHL(ctx,ctx.SP) }
  // *** SBC HL,rr
  // **************
  @inline private def sbcHL(ctx:Context,value:Int) {
    val valueH = (value >> 8) & 0xFF
    val tmp = ctx.HL - value - ctx.carry
    ctx.setZero((tmp & 0xFFFF) == 0)
    ctx.setSign((tmp & 0x8000) > 0)
    ctx.setNegative(true)
    ctx.setCarry((tmp & 0x10000) > 0)
    ctx.setHalf((((tmp >> 8) ^ ctx.H ^ valueH) & 0x10) > 0)
    ctx.setParity(((ctx.H ^ (tmp >> 8)) & (ctx.H ^ valueH) & 0x80) > 0)
    ctx.HL = tmp
  }
  val SBC_HL_BC = Opcode((0xED,0x42),15,2,"SBC HL,BC") { ctx => sbcHL(ctx,ctx.BC) }
  val SBC_HL_DE = Opcode((0xED,0x52),15,2,"SBC HL,DE") { ctx => sbcHL(ctx,ctx.DE) }
  val SBC_HL_HL = Opcode((0xED,0x62),15,2,"SBC HL,HL") { ctx => sbcHL(ctx,ctx.HL) }
  val SBC_HL_SP = Opcode((0xED,0x72),15,2,"SBC HL,SP") { ctx => sbcHL(ctx,ctx.SP) }
  // *** INC rr
  // **************
  val INC_BC = Opcode(0x03,6,1,"INC BC") { ctx => ctx.incDecBC(true) }
  val INC_DE = Opcode(0x13,6,1,"INC DE") { ctx => ctx.incDecDE(true) }
  val INC_HL = Opcode(0x23,6,1,"INC HL") { ctx => ctx.incDecHL(true) }
  val INC_SP = Opcode(0x33,6,1,"INC SP") { ctx => ctx.incDecSP(true) }
  val INC_IX = Opcode((0xDD,0x23),10,2,"INC IX") { ctx => ctx.incDecIX(true) }
  val INC_IY = Opcode((0xFD,0x23),10,2,"INC IY") { ctx => ctx.incDecIY(true) }
  // *** DEC rr
  // **************
  val DEC_BC = Opcode(0x0B,6,1,"DEC BC") { ctx => ctx.incDecBC(false) }
  val DEC_DE = Opcode(0x1B,6,1,"DEC DE") { ctx => ctx.incDecDE(false) }
  val DEC_HL = Opcode(0x2B,6,1,"DEC HL") { ctx => ctx.incDecHL(false) }
  val DEC_SP = Opcode(0x3B,6,1,"DEC SP") { ctx => ctx.incDecSP(false) }
  val DEC_IX = Opcode((0xDD,0x2B),10,2,"DEC IX") { ctx => ctx.incDecIX(false) }
  val DEC_IY = Opcode((0xFD,0x2B),10,2,"DEC IY") { ctx => ctx.incDecIY(false) }
  // ==================================== General Purpose Arithmetic and Control Groups ======================
  // *** NOP
  // **************
  val NOP = Opcode(0x00,4,1,"NOP") { ctx => }
  val NOP_fd = Opcode((0xFD,0xFD),4,1,"NOP") { ctx => } // skip FD
  // *** HALT
  // **************
  val HALT = Opcode(0x76,4,1,"HALT",modifyPC = true) { ctx => ctx.halted = true }
  // *** EI
  // **************
  val EI = Opcode(0xFB,4,1,"EI") { ctx => ctx.IFF1 = 1 ; ctx.IFF2 = 1 ; ctx.setDelayInt }
  // *** DI
  // **************
  val DI = Opcode(0xF3,4,1,"DI") { ctx => ctx.IFF1 = 0 ; ctx.IFF2 = 0 }
  // *** IM x
  // **************
  val IM_0 = Opcode((0xED,0x46),8,2,"IM 0") { ctx => ctx.im = 0 }
  val IM_1 = Opcode((0xED,0x56),8,2,"IM 1") { ctx => ctx.im = 1 }
  val IM_2 = Opcode((0xED,0x5E),8,2,"IM 2") { ctx => ctx.im = 2 }
  // *** CPL
  // **************
  val CPL = Opcode(0x2F,4,1,"CPL") { ctx => ctx.A = ~ctx.A & 0xFF ; ctx.setHalf(true) ; ctx.setNegative(true) }
  // *** NEG
  // **************
  val NEG = Opcode((0xED,0x44),8,2,"NEG") { ctx =>
    val tmp = (0 - ctx.A) & 0xFF
    ctx.F = ctx.SZP(tmp)
    ctx.setNegative(true)
    ctx.setHalf(((ctx.A ^ tmp) & 0x10) > 0)
    ctx.setParity((ctx.A & tmp & 0x80) > 0)
    ctx.setCarry(ctx.A > 0)
    ctx.A = tmp
  }
  // *** CCF
  // **************
  val CCF = Opcode(0x3F,4,1,"CCF") { ctx =>
    ctx.setHalf(ctx.carry > 0)
    ctx.setNegative(false)
    ctx.setCarry(ctx.carry == 0)    
  }
  // *** SCF
  // **************
  val SCF = Opcode(0x37,4,1,"SCF") { ctx =>
    ctx.setHalf(false)
    ctx.setNegative(false)
    ctx.setCarry(true)    
  }
  /* *** DAA
   *tmp := a,
    if nf then
     if hf or [a AND 0x0f > 9] then tmp -= 0x06
     if cf or [a > 0x99] then tmp -= 0x60
    else
     if hf or [a AND 0x0f > 9] then tmp += 0x06
     if cf or [a > 0x99] then tmp += 0x60
    endif,
    tmp => flags, cf := cf OR [a > 0x99],
    hf := a.4 XOR tmp.4, a := tmp
  */
  val DAA = Opcode(0x27,4,1,"DAA") { ctx =>
    var tmp = ctx.A
    if (ctx.negative > 0) {
      if (ctx.half > 0 || (ctx.A & 0x0F) > 9) tmp -= 0x06
      if (ctx.carry > 0 || ctx.A > 0x99) tmp -= 0x60
    }
    else {
      if (ctx.half > 0 || (ctx.A & 0x0F) > 9) tmp += 0x06
      if (ctx.carry > 0 || ctx.A > 0x99) tmp += 0x60
    }
    ctx.F = ctx.SZP(tmp & 0xFF) | ctx.negative
    if (ctx.A > 0x99) ctx.setCarry(true)
    ctx.setHalf(((ctx.A & 0x10) ^ (tmp & 0x10)) > 0)
    ctx.A = tmp & 0xFF
  }
  // ==================================== Rotate and Shift Group =============================================
  // *** RLC r
  // **************
  @inline private def rotLC(ctx:Context,value:Int) : Int = {
    val h = (value & 0x80) >> 7
    val rot = (value << 1 | h) & 0xFF
    ctx.F = ctx.SZP(rot)
    ctx.setCarry(h > 0)
    rot
  }
  val RLC_A = Opcode((0xCB,0x07),8,2,"RLC A") { ctx => ctx.A = rotLC(ctx,ctx.A) }
  val RLC_B = Opcode((0xCB,0x00),8,2,"RLC B") { ctx => ctx.B = rotLC(ctx,ctx.B) }
  val RLC_C = Opcode((0xCB,0x01),8,2,"RLC C") { ctx => ctx.C = rotLC(ctx,ctx.C) }
  val RLC_D = Opcode((0xCB,0x02),8,2,"RLC D") { ctx => ctx.D = rotLC(ctx,ctx.D) }
  val RLC_E = Opcode((0xCB,0x03),8,2,"RLC E") { ctx => ctx.E = rotLC(ctx,ctx.E) }
  val RLC_H = Opcode((0xCB,0x04),8,2,"RLC H") { ctx => ctx.H = rotLC(ctx,ctx.H) }
  val RLC_L = Opcode((0xCB,0x05),8,2,"RLC L") { ctx => ctx.L = rotLC(ctx,ctx.L) }
  // *** RLC (HL)
  // **************
  val RLC_$HL$ = Opcode((0xCB,0x06),15,2,"RLC (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,rotLC(ctx,ctx.read(ctx.HL)))
  }
  // *** RLC (IX+d)
  // **************
  val RLC_$IX_d$ = Opcode((0xDD,0xCB,0x06),23,4,MNEMONIC_IXY_d("RLC (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,rotLC(ctx,ctx.read(adr))) 
  }
  // *** RLC (IY+d)
  // **************
  val RLC_$IY_d$ = Opcode((0xFD,0xCB,0x06),23,4,MNEMONIC_IXY_d("RLC (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,rotLC(ctx,ctx.read(adr))) 
  }
  // *** RRC r
  // **************  
  @inline private def rotRC(ctx:Context,value:Int) : Int = {
    val carry = value & 0x01
    val rot = (value >> 1 | carry << 7) & 0xFF
    ctx.F = ctx.SZP(rot)
    ctx.setCarry(carry > 0)
    rot
  }  
  val RRC_A = Opcode((0xCB,0x0F),8,2,"RRC A") { ctx => ctx.A = rotRC(ctx,ctx.A) }
  val RRC_B = Opcode((0xCB,0x08),8,2,"RRC B") { ctx => ctx.B = rotRC(ctx,ctx.B) }
  val RRC_C = Opcode((0xCB,0x09),8,2,"RRC C") { ctx => ctx.C = rotRC(ctx,ctx.C) }
  val RRC_D = Opcode((0xCB,0x0A),8,2,"RRC D") { ctx => ctx.D = rotRC(ctx,ctx.D) }
  val RRC_E = Opcode((0xCB,0x0B),8,2,"RRC E") { ctx => ctx.E = rotRC(ctx,ctx.E) }
  val RRC_H = Opcode((0xCB,0x0C),8,2,"RRC H") { ctx => ctx.H = rotRC(ctx,ctx.H) }
  val RRC_L = Opcode((0xCB,0x0D),8,2,"RRC L") { ctx => ctx.L = rotRC(ctx,ctx.L) }
  // *** RRC (HL)
  // **************
  val RRC_$HL$ = Opcode((0xCB,0x0E),15,2,"RRC (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,rotRC(ctx,ctx.read(ctx.HL)))
  }
  // *** RLC (IX+d)
  // **************
  val RRC_$IX_d$ = Opcode((0xDD,0xCB,0x0E),23,4,MNEMONIC_IXY_d("RRC (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,rotRC(ctx,ctx.read(adr))) 
  }
  // *** RRC (IY+d)
  // **************
  val RRC_$IY_d$ = Opcode((0xFD,0xCB,0x0E),23,4,MNEMONIC_IXY_d("RRC (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,rotRC(ctx,ctx.read(adr))) 
  }
  // *** RL r
  // **************
  @inline private def rotL(ctx:Context,value:Int) : Int = {
    val carry = ctx.carry
    val h = (value & 0x80) >> 7
    val rot = (value << 1 | carry) & 0xFF
    ctx.F = ctx.SZP(rot)
    ctx.setCarry(h > 0) 
    rot
  }
  val RL_A = Opcode((0xCB,0x17),8,2,"RL A") { ctx => ctx.A = rotL(ctx,ctx.A) }
  val RL_B = Opcode((0xCB,0x10),8,2,"RL B") { ctx => ctx.B = rotL(ctx,ctx.B) }
  val RL_C = Opcode((0xCB,0x11),8,2,"RL C") { ctx => ctx.C = rotL(ctx,ctx.C) }
  val RL_D = Opcode((0xCB,0x12),8,2,"RL D") { ctx => ctx.D = rotL(ctx,ctx.D) }
  val RL_E = Opcode((0xCB,0x13),8,2,"RL E") { ctx => ctx.E = rotL(ctx,ctx.E) }
  val RL_H = Opcode((0xCB,0x14),8,2,"RL H") { ctx => ctx.H = rotL(ctx,ctx.H) }
  val RL_L = Opcode((0xCB,0x15),8,2,"RL L") { ctx => ctx.L = rotL(ctx,ctx.L) }
  // *** RL (HL)
  // **************
  val RL_$HL$ = Opcode((0xCB,0x16),15,2,"RL (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,rotL(ctx,ctx.read(ctx.HL)))
  }
  // *** RLC (IX+d)
  // **************
  val RL_$IX_d$ = Opcode((0xDD,0xCB,0x16),23,4,MNEMONIC_IXY_d("RL (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,rotL(ctx,ctx.read(adr))) 
  }
  // *** RRC (IY+d)
  // **************
  val RL_$IY_d$ = Opcode((0xFD,0xCB,0x16),23,4,MNEMONIC_IXY_d("RL (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,rotL(ctx,ctx.read(adr))) 
  }
  // *** RR r
  // **************
  @inline private def rotR(ctx:Context,value:Int) : Int = {
    val carry = ctx.carry
    val l = (value & 0x01)
    val rot = (value >> 1 | carry << 7) & 0xFF
    ctx.F = ctx.SZP(rot)
    ctx.setCarry(l > 0)    
    rot
  }
  val RR_A = Opcode((0xCB,0x1F),8,2,"RR A") { ctx => ctx.A = rotR(ctx,ctx.A) }
  val RR_B = Opcode((0xCB,0x18),8,2,"RR B") { ctx => ctx.B = rotR(ctx,ctx.B) }
  val RR_C = Opcode((0xCB,0x19),8,2,"RR C") { ctx => ctx.C = rotR(ctx,ctx.C) }
  val RR_D = Opcode((0xCB,0x1A),8,2,"RR D") { ctx => ctx.D = rotR(ctx,ctx.D) }
  val RR_E = Opcode((0xCB,0x1B),8,2,"RR E") { ctx => ctx.E = rotR(ctx,ctx.E) }
  val RR_H = Opcode((0xCB,0x1C),8,2,"RR H") { ctx => ctx.H = rotR(ctx,ctx.H) }
  val RR_L = Opcode((0xCB,0x1D),8,2,"RR L") { ctx => ctx.L = rotR(ctx,ctx.L) }
  // *** RR (HL)
  // **************
  val RR_$HL$ = Opcode((0xCB,0x1E),15,2,"RR (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,rotR(ctx,ctx.read(ctx.HL)))
  }
  // *** RR (IX+d)
  // **************
  val RR_$IX_d$ = Opcode((0xDD,0xCB,0x1E),23,4,MNEMONIC_IXY_d("RR (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,rotR(ctx,ctx.read(adr))) 
  }
  // *** RR (IY+d)
  // **************
  val RR_$IY_d$ = Opcode((0xFD,0xCB,0x1E),23,4,MNEMONIC_IXY_d("RR (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,rotR(ctx,ctx.read(adr))) 
  }
  // *** SLA r
  // **************
  @inline private def sla(ctx:Context,value:Int) : Int = {
    val h = (value & 0x80)
    val shift = (value << 1) & 0xFF
    ctx.F = ctx.SZP(shift)
    ctx.setCarry(h > 0)    
    shift
  }
  val SLA_A = Opcode((0xCB,0x27),8,2,"SLA A") { ctx => ctx.A = sla(ctx,ctx.A) }
  val SLA_B = Opcode((0xCB,0x20),8,2,"SLA B") { ctx => ctx.B = sla(ctx,ctx.B) }
  val SLA_C = Opcode((0xCB,0x21),8,2,"SLA C") { ctx => ctx.C = sla(ctx,ctx.C) }
  val SLA_D = Opcode((0xCB,0x22),8,2,"SLA D") { ctx => ctx.D = sla(ctx,ctx.D) }
  val SLA_E = Opcode((0xCB,0x23),8,2,"SLA E") { ctx => ctx.E = sla(ctx,ctx.E) }
  val SLA_H = Opcode((0xCB,0x24),8,2,"SLA H") { ctx => ctx.H = sla(ctx,ctx.H) }
  val SLA_L = Opcode((0xCB,0x25),8,2,"SLA L") { ctx => ctx.L = sla(ctx,ctx.L) }
  // *** SLA (HL)
  // **************
  val SLA_$HL$ = Opcode((0xCB,0x26),15,2,"SLA (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,sla(ctx,ctx.read(ctx.HL)))
  }
  // *** SLA (IX+d)
  // **************
  val SLA_$IX_d$ = Opcode((0xDD,0xCB,0x26),23,4,MNEMONIC_IXY_d("SLA (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,sla(ctx,ctx.read(adr))) 
  }
  // *** SLA (IY+d)
  // **************
  val SLA_$IY_d$ = Opcode((0xFD,0xCB,0x26),23,4,MNEMONIC_IXY_d("SLA (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,sla(ctx,ctx.read(adr))) 
  }
  // *** SRA r
  // **************
  @inline private def sra(ctx:Context,value:Int) : Int = {
    val l = (value & 0x01)
    val h = (value & 0x80)
    val shift = (value >> 1 | h) & 0xFF
    ctx.F = ctx.SZP(shift)
    ctx.setCarry(l > 0)    
    shift
  }
  val SRA_A = Opcode((0xCB,0x2F),8,2,"SRA A") { ctx => ctx.A = sra(ctx,ctx.A) }
  val SRA_B = Opcode((0xCB,0x28),8,2,"SRA B") { ctx => ctx.B = sra(ctx,ctx.B) }
  val SRA_C = Opcode((0xCB,0x29),8,2,"SRA C") { ctx => ctx.C = sra(ctx,ctx.C) }
  val SRA_D = Opcode((0xCB,0x2A),8,2,"SRA D") { ctx => ctx.D = sra(ctx,ctx.D) }
  val SRA_E = Opcode((0xCB,0x2B),8,2,"SRA E") { ctx => ctx.E = sra(ctx,ctx.E) }
  val SRA_H = Opcode((0xCB,0x2C),8,2,"SRA H") { ctx => ctx.H = sra(ctx,ctx.H) }
  val SRA_L = Opcode((0xCB,0x2D),8,2,"SRA L") { ctx => ctx.L = sra(ctx,ctx.L) }
  // *** SRA (HL)
  // **************
  val SRA_$HL$ = Opcode((0xCB,0x2E),15,2,"SRA (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,sra(ctx,ctx.read(ctx.HL)))
  }
  // *** SRA (IX+d)
  // **************
  val SRA_$IX_d$ = Opcode((0xDD,0xCB,0x2E),23,4,MNEMONIC_IXY_d("SRA (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,sra(ctx,ctx.read(adr))) 
  }
  // *** SRA (IY+d)
  // **************
  val SRA_$IY_d$ = Opcode((0xFD,0xCB,0x2E),23,4,MNEMONIC_IXY_d("SRA (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,sra(ctx,ctx.read(adr))) 
  }
  // *** SRL r
  // **************
  @inline private def srl(ctx:Context,value:Int) : Int = {
    val l = (value & 0x01)
    val shift = (value >> 1) & 0xFF
    ctx.F = ctx.SZP(shift)
    ctx.setCarry(l > 0)    
    shift
  }
  val SRL_A = Opcode((0xCB,0x3F),8,2,"SRL A") { ctx => ctx.A = srl(ctx,ctx.A) }
  val SRL_B = Opcode((0xCB,0x38),8,2,"SRL B") { ctx => ctx.B = srl(ctx,ctx.B) }
  val SRL_C = Opcode((0xCB,0x39),8,2,"SRL C") { ctx => ctx.C = srl(ctx,ctx.C) }
  val SRL_D = Opcode((0xCB,0x3A),8,2,"SRL D") { ctx => ctx.D = srl(ctx,ctx.D) }
  val SRL_E = Opcode((0xCB,0x3B),8,2,"SRL E") { ctx => ctx.E = srl(ctx,ctx.E) }
  val SRL_H = Opcode((0xCB,0x3C),8,2,"SRL H") { ctx => ctx.H = srl(ctx,ctx.H) }
  val SRL_L = Opcode((0xCB,0x3D),8,2,"SRL L") { ctx => ctx.L = srl(ctx,ctx.L) }
  // *** SRL (HL)
  // **************
  val SRL_$HL$ = Opcode((0xCB,0x3E),15,2,"SRL (HL)") { ctx =>
    val adr = ctx.HL
    ctx.write(adr,srl(ctx,ctx.read(ctx.HL)))
  }
  // *** SRL (IX+d)
  // **************
  val SRL_$IX_d$ = Opcode((0xDD,0xCB,0x3E),23,4,MNEMONIC_IXY_d("SRL (IX%s)")) { ctx =>
    val adr = ctx.IX_+(ctx.byte(2))
    ctx.write(adr,srl(ctx,ctx.read(adr))) 
  }
  // *** SRL (IY+d)
  // **************
  val SRL_$IY_d$ = Opcode((0xFD,0xCB,0x3E),23,4,MNEMONIC_IXY_d("SRL (IY%s)")) { ctx =>
    val adr = ctx.IY_+(ctx.byte(2))
    ctx.write(adr,srl(ctx,ctx.read(adr))) 
  }
  // *** RLCA
  // **************
  @inline private def rlca(ctx:Context) {
    val value = ctx.A
    val h = (value & 0x80) >> 7
    val rot = (value << 1 | h) & 0xFF
    ctx.setHalf(false)
    ctx.setNegative(false)
    ctx.setCarry(h > 0)    
    ctx.A = rot
  }
  val RLCA = Opcode(0x07,4,1,"RLCA") { ctx => rlca(ctx) }
  // *** RRCA
  // **************
  @inline private def rrca(ctx:Context) {
    val value = ctx.A
    val carry = value & 0x01
    val rot = (value >> 1 | carry << 7) & 0xFF
    ctx.setHalf(false)
    ctx.setNegative(false)
    ctx.setCarry(carry > 0)    
    ctx.A = rot
  }
  val RRCA = Opcode(0x0F,4,1,"RRCA") { ctx => rrca(ctx) }
  // *** RLA
  // **************
  @inline private def rla(ctx:Context) {
    val value = ctx.A
    val carry = ctx.carry
    val h = value & 0x80
    val rot = (value << 1 | carry) & 0xFF
    ctx.setHalf(false)
    ctx.setNegative(false)
    ctx.setCarry(h > 0)    
    ctx.A = rot
  }
  val RLA = Opcode(0x17,4,1,"RLA") { ctx => rla(ctx) }
  // *** RRA
  // **************
  @inline private def rra(ctx:Context) {
    val value = ctx.A
    val carry = ctx.carry
    val l = value & 0x01
    val rot = (value >> 1 | carry << 7) & 0xFF
    ctx.setHalf(false)
    ctx.setNegative(false)
    ctx.setCarry(l > 0)    
    ctx.A = rot
  }
  val RRA = Opcode(0x1F,4,1,"RRA") { ctx => rra(ctx) }
  // *** RLD
  // **************
  val RLD = Opcode((0xED,0x6F),18,2,"RLD") { ctx =>
    val memHL = ctx.read(ctx.HL)
    ctx.write(ctx.HL,(memHL & 0x0F) << 4 | ctx.A & 0x0F)
    ctx.A = ctx.A & 0xF0 | (memHL & 0xF0) >> 4
    ctx.F = ctx.SZP(ctx.A) | ctx.carry
    ctx.setHalf(false)    
    ctx.setNegative(false)
  }
  // *** RRD
  // **************
  val RRD = Opcode((0xED,0x67),18,2,"RRD") { ctx =>
    val memHL = ctx.read(ctx.HL)
    ctx.write(ctx.HL,(ctx.A & 0x0F) << 4 | (memHL & 0xF0) >> 4)
    ctx.A = ctx.A & 0xF0 | memHL & 0x0F
    ctx.F = ctx.SZP(ctx.A) | ctx.carry
    ctx.setHalf(false)    
    ctx.setNegative(false)
  }
  // ==================================== Bit manipulation ===================================================
  // *** BIT b,r
  // **************
  @inline private def bit(ctx:Context,b:Int,value:Int) = {
    val isZero = (value & (1 << b)) == 0
    ctx.setZero(isZero)
    ctx.setParity(isZero)
    ctx.setHalf(true)
    ctx.setNegative(false)
    ctx.setSign(b == 7 && (value & 0x80) > 0)
  }
  val BIT_0_A = Opcode((0xCB,0x47),8,2,"BIT 0,A") { ctx => bit(ctx,0,ctx.A) }
  val BIT_0_B = Opcode((0xCB,0x40),8,2,"BIT 0,B") { ctx => bit(ctx,0,ctx.B) }
  val BIT_0_C = Opcode((0xCB,0x41),8,2,"BIT 0,C") { ctx => bit(ctx,0,ctx.C) }
  val BIT_0_D = Opcode((0xCB,0x42),8,2,"BIT 0,D") { ctx => bit(ctx,0,ctx.D) }
  val BIT_0_E = Opcode((0xCB,0x43),8,2,"BIT 0,E") { ctx => bit(ctx,0,ctx.E) }
  val BIT_0_H = Opcode((0xCB,0x44),8,2,"BIT 0,H") { ctx => bit(ctx,0,ctx.H) }
  val BIT_0_L = Opcode((0xCB,0x45),8,2,"BIT 0,L") { ctx => bit(ctx,0,ctx.L) }
  val BIT_1_A = Opcode((0xCB,0x4F),8,2,"BIT 1,A") { ctx => bit(ctx,1,ctx.A) }
  val BIT_1_B = Opcode((0xCB,0x48),8,2,"BIT 1,B") { ctx => bit(ctx,1,ctx.B) }
  val BIT_1_C = Opcode((0xCB,0x49),8,2,"BIT 1,C") { ctx => bit(ctx,1,ctx.C) }
  val BIT_1_D = Opcode((0xCB,0x4A),8,2,"BIT 1,D") { ctx => bit(ctx,1,ctx.D) }
  val BIT_1_E = Opcode((0xCB,0x4B),8,2,"BIT 1,E") { ctx => bit(ctx,1,ctx.E) }
  val BIT_1_H = Opcode((0xCB,0x4C),8,2,"BIT 1,H") { ctx => bit(ctx,1,ctx.H) }
  val BIT_1_L = Opcode((0xCB,0x4D),8,2,"BIT 1,L") { ctx => bit(ctx,1,ctx.L) }
  val BIT_2_A = Opcode((0xCB,0x57),8,2,"BIT 2,A") { ctx => bit(ctx,2,ctx.A) }
  val BIT_2_B = Opcode((0xCB,0x50),8,2,"BIT 2,B") { ctx => bit(ctx,2,ctx.B) }
  val BIT_2_C = Opcode((0xCB,0x51),8,2,"BIT 2,C") { ctx => bit(ctx,2,ctx.C) }
  val BIT_2_D = Opcode((0xCB,0x52),8,2,"BIT 2,D") { ctx => bit(ctx,2,ctx.D) }
  val BIT_2_E = Opcode((0xCB,0x53),8,2,"BIT 2,E") { ctx => bit(ctx,2,ctx.E) }
  val BIT_2_H = Opcode((0xCB,0x54),8,2,"BIT 2,H") { ctx => bit(ctx,2,ctx.H) }
  val BIT_2_L = Opcode((0xCB,0x55),8,2,"BIT 2,L") { ctx => bit(ctx,2,ctx.L) }
  val BIT_3_A = Opcode((0xCB,0x5F),8,2,"BIT 3,A") { ctx => bit(ctx,3,ctx.A) }
  val BIT_3_B = Opcode((0xCB,0x58),8,2,"BIT 3,B") { ctx => bit(ctx,3,ctx.B) }
  val BIT_3_C = Opcode((0xCB,0x59),8,2,"BIT 3,C") { ctx => bit(ctx,3,ctx.C) }
  val BIT_3_D = Opcode((0xCB,0x5A),8,2,"BIT 3,D") { ctx => bit(ctx,3,ctx.D) }
  val BIT_3_E = Opcode((0xCB,0x5B),8,2,"BIT 3,E") { ctx => bit(ctx,3,ctx.E) }
  val BIT_3_H = Opcode((0xCB,0x5C),8,2,"BIT 3,H") { ctx => bit(ctx,3,ctx.H) }
  val BIT_3_L = Opcode((0xCB,0x5D),8,2,"BIT 3,L") { ctx => bit(ctx,3,ctx.L) }
  val BIT_4_A = Opcode((0xCB,0x67),8,2,"BIT 4,A") { ctx => bit(ctx,4,ctx.A) }
  val BIT_4_B = Opcode((0xCB,0x60),8,2,"BIT 4,B") { ctx => bit(ctx,4,ctx.B) }
  val BIT_4_C = Opcode((0xCB,0x61),8,2,"BIT 4,C") { ctx => bit(ctx,4,ctx.C) }
  val BIT_4_D = Opcode((0xCB,0x62),8,2,"BIT 4,D") { ctx => bit(ctx,4,ctx.D) }
  val BIT_4_E = Opcode((0xCB,0x63),8,2,"BIT 4,E") { ctx => bit(ctx,4,ctx.E) }
  val BIT_4_H = Opcode((0xCB,0x64),8,2,"BIT 4,H") { ctx => bit(ctx,4,ctx.H) }
  val BIT_4_L = Opcode((0xCB,0x65),8,2,"BIT 4,L") { ctx => bit(ctx,4,ctx.L) }
  val BIT_5_A = Opcode((0xCB,0x6F),8,2,"BIT 5,A") { ctx => bit(ctx,5,ctx.A) }
  val BIT_5_B = Opcode((0xCB,0x68),8,2,"BIT 5,B") { ctx => bit(ctx,5,ctx.B) }
  val BIT_5_C = Opcode((0xCB,0x69),8,2,"BIT 5,C") { ctx => bit(ctx,5,ctx.C) }
  val BIT_5_D = Opcode((0xCB,0x6A),8,2,"BIT 5,D") { ctx => bit(ctx,5,ctx.D) }
  val BIT_5_E = Opcode((0xCB,0x6B),8,2,"BIT 5,E") { ctx => bit(ctx,5,ctx.E) }
  val BIT_5_H = Opcode((0xCB,0x6C),8,2,"BIT 5,H") { ctx => bit(ctx,5,ctx.H) }
  val BIT_5_L = Opcode((0xCB,0x6D),8,2,"BIT 5,L") { ctx => bit(ctx,5,ctx.L) }
  val BIT_6_A = Opcode((0xCB,0x77),8,2,"BIT 6,A") { ctx => bit(ctx,6,ctx.A) }
  val BIT_6_B = Opcode((0xCB,0x70),8,2,"BIT 6,B") { ctx => bit(ctx,6,ctx.B) }
  val BIT_6_C = Opcode((0xCB,0x71),8,2,"BIT 6,C") { ctx => bit(ctx,6,ctx.C) }
  val BIT_6_D = Opcode((0xCB,0x72),8,2,"BIT 6,D") { ctx => bit(ctx,6,ctx.D) }
  val BIT_6_E = Opcode((0xCB,0x73),8,2,"BIT 6,E") { ctx => bit(ctx,6,ctx.E) }
  val BIT_6_H = Opcode((0xCB,0x74),8,2,"BIT 6,H") { ctx => bit(ctx,6,ctx.H) }
  val BIT_6_L = Opcode((0xCB,0x75),8,2,"BIT 6,L") { ctx => bit(ctx,6,ctx.L) }
  val BIT_7_A = Opcode((0xCB,0x7F),8,2,"BIT 7,A") { ctx => bit(ctx,7,ctx.A) }
  val BIT_7_B = Opcode((0xCB,0x78),8,2,"BIT 7,B") { ctx => bit(ctx,7,ctx.B) }
  val BIT_7_C = Opcode((0xCB,0x79),8,2,"BIT 7,C") { ctx => bit(ctx,7,ctx.C) }
  val BIT_7_D = Opcode((0xCB,0x7A),8,2,"BIT 7,D") { ctx => bit(ctx,7,ctx.D) }
  val BIT_7_E = Opcode((0xCB,0x7B),8,2,"BIT 7,E") { ctx => bit(ctx,7,ctx.E) }
  val BIT_7_H = Opcode((0xCB,0x7C),8,2,"BIT 7,H") { ctx => bit(ctx,7,ctx.H) }
  val BIT_7_L = Opcode((0xCB,0x7D),8,2,"BIT 7,L") { ctx => bit(ctx,7,ctx.L) }
  // *** BIT b,(HL)
  // **************
  val BIT_0_$HL$ = Opcode((0xCB,0x46),12,2,"BIT 0,(HL)") { ctx => bit(ctx,0,ctx.read(ctx.HL)) }
  val BIT_1_$HL$ = Opcode((0xCB,0x4E),12,2,"BIT 1,(HL)") { ctx => bit(ctx,1,ctx.read(ctx.HL)) }
  val BIT_2_$HL$ = Opcode((0xCB,0x56),12,2,"BIT 2,(HL)") { ctx => bit(ctx,2,ctx.read(ctx.HL)) }
  val BIT_3_$HL$ = Opcode((0xCB,0x5E),12,2,"BIT 3,(HL)") { ctx => bit(ctx,3,ctx.read(ctx.HL)) }
  val BIT_4_$HL$ = Opcode((0xCB,0x66),12,2,"BIT 4,(HL)") { ctx => bit(ctx,4,ctx.read(ctx.HL)) }
  val BIT_5_$HL$ = Opcode((0xCB,0x6E),12,2,"BIT 5,(HL)") { ctx => bit(ctx,5,ctx.read(ctx.HL)) }
  val BIT_6_$HL$ = Opcode((0xCB,0x76),12,2,"BIT 6,(HL)") { ctx => bit(ctx,6,ctx.read(ctx.HL)) }
  val BIT_7_$HL$ = Opcode((0xCB,0x7E),12,2,"BIT 7,(HL)") { ctx => bit(ctx,7,ctx.read(ctx.HL)) }
  // *** BIT b,(IX + d)
  // **************
  val BIT_0_$IX_d$ = Opcode((0xDD,0xCB,0x46),20,4,MNEMONIC_IXY_d("BIT 0,(IX%s)")) { ctx => bit(ctx,0,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_1_$IX_d$ = Opcode((0xDD,0xCB,0x4E),20,4,MNEMONIC_IXY_d("BIT 1,(IX%s)")) { ctx => bit(ctx,1,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_2_$IX_d$ = Opcode((0xDD,0xCB,0x56),20,4,MNEMONIC_IXY_d("BIT 2,(IX%s)")) { ctx => bit(ctx,2,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_3_$IX_d$ = Opcode((0xDD,0xCB,0x5E),20,4,MNEMONIC_IXY_d("BIT 3,(IX%s)")) { ctx => bit(ctx,3,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_4_$IX_d$ = Opcode((0xDD,0xCB,0x66),20,4,MNEMONIC_IXY_d("BIT 4,(IX%s)")) { ctx => bit(ctx,4,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_5_$IX_d$ = Opcode((0xDD,0xCB,0x6E),20,4,MNEMONIC_IXY_d("BIT 5,(IX%s)")) { ctx => bit(ctx,5,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_6_$IX_d$ = Opcode((0xDD,0xCB,0x76),20,4,MNEMONIC_IXY_d("BIT 6,(IX%s)")) { ctx => bit(ctx,6,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  val BIT_7_$IX_d$ = Opcode((0xDD,0xCB,0x7E),20,4,MNEMONIC_IXY_d("BIT 7,(IX%s)")) { ctx => bit(ctx,7,ctx.read(ctx.IX_+(ctx.byte(2)))) }
  // *** BIT b,(IY + d)
  // **************
  val BIT_0_$IY_d$ = Opcode((0xFD,0xCB,0x46),20,4,MNEMONIC_IXY_d("BIT 0,(IY%s)")) { ctx => bit(ctx,0,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_1_$IY_d$ = Opcode((0xFD,0xCB,0x4E),20,4,MNEMONIC_IXY_d("BIT 1,(IY%s)")) { ctx => bit(ctx,1,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_2_$IY_d$ = Opcode((0xFD,0xCB,0x56),20,4,MNEMONIC_IXY_d("BIT 2,(IY%s)")) { ctx => bit(ctx,2,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_3_$IY_d$ = Opcode((0xFD,0xCB,0x5E),20,4,MNEMONIC_IXY_d("BIT 3,(IY%s)")) { ctx => bit(ctx,3,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_4_$IY_d$ = Opcode((0xFD,0xCB,0x66),20,4,MNEMONIC_IXY_d("BIT 4,(IY%s)")) { ctx => bit(ctx,4,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_5_$IY_d$ = Opcode((0xFD,0xCB,0x6E),20,4,MNEMONIC_IXY_d("BIT 5,(IY%s)")) { ctx => bit(ctx,5,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_6_$IY_d$ = Opcode((0xFD,0xCB,0x76),20,4,MNEMONIC_IXY_d("BIT 6,(IY%s)")) { ctx => bit(ctx,6,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  val BIT_7_$IY_d$ = Opcode((0xFD,0xCB,0x7E),20,4,MNEMONIC_IXY_d("BIT 7,(IY%s)")) { ctx => bit(ctx,7,ctx.read(ctx.IY_+(ctx.byte(2)))) }
  // *** RES b,r
  // **************
  @inline private def res(b:Int,value:Int) = value & ~(1 << b)
  val RES_0_A = Opcode((0xCB,0x87),8,2,"RES 0,A") { ctx => ctx.A = res(0,ctx.A) }
  val RES_0_B = Opcode((0xCB,0x80),8,2,"RES 0,B") { ctx => ctx.B = res(0,ctx.B) }
  val RES_0_C = Opcode((0xCB,0x81),8,2,"RES 0,C") { ctx => ctx.C = res(0,ctx.C) }
  val RES_0_D = Opcode((0xCB,0x82),8,2,"RES 0,D") { ctx => ctx.D = res(0,ctx.D) }
  val RES_0_E = Opcode((0xCB,0x83),8,2,"RES 0,E") { ctx => ctx.E = res(0,ctx.E) }
  val RES_0_H = Opcode((0xCB,0x84),8,2,"RES 0,H") { ctx => ctx.H = res(0,ctx.H) }
  val RES_0_L = Opcode((0xCB,0x85),8,2,"RES 0,L") { ctx => ctx.L = res(0,ctx.L) }
  val RES_1_A = Opcode((0xCB,0x8F),8,2,"RES 1,A") { ctx => ctx.A = res(1,ctx.A) }
  val RES_1_B = Opcode((0xCB,0x88),8,2,"RES 1,B") { ctx => ctx.B = res(1,ctx.B) }
  val RES_1_C = Opcode((0xCB,0x89),8,2,"RES 1,C") { ctx => ctx.C = res(1,ctx.C) }
  val RES_1_D = Opcode((0xCB,0x8A),8,2,"RES 1,D") { ctx => ctx.D = res(1,ctx.D) }
  val RES_1_E = Opcode((0xCB,0x8B),8,2,"RES 1,E") { ctx => ctx.E = res(1,ctx.E) }
  val RES_1_H = Opcode((0xCB,0x8C),8,2,"RES 1,H") { ctx => ctx.H = res(1,ctx.H) }
  val RES_1_L = Opcode((0xCB,0x8D),8,2,"RES 1,L") { ctx => ctx.L = res(1,ctx.L) }
  val RES_2_A = Opcode((0xCB,0x97),8,2,"RES 2,A") { ctx => ctx.A = res(2,ctx.A) }
  val RES_2_B = Opcode((0xCB,0x90),8,2,"RES 2,B") { ctx => ctx.B = res(2,ctx.B) }
  val RES_2_C = Opcode((0xCB,0x91),8,2,"RES 2,C") { ctx => ctx.C = res(2,ctx.C) }
  val RES_2_D = Opcode((0xCB,0x92),8,2,"RES 2,D") { ctx => ctx.D = res(2,ctx.D) }
  val RES_2_E = Opcode((0xCB,0x93),8,2,"RES 2,E") { ctx => ctx.E = res(2,ctx.E) }
  val RES_2_H = Opcode((0xCB,0x94),8,2,"RES 2,H") { ctx => ctx.H = res(2,ctx.H) }
  val RES_2_L = Opcode((0xCB,0x95),8,2,"RES 2,L") { ctx => ctx.L = res(2,ctx.L) }
  val RES_3_A = Opcode((0xCB,0x9F),8,2,"RES 3,A") { ctx => ctx.A = res(3,ctx.A) }
  val RES_3_B = Opcode((0xCB,0x98),8,2,"RES 3,B") { ctx => ctx.B = res(3,ctx.B) }
  val RES_3_C = Opcode((0xCB,0x99),8,2,"RES 3,C") { ctx => ctx.C = res(3,ctx.C) }
  val RES_3_D = Opcode((0xCB,0x9A),8,2,"RES 3,D") { ctx => ctx.D = res(3,ctx.D) }
  val RES_3_E = Opcode((0xCB,0x9B),8,2,"RES 3,E") { ctx => ctx.E = res(3,ctx.E) }
  val RES_3_H = Opcode((0xCB,0x9C),8,2,"RES 3,H") { ctx => ctx.H = res(3,ctx.H) }
  val RES_3_L = Opcode((0xCB,0x9D),8,2,"RES 3,L") { ctx => ctx.L = res(3,ctx.L) }
  val RES_4_A = Opcode((0xCB,0xA7),8,2,"RES 4,A") { ctx => ctx.A = res(4,ctx.A) }
  val RES_4_B = Opcode((0xCB,0xA0),8,2,"RES 4,B") { ctx => ctx.B = res(4,ctx.B) }
  val RES_4_C = Opcode((0xCB,0xA1),8,2,"RES 4,C") { ctx => ctx.C = res(4,ctx.C) }
  val RES_4_D = Opcode((0xCB,0xA2),8,2,"RES 4,D") { ctx => ctx.D = res(4,ctx.D) }
  val RES_4_E = Opcode((0xCB,0xA3),8,2,"RES 4,E") { ctx => ctx.E = res(4,ctx.E) }
  val RES_4_H = Opcode((0xCB,0xA4),8,2,"RES 4,H") { ctx => ctx.H = res(4,ctx.H) }
  val RES_4_L = Opcode((0xCB,0xA5),8,2,"RES 4,L") { ctx => ctx.L = res(4,ctx.L) }
  val RES_5_A = Opcode((0xCB,0xAF),8,2,"RES 5,A") { ctx => ctx.A = res(5,ctx.A) }
  val RES_5_B = Opcode((0xCB,0xA8),8,2,"RES 5,B") { ctx => ctx.B = res(5,ctx.B) }
  val RES_5_C = Opcode((0xCB,0xA9),8,2,"RES 5,C") { ctx => ctx.C = res(5,ctx.C) }
  val RES_5_D = Opcode((0xCB,0xAA),8,2,"RES 5,D") { ctx => ctx.D = res(5,ctx.D) }
  val RES_5_E = Opcode((0xCB,0xAB),8,2,"RES 5,E") { ctx => ctx.E = res(5,ctx.E) }
  val RES_5_H = Opcode((0xCB,0xAC),8,2,"RES 5,H") { ctx => ctx.H = res(5,ctx.H) }
  val RES_5_L = Opcode((0xCB,0xAD),8,2,"RES 5,L") { ctx => ctx.L = res(5,ctx.L) }
  val RES_6_A = Opcode((0xCB,0xB7),8,2,"RES 6,A") { ctx => ctx.A = res(6,ctx.A) }
  val RES_6_B = Opcode((0xCB,0xB0),8,2,"RES 6,B") { ctx => ctx.B = res(6,ctx.B) }
  val RES_6_C = Opcode((0xCB,0xB1),8,2,"RES 6,C") { ctx => ctx.C = res(6,ctx.C) }
  val RES_6_D = Opcode((0xCB,0xB2),8,2,"RES 6,D") { ctx => ctx.D = res(6,ctx.D) }
  val RES_6_E = Opcode((0xCB,0xB3),8,2,"RES 6,E") { ctx => ctx.E = res(6,ctx.E) }
  val RES_6_H = Opcode((0xCB,0xB4),8,2,"RES 6,H") { ctx => ctx.H = res(6,ctx.H) }
  val RES_6_L = Opcode((0xCB,0xB5),8,2,"RES 6,L") { ctx => ctx.L = res(6,ctx.L) }
  val RES_7_A = Opcode((0xCB,0xBF),8,2,"RES 7,A") { ctx => ctx.A = res(7,ctx.A) }
  val RES_7_B = Opcode((0xCB,0xB8),8,2,"RES 7,B") { ctx => ctx.B = res(7,ctx.B) }
  val RES_7_C = Opcode((0xCB,0xB9),8,2,"RES 7,C") { ctx => ctx.C = res(7,ctx.C) }
  val RES_7_D = Opcode((0xCB,0xBA),8,2,"RES 7,D") { ctx => ctx.D = res(7,ctx.D) }
  val RES_7_E = Opcode((0xCB,0xBB),8,2,"RES 7,E") { ctx => ctx.E = res(7,ctx.E) }
  val RES_7_H = Opcode((0xCB,0xBC),8,2,"RES 7,H") { ctx => ctx.H = res(7,ctx.H) }
  val RES_7_L = Opcode((0xCB,0xBD),8,2,"RES 7,L") { ctx => ctx.L = res(7,ctx.L) }
  // *** RES b,(HL)
  // **************
  val RES_0_$HL$ = Opcode((0xCB,0x86),15,2,"RES 0,(HL)") { ctx => ctx.write(ctx.HL,res(0,ctx.read(ctx.HL))) }
  val RES_1_$HL$ = Opcode((0xCB,0x8E),15,2,"RES 1,(HL)") { ctx => ctx.write(ctx.HL,res(1,ctx.read(ctx.HL))) }
  val RES_2_$HL$ = Opcode((0xCB,0x96),15,2,"RES 2,(HL)") { ctx => ctx.write(ctx.HL,res(2,ctx.read(ctx.HL))) }
  val RES_3_$HL$ = Opcode((0xCB,0x9E),15,2,"RES 3,(HL)") { ctx => ctx.write(ctx.HL,res(3,ctx.read(ctx.HL))) }
  val RES_4_$HL$ = Opcode((0xCB,0xA6),15,2,"RES 4,(HL)") { ctx => ctx.write(ctx.HL,res(4,ctx.read(ctx.HL))) }
  val RES_5_$HL$ = Opcode((0xCB,0xAE),15,2,"RES 5,(HL)") { ctx => ctx.write(ctx.HL,res(5,ctx.read(ctx.HL))) }
  val RES_6_$HL$ = Opcode((0xCB,0xB6),15,2,"RES 6,(HL)") { ctx => ctx.write(ctx.HL,res(6,ctx.read(ctx.HL))) }
  val RES_7_$HL$ = Opcode((0xCB,0xBE),15,2,"RES 7,(HL)") { ctx => ctx.write(ctx.HL,res(7,ctx.read(ctx.HL))) }
  // *** RES b,(IX + d)
  // **************
  val RES_0_$IX_d$ = Opcode((0xDD,0xCB,0x86),23,4,MNEMONIC_IXY_d("RES 0,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(0,ctx.read(adr))) }  
  val RES_1_$IX_d$ = Opcode((0xDD,0xCB,0x8E),23,4,MNEMONIC_IXY_d("RES 1,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(1,ctx.read(adr))) }
  val RES_2_$IX_d$ = Opcode((0xDD,0xCB,0x96),23,4,MNEMONIC_IXY_d("RES 2,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(2,ctx.read(adr))) }
  val RES_3_$IX_d$ = Opcode((0xDD,0xCB,0x9E),23,4,MNEMONIC_IXY_d("RES 3,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(3,ctx.read(adr))) }
  val RES_4_$IX_d$ = Opcode((0xDD,0xCB,0xA6),23,4,MNEMONIC_IXY_d("RES 4,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(4,ctx.read(adr))) }
  val RES_5_$IX_d$ = Opcode((0xDD,0xCB,0xAE),23,4,MNEMONIC_IXY_d("RES 5,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(5,ctx.read(adr))) }
  val RES_6_$IX_d$ = Opcode((0xDD,0xCB,0xB6),23,4,MNEMONIC_IXY_d("RES 6,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(6,ctx.read(adr))) }
  val RES_7_$IX_d$ = Opcode((0xDD,0xCB,0xBE),23,4,MNEMONIC_IXY_d("RES 7,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,res(7,ctx.read(adr))) }
  // *** RES b,(IY + d)
  // **************
  val RES_0_$IY_d$ = Opcode((0xFD,0xCB,0x86),23,4,MNEMONIC_IXY_d("RES 0,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(0,ctx.read(adr))) }
  val RES_1_$IY_d$ = Opcode((0xFD,0xCB,0x8E),23,4,MNEMONIC_IXY_d("RES 1,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(1,ctx.read(adr))) }
  val RES_2_$IY_d$ = Opcode((0xFD,0xCB,0x96),23,4,MNEMONIC_IXY_d("RES 2,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(2,ctx.read(adr))) }
  val RES_3_$IY_d$ = Opcode((0xFD,0xCB,0x9E),23,4,MNEMONIC_IXY_d("RES 3,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(3,ctx.read(adr))) }
  val RES_4_$IY_d$ = Opcode((0xFD,0xCB,0xA6),23,4,MNEMONIC_IXY_d("RES 4,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(4,ctx.read(adr))) }
  val RES_5_$IY_d$ = Opcode((0xFD,0xCB,0xAE),23,4,MNEMONIC_IXY_d("RES 5,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(5,ctx.read(adr))) }
  val RES_6_$IY_d$ = Opcode((0xFD,0xCB,0xB6),23,4,MNEMONIC_IXY_d("RES 6,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(6,ctx.read(adr))) }
  val RES_7_$IY_d$ = Opcode((0xFD,0xCB,0xBE),23,4,MNEMONIC_IXY_d("RES 7,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,res(7,ctx.read(adr))) }
  // *** SET b,r
  // **************
  @inline private def set(b:Int,value:Int) = value | (1 << b)
  val SET_0_A = Opcode((0xCB,0xC7),8,2,"SET 0,A") { ctx => ctx.A = set(0,ctx.A) }
  val SET_0_B = Opcode((0xCB,0xC0),8,2,"SET 0,B") { ctx => ctx.B = set(0,ctx.B) }
  val SET_0_C = Opcode((0xCB,0xC1),8,2,"SET 0,C") { ctx => ctx.C = set(0,ctx.C) }
  val SET_0_D = Opcode((0xCB,0xC2),8,2,"SET 0,D") { ctx => ctx.D = set(0,ctx.D) }
  val SET_0_E = Opcode((0xCB,0xC3),8,2,"SET 0,E") { ctx => ctx.E = set(0,ctx.E) }
  val SET_0_H = Opcode((0xCB,0xC4),8,2,"SET 0,H") { ctx => ctx.H = set(0,ctx.H) }
  val SET_0_L = Opcode((0xCB,0xC5),8,2,"SET 0,L") { ctx => ctx.L = set(0,ctx.L) }
  val SET_1_A = Opcode((0xCB,0xCF),8,2,"SET 1,A") { ctx => ctx.A = set(1,ctx.A) }
  val SET_1_B = Opcode((0xCB,0xC8),8,2,"SET 1,B") { ctx => ctx.B = set(1,ctx.B) }
  val SET_1_C = Opcode((0xCB,0xC9),8,2,"SET 1,C") { ctx => ctx.C = set(1,ctx.C) }
  val SET_1_D = Opcode((0xCB,0xCA),8,2,"SET 1,D") { ctx => ctx.D = set(1,ctx.D) }
  val SET_1_E = Opcode((0xCB,0xCB),8,2,"SET 1,E") { ctx => ctx.E = set(1,ctx.E) }
  val SET_1_H = Opcode((0xCB,0xCC),8,2,"SET 1,H") { ctx => ctx.H = set(1,ctx.H) }
  val SET_1_L = Opcode((0xCB,0xCD),8,2,"SET 1,L") { ctx => ctx.L = set(1,ctx.L) }
  val SET_2_A = Opcode((0xCB,0xD7),8,2,"SET 2,A") { ctx => ctx.A = set(2,ctx.A) }
  val SET_2_B = Opcode((0xCB,0xD0),8,2,"SET 2,B") { ctx => ctx.B = set(2,ctx.B) }
  val SET_2_C = Opcode((0xCB,0xD1),8,2,"SET 2,C") { ctx => ctx.C = set(2,ctx.C) }
  val SET_2_D = Opcode((0xCB,0xD2),8,2,"SET 2,D") { ctx => ctx.D = set(2,ctx.D) }
  val SET_2_E = Opcode((0xCB,0xD3),8,2,"SET 2,E") { ctx => ctx.E = set(2,ctx.E) }
  val SET_2_H = Opcode((0xCB,0xD4),8,2,"SET 2,H") { ctx => ctx.H = set(2,ctx.H) }
  val SET_2_L = Opcode((0xCB,0xD5),8,2,"SET 2,L") { ctx => ctx.L = set(2,ctx.L) }
  val SET_3_A = Opcode((0xCB,0xDF),8,2,"SET 3,A") { ctx => ctx.A = set(3,ctx.A) }
  val SET_3_B = Opcode((0xCB,0xD8),8,2,"SET 3,B") { ctx => ctx.B = set(3,ctx.B) }
  val SET_3_C = Opcode((0xCB,0xD9),8,2,"SET 3,C") { ctx => ctx.C = set(3,ctx.C) }
  val SET_3_D = Opcode((0xCB,0xDA),8,2,"SET 3,D") { ctx => ctx.D = set(3,ctx.D) }
  val SET_3_E = Opcode((0xCB,0xDB),8,2,"SET 3,E") { ctx => ctx.E = set(3,ctx.E) }
  val SET_3_H = Opcode((0xCB,0xDC),8,2,"SET 3,H") { ctx => ctx.H = set(3,ctx.H) }
  val SET_3_L = Opcode((0xCB,0xDD),8,2,"SET 3,L") { ctx => ctx.L = set(3,ctx.L) }
  val SET_4_A = Opcode((0xCB,0xE7),8,2,"SET 4,A") { ctx => ctx.A = set(4,ctx.A) }
  val SET_4_B = Opcode((0xCB,0xE0),8,2,"SET 4,B") { ctx => ctx.B = set(4,ctx.B) }
  val SET_4_C = Opcode((0xCB,0xE1),8,2,"SET 4,C") { ctx => ctx.C = set(4,ctx.C) }
  val SET_4_D = Opcode((0xCB,0xE2),8,2,"SET 4,D") { ctx => ctx.D = set(4,ctx.D) }
  val SET_4_E = Opcode((0xCB,0xE3),8,2,"SET 4,E") { ctx => ctx.E = set(4,ctx.E) }
  val SET_4_H = Opcode((0xCB,0xE4),8,2,"SET 4,H") { ctx => ctx.H = set(4,ctx.H) }
  val SET_4_L = Opcode((0xCB,0xE5),8,2,"SET 4,L") { ctx => ctx.L = set(4,ctx.L) }
  val SET_5_A = Opcode((0xCB,0xEF),8,2,"SET 5,A") { ctx => ctx.A = set(5,ctx.A) }
  val SET_5_B = Opcode((0xCB,0xE8),8,2,"SET 5,B") { ctx => ctx.B = set(5,ctx.B) }
  val SET_5_C = Opcode((0xCB,0xE9),8,2,"SET 5,C") { ctx => ctx.C = set(5,ctx.C) }
  val SET_5_D = Opcode((0xCB,0xEA),8,2,"SET 5,D") { ctx => ctx.D = set(5,ctx.D) }
  val SET_5_E = Opcode((0xCB,0xEB),8,2,"SET 5,E") { ctx => ctx.E = set(5,ctx.E) }
  val SET_5_H = Opcode((0xCB,0xEC),8,2,"SET 5,H") { ctx => ctx.H = set(5,ctx.H) }
  val SET_5_L = Opcode((0xCB,0xED),8,2,"SET 5,L") { ctx => ctx.L = set(5,ctx.L) }
  val SET_6_A = Opcode((0xCB,0xF7),8,2,"SET 6,A") { ctx => ctx.A = set(6,ctx.A) }
  val SET_6_B = Opcode((0xCB,0xF0),8,2,"SET 6,B") { ctx => ctx.B = set(6,ctx.B) }
  val SET_6_C = Opcode((0xCB,0xF1),8,2,"SET 6,C") { ctx => ctx.C = set(6,ctx.C) }
  val SET_6_D = Opcode((0xCB,0xF2),8,2,"SET 6,D") { ctx => ctx.D = set(6,ctx.D) }
  val SET_6_E = Opcode((0xCB,0xF3),8,2,"SET 6,E") { ctx => ctx.E = set(6,ctx.E) }
  val SET_6_H = Opcode((0xCB,0xF4),8,2,"SET 6,H") { ctx => ctx.H = set(6,ctx.H) }
  val SET_6_L = Opcode((0xCB,0xF5),8,2,"SET 6,L") { ctx => ctx.L = set(6,ctx.L) }
  val SET_7_A = Opcode((0xCB,0xFF),8,2,"SET 7,A") { ctx => ctx.A = set(7,ctx.A) }
  val SET_7_B = Opcode((0xCB,0xF8),8,2,"SET 7,B") { ctx => ctx.B = set(7,ctx.B) }
  val SET_7_C = Opcode((0xCB,0xF9),8,2,"SET 7,C") { ctx => ctx.C = set(7,ctx.C) }
  val SET_7_D = Opcode((0xCB,0xFA),8,2,"SET 7,D") { ctx => ctx.D = set(7,ctx.D) }
  val SET_7_E = Opcode((0xCB,0xFB),8,2,"SET 7,E") { ctx => ctx.E = set(7,ctx.E) }
  val SET_7_H = Opcode((0xCB,0xFC),8,2,"SET 7,H") { ctx => ctx.H = set(7,ctx.H) }
  val SET_7_L = Opcode((0xCB,0xFD),8,2,"SET 7,L") { ctx => ctx.L = set(7,ctx.L) }
  // *** SET b,(HL)
  // **************
  val SET_0_$HL$ = Opcode((0xCB,0xC6),15,2,"SET 0,(HL)") { ctx => ctx.write(ctx.HL,set(0,ctx.read(ctx.HL))) }
  val SET_1_$HL$ = Opcode((0xCB,0xCE),15,2,"SET 1,(HL)") { ctx => ctx.write(ctx.HL,set(1,ctx.read(ctx.HL))) }
  val SET_2_$HL$ = Opcode((0xCB,0xD6),15,2,"SET 2,(HL)") { ctx => ctx.write(ctx.HL,set(2,ctx.read(ctx.HL))) }
  val SET_3_$HL$ = Opcode((0xCB,0xDE),15,2,"SET 3,(HL)") { ctx => ctx.write(ctx.HL,set(3,ctx.read(ctx.HL))) }
  val SET_4_$HL$ = Opcode((0xCB,0xE6),15,2,"SET 4,(HL)") { ctx => ctx.write(ctx.HL,set(4,ctx.read(ctx.HL))) }
  val SET_5_$HL$ = Opcode((0xCB,0xEE),15,2,"SET 5,(HL)") { ctx => ctx.write(ctx.HL,set(5,ctx.read(ctx.HL))) }
  val SET_6_$HL$ = Opcode((0xCB,0xF6),15,2,"SET 6,(HL)") { ctx => ctx.write(ctx.HL,set(6,ctx.read(ctx.HL))) }
  val SET_7_$HL$ = Opcode((0xCB,0xFE),15,2,"SET 7,(HL)") { ctx => ctx.write(ctx.HL,set(7,ctx.read(ctx.HL))) }
  // *** SET b,(IX + d)
  // **************
  val SET_0_$IX_d$ = Opcode((0xDD,0xCB,0xC6),23,4,MNEMONIC_IXY_d("SET 0,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(0,ctx.read(adr))) }
  val SET_1_$IX_d$ = Opcode((0xDD,0xCB,0xCE),23,4,MNEMONIC_IXY_d("SET 1,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(1,ctx.read(adr))) }
  val SET_2_$IX_d$ = Opcode((0xDD,0xCB,0xD6),23,4,MNEMONIC_IXY_d("SET 2,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(2,ctx.read(adr))) }
  val SET_3_$IX_d$ = Opcode((0xDD,0xCB,0xDE),23,4,MNEMONIC_IXY_d("SET 3,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(3,ctx.read(adr))) }
  val SET_4_$IX_d$ = Opcode((0xDD,0xCB,0xE6),23,4,MNEMONIC_IXY_d("SET 4,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(4,ctx.read(adr))) }
  val SET_5_$IX_d$ = Opcode((0xDD,0xCB,0xEE),23,4,MNEMONIC_IXY_d("SET 5,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(5,ctx.read(adr))) }
  val SET_6_$IX_d$ = Opcode((0xDD,0xCB,0xF6),23,4,MNEMONIC_IXY_d("SET 6,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(6,ctx.read(adr))) }
  val SET_7_$IX_d$ = Opcode((0xDD,0xCB,0xFE),23,4,MNEMONIC_IXY_d("SET 7,(IX%s)")) { ctx => val adr = ctx.IX_+(ctx.byte(2)) ; ctx.write(adr,set(7,ctx.read(adr))) }
  // *** SET b,(IY + d)
  // **************
  val SET_0_$IY_d$ = Opcode((0xFD,0xCB,0xC6),23,4,MNEMONIC_IXY_d("SET 0,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(0,ctx.read(adr))) }
  val SET_1_$IY_d$ = Opcode((0xFD,0xCB,0xCE),23,4,MNEMONIC_IXY_d("SET 1,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(1,ctx.read(adr))) }
  val SET_2_$IY_d$ = Opcode((0xFD,0xCB,0xD6),23,4,MNEMONIC_IXY_d("SET 2,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(2,ctx.read(adr))) }
  val SET_3_$IY_d$ = Opcode((0xFD,0xCB,0xDE),23,4,MNEMONIC_IXY_d("SET 3,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(3,ctx.read(adr))) }
  val SET_4_$IY_d$ = Opcode((0xFD,0xCB,0xE6),23,4,MNEMONIC_IXY_d("SET 4,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(4,ctx.read(adr))) }
  val SET_5_$IY_d$ = Opcode((0xFD,0xCB,0xEE),23,4,MNEMONIC_IXY_d("SET 5,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(5,ctx.read(adr))) }
  val SET_6_$IY_d$ = Opcode((0xFD,0xCB,0xF6),23,4,MNEMONIC_IXY_d("SET 6,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(6,ctx.read(adr))) }
  val SET_7_$IY_d$ = Opcode((0xFD,0xCB,0xFE),23,4,MNEMONIC_IXY_d("SET 7,(IY%s)")) { ctx => val adr = ctx.IY_+(ctx.byte(2)) ; ctx.write(adr,set(7,ctx.read(adr))) }  
  // ========================================= Jump Call and Return Group ====================================
  // *** JP nn
  // **************
  val JP_nn = Opcode(0xC3,10,3,MNEMONIC_nn("JP %s"),modifyPC = true) { ctx => ctx.PC = ctx.word(1) }
  // *** JP cc,nn
  // **************
  @inline private def jp_cond_nn(ctx:Context,cond:Boolean) {
    if (cond) ctx.PC = ctx.word(1) else ctx.PC = (ctx.PC + 3) & 0xFFFF
  }
  val JP_C_nn = Opcode(0xDA,10,3,MNEMONIC_nn("JP C,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.carry > 0) }
  val JP_NC_nn = Opcode(0xD2,10,3,MNEMONIC_nn("JP NC,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.carry == 0) }
  val JP_Z_nn = Opcode(0xCA,10,3,MNEMONIC_nn("JP Z,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.zero > 0) }
  val JP_NZ_nn = Opcode(0xC2,10,3,MNEMONIC_nn("JP NZ,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.zero == 0) }
  val JP_PO_nn = Opcode(0xE2,10,3,MNEMONIC_nn("JP PO,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.parity == 0) }
  val JP_PE_nn = Opcode(0xEA,10,3,MNEMONIC_nn("JP PE,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.parity > 0) }
  val JP_P_nn = Opcode(0xF2,10,3,MNEMONIC_nn("JP P,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.sign == 0) }
  val JP_M_nn = Opcode(0xFA,10,3,MNEMONIC_nn("JP M,%s"),modifyPC = true) { ctx => jp_cond_nn(ctx,ctx.sign > 0) }
  // *** JR e
  // **************
  @inline private def MNEMONIC_jr(pattern:String) = (m:Memory,PC:Int) => pattern.format(hex4(PC + 2 + m.read(PC + 1).asInstanceOf[Byte]))  
  val JR_e = Opcode(0x18,12,2,MNEMONIC_jr("JR %s"),modifyPC = true) { ctx => ctx.PC = (ctx.PC + 2 + ctx.byte(1).asInstanceOf[Byte]) & 0xFFFF }
  // *** JR cc,e
  // **************
  @inline private def jr_cond_e(ctx:Context,cond:Boolean) {
    if (cond) ctx.PC = (ctx.PC + 2 + ctx.byte(1).asInstanceOf[Byte]) & 0xFFFF
    else { ctx.PC = (ctx.PC + 2) & 0xFFFF ; ctx.setAdditionalClockCycles(-5) }
  }
  val JR_C_e = Opcode(0x38,12,2,MNEMONIC_jr("JR C,%s"),modifyPC = true) { ctx => jr_cond_e(ctx,ctx.carry > 0) }
  val JR_NC_e = Opcode(0x30,12,2,MNEMONIC_jr("JR NC,%s"),modifyPC = true) { ctx => jr_cond_e(ctx,ctx.carry == 0) }
  val JR_Z_e = Opcode(0x28,12,2,MNEMONIC_jr("JR Z,%s"),modifyPC = true) { ctx => jr_cond_e(ctx,ctx.zero > 0) }
  val JR_NZ_e = Opcode(0x20,12,2,MNEMONIC_jr("JR NZ,%s"),modifyPC = true) { ctx => jr_cond_e(ctx,ctx.zero == 0) }
  // *** JP (HL)
  // **************
  val JP_$HL$ = Opcode(0xE9,4,1,"JP (HL)",modifyPC = true) { ctx => ctx.PC = ctx.HL }
  // *** JP (IX)
  // **************
  val JP_$IX$ = Opcode((0xDD,0xE9),8,1,"JP (IX)",modifyPC = true) { ctx => ctx.PC = ctx.IX }
  // *** JP (IY)
  // **************
  val JP_$IY$ = Opcode((0xFD,0xE9),8,1,"JP (IY)",modifyPC = true) { ctx => ctx.PC = ctx.IY }
  // *** CALL nn
  // **************
  @inline private def call(ctx:Context,addr:Int) {
    ctx.push(ctx.PC + 3)
    ctx.PC = addr
  }
  val CALL_nn = Opcode(0xCD,17,3,MNEMONIC_nn("CALL %s"),modifyPC = true) { ctx => call(ctx,ctx.word(1)) }
  // *** CALL cc,nn
  // **************
  @inline private def call_cond_nn(ctx:Context,cond:Boolean) {
    if (cond) { call(ctx,ctx.word(1)) ; ctx.setAdditionalClockCycles(7) } 
    else ctx.PC = (ctx.PC + 3) & 0xFFFF
  }
  val CALL_C_nn = Opcode(0xDC,10,3,MNEMONIC_nn("CALL C,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.carry > 0) }
  val CALL_NC_nn = Opcode(0xD4,10,3,MNEMONIC_nn("CALL NC,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.carry == 0) }
  val CALL_Z_nn = Opcode(0xCC,10,3,MNEMONIC_nn("CALL Z,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.zero > 0) }
  val CALL_NZ_nn = Opcode(0xC4,10,3,MNEMONIC_nn("CALL NZ,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.zero == 0) }
  val CALL_PO_nn = Opcode(0xE4,10,3,MNEMONIC_nn("CALL PO,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.parity == 0) }
  val CALL_PE_nn = Opcode(0xEC,10,3,MNEMONIC_nn("CALL PE,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.parity > 0) }
  val CALL_P_nn = Opcode(0xF4,10,3,MNEMONIC_nn("CALL P,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.sign == 0) }
  val CALL_M_nn = Opcode(0xFC,10,3,MNEMONIC_nn("CALL M,%s"),modifyPC = true) { ctx => call_cond_nn(ctx,ctx.sign > 0) }
  // *** DJNZ e
  // **************
  val DJNZ_e = Opcode(0x10,8,2,MNEMONIC_jr("DJNZ %s"),modifyPC = true) { ctx =>
    ctx.B = (ctx.B - 1) & 0xFF
    if (ctx.B == 0) ctx.PC = (ctx.PC + 2) & 0xFFFF
    else {
      ctx.setAdditionalClockCycles(5)
      ctx.PC = (ctx.PC + 2 + ctx.byte(1).asInstanceOf[Byte]) & 0xFFFF
    }
  }
  // *** RET
  // **************
  val RET = Opcode(0xC9,10,1,"RET",modifyPC = true) { ctx => ctx.PC = ctx.pop }
  // *** RET cc
  // **************
  @inline private def ret_cond(ctx:Context,cond:Boolean) {
    if (cond) {
      ctx.PC = ctx.pop
      ctx.setAdditionalClockCycles(6)
    }
    else ctx.PC = (ctx.PC + 1) & 0xFFFF
  }
  val RET_C_nn = Opcode(0xD8,5,1,"RET C",modifyPC = true) { ctx => ret_cond(ctx,ctx.carry > 0) }
  val RET_NC_nn = Opcode(0xD0,5,1,"RET NC",modifyPC = true) { ctx => ret_cond(ctx,ctx.carry == 0) }
  val RET_Z_nn = Opcode(0xC8,5,1,"RET Z",modifyPC = true) { ctx => ret_cond(ctx,ctx.zero > 0) }
  val RET_NZ_nn = Opcode(0xC0,5,1,"RET NZ",modifyPC = true) { ctx => ret_cond(ctx,ctx.zero == 0) }
  val RET_PO_nn = Opcode(0xE0,5,1,"RET PO",modifyPC = true) { ctx => ret_cond(ctx,ctx.parity == 0) }
  val RET_PE_nn = Opcode(0xE8,5,1,"RET PE",modifyPC = true) { ctx => ret_cond(ctx,ctx.parity > 0) }
  val RET_P_nn = Opcode(0xF0,5,1,"RET P",modifyPC = true) { ctx => ret_cond(ctx,ctx.sign == 0) }
  val RET_M_nn = Opcode(0xF8,5,1,"RET M",modifyPC = true) { ctx => ret_cond(ctx,ctx.sign > 0) }
  // *** RETI
  // **************
  @inline private def retni(ctx:Context) {
    ctx.PC = ctx.pop
    ctx.IFF1 = ctx.IFF2
  }
  val RETI = Opcode((0xED,0x4D),14,2,"RETI",modifyPC = true) { ctx => retni(ctx) }
  val RETN = Opcode((0xED,0x45),14,2,"RETN",modifyPC = true) { ctx => retni(ctx) }
  // *** RST p
  // **************
  @inline private def rst(ctx:Context,pcl:Int) {
    ctx.push(ctx.PC + 1)
    ctx.PC = pcl
  }
  val RST_0 = Opcode(0xC7,11,1,"RST 0",modifyPC = true) { ctx => rst(ctx,0x00) }
  val RST_8 = Opcode(0xCF,11,1,"RST 8",modifyPC = true) { ctx => rst(ctx,0x08) }
  val RST_10 = Opcode(0xD7,11,1,"RST 10",modifyPC = true) { ctx => rst(ctx,0x10) }
  val RST_18 = Opcode(0xDF,11,1,"RST 18",modifyPC = true) { ctx => rst(ctx,0x18) }
  val RST_20 = Opcode(0xE7,11,1,"RST 20",modifyPC = true) { ctx => rst(ctx,0x20) }
  val RST_28 = Opcode(0xEF,11,1,"RST 28",modifyPC = true) { ctx => rst(ctx,0x28) }
  val RST_30 = Opcode(0xF7,11,1,"RST 30",modifyPC = true) { ctx => rst(ctx,0x30) }
  val RST_38 = Opcode(0xFF,11,1,"RST 38",modifyPC = true) { ctx => rst(ctx,0x38) }
  // ====================================== Input Group ======================================================
  // *** IN A,n
  // **************
  val IN_A_n = Opcode(0xDB,11,2,MNEMONIC_n("IN A,%s")) { ctx => ctx.A = ctx.io.in(ctx.A,ctx.byte(1)) }
  // *** IN r,(C)
  // **************
  @inline private def in_r_c(ctx:Context) = {
    val io = ctx.io.in(ctx.B,ctx.C)
    ctx.F = ctx.SZP(io) | ctx.carry
    ctx.setHalf(false)
    ctx.setNegative(false)
    io
  }
  val IN_A_$C$ = Opcode((0xED,0x78),12,2,"IN A,(C)") { ctx => ctx.A = in_r_c(ctx) }
  val IN_B_$C$ = Opcode((0xED,0x40),12,2,"IN B,(C)") { ctx => ctx.B = in_r_c(ctx) }
  val IN_C_$C$ = Opcode((0xED,0x48),12,2,"IN C,(C)") { ctx => ctx.C = in_r_c(ctx) }
  val IN_D_$C$ = Opcode((0xED,0x50),12,2,"IN D,(C)") { ctx => ctx.D = in_r_c(ctx) }
  val IN_E_$C$ = Opcode((0xED,0x58),12,2,"IN E,(C)") { ctx => ctx.E = in_r_c(ctx) }
  val IN_H_$C$ = Opcode((0xED,0x60),12,2,"IN H,(C)") { ctx => ctx.H = in_r_c(ctx) }
  val IN_L_$C$ = Opcode((0xED,0x68),12,2,"IN L,(C)") { ctx => ctx.L = in_r_c(ctx) }
  // *** INI
  // **************
  /*
   *  mp := ((c)), (hl) := tmp, hl += 1,
      b -= 1 => flags, nf := tmp.7,
      tmp2 := tmp + [[c +/- 1] AND 0xff],
      pf := parity of [[tmp2 AND 0x07] XOR b],
      hf := cf := tmp2 > 255
   */
  @inline private def ini(ctx:Context,inc:Boolean) {
    val tmp = ctx.io.in(ctx.B,ctx.C)
    ctx.write(ctx.HL,tmp)
    ctx.incDecHL(inc)
    ctx.B = (ctx.B - 1) & 0xFF
    ctx.F = ctx.SZP(ctx.B)
    ctx.setNegative((tmp & 0x80) > 0)
    val tmp2 = if (inc) (tmp + ctx.C + 1) & 0xFF else (tmp + ctx.C - 1) & 0xFF
    val parity = (ctx.SZP((tmp2 & 0x07) ^ ctx.B) & 0x04) > 0
    ctx.setParity(parity)
    ctx.setHalf(tmp2 < tmp)
    ctx.setCarry(tmp2 < tmp)
  }
  val INI = Opcode((0xED,0xA2),16,2,"INI") { ctx => ini(ctx,true) }
  // *** INIR
  // **************
  val INIR = Opcode((0xED,0xB2),16,2,"INIR",modifyPC = true) { ctx => 
    ini(ctx,true)
    if (ctx.B == 0) ctx.PC = (ctx.PC + 2) & 0xFFFF
    else ctx.setAdditionalClockCycles(5)
  }
  // *** IND
  // **************
  val IND = Opcode((0xED,0xAA),16,2,"IND") { ctx => ini(ctx,false) }
  // *** INDR
  // **************
  val INDR = Opcode((0xED,0xBA),16,2,"INDR",modifyPC = true) { ctx => 
    ini(ctx,false)
    if (ctx.B == 0) ctx.PC = (ctx.PC + 2) & 0xFFFF
    else ctx.setAdditionalClockCycles(5)
  }
  // ====================================== Output Group =====================================================
  // *** OUT (n),A
  // **************
  val OUT_$n$_A = Opcode(0xD3,11,2,MNEMONIC_n("OUT (%s),A")) { ctx => ctx.io.out(ctx.A,ctx.read(1),ctx.A) }
  // *** OUT (C),r
  // **************
  val OUT_$C$_A = Opcode((0xED,0x79),12,2,"OUT (C),A") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.A) }
  val OUT_$C$_B = Opcode((0xED,0x41),12,2,"OUT (C),B") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.B) }
  val OUT_$C$_C = Opcode((0xED,0x49),12,2,"OUT (C),C") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.C) }
  val OUT_$C$_D = Opcode((0xED,0x51),12,2,"OUT (C),D") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.D) }
  val OUT_$C$_E = Opcode((0xED,0x59),12,2,"OUT (C),E") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.E) }
  val OUT_$C$_H = Opcode((0xED,0x61),12,2,"OUT (C),H") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.H) }
  val OUT_$C$_L = Opcode((0xED,0x69),12,2,"OUT (C),L") { ctx => ctx.io.out(ctx.B,ctx.C,ctx.L) }
  // *** OUTI
  // **************
  /*
   *  tmp := (hl), ((c)) := tmp, hl += 1,
      b -= 1 => flags, nf := tmp.7, tmp2 = tmp + l,
      pf := parity of [[tmp2 AND 0x07] XOR b],
      hf := cf := tmp2 > 255
   */
  @inline private def outi(ctx:Context,inc:Boolean) {
    val tmp = ctx.read(ctx.HL)
    ctx.incDecHL(inc)
    ctx.B = (ctx.B - 1) & 0xFF
    ctx.F = ctx.SZP(ctx.B)
    ctx.setNegative((tmp & 0x80) > 0)
    val tmp2 = tmp + ctx.L
    val parity = (ctx.SZP((tmp2 & 0x07) ^ ctx.B) & 0x04) > 0
    ctx.setParity(parity)
    ctx.setHalf(tmp2 > 0xFF)
    ctx.setCarry(tmp2 > 0xFF)
  }
  val OUTI = Opcode((0xED,0xA3),16,2,"OUTI") { ctx => outi(ctx,true) }
  // *** OTIR
  // **************
  val OTIR = Opcode((0xED,0xB3),16,2,"OTIR",modifyPC = true) { ctx => 
    outi(ctx,true)
    if (ctx.B == 0) ctx.PC = (ctx.PC + 2) & 0xFFFF
    else ctx.setAdditionalClockCycles(5)
  }
  // *** OUTD
  // **************
  val OUTD = Opcode((0xED,0xAB),16,2,"OUTD") { ctx => outi(ctx,false) }
  // *** OTDR
  // **************
  val OTDR = Opcode((0xED,0xBB),16,2,"OTDR",modifyPC = true) { ctx => 
    outi(ctx,false)
    if (ctx.B == 0) ctx.PC = (ctx.PC + 2) & 0xFFFF
    else ctx.setAdditionalClockCycles(5)
  }
  // =========================================================================================================
  // ====================================== Reflection =======================================================
  def initOpcodes {
    if (opcodes_1(0) != null) return
    
    val fields = getClass.getDeclaredFields    
    val opcodes = fields filter { _.getType == classOf[Z80.Opcode] } map { f => f.setAccessible(true); f.get(this).asInstanceOf[Opcode] }
    for(o <- opcodes) {
      o.opcodes match {
        case Array(op) => 
          if (opcodes_1(op) == null) opcodes_1(op) = o else { println(s"$op already set"); sys.exit(-1) }
        case Array(0xED,op) =>
          if (opcodes_ed(op) == null) opcodes_ed(op) = o else { println(s"0xED,$op already set"); sys.exit(-1) }
        case Array(0xCB,op) =>
          if (opcodes_cb(op) == null) opcodes_cb(op) = o else { println(s"0xCB,$op already set"); sys.exit(-1) }
//        case Array(0xDD,0xCB,op) =>
//          if (opcodes_ddcb(op) == null) opcodes_ddcb(op) = o else { println(s"0xDD,0xCB,$op already set"); sys.exit(-1) }
        case Array(0xDD,0xCB,op) =>
          if (opcodes_ddcb4(op) == null) opcodes_ddcb4(op) = o else { println(s"0xDD,0xCB,_,$op already set"); sys.exit(-1) }
//        case Array(0xFD,0xCB,op) =>
//          if (opcodesFdcb(op) == null) opcodesFdcb(op) = o else { println(s"0xFD,0xCB,$op already set"); sys.exit(-1) }
        case Array(0xFD,0xCB,op) =>
          if (opcodesFdcb4(op) == null) opcodesFdcb4(op) = o else { println(s"0xFD,0xCB,_,$op already set"); sys.exit(-1) }
        case Array(0xDD,op) =>
          if (opcodes_dd(op) == null) opcodes_dd(op) = o else { println(s"0xDD,$op already set"); sys.exit(-1) }
        case Array(0xFD,op) =>
          if (opcodesFd(op) == null) opcodesFd(op) = o else { println(s"0xFD,$op already set"); sys.exit(-1) }        
        case x =>
          println(s"Fatal error: opcodes ${x.mkString(",")} unknown")
          sys.exit(-1)
      }
    }
    //println(s"Initialized ${opcodes.length} opcodes")
  }
  // =========================================================================================================
}

/**
 * @author ealeame
 */
class Z80(mem:Memory,io_memory:Z80.IOMemory = null) extends Chip with Z80.IOMemory with TraceListener {
  val id = ChipID.CPU
  override lazy val componentID = "Z80"
  import Z80._
  val ctx = new Context(mem,this)
  private[this] var irqLow,nmiLow,nmiOnNegativeEdge = false
  private[this] var cpuWaitUntil = 0L
  private[this] var cpuRestCycles = 0.0
  private[this] var busREQ = false
  private[this] var tracing = false
  private[this] var stepCallBack : (String) => Unit = _
  private[this] val syncObject = new Object
  private[this] var breakCallBack : (String) => Unit = _
  private[this] var breakType : BreakType = _
  
  // =================================== Tracing =============================================================
  
  def setTraceOnFile(out:PrintWriter,enabled:Boolean) {
    // TODO
  }
  def setTrace(traceOn:Boolean) = tracing = traceOn
  def step(updateRegisters: (String) => Unit) {
    stepCallBack = updateRegisters
    syncObject.synchronized {
      syncObject.notify
    }
  }
  def setBreakAt(breakType:BreakType,callback:(String) => Unit) {
    tracing = false
    breakCallBack = callback
    this.breakType = breakType
  }
  def jmpTo(pc:Int) {
    ctx.PC = pc & 0xFFFF 
  }
  def disassemble(mem:Memory,address:Int) : (String,Int) = {
    val opcode = fetch(address)
    (opcode.disassemble(mem,address),opcode.size)
  }
  // =================================== Interrupt Handling ==================================================
  
  final def irq(low:Boolean) = irqLow = low
  final def nmi(low:Boolean) {
    if (!nmiLow && low) {
      nmiOnNegativeEdge = true
    }
    nmiLow = low 
  }
  
  // ======================================== Bus Request (threee state) =====================================
  def requestBUS(request:Boolean) = busREQ = request
  // ======================================== I/O Handling default implementation ============================  
  def in(addressHI:Int,addressLO:Int) = {
    if (io_memory == null) mem.read(addressHI << 8 | addressLO)
    else io_memory.in(addressHI,addressLO)
  }
  def out(addressHI:Int,addressLO:Int,value:Int) {
    if (io_memory == null) mem.write(addressHI << 8 | addressLO,value)
    else io_memory.out(addressHI,addressLO,value)
  }
  // ======================================== Fetch & Execute ================================================
  
  def init {
    Log.info("Z80 initializing opcodes...")
    Z80.initOpcodes
  }
  def reset {
    ctx.reset
    irqLow = false
    nmiLow = false
    nmiOnNegativeEdge = false
  }
  
  @inline private[this] def fetch(pc:Int) : Opcode = {
    val op = mem.read(pc)    
    // single opcode
    var opcode = opcodes_1(op)
    if (opcode != null) return opcode
    val op1 = mem.read(pc + 1)
    // ED
    if (op == 0xED) return opcodes_ed(op1)
    // CB    
    if (op == 0xCB) return opcodes_cb(op1)
    // DD
    if (op == 0xDD) {
      if (op1 == 0xCB) {
        val op2 = mem.read(pc + 3)
//        if (opcodes_ddcb4(op2) == null) return opcodes_ddcb(mem.read(pc + 2)) else 
        return opcodes_ddcb4(op2)
      }
      else return opcodes_dd(op1)
    }
    // FD
    if (op == 0xFD) {
      if (op1 == 0xCB) {
        val op2 = mem.read(pc + 3)
//        if (opcodesFdcb4(op2) == null) return opcodesFdcb(mem.read(pc + 2)) else 
        return opcodesFdcb4(op2)
      }
      else return opcodesFd(op1)
    }
    null
  }
  
  @inline private def incR(deltaR:Int) = ctx.R = (ctx.R & 0x80) | (ctx.R + deltaR) & 0x7F
  
  @inline private def interruptMode0Handling {
    throw new IllegalArgumentException("Interrupt mode 0 is not implemented")
  }
  @inline private def interruptMode2Handling {
    val addr = ctx.I << 8
    ctx.PC = (mem.read(addr) << 8) | mem.read(addr + 1)
  }
  
  private def fetchAndExecute : Int = {    
    if (breakType != null && breakType.isBreak(ctx.PC,false,false)) {
      breakType = null
      tracing = true
      breakCallBack(ctx.toString)
    }
    
    if ((irqLow || nmiOnNegativeEdge) && !ctx.mustDelayInt) { // any interrupt pending ?      
      if (nmiOnNegativeEdge) { // NMI
        ctx.push(ctx.PC)
        incR(1)
        if (breakType != null && breakType.isBreak(ctx.PC,false,true)) {
          breakType = null
          tracing = true
          breakCallBack(ctx.toString)
          Log.debug("NMI Break")
        }
        nmiOnNegativeEdge = false
        if (ctx.halted) {
          ctx.halted = false
          ctx.PC = (ctx.PC + 1) & 0xFFFF
        }
        ctx.IFF2 = ctx.IFF1
        ctx.IFF1 = 0
        ctx.PC = 0x0066
        return 11
      }
      else { // IRQ
        if (ctx.IFF1 == 1) {
          ctx.push(ctx.PC)
          incR(1)
          if (breakType != null && breakType.isBreak(ctx.PC,true,false)) {
            breakType = null
            tracing = true
            breakCallBack(ctx.toString)
            Log.debug("IRQ Break")
          }
          if (ctx.halted) {
            ctx.halted = false
            ctx.PC = (ctx.PC + 1) & 0xFFFF
          }
          ctx.IFF1 = 0
          ctx.IFF2 = 0
          ctx.im match {
            case 0 => 
              interruptMode0Handling
              return 13
            case 1 =>
              ctx.PC = 0x38
              return 13
            case 2 =>
              interruptMode2Handling
              return 19
          }
          return 0
        }
      }
    }
    
    val opcode = fetch(ctx.PC)
    if (opcode == null) throw new IllegalArgumentException(s"Can't find opcode at ${hex4(ctx.PC)}: ${hex2(mem.read(ctx.PC))} ${hex2(mem.read(ctx.PC + 1))} ${hex2(mem.read(ctx.PC + 2))}")
    // tracing
    if (tracing) {
      Log.debug("[Z80] " + opcode.disassemble(mem,ctx.PC))      
      stepCallBack(ctx.toString)
      syncObject.synchronized { syncObject.wait }
    }
    // execute
    val deltaR = if (opcode.opcodes.length == 1) 1 else 2
    incR(deltaR)
    opcode.executeFunction(ctx)
    val clocks = opcode.cycles + ctx.getAdditionalClockSycles
    if (!opcode.modifyPC) ctx.PC += opcode.size    
    clocks    
  }
  
  // ======================================== Clock ==========================================================
  final def clock(cycles:Long,scaleFactor:Double = 1) {
    val canExecCPU = cycles > cpuWaitUntil && !busREQ
    if (canExecCPU) {
      val nextCPUCycles = cpuRestCycles + cycles + (fetchAndExecute - 1) / scaleFactor
      cpuWaitUntil = nextCPUCycles.toInt
      cpuRestCycles = nextCPUCycles - cpuWaitUntil
    }
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) {
  	out.writeBoolean(irqLow)
  	out.writeBoolean(nmiLow)
  	out.writeBoolean(nmiOnNegativeEdge)
  	out.writeLong(cpuWaitUntil)
  	out.writeBoolean(busREQ)
  	ctx.saveState(out)
  }
  protected def loadState(in:ObjectInputStream) {
    irqLow = in.readBoolean
    nmiLow = in.readBoolean
    nmiOnNegativeEdge = in.readBoolean
    cpuWaitUntil = in.readLong
    busREQ = in.readBoolean
    ctx.loadState(in)
  }
  protected def allowsStateRestoring(parent:JFrame) : Boolean = true
}