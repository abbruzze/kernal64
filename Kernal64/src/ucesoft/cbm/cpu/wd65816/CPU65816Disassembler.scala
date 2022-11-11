package ucesoft.cbm.cpu.wd65816

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.trace.TraceListener.TraceRegister

object CPU65816Disassembler {
  final val MNEMONICS : Array[String] = Array(
//    00      01    02      03    04      05     06     07    08      09    0A      0B      0C    0D    0E      0F
      "BRK", "ORA", "COP", "ORA", "TSB", "ORA", "ASL", "ORA", "PHP", "ORA", "ASL", "PHD", "TSB", "ORA", "ASL", "ORA",// 00
      "BPL", "ORA", "ORA", "ORA", "TRB", "ORA", "ASL", "ORA", "CLC", "ORA", "INC", "TCS", "TRB", "ORA", "ASL", "ORA",// 01
      "JSR", "AND", "JSL", "AND", "BIT", "AND", "ROL", "AND", "PLP", "AND", "ROL", "PLD", "BIT", "AND", "ROL", "AND",// 02
      "BMI", "AND", "AND", "AND", "BIT", "AND", "ROL", "AND", "SEC", "AND", "DEC", "TSC", "BIT", "AND", "ROL", "AND",// 03
      "RTI", "EOR", "WDM", "EOR", "MVP", "EOR", "LSR", "EOR", "PHA", "EOR", "LSR", "PHK", "JMP", "EOR", "LSR", "EOR",// 04
      "BVC", "EOR", "EOR", "EOR", "MVN", "EOR", "LSR", "EOR", "CLI", "EOR", "PHY", "TCD", "JMP", "EOR", "LSR", "EOR",// 05
      "RTS", "ADC", "PER", "ADC", "STZ", "ADC", "ROR", "ADC", "PLA", "ADC", "ROR", "RTL", "JMP", "ADC", "ROR", "ADC",// 06
      "BVS", "ADC", "ADC", "ADC", "STZ", "ADC", "ROR", "ADC", "SEI", "ADC", "PLY", "TDC", "JMP", "ADC", "ROR", "ADC",// 07
      "BRA", "STA", "BRL", "STA", "STY", "STA", "STX", "STA", "DEY", "BIT", "TXA", "PHB", "STY", "STA", "STX", "STA",// 08
      "BCC", "STA", "STA", "STA", "STY", "STA", "STX", "STA", "TYA", "STA", "TXS", "TXY", "STZ", "STA", "STZ", "STA",// 09
      "LDY", "LDA", "LDX", "LDA", "LDY", "LDA", "LDX", "LDA", "TAY", "LDA", "TAX", "PLB", "LDY", "LDA", "LDX", "LDA",// 0A
      "BCS", "LDA", "LDA", "LDA", "LDY", "LDA", "LDX", "LDA", "CLV", "LDA", "TSX", "TYX", "LDY", "LDA", "LDX", "LDA",// 0B
      "CPY", "CMP", "REP", "CMP", "CPY", "CMP", "DEC", "CMP", "INY", "CMP", "DEX", "WAI", "CPY", "CMP", "DEC", "CMP",// 0C
      "BNE", "CMP", "CMP", "CMP", "PEI", "CMP", "DEC", "CMP", "CLD", "CMP", "PHX", "STP", "JML", "CMP", "DEC", "CMP",// 0D
      "CPX", "SBC", "SEP", "SBC", "CPX", "SBC", "INC", "SBC", "INX", "SBC", "NOP", "XBA", "CPX", "SBC", "INC", "SBC",// 0E
      "BEQ", "SBC", "SBC", "SBC", "PEA", "SBC", "INC", "SBC", "SED", "SBC", "PLX", "XCE", "JSR", "SBC", "INC", "SBC" // 0F
  )

  case class DisassembledInfo(address:String,mnemonic:String,bytes:Int,P:Int) {
    override def toString: String = s"$address\t$mnemonic"
  }

  private def read8(implicit mem:Memory,adr:Int) : Int = mem.read(adr)
  private def read16(implicit mem:Memory,adr:Int) : Int = mem.read(adr) | mem.read(adr + 1) << 8
  private def read24(implicit mem:Memory,adr:Int) : Int = mem.read(adr) | mem.read(adr + 1) << 8 | mem.read(adr + 2) << 16

  private def fmt8(v:Int,prefix:String="") : String = s"$prefix%02X".format(v)
  private def fmt16(v:Int,prefix:String="") : String = s"$prefix%04X".format(v)
  private def fmt24(v:Int,prefix:String="") : String = s"$prefix%06X".format(v)

  def disassemble(mem:Memory,PC:Int,P:Int) : DisassembledInfo = {
    val opcode = mem.read(PC)
    val mnemonic = MNEMONICS(opcode)

    implicit val imem = mem
    implicit val ipc = PC + 1

    val (bytes,operands,newP) = opcode match {
        // Absolute
      case 0x0C|
           0x0D|
           0x0E|
           0x1C|
           0x20|
           0x2C|
           0x2D|
           0x2E|
           0x4C|
           0x4D|
           0x4E|
           0x6D|
           0x6E|
           0x8C|
           0x8D|
           0x8E|
           0x9C|
           0xAC|
           0xAD|
           0xAE|
           0xCC|
           0xCD|
           0xCE|
           0xEC|
           0xED|
           0xEE =>
        (3,fmt16(read16,"$"),P)
        // Absolute Indexed Indirect
      case 0x7C|
           0xFC =>
        (3,s"(${fmt16(read16,"$")},X)",P)
        // Absolute Indexed, X
      case 0x1D|
           0x1E|
           0x3C|
           0x3D|
           0x3E|
           0x5D|
           0x5E|
           0x7D|
           0x7E|
           0x9D|
           0x9E|
           0xBC|
           0xBD|
           0xDD|
           0xDE|
           0xFD|
           0xFE =>
        (3,s"${fmt16(read16,"$")},X",P)
        // Absolute Indexed, Y
      case 0x19|
           0x39|
           0x59|
           0x79|
           0x99|
           0xB9|
           0xBE|
           0xD9|
           0xF9 =>
        (3,s"${fmt16(read16,"$")},Y",P)
        // Absolute Indirect
      case 0x6C =>
        (3,s"(${fmt16(read16,"$")})",P)
        // Absolute Indirect Long
      case 0xDC =>
        (3,s"[${fmt16(read16,"$")}]",P)
        // Absolute Long
      case 0x0F|
           0x22|
           0x2F|
           0x4F|
           0x5C|
           0x6F|
           0x8F|
           0xAF|
           0xCF|
           0xEF =>
        (4,s"${fmt24(read24,"$")}",P)
        // Absolute Long Indexed, X
      case 0x1F|
           0x3F|
           0x5F|
           0x7F|
           0x9F|
           0xBF|
           0xDF|
           0xFF =>
        (4,s"${fmt24(read24,"$")},X",P)
        // Accumulator
      case 0x0A|
           0x1A|
           0x2A|
           0x3A|
           0x4A|
           0x6A =>
        (1,"A",P)
      // Block Move
      case 0x44|
           0x54 =>
        (3,s"${fmt8(read8,"$")},${fmt8(read8(mem,ipc + 1),"$")}",P)
        // Direct Page
      case 0x04|
           0x05|
           0x06|
           0x14|
           0x24|
           0x25|
           0x26|
           0x45|
           0x46|
           0x64|
           0x65|
           0x66|
           0x84|
           0x85|
           0x86|
           0xA4|
           0xA5|
           0xA6|
           0xC4|
           0xC5|
           0xC6|
           0xE4|
           0xE5|
           0xE6 =>
        (2,fmt8(read8,"$"),P)
        // Direct Page Indexed, X
      case 0x15|
           0x16|
           0x34|
           0x35|
           0x36|
           0x55|
           0x56|
           0x74|
           0x75|
           0x76|
           0x94|
           0x95|
           0xB4|
           0xB5|
           0xD5|
           0xD6|
           0xF5|
           0xF6 =>
        (2,s"${fmt8(read8,"$")},X",P)
        // Direct Page Indexed, Y
      case 0x96|
           0xB6 =>
        (2,s"${fmt8(read8,"$")},Y",P)
        // Direct Page Indirect
      case 0x12|
           0x32|
           0x52|
           0x72|
           0x92|
           0xB2|
           0xD2|
           0xF2 =>
        (2,s"(${fmt8(read8,"$")})",P)
        // Direct Page Indirect Long
      case 0x07|
           0x27|
           0x47|
           0x67|
           0x87|
           0xA7|
           0xC7|
           0xE7 =>
        (2,s"[${fmt8(read8,"$")}]",P)
        // Direct Page Indexed Indirect, X
      case 0x01|
           0x21|
           0x41|
           0x61|
           0x81|
           0xA1|
           0xC1|
           0xE1 =>
        (2,s"(${fmt8(read8,"$")},X)",P)
        // Direct Page Indirect Indexed, Y
      case 0x11|
           0x31|
           0x51|
           0x71|
           0x91|
           0xB1|
           0xD1|
           0xF1 =>
        (2,s"(${fmt8(read8,"$")}),Y",P)
        // Direct Page Indirect Long Indexed, Y
      case 0x17|
           0x37|
           0x57|
           0x77|
           0x97|
           0xB7|
           0xD7|
           0xF7 =>
        (2,s"[${fmt8(read8,"$")}],Y",P)
        // Stack (Pull)
      case 0x28|
           0x2B|
           0x68|
           0x7A|
           0xAB|
           0xFA|
        // Stack (Push)
           0x08|
           0x0B|
           0x48|
           0x4B|
           0x5A|
           0x8B|
           0xDA|
        // Stack (RTL)
           0x6B|
        // Stack (RTS)
           0x60|
        // Stack/RTI
           0x40|
        // Implied
           0x18|
           0x1B|
           0x38|
           0x3B|
           0x58|
           0x5B|
           0x78|
           0x7B|
           0x88|
           0x8A|
           0x98|
           0x9A|
           0x9B|
           0xA8|
           0xAA|
           0xB8|
           0xBA|
           0xBB|
           0xC8|
           0xCA|
           0xCB|
           0xD8|
           0xDB|
           0xE8|
           0xEA|
           0xEB|
           0xF8|
           0xFB =>
        (1,"",P)
        // Program Counter Relative
      case 0x10|
           0x30|
           0x50|
           0x70|
           0x80|
           0x90|
           0xB0|
           0xD0|
           0xF0 =>
        var rel = read8
        if (rel > 0x7F) rel -= 0x100
        val addr = (PC + rel + 2) & 0xFFFF
        (2,fmt16(addr,"$"),P)
        // Stack (Program Counter Relative Long)
      case 0x62|
        // Program Counter Relative Long
           0x82 =>
        var rel = read16
        if (rel > 0x7FFF) rel -= 0x10000
        val addr = (PC + rel + 3) & 0xFFFF
        (3,fmt16(addr,"$"),P)
        // Stack Relative Indirect Indexed, Y
      case 0x13|
           0x33|
           0x53|
           0x73|
           0x93|
           0xB3|
           0xD3|
           0xF3 =>
        (2,s"(${fmt8(read8,"$")},S),Y",P)
        // Stack (Absolute)
      case 0xF4 =>
        (3,fmt16(read16,"$"),P)
        // Stack (Direct Page Indirect)
      case 0xD4 =>
        (2,s"(${fmt8(read8,"$")})",P)
        // Stack Relative
      case 0x03|
           0x23|
           0x43|
           0x63|
           0x83|
           0xA3|
           0xC3|
           0xE3 =>
        (2,s"${fmt8(read8,"$")},S",P)
        // WDM mode
      case 0x42|
        // Stack/Interrupt
           0x00|
           0x02 =>
        (2,fmt8(read8,"$"),P)
        // Immediate (Invariant)
      case 0xC2 => // REP
        (2,fmt8(read8,"#$"),(P & ~read8) & 0xFF)
      case 0xE2 => // SEP
        (2,fmt8(read8,"#$"),P | read8)
        // Immediate (A size dependent)
      case 0x09|
           0x29|
           0x49|
           0x69|
           0x89|
           0xA9|
           0xC9|
           0xE9 =>
        if ((P & 0x20) > 0) (2,fmt8(read8,"#$"),P)
        else (3,fmt16(read16,"#$"),P)
        // Immediate (X/Y size dependent)
      case 0xA0|
           0xA2|
           0xC0|
           0xE0 =>
        if ((P & 0x10) > 0) (2,fmt8(read8,"#$"),P)
        else (3,fmt16(read16,"#$"),P)
      case _ =>
        throw new IllegalArgumentException(s"Unknow 65816's opcode: ${opcode.toHexString}")
    }
    val address = (for(i <- 0 to 3) yield {
      if (i < bytes) fmt8(read8(mem,PC + i)) else "  "
    }).mkString(" ")

    DisassembledInfo(s"${fmt24(PC)}  $address",s"$mnemonic $operands",bytes,newP)
  }

  def formatRegisters(A:Word16,
                      X:Word16,
                      Y:Word16,
                      D:Word16,
                      S:Word16,
                      DB:Int,
                      PC:ProgramCounter,
                      P:Int,
                      E:Boolean
                     ) : String = {
    val sb = new StringBuilder
    sb ++= s"E = ${if (E) 1 else 0}"
    val a8 = E //|| (P & 0x20) > 0
    val xy8 = E //|| (P & 0x10) > 0
    sb ++= s" A = ${if (a8) fmt8(A.B.L) else fmt16(A.W)}"
    sb ++= s" X = ${if (xy8) fmt8(X.B.L) else fmt16(X.W)}"
    sb ++= s" Y = ${if (xy8) fmt8(Y.B.L) else fmt16(Y.W)}"
    sb ++= s" SP = ${if (E) fmt8(S.B.L) else fmt16(S.W)}"
    sb ++= s" PC = ${fmt24(PC.A)}"
    if (!E) sb ++= s" D = ${fmt16(D.W)} DB = ${fmt8(DB)}"

    sb ++= " P = |"
    sb ++= (if ((P & 0x80) > 0) "N|" else "_|")
    sb ++= (if ((P & 0x40) > 0) "V|" else "_|")
    sb ++= (if (E) "1|" else if ((P & 0x20) > 0) "M|" else "_|")
    sb ++= (if ((P & 0x10) > 0) { if (E) "B|" else "X|" } else "_|")
    sb ++= (if ((P & 0x08) > 0) "D|" else "_|")
    sb ++= (if ((P & 0x04) > 0) "I|" else "_|")
    sb ++= (if ((P & 0x02) > 0) "Z|" else "_|")
    sb ++= (if ((P & 0x01) > 0) "C|" else "_|")

    sb.toString
  }

  def traceRegisters(A: Word16,
                      X: Word16,
                      Y: Word16,
                      D: Word16,
                      S: Word16,
                      DB: Int,
                      PC: ProgramCounter,
                      P: Int,
                      E: Boolean
                     ): List[TraceRegister] = {
    val a8 = E
    val xy8 = E
    val sb = new StringBuilder("|")
    sb ++= (if ((P & 0x80) > 0) "N|" else "_|")
    sb ++= (if ((P & 0x40) > 0) "V|" else "_|")
    sb ++= (if (E) "1|" else if ((P & 0x20) > 0) "M|" else "_|")
    sb ++= (if ((P & 0x10) > 0) {
      if (E) "B|" else "X|"
    } else "_|")
    sb ++= (if ((P & 0x08) > 0) "D|" else "_|")
    sb ++= (if ((P & 0x04) > 0) "I|" else "_|")
    sb ++= (if ((P & 0x02) > 0) "Z|" else "_|")
    sb ++= (if ((P & 0x01) > 0) "C|" else "_|")

    TraceRegister.builder().
      add("PC",fmt24(PC.A),PC.A).
      add("E", if (E) "01" else "00",if (E) 1 else 0).
      add("A",s"${if (a8) fmt8(A.B.L) else fmt16(A.W)}",if (a8) A.B.L else A.W).
      add("X",s"${if (xy8) fmt8(X.B.L) else fmt16(X.W)}",if (xy8) X.B.L else X.W).
      add("Y",s"${if (xy8) fmt8(Y.B.L) else fmt16(Y.W)}",if (xy8) Y.B.L else Y.W).
      add("SP",s"${if (E) fmt8(S.B.L) else fmt16(S.W)}", if (E) S.B.L else S.W).
      addIf(!E,"D",fmt16(D.W),D.W).
      addIf(!E,"DB",fmt8(DB),DB).
      add("STATUS",sb.toString,P,"STATUS").
      build()
  }
}
