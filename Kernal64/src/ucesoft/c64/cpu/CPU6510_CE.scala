package ucesoft.c64.cpu

import ucesoft.c64.ChipID
import ucesoft.c64.Log

class CPU6510_CE(mem: Memory, val id: ChipID.ID) extends CPU6510 {
  private[this] var baLow = false
  // ------------- Tracing --------------------
  private[this] var tracing = false
  private[this] var breakAt = -1
  private[this] var breakCallBack: (String) => Unit = _
  private[this] var stepCallBack: (String) => Unit = _
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

  private[this] var PC = 0
  private[this] var CURRENT_OP_PC = 0
  private[this] var SP = 0
  private[this] var A = 0
  private[this] var X = 0
  private[this] var Y = 0
  private[this] var SREG = FLAG_#

  private[this] var nmiOnNegativeEdge = false
  private[this] var irqLow = false
  private[this] var nmiLow = false

  // -----------------------------------------

  final def isWriting = false
  final override def setBaLow(baLow: Boolean) { this.baLow = baLow }
  final def getPC = PC
  final def getCurrentInstructionPC = CURRENT_OP_PC
  final def getMem(address: Int) = mem.read(address)
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

  @inline private[this] def SR = SREG | FLAG_#
  @inline private[this] def SR_=(sr: Int) = SREG = (sr | FLAG_#) & NOT_B_FLAG

  @inline private[this] def sen { SREG |= N_FLAG }
  @inline private[this] def cln { SREG &= (~N_FLAG & 0xFF) }
  @inline private[this] def sev { SREG |= V_FLAG }
  @inline private[this] def clv { SREG &= (~V_FLAG & 0xFF) }
  @inline private[this] def seb { SREG |= B_FLAG }
  @inline private[this] def clb { SREG &= (~B_FLAG & 0xFF) }
  @inline private[this] def sed { SREG |= D_FLAG }
  @inline private[this] def cld { SREG &= (~D_FLAG & 0xFF) }
  @inline private[this] def sei { SREG |= I_FLAG }
  @inline private[this] def cli { SREG &= (~I_FLAG & 0xFF) }
  @inline private[this] def sez { SREG |= Z_FLAG }
  @inline private[this] def clz { SREG &= (~Z_FLAG & 0xFF) }
  @inline private[this] def sec { SREG |= C_FLAG }
  @inline private[this] def clc { SREG &= (~C_FLAG & 0xFF) }

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
    properties.setProperty("PC", hex4(PC))
    properties.setProperty("A", hex2(A))
    properties.setProperty("X", hex2(X))
    properties.setProperty("Y", hex2(Y))
    properties.setProperty("S", hex2(SP))
    properties.setProperty("NV#BDIZC", sr2String)
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

  // TRACING ---------------------------------------------
  def setTrace(traceOn: Boolean) = tracing = traceOn

  def step(updateRegisters: (String) => Unit) {
    stepCallBack = updateRegisters
    syncObject.synchronized {
      syncObject.notify
    }
  }
  def setBreakAt(address: Int, callback: (String) => Unit) {
    tracing = false
    breakCallBack = callback
    breakAt = address
  }
  def jmpTo(pc: Int) {
    PC = pc
  }

  //import CPU6510._
  // -------------------------------- STATES -------------------------------------------------------------
  private[this] var op = 0
  private[this] var state = 0
  private[this] var ar,ar2,data,rdbuf = 0

  // Read effective address, no extra cycles
  final private[this] val A_ZERO = 0x100
  final private[this] val A_ZEROX = 0x101
  final private[this] val A_ZEROX1 = 0x102
  final private[this] val A_ZEROY = 0x103
  final private[this] val A_ZEROY1 = 0x104
  final private[this] val A_ABS = 0x105
  final private[this] val A_ABS1 = 0x106
  final private[this] val A_ABSX = 0x107
  final private[this] val A_ABSX1 = 0x108
  final private[this] val A_ABSX2 = 0x109
  final private[this] val A_ABSX3 = 0x10A
  final private[this] val A_ABSY = 0x10B
  final private[this] val A_ABSY1 = 0x10C
  final private[this] val A_ABSY2 = 0x10D
  final private[this] val A_ABSY3 = 0x10E
  final private[this] val A_INDX = 0x10F
  final private[this] val A_INDX1 = 0x110
  final private[this] val A_INDX2 = 0x111
  final private[this] val A_INDX3 = 0x112
  final private[this] val A_INDY = 0x113
  final private[this] val A_INDY1 = 0x114
  final private[this] val A_INDY2 = 0x115
  final private[this] val A_INDY3 = 0x116
  final private[this] val A_INDY4 = 0x117
  // Read effective address, extra cycle on page crossing
  final private[this] val AE_ABSX = 0x118
  final private[this] val AE_ABSX1 = 0x119
  final private[this] val AE_ABSX2 = 0x11A
  final private[this] val AE_ABSY = 0x11B
  final private[this] val AE_ABSY1 = 0x11C
  final private[this] val AE_ABSY2 = 0x11D
  final private[this] val AE_INDY = 0x11E
  final private[this] val AE_INDY1 = 0x11F
  final private[this] val AE_INDY2 = 0x120
  final private[this] val AE_INDY3 = 0x121
  // Read operand and write it back (for RMW instructions), no extra cycles
  final private[this] val M_ZERO = 0x122
  final private[this] val M_ZEROX = 0x123
  final private[this] val M_ZEROX1 = 0x124
  final private[this] val M_ZEROY = 0x125
  final private[this] val M_ZEROY1 = 0x126
  final private[this] val M_ABS = 0x127
  final private[this] val M_ABS1 = 0x128
  final private[this] val M_ABSX = 0x129
  final private[this] val M_ABSX1 = 0x12A
  final private[this] val M_ABSX2 = 0x12B
  final private[this] val M_ABSX3 = 0x12C
  final private[this] val M_ABSY = 0x12D
  final private[this] val M_ABSY1 = 0x12E
  final private[this] val M_ABSY2 = 0x12F
  final private[this] val M_ABSY3 = 0x130
  final private[this] val M_INDX = 0x131
  final private[this] val M_INDX1 = 0x132
  final private[this] val M_INDX2 = 0x133
  final private[this] val M_INDX3 = 0x134
  final private[this] val M_INDY = 0x135
  final private[this] val M_INDY1 = 0x136
  final private[this] val M_INDY2 = 0x137
  final private[this] val M_INDY3 = 0x138
  final private[this] val M_INDY4 = 0x139
  final private[this] val RMW_DO_IT = 0x13A
  final private[this] val RMW_DO_IT1 = 0x13B
  // Operations (_I = Immediate/Indirect, _A = Accumulator)
  final private[this] val O_LDA = 0x13C
  final private[this] val O_LDA_I = 0x13D
  final private[this] val O_LDX = 0x13E
  final private[this] val O_LDX_I = 0x13F
  final private[this] val O_LDY = 0x140
  final private[this] val O_LDY_I = 0x141
  final private[this] val O_STA = 0x142
  final private[this] val O_STX = 0x143
  final private[this] val O_STY = 0x144
  final private[this] val O_TAX = 0x145
  final private[this] val O_TXA = 0x146
  final private[this] val O_TAY = 0x147
  final private[this] val O_TYA = 0x148
  final private[this] val O_TSX = 0x149
  final private[this] val O_TXS = 0x14A
  final private[this] val O_ADC = 0x14B
  final private[this] val O_ADC_I = 0x14C
  final private[this] val O_SBC = 0x14D
  final private[this] val O_SBC_I = 0x14E
  final private[this] val O_INX = 0x14F
  final private[this] val O_DEX = 0x150
  final private[this] val O_INY = 0x151
  final private[this] val O_DEY = 0x152
  final private[this] val O_INC = 0x153
  final private[this] val O_DEC = 0x154
  final private[this] val O_AND = 0x155
  final private[this] val O_AND_I = 0x156
  final private[this] val O_ORA = 0x157
  final private[this] val O_ORA_I = 0x158
  final private[this] val O_EOR = 0x159
  final private[this] val O_EOR_I = 0x15A
  final private[this] val O_CMP = 0x15B
  final private[this] val O_CMP_I = 0x15C
  final private[this] val O_CPX = 0x15D
  final private[this] val O_CPX_I = 0x15E
  final private[this] val O_CPY = 0x15F
  final private[this] val O_CPY_I = 0x160
  final private[this] val O_BIT = 0x161
  final private[this] val O_ASL = 0x162
  final private[this] val O_ASL_A = 0x163
  final private[this] val O_LSR = 0x164
  final private[this] val O_LSR_A = 0x165
  final private[this] val O_ROL = 0x166
  final private[this] val O_ROL_A = 0x167
  final private[this] val O_ROR = 0x168
  final private[this] val O_ROR_A = 0x169
  final private[this] val O_PHA = 0x16A
  final private[this] val O_PHA1 = 0x16B
  final private[this] val O_PLA = 0x16C
  final private[this] val O_PLA1 = 0x16D
  final private[this] val O_PLA2 = 0x16E
  final private[this] val O_PHP = 0x16F
  final private[this] val O_PHP1 = 0x170
  final private[this] val O_PLP = 0x171
  final private[this] val O_PLP1 = 0x172
  final private[this] val O_PLP2 = 0x173
  final private[this] val O_JMP = 0x174
  final private[this] val O_JMP1 = 0x175
  final private[this] val O_JMP_I = 0x176
  final private[this] val O_JMP_I1 = 0x177
  final private[this] val O_JSR = 0x178
  final private[this] val O_JSR1 = 0x179
  final private[this] val O_JSR2 = 0x17A
  final private[this] val O_JSR3 = 0x17B
  final private[this] val O_JSR4 = 0x17C
  final private[this] val O_RTS = 0x17D
  final private[this] val O_RTS1 = 0x17E
  final private[this] val O_RTS2 = 0x17F
  final private[this] val O_RTS3 = 0x180
  final private[this] val O_RTS4 = 0x181
  final private[this] val O_RTI = 0x182
  final private[this] val O_RTI1 = 0x183
  final private[this] val O_RTI2 = 0x184
  final private[this] val O_RTI3 = 0x185
  final private[this] val O_RTI4 = 0x186
  final private[this] val O_BRK = 0x187
  final private[this] val O_BRK1 = 0x188
  final private[this] val O_BRK2 = 0x189
  final private[this] val O_BRK3 = 0x18A
  final private[this] val O_BRK4 = 0x18B
  final private[this] val O_BRK5 = 0x18C
  final private[this] val O_BRK5NMI = 0x18D
  final private[this] val O_BCS = 0x18E
  final private[this] val O_BCC = 0x18F
  final private[this] val O_BEQ = 0x190
  final private[this] val O_BNE = 0x191
  final private[this] val O_BVS = 0x192
  final private[this] val O_BVC = 0x193
  final private[this] val O_BMI = 0x194
  final private[this] val O_BPL = 0x195
  final private[this] val O_BRANCH_NP = 0x196
  final private[this] val O_BRANCH_BP = 0x197
  final private[this] val O_BRANCH_BP1 = 0x198
  final private[this] val O_BRANCH_FP = 0x199
  final private[this] val O_BRANCH_FP1 = 0x19A
  final private[this] val O_SEC = 0x19B
  final private[this] val O_CLC = 0x19C
  final private[this] val O_SED = 0x19D
  final private[this] val O_CLD = 0x19E
  final private[this] val O_SEI = 0x19F
  final private[this] val O_CLI = 0x1A0
  final private[this] val O_CLV = 0x1A1
  final private[this] val O_NOP = 0x1A2
  final private[this] val O_NOP_I = 0x1A3
  final private[this] val O_NOP_A = 0x1A4
  final private[this] val O_LAX = 0x1A5
  final private[this] val O_SAX = 0x1A6
  final private[this] val O_SLO = 0x1A7
  final private[this] val O_RLA = 0x1A8
  final private[this] val O_SRE = 0x1A9
  final private[this] val O_RRA = 0x1AA
  final private[this] val O_DCP = 0x1AB
  final private[this] val O_ISB = 0x1AC
  final private[this] val O_ANC_I = 0x1AD
  final private[this] val O_ASR_I = 0x1AE
  final private[this] val O_ARR_I = 0x1AF
  final private[this] val O_ANE_I = 0x1B0
  final private[this] val O_LXA_I = 0x1B1
  final private[this] val O_SBX_I = 0x1B2
  final private[this] val O_LAS = 0x1B3
  final private[this] val O_SHS = 0x1B4
  final private[this] val O_SHY = 0x1B5
  final private[this] val O_SHX = 0x1B6
  final private[this] val O_SHA = 0x1B7
  final private[this] val O_EXT = 0x1B8

  private[this] val MODE_TAB = Array(
    O_BRK, A_INDX, 1, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // 00
    O_PHP, O_ORA_I, O_ASL_A, O_ANC_I, A_ABS, A_ABS, M_ABS, M_ABS,
    O_BPL, AE_INDY, 1, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // 10
    O_CLC, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX,
    O_JSR, A_INDX, 1, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // 20
    O_PLP, O_AND_I, O_ROL_A, O_ANC_I, A_ABS, A_ABS, M_ABS, M_ABS,
    O_BMI, AE_INDY, 1, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // 30
    O_SEC, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX,
    O_RTI, A_INDX, 1, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // 40
    O_PHA, O_EOR_I, O_LSR_A, O_ASR_I, O_JMP, A_ABS, M_ABS, M_ABS,
    O_BVC, AE_INDY, 1, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // 50
    O_CLI, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX,
    O_RTS, A_INDX, 1, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // 60
    O_PLA, O_ADC_I, O_ROR_A, O_ARR_I, A_ABS, A_ABS, M_ABS, M_ABS,
    O_BVS, AE_INDY, 1, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // 70
    O_SEI, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX,
    O_NOP_I, A_INDX, O_NOP_I, A_INDX, A_ZERO, A_ZERO, A_ZERO, A_ZERO, // 80
    O_DEY, O_NOP_I, O_TXA, O_ANE_I, A_ABS, A_ABS, A_ABS, A_ABS,
    O_BCC, A_INDY, 1, A_INDY, A_ZEROX, A_ZEROX, A_ZEROY, A_ZEROY, // 90
    O_TYA, A_ABSY, O_TXS, A_ABSY, A_ABSX, A_ABSX, A_ABSY, A_ABSY,
    O_LDY_I, A_INDX, O_LDX_I, A_INDX, A_ZERO, A_ZERO, A_ZERO, A_ZERO, // a0
    O_TAY, O_LDA_I, O_TAX, O_LXA_I, A_ABS, A_ABS, A_ABS, A_ABS,
    O_BCS, AE_INDY, 1, AE_INDY, A_ZEROX, A_ZEROX, A_ZEROY, A_ZEROY, // b0
    O_CLV, AE_ABSY, O_TSX, AE_ABSY, AE_ABSX, AE_ABSX, AE_ABSY, AE_ABSY,
    O_CPY_I, A_INDX, O_NOP_I, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // c0
    O_INY, O_CMP_I, O_DEX, O_SBX_I, A_ABS, A_ABS, M_ABS, M_ABS,
    O_BNE, AE_INDY, 1, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // d0
    O_CLD, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX,
    O_CPX_I, A_INDX, O_NOP_I, M_INDX, A_ZERO, A_ZERO, M_ZERO, M_ZERO, // e0
    O_INX, O_SBC_I, O_NOP, O_SBC_I, A_ABS, A_ABS, M_ABS, M_ABS,
    O_BEQ, AE_INDY, O_EXT, M_INDY, A_ZEROX, A_ZEROX, M_ZEROX, M_ZEROX, // f0
    O_SED, AE_ABSY, O_NOP, M_ABSY, AE_ABSX, AE_ABSX, M_ABSX, M_ABSX)
  private[this] val OP_TAB = Array(
    1, O_ORA, 1, O_SLO, O_NOP_A, O_ORA, O_ASL, O_SLO, // 00
    1, 1, 1, 1, O_NOP_A, O_ORA, O_ASL, O_SLO,
    1, O_ORA, 1, O_SLO, O_NOP_A, O_ORA, O_ASL, O_SLO, // 10
    1, O_ORA, 1, O_SLO, O_NOP_A, O_ORA, O_ASL, O_SLO,
    1, O_AND, 1, O_RLA, O_BIT, O_AND, O_ROL, O_RLA, // 20
    1, 1, 1, 1, O_BIT, O_AND, O_ROL, O_RLA,
    1, O_AND, 1, O_RLA, O_NOP_A, O_AND, O_ROL, O_RLA, // 30
    1, O_AND, 1, O_RLA, O_NOP_A, O_AND, O_ROL, O_RLA,
    1, O_EOR, 1, O_SRE, O_NOP_A, O_EOR, O_LSR, O_SRE, // 40
    1, 1, 1, 1, 1, O_EOR, O_LSR, O_SRE,
    1, O_EOR, 1, O_SRE, O_NOP_A, O_EOR, O_LSR, O_SRE, // 50
    1, O_EOR, 1, O_SRE, O_NOP_A, O_EOR, O_LSR, O_SRE,
    1, O_ADC, 1, O_RRA, O_NOP_A, O_ADC, O_ROR, O_RRA, // 60
    1, 1, 1, 1, O_JMP_I, O_ADC, O_ROR, O_RRA,
    1, O_ADC, 1, O_RRA, O_NOP_A, O_ADC, O_ROR, O_RRA, // 70
    1, O_ADC, 1, O_RRA, O_NOP_A, O_ADC, O_ROR, O_RRA,
    1, O_STA, 1, O_SAX, O_STY, O_STA, O_STX, O_SAX, // 80
    1, 1, 1, 1, O_STY, O_STA, O_STX, O_SAX,
    1, O_STA, 1, O_SHA, O_STY, O_STA, O_STX, O_SAX, // 90
    1, O_STA, 1, O_SHS, O_SHY, O_STA, O_SHX, O_SHA,
    1, O_LDA, 1, O_LAX, O_LDY, O_LDA, O_LDX, O_LAX, // a0
    1, 1, 1, 1, O_LDY, O_LDA, O_LDX, O_LAX,
    1, O_LDA, 1, O_LAX, O_LDY, O_LDA, O_LDX, O_LAX, // b0
    1, O_LDA, 1, O_LAS, O_LDY, O_LDA, O_LDX, O_LAX,
    1, O_CMP, 1, O_DCP, O_CPY, O_CMP, O_DEC, O_DCP, // c0
    1, 1, 1, 1, O_CPY, O_CMP, O_DEC, O_DCP,
    1, O_CMP, 1, O_DCP, O_NOP_A, O_CMP, O_DEC, O_DCP, // d0
    1, O_CMP, 1, O_DCP, O_NOP_A, O_CMP, O_DEC, O_DCP,
    1, O_SBC, 1, O_ISB, O_CPX, O_SBC, O_INC, O_ISB, // e0
    1, 1, 1, 1, O_CPX, O_SBC, O_INC, O_ISB,
    1, O_SBC, 1, O_ISB, O_NOP_A, O_SBC, O_INC, O_ISB, // f0
    1, O_SBC, 1, O_ISB, O_NOP_A, O_SBC, O_INC, O_ISB)

  final private[this] val IRQ_STATE = 0x8
  final private[this] val IRQ_STATE_2 = 0x9
  final private[this] val IRQ_STATE_3 = 0xA
  final private[this] val IRQ_STATE_4 = 0xB
  final private[this] val IRQ_STATE_5 = 0xC
  final private[this] val IRQ_STATE_6 = 0xD
  final private[this] val IRQ_STATE_7 = 0xE
  final private[this] val NMI_STATE = 0x10
  final private[this] val NMI_STATE_2 = 0x11
  final private[this] val NMI_STATE_3 = 0x12
  final private[this] val NMI_STATE_4 = 0x13
  final private[this] val NMI_STATE_5 = 0x14
  final private[this] val NMI_STATE_6 = 0x15
  final private[this] val NMI_STATE_7 = 0x16

  // -----------------------------------------------------------------------------------------------------  

  @inline private[this] def push(data: Int) {
    mem.write(0x0100 | SP, data)
    SP = (SP - 1) & 0xff
  }

  @inline private[this] def pop = {
    SP = (SP + 1) & 0xff
    mem.read(0x0100 | SP)
  }

  @inline private[this] final def set_nz(src: Int) {
    val value = src & 0xff
    if (value >= 0x80) sen else cln
    if (value == 0) sez else clz
  }

  @inline private[this] def DoRMW { state = RMW_DO_IT }
  @inline private[this] def Execute { state = OP_TAB(op) }
  @inline private[this] def Last { state = 0 }

  final def execute {
    import annotation.switch
    (state: @switch) match {
      // Opcode fetch (cycle 0)
      case 0 =>
        if (baLow) return
        op = mem.read(PC); PC += 1
        state = MODE_TAB(op)
      // IRQ
      case IRQ_STATE =>
        if (baLow) return
        mem.read(PC)
        state = IRQ_STATE_2
      case IRQ_STATE_2 =>
        if (baLow) return
        mem.read(PC)
        state = IRQ_STATE_3
      case IRQ_STATE_3 =>
        push((PC >> 8) & 0xFF)
        state = IRQ_STATE_4
      case IRQ_STATE_4 =>
        push(PC & 0xFF)
        state = IRQ_STATE_5
      case IRQ_STATE_5 =>
        push(SR & ~B_FLAG)
        sei
        state = IRQ_STATE_6
      case IRQ_STATE_6 =>
        if (baLow) return
        PC = mem.read(0xfffe)
        state = IRQ_STATE_7
      case IRQ_STATE_7 =>
        if (baLow) return
        PC |= mem.read(0xffff) << 8
        Last
      // NMI
      case NMI_STATE =>
        if (baLow) return
        mem.read(PC)
        state = IRQ_STATE_2
      case NMI_STATE_2 =>
        if (baLow) return
        mem.read(PC)
        state = IRQ_STATE_3
      case NMI_STATE_3 =>
        push((PC >> 8) & 0xFF)
        state = IRQ_STATE_4
      case NMI_STATE_4 =>
        push(PC & 0xFF)
        state = IRQ_STATE_5
      case NMI_STATE_5 =>
        push(SR & ~B_FLAG)
        sei
        state = IRQ_STATE_6
      case NMI_STATE_6 =>
        if (baLow) return
        PC = mem.read(0xfffa)
        state = IRQ_STATE_7
      case NMI_STATE_7 =>
        if (baLow) return
        PC |= mem.read(0xfffb) << 8
        Last
      // Addressing modes: Fetch effective address, no extra cycles (-> ar)
      case A_ZERO =>
        if (baLow) return
        ar = mem.read(PC) ; PC += 1
		Execute
	  case A_ZEROX =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = A_ZEROX1
	  case A_ZEROX1 =>
	    if (baLow) return
		mem.read(ar)
		ar = (ar + X) & 0xff
		Execute
	  case A_ZEROY =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = A_ZEROY1
	  case A_ZEROY1 =>
		if (baLow) return
		mem.read(ar)
		ar = (ar + Y) & 0xff;
		Execute
	  case A_ABS =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = A_ABS1
	  case A_ABS1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		ar = ar | (data << 8)
		Execute
	  case A_ABSX =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = A_ABSX1
	  case A_ABSX1 =>
		if (baLow) return
		ar2 = mem.read(PC) ; PC += 1 // Note: Some undocumented opcodes rely on the value of ar2
		if (ar + X < 0x100) state = A_ABSX2 else state = A_ABSX3
		ar = (ar + X) & 0xff | (ar2 << 8)
	  case A_ABSX2 =>	// No page crossed
	    if (baLow) return
		mem.read(ar)
		Execute
	  case A_ABSX3 =>	// Page crossed
	    if (baLow) return
		mem.read(ar)
		ar += 0x100
		Execute
	  case A_ABSY =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = A_ABSY1
	  case A_ABSY1 =>
		if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1 // Note: Some undocumented opcodes rely on the value of ar2
		if (ar + Y < 0x100) state = A_ABSY2 else state = A_ABSY3
		ar = (ar + Y) & 0xff | (ar2 << 8)
	  case A_ABSY2 =>	// No page crossed
	    if (baLow) return
		mem.read(ar)
		Execute
	  case A_ABSY3 =>	// Page crossed
		if (baLow) return
		mem.read(ar)
		ar += 0x100
		Execute
	  case A_INDX =>
	    if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1 // Note: Some undocumented opcodes rely on the value of ar2
		state = A_INDX1
	  case A_INDX1 =>
	    if (baLow) return
		mem.read(ar2)
		ar2 = (ar2 + X) & 0xff
		state = A_INDX2
	  case A_INDX2 =>
	    if (baLow) return
	    ar = mem.read(ar2)
		state = A_INDX3
	  case A_INDX3 =>
	    if (baLow) return
	    data = mem.read((ar2 + 1) & 0xff)
		ar = ar | (data << 8)
		Execute
	  case A_INDY =>
	    if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1
		state = A_INDY1
	  case A_INDY1 =>
	    if (baLow) return
	    ar = mem.read(ar2)
		state = A_INDY2
	  case A_INDY2 =>
	    if (baLow) return
	    ar2 = mem.read((ar2 + 1) & 0xff) // Note: Some undocumented opcodes rely on the value of ar2
		if (ar + Y < 0x100) state = A_INDY3 else state = A_INDY4
		ar = (ar + Y) & 0xff | (ar2 << 8)
	  case A_INDY3 =>	// No page crossed
	    if (baLow) return
	    mem.read(ar)
		Execute
	  case A_INDY4 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		Execute
	  // Addressing modes: Fetch effective address, extra cycle on page crossing (-> ar)
	  case AE_ABSX =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = AE_ABSX1
	  case AE_ABSX1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		if (ar + X < 0x100) {
			ar = (ar + X) & 0xff | (data << 8)
			Execute
		} else {
			ar = (ar + X) & 0xff | (data << 8)
			state = AE_ABSX2
		}
	  case AE_ABSX2 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		Execute
	  case AE_ABSY =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = AE_ABSY1
	  case AE_ABSY1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		if (ar + Y < 0x100) {
			ar = (ar + Y) & 0xff | (data << 8)
			Execute
		} else {
			ar = (ar + Y) & 0xff | (data << 8)
			state = AE_ABSY2
		}
	  case AE_ABSY2 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		Execute
	  case AE_INDY =>
	    if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1
		state = AE_INDY1
	  case AE_INDY1 =>
	    if (baLow) return
	    ar = mem.read(ar2)
		state = AE_INDY2
	  case AE_INDY2 =>
	    if (baLow) return
	    data = mem.read((ar2 + 1) & 0xff)
		if (ar + Y < 0x100) {
			ar = (ar + Y) & 0xff | (data << 8)
			Execute
		} else {
			ar = (ar + Y) & 0xff | (data << 8)
			state = AE_INDY3
		}
	  case AE_INDY3 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		Execute
	  // Addressing modes: Read operand, write it back, no extra cycles (-> ar, rdbuf)
	  case M_ZERO =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		DoRMW
	  case M_ZEROX =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = M_ZEROX1
	  case M_ZEROX1 =>
	    if (baLow) return
	    mem.read(ar)
		ar = (ar + X) & 0xff
		DoRMW
	  case M_ZEROY =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = M_ZEROY1
	  case M_ZEROY1 =>
	    if (baLow) return
	    mem.read(ar)
		ar = (ar + Y) & 0xff
		DoRMW
	  case M_ABS =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = M_ABS1
	  case M_ABS1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		ar = ar | (data << 8)
		DoRMW
	  case M_ABSX =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = M_ABSX1
	  case M_ABSX1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		if (ar + X < 0x100) state = M_ABSX2 else state = M_ABSX3
		ar = (ar + X) & 0xff | (data << 8)
	  case M_ABSX2 =>	// No page crossed
	    if (baLow) return
	    mem.read(ar)
		DoRMW
	  case M_ABSX3 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		DoRMW
	  case M_ABSY =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = M_ABSY1
	  case M_ABSY1 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		if (ar + Y < 0x100) state = M_ABSY2	else state = M_ABSY3
		ar = (ar + Y) & 0xff | (data << 8)
	  case M_ABSY2 =>	// No page crossed
	    if (baLow) return
	    mem.read(ar)
		DoRMW
	  case M_ABSY3 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		DoRMW
	  case M_INDX =>
	    if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1
		state = M_INDX1
	  case M_INDX1 =>
	    if (baLow) return
	    mem.read(ar2)
		ar2 = (ar2 + X) & 0xff
		state = M_INDX2
	  case M_INDX2 =>
	    if (baLow) return
	    ar = mem.read(ar2)
		state = M_INDX3
	  case M_INDX3 =>
	    if (baLow) return
	    data = mem.read((ar2 + 1) & 0xff)
		ar = ar | (data << 8)
		DoRMW
	  case M_INDY =>
	    if (baLow) return
	    ar2 = mem.read(PC) ; PC += 1
		state = M_INDY1
	  case M_INDY1 =>
	    if (baLow) return
	    ar = mem.read(ar2)
		state = M_INDY2
	  case M_INDY2 =>
	    if (baLow) return
	    data = mem.read((ar2 + 1) & 0xff)
		if (ar + Y < 0x100) state = M_INDY3 else state = M_INDY4
		ar = (ar + Y) & 0xff | (data << 8)
	  case M_INDY3 =>	// No page crossed
	    if (baLow) return
	    mem.read(ar)
		DoRMW
	  case M_INDY4 =>	// Page crossed
	    if (baLow) return
	    mem.read(ar)
		ar += 0x100
		DoRMW
	  case RMW_DO_IT =>
	    if (baLow) return
	    rdbuf = mem.read(ar)
		state = RMW_DO_IT1
	  case RMW_DO_IT1 =>
	    mem.write(ar,rdbuf)
		Execute
	  // ------------------ OPERATIONS -------------------------------
	  // Load group
	  case O_LDA =>
	    if (baLow) return
	    data = mem.read(ar)
	    A = data
		set_nz(A)
		Last
	  case O_LDA_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A = data
		set_nz(A)
		Last
	  case O_LDX =>
	    if (baLow) return
	    data = mem.read(ar)
	    X = data
		set_nz(X)
		Last
	  case O_LDX_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    X = data
		set_nz(X)
		Last
	  case O_LDY =>
	    if (baLow) return
	    data = mem.read(ar)
	    Y = data
		set_nz(Y)
		Last
	  case O_LDY_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    Y = data
		set_nz(Y)
		Last
	  // Store group
	  case O_STA =>
	    mem.write(ar,A)
		Last
	  case O_STX =>
	    mem.write(ar,X)
	    Last
	  case O_STY =>
	    mem.write(ar,Y)
		Last
	  // Transfer group
	  case O_TAX =>
	    if (baLow) return
	    mem.read(PC)
	    X = A
		set_nz(X)
		Last
	  case O_TXA =>
	    if (baLow) return
	    mem.read(PC)
	    A = X
		set_nz(A)
		Last
	  case O_TAY =>
	    if (baLow) return
	    mem.read(PC)
	    Y = A
		set_nz(Y)
		Last
	  case O_TYA =>
	    if (baLow) return
	    mem.read(PC)
	    A = Y
		set_nz(A)
		Last
	  case O_TSX =>
	    if (baLow) return
	    mem.read(PC)
	    X = SP
		set_nz(X)
		Last
	  case O_TXS =>
	    if (baLow) return
	    mem.read(PC)
		SP = X
		Last
	  // Arithmetic group
	  case O_ADC =>
	    if (baLow) return
	    data = mem.read(ar)
		do_adc(data)
		Last
	  case O_ADC_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		do_adc(data)
		Last
	  case O_SBC =>
	    if (baLow) return
	    data = mem.read(ar)
		do_sbc(data)
		Last
	  case O_SBC_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		do_sbc(data)
		Last
	  // Increment/decrement group
	  case O_INX =>
	    if (baLow) return
	    mem.read(PC)
	    X = (X + 1) & 0xFF
		set_nz(X);
		Last
	  case O_DEX =>
	    if (baLow) return
	    mem.read(PC)
	    X = (X - 1) & 0xFF
		set_nz(X)
		Last
	  case O_INY =>
	    if (baLow) return
	    mem.read(PC)
		Y = (Y + 1) & 0xFF
		set_nz(Y)
		Last
	  case O_DEY =>
	    if (baLow) return
	    mem.read(PC)
	    Y = (Y - 1) & 0xFF
		set_nz(Y)
		Last
	  case O_INC =>
	    rdbuf = (rdbuf + 1) & 0xFF
	    set_nz(rdbuf)
	    mem.write(ar,rdbuf)
		Last
	  case O_DEC =>
	    rdbuf = (rdbuf - 1) & 0xFF
	    set_nz(rdbuf)
	    mem.write(ar,rdbuf)
		Last
	  // Logic group
	  case O_AND =>
	    if (baLow) return
	    data = mem.read(ar)
	    A &= data
		set_nz(A)
		Last
	  case O_AND_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A &= data	    
		set_nz(A)
		Last
	  case O_ORA =>
	    if (baLow) return
	    data = mem.read(ar)
	    A |= data
		set_nz(A)
		Last
	  case O_ORA_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A |= data
		set_nz(A)
		Last
	  case O_EOR =>
	    if (baLow) return
	    data = mem.read(ar)
	    A ^= data
		set_nz(A)
		Last
	  case O_EOR_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A ^= data
		set_nz(A)
		Last
	  // Compare group
	  case O_CMP =>
	    if (baLow) return
	    data = mem.read(ar)
	    ar = A - data
		set_nz(ar)
	    if (ar >= 0) sec else clc
		Last
	  case O_CMP_I =>
		if (baLow) return
		data = mem.read(PC) ; PC += 1
		ar = A - data
		set_nz(ar)
		if (ar >= 0) sec else clc
		Last
	  case O_CPX =>
	    if (baLow) return
	    data = mem.read(ar)
	    ar = X - data
		set_nz(ar)
	    if (ar >= 0) sec else clc
		Last
	  case O_CPX_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    ar = X - data
		set_nz(ar)
		if (ar >= 0) sec else clc
		Last
	  case O_CPY =>
	    if (baLow) return
	    data = mem.read(ar)
	    ar = Y - data
		set_nz(ar)
		if (ar >= 0) sec else clc
		Last
	  case O_CPY_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    ar = Y - data
		set_nz(ar)
		if (ar >= 0) sec else clc
		Last
	  // Bit-test group
	  case O_BIT =>
	    if (baLow) return
	    data = mem.read(ar)
	    if (data >= 0x80) sen else cln
	    if ((data & V_FLAG) == V_FLAG) sev else clv
	    if ((A & data) == 0) sez else clz
		Last
	  // Shift/rotate group
	  case O_ASL =>
	    if ((rdbuf & N_FLAG) == N_FLAG) sec else clc
	    rdbuf = (rdbuf << 1) & 0xFF
	    mem.write(ar,rdbuf)
	    set_nz(rdbuf)
		Last
	  case O_ASL_A =>
	    if (baLow) return
	    mem.read(PC)
	    if ((A & N_FLAG) == N_FLAG) sec else clc
	    A = (A << 1) & 0xFF
		set_nz(A)
		Last
	  case O_LSR =>
	    if ((rdbuf & 0x01) == 0x01) sec else clc
	    rdbuf = (rdbuf >> 1) & 0xFF
	    mem.write(ar,rdbuf)
	    set_nz(rdbuf)
		Last
	  case O_LSR_A =>
	    if (baLow) return
	    mem.read(PC)
	    if ((A & 0x01) == 0x01) sec else clc
	    A = (A >> 1) & 0xFF
		set_nz(A)
		Last
	  case O_ROL =>
	    val oldC = if (isCarry) 1 else 0
	    if ((rdbuf & N_FLAG) == N_FLAG) sec else clc
        rdbuf = ((rdbuf << 1) & 0xff) | oldC
	    set_nz(rdbuf)
	    mem.write(ar,rdbuf)
		Last
	  case O_ROL_A =>
	    if (baLow) return
	    mem.read(PC)		
	    val oldC = if (isCarry) 1 else 0
	    if ((A & N_FLAG) == N_FLAG) sec else clc
        A = ((A << 1) & 0xff) | oldC
	    set_nz(A)
		Last
	  case O_ROR =>
	    val oldC = if (isCarry) 0x80 else 0
	    if ((rdbuf & 1) == 1) sec else clc
        rdbuf = ((rdbuf >> 1) & 0xff) | oldC
        set_nz(rdbuf)
        mem.write(ar,rdbuf)
		Last
	  case O_ROR_A =>
	    if (baLow) return
	    mem.read(PC)
	    val oldC = if (isCarry) 0x80 else 0
	    if ((A & 1) == 1) sec else clc
        A = ((A >> 1) & 0xff) | oldC
        set_nz(A)
		Last
	  // Stack group
	  case O_PHA =>
	    if (baLow) return
	    mem.read(PC)
		state = O_PHA1
	  case O_PHA1 =>
	    //push(A)
	    mem.write(SP | 0x100,A)
	    SP = (SP - 1) & 0xFF
		Last
	  case O_PLA =>
	    if (baLow) return
	    mem.read(PC)
		state = O_PLA1
	  case O_PLA1 =>
	    if (baLow) return
	    mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_PLA2
	  case O_PLA2 =>
	    A = mem.read(SP | 0x100)	    
		set_nz(A)
		Last
	  case O_PHP =>
	    if (baLow) return
	    mem.read(PC)
		state = O_PHP1
	  case O_PHP1 =>
	    val sr = SR | B_FLAG
	    push(sr)
		Last
	  case O_PLP =>
	    if (baLow) return
	    mem.read(PC)
		state = O_PLP1
	  case O_PLP1 =>
	    if (baLow) return
	    mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_PLP2
	  case O_PLP2 =>
	    if (baLow) return
	    data = mem.read(SP | 0x100)
	    SR = (data & ~B_FLAG) | (SR & B_FLAG)
		Last
	  // Jump/branch group
	  case O_JMP =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = O_JMP1
	  case O_JMP1 =>
	    if (baLow) return
	    data = mem.read(PC)
		PC = (data << 8) | ar
		Last
	  case O_JMP_I =>
	    if (baLow) return
	    PC = mem.read(ar)
		state = O_JMP_I1
	  case O_JMP_I1 =>
	    if (baLow) return
	    data = mem.read((ar + 1) & 0xff | ar & 0xff00)
		PC |= data << 8
		Last
	  case O_JSR =>
	    if (baLow) return
	    ar = mem.read(PC) ; PC += 1
		state = O_JSR1
	  case O_JSR1 =>
	    if (baLow) return
	    mem.read(SP | 0x100)
		state = O_JSR2
	  case O_JSR2 =>
	    mem.write(SP | 0x100,(PC >> 8) & 0xFF)
	    SP = (SP - 1) & 0xFF
		state = O_JSR3
	  case O_JSR3 =>
	    mem.write(SP | 0x100,PC & 0xFF)
	    SP = (SP - 1) & 0xFF
		state = O_JSR4
	  case O_JSR4 =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
		PC = ar | (data << 8)
		Last
	  case O_RTS =>
	    if (baLow) return
	    mem.read(PC)
		state = O_RTS1
	  case O_RTS1 =>
	    if (baLow) return
	    mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_RTS2
	  case O_RTS2 =>
	    if (baLow) return
	    PC = mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_RTS3
	  case O_RTS3 =>
	    if (baLow) return
	    data = mem.read(SP | 0x100)
		PC |= data << 8
		state = O_RTS4
	  case O_RTS4 =>
	    if (baLow) return
	    mem.read(PC) ; PC += 1
		Last
	  case O_RTI =>
	    if (baLow) return
	    mem.read(PC)
		state = O_RTI1
	  case O_RTI1 =>
	    if (baLow) return
	    mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_RTI2
	  case O_RTI2 =>
	    if (baLow) return
	    data = mem.read(SP | 0x100)
	    SR = (data & ~B_FLAG) | (SR & B_FLAG)
		SP = (SP + 1) & 0xFF
		state = O_RTI3
	  case O_RTI3 =>
	    if (baLow) return
	    PC = mem.read(SP | 0x100)
	    SP = (SP + 1) & 0xFF
		state = O_RTI4
	  case O_RTI4 =>
	    if (baLow) return
	    data = mem.read(SP | 0x100)
		PC |= data << 8
		Last
	  case O_BRK =>
	    if (baLow) return
	    mem.read(PC) ; PC += 1
		state = O_BRK1
	  case O_BRK1 =>
	    mem.write(SP | 0x100,PC >> 8)
	    SP = (SP - 1) & 0xFF
		state = O_BRK2
	  case O_BRK2 =>
	    mem.write(SP | 0x100,PC & 0xFF)
	    SP = (SP - 1) & 0xFF
		state = O_BRK3
	  case O_BRK3 =>
	    push(SR | B_FLAG)
		sei
		// CHECK NMI
		state = O_BRK4
	  case O_BRK4 =>
	    // first_nmi_cycle++
	    if (baLow) return	
	    PC = mem.read(0xfffe)
		state = O_BRK5
	  case O_BRK5 =>
	    if (baLow) return
	    data = mem.read(0xffff)
		PC |= data << 8
		Last
	  case O_BCS => branch(isCarry)
	  case O_BCC => branch(!isCarry)
	  case O_BEQ => branch(isZero)
	  case O_BNE => branch(!isZero)
	  case O_BVS => branch(isOverflow)
	  case O_BVC => branch(!isOverflow)
	  case O_BMI => branch(isNegative)
	  case O_BPL => branch(!isNegative)
	  case O_BRANCH_NP =>	// No page crossed
	    if (baLow) return
	    mem.read(PC)
		PC = ar
		Last
	  case O_BRANCH_BP =>	// Page crossed, branch backwards
	    if (baLow) return
	    mem.read(PC)
		PC = ar
		state = O_BRANCH_BP1
	  case O_BRANCH_BP1 =>
	    if (baLow) return
	    mem.read(PC | 0x100)
		Last
	  case O_BRANCH_FP =>	// Page crossed, branch forwards
	    if (baLow) return
	    mem.read(PC)
		PC = ar
		state = O_BRANCH_FP1
	  case O_BRANCH_FP1 =>
	    if (baLow) return
	    mem.read((PC - 0x100) & 0xFFFF)
		Last
	  // Flag group
	  case O_SEC =>
	    if (baLow) return
	    mem.read(PC)
		sec
		Last
	  case O_CLC =>
	    if (baLow) return
	    mem.read(PC)
	    clc
		Last
	  case O_SED =>
	    if (baLow) return
	    mem.read(PC)
	    sed
		Last
	  case O_CLD =>
	    if (baLow) return
	    mem.read(PC)
	    cld
		Last
	  case O_SEI =>
	    if (baLow) return
	    mem.read(PC)
	    sei
		Last
	  case O_CLI =>
	    if (baLow) return
	    mem.read(PC)
	    cli
		Last
	  case O_CLV =>
	    if (baLow) return
	    mem.read(PC)
	    clv
		Last
	  // NOP group
	  case O_NOP =>
	    if (baLow) return
	    mem.read(PC)
		Last
	  // Undocumented
		// NOP group
	  case O_NOP_I =>
	    if (baLow) return
	    mem.read(PC)
	    PC += 1
		Last
	  case O_NOP_A =>
	    if (baLow) return
	    mem.read(ar)
		Last
	  // Load A/X group
	  case O_LAX =>
	    if (baLow) return
	    data = mem.read(ar)
	    A = data
	    X = data
	    set_nz(A)
		Last
	  // Store A/X group
	  case O_SAX =>
	    mem.write(ar,A & X)
		Last
	  // ASL/ORA group
	  case O_SLO =>
	    if ((rdbuf & 0x80) > 0) sec else clc
		rdbuf <<= 1
		mem.write(ar,rdbuf)
		A |= rdbuf
		set_nz(A)
		Last
	  // ROL/AND group
	  case O_RLA =>
		val tmp = (rdbuf & 0x80) > 0
		rdbuf = if (isCarry) (rdbuf << 1) | 0x01 else rdbuf << 1
		if (tmp) sec else clc
		mem.write(ar,rdbuf)
		A &= rdbuf
		set_nz(A)
		Last
	  // LSR/EOR group
	  case O_SRE =>
	    if ((rdbuf & 0x01) > 0) sec else clc
		rdbuf >>= 1
		mem.write(ar,rdbuf)
		A ^= rdbuf
		set_nz(A)
		Last
	  // ROR/ADC group
	  case O_RRA =>
		val tmp = (rdbuf & 0x01) > 0
		rdbuf = if (isCarry) (rdbuf >> 1) | 0x80 else rdbuf >> 1
		if (tmp) sec else clc
		mem.write(ar,rdbuf)
		do_adc(rdbuf)
		Last
	  // DEC/CMP group
	  case O_DCP =>
	    rdbuf -= 1
	    mem.write(ar,rdbuf & 0xFF)
	    ar = A - rdbuf
	    set_nz(ar)
	    if (ar < 0x100) sec else clc
		Last
	  // INC/SBC group
	  case O_ISB =>
	    rdbuf = (rdbuf + 1) & 0xFF
	    mem.write(ar,rdbuf)
		do_sbc(rdbuf)
		Last
	  // Complex functions
	  case O_ANC_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A &= data
		set_nz(A)
	    if (isNegative) sec else clc
		Last
	  case O_ASR_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A &= data
	    if ((A & 0x01) > 0) sec else clc
	    A >>= 1
	    set_nz(A)
		Last
	  case O_ARR_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    val _data = data & A
	    A = if (isCarry) (_data >> 1) | 0x80 else _data >> 1
	    if (!isDecimal) {
          set_nz(A)
          if ((A & 0x40) > 0) sec else clc
          if (((A & 0x40) ^ ((A & 0x20) << 1)) > 0) sev else clv
        }
        else {
          if (isCarry) sen else cln
          if (A == 0) sez else clz
          if (((_data ^ A) & 0x40) != 0) sev else clv
          if ((_data & 0x0F) + (_data & 0x01) > 5) A = A & 0xF0 | (A + 6) & 0x0F
          if (((_data + (_data & 0x10)) & 0x1F0) > 0x50) {
            sec
            A += 0x60
          } 
          else clc
        }
	  case O_ANE_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A = (A | 0xee) & X & data
		set_nz(A)
		Last
	  case O_LXA_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    A = (A | 0xee) & data
	    X = A
		set_nz(A)
		Last
	  case O_SBX_I =>
	    if (baLow) return
	    data = mem.read(PC) ; PC += 1
	    ar = (X & A) - data
	    X = ar & 0xFF
	    set_nz(X)
	    if (ar < 0x100) sec else clc
		Last
	  case O_LAS =>
	    if (baLow) return
	    data = mem.read(ar)
	    X = data & SP
	    SP = X
	    A = X
		set_nz(A)
		Last
	  case O_SHS =>		// ar2 contains the high byte of the operand address
	    SP = A & X
	    mem.write(ar,(ar2 + 1) & SP)
		Last
	  case O_SHY =>		// ar2 contains the high byte of the operand address
	    mem.write(ar,Y & (ar2 + 1))
		Last
	  case O_SHX =>		// ar2 contains the high byte of the operand address
	    mem.write(ar,X & (ar2 + 1))
		Last
	  case O_SHA =>		// ar2 contains the high byte of the operand address
	    mem.write(ar,A & X & (ar2 + 1))
		Last
	  case 1 => throw new CPU6510.CPUJammedException
	  case _ => throw new IllegalArgumentException("Bad state " + state + " at PC=" + Integer.toHexString(PC))
    }
  }
  
  @inline private[this] def branch(flag:Boolean) {
    data = mem.read(PC) ; PC += 1
	if (flag) {
	  ar = PC + data.asInstanceOf[Byte]
	  if ((ar >> 8) != (PC >> 8)) {
		if ((data & 0x80) > 0) state = O_BRANCH_BP else state = O_BRANCH_FP
	  } else state = O_BRANCH_NP
	} else state = 0
  }
  
  @inline private[this] def do_adc(data:Int) {
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
	  if ((((A ^ data) & 0x80) == 0) && (((A ^ tmp) & 0x80) != 0)) sev else clv
	  if (tmp > 0xff) sec else clc
	  A = tmp & 0xff
	  set_nz(A)
	}
  }
  
  @inline private[this] def do_sbc(data:Int) {
    var tmp = A - data - (if (isCarry) 0 else 1)
    val nextCarry = tmp >= 0
    tmp = tmp & 0x1ff
    set_nz(tmp)
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

  def init = reset

  def reset {
    PC = readWordFrom(0xfffc, mem)
    irqLow = false
    nmiLow = false
    nmiOnNegativeEdge = false
    state = 0
    A = 0
    X = 0
    Y = 0
    SR = 0
    SP = 0
    Log.info(s"CPU reset! PC = ${hex4(PC)}")
  }

  final def fetchAndExecute: Int = {
    if (breakAt != -1 && PC == breakAt && state == 0) {
      breakAt = -1
      tracing = true
      breakCallBack(toString)
    }
    
    // check interrupts
    if (nmiOnNegativeEdge && state == 0) {
      nmiOnNegativeEdge = false
      state = NMI_STATE
    } 
    else 
    if (irqLow && !isInterrupt && state == 0) {      
      state = IRQ_STATE
    }
    else {
      if (tracing && state == 0) Log.debug(formatDebug)
      //if (state == 0) println(formatDebug + "\t" + this)
      CURRENT_OP_PC = PC
      execute
      if (tracing && state == 0) {
        syncObject.synchronized { syncObject.wait }
        stepCallBack(toString)
      }
    }
    0
  }
  
  protected def formatDebug = s"[${id.toString}] ${CPU6510.disassemble(mem,PC).toString}"
}

object TestCPU extends App {
  val mem = new CPU6510Mems.MAIN_MEMORY
  mem.initComponent
  val cpu = new CPU6510_CE(mem,ChipID.CPU)
  cpu.init
  while (true) {
    cpu.fetchAndExecute
    readLine
  }
}