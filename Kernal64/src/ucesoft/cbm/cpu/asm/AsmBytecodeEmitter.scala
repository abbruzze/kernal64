package ucesoft.cbm.cpu.asm

import ucesoft.cbm.cpu.CPU65xx
import ucesoft.cbm.cpu.asm.AsmEvaluator._
import ucesoft.cbm.cpu.asm.AsmParser.{Patch, Virtual}

import java.io.PrintWriter

case class AsmEncoding(org:Int,mem:Array[Byte])

class AsmBytecodeEmitter(console:PrintWriter,blocks:List[ByteCodeBlock]) {
  def encode : AsmEncoding = {
    val ordered = orderAndCheck
    val startEnd = getStartEnd(ordered)
    console.println(s"Going to emit bytecode from ${startEnd._1.toHexString} to ${startEnd._2.toHexString}")
    val mem = buildInitalMemory(startEnd)
    for(b <- ordered) {
      console.println(s"Emitting bytecode for block ${b.getOrg.toHexString}")
      for(asm <- b.asmStatements) {
        emitByteCodeFor(startEnd._1,asm,mem)
      }
    }
    AsmEncoding(startEnd._1,mem)
  }
  private def orderAndCheck : List[ByteCodeBlock] = {
    val orderedBlocks = blocks.filterNot(_.orgType == Virtual).sortBy { _.getOrg }
    // check for intersections
    var ptr = orderedBlocks
    while (!ptr.isEmpty) {
      if (!ptr.tail.isEmpty) {
        val next = ptr.tail.head
        val current = ptr.head
        if (next.orgType != Patch && current.getOrg + current.size > next.getOrg)
          throw new CompilerException(s"Invalid code block starting at ${Integer.toHexString(next.getOrg)}: it overlaps with code block [${Integer.toHexString(current.getOrg)} - ${Integer.toHexString(current.getOrg + current.size)}]",None)      
      }
      ptr = ptr.tail
    }
    orderedBlocks
  }
  
  private def getStartEnd(blocks:List[ByteCodeBlock]) : (Int,Int) = {
    val last = blocks.last
    val first = blocks.head
    (first.getOrg,last.getOrg + last.size)
  }
  
  private def buildInitalMemory(fromEnd:(Int,Int)) : Array[Byte] = Array.ofDim[Byte](fromEnd._2 - fromEnd._1)
  
  private def getOperand(v:RuntimeValue) : List[Int] = {
    v match {
      case NumberVal(n) =>
        if (n.isValidInt) List(n.toInt) else Nil
      case StringVal(s) if s.length == 1 =>
        List(encodeChar(s.charAt(0)))
      case ListVal(l) =>
        l filter { _.isInstanceOf[NumberVal]} map { _.asInstanceOf[NumberVal].n.toInt} toList
      case _ => Nil
    }
  }
  
  private def encodeChar(c:Char) : Byte = c.toByte
  
  private def encodeText(s:String) : Array[Byte] = {
    s map encodeChar toArray
  }
  
  private def emitByteCodeFor(startOffset:Int,bcs:ByteCodeStatement,mem:Array[Byte]) : Unit = {
    import AsmParser._
    import CPU65xx._
    import Mode._
    
    var pc = bcs.pc - startOffset
    
    def emitListOf(isByte:Boolean) : Unit = {
      for(ListVal(l) <- bcs.operandValue;
          e <- l) {
        getOperand(e) match {
          case Nil =>
            throw new IllegalArgumentException(s"Error on line ${bcs.source.line}: type mismatch on operand")
          case list =>
            for(o <- list) {
              if (isByte) {
                mem(pc) = (o & 0xFF).toByte
                pc += 1
              } else {
                mem(pc) = (o & 0xFF).toByte
                mem(pc + 1) = (o >> 8).toByte
                pc += 2
              }
            }
        }
      }
    }
    
    def operand : Int = getOperand(bcs.operandValue.get) match {
      case v :: Nil => v
      case Nil =>
        throw new IllegalArgumentException(s"Error on line ${bcs.source.line}: type mismatch on operand")
    }
    
    bcs.asm match {
      case ORG(_,_,_) => // ignored
      case FILL(_,_) =>
        emitListOf(true)
      case WORD(_,isByte) =>
        emitListOf(isByte)
      case WORD_GEN(_,isByte) =>
        emitListOf(isByte)
      case BYTE_LIST(_) =>
        emitListOf(true)
      case TEXT(_,enc) =>
        for(StringVal(s) <- bcs.operandValue;c <- encodeText(s)) {
          val ch = if (enc.upper) c.toChar.toUpper.toByte else c
          val cc = if (enc.screenCode) ascii2screenCode(ch) else ch
          mem(pc) = cc.toByte
          pc += 1
        } 
      case ASM(mnemonic,mode,_) =>
        findCode(mnemonic,mode) match {
          case None => 
            throw new IllegalArgumentException(s"Invalid mode for $mnemonic mnemonic")
          case Some(opcode) =>
            mem(pc) = opcode.toByte
            pc += 1
          mode match {
            case IMP => // implicit, nothing to add
            case IMM | ZP | ZPX | ZPY | IZX | IZY =>
              mem(pc) = operand.toByte
              pc += 1
            case REL =>
              mem(pc) = (operand - (pc + 1 + startOffset)).toByte
              pc += 1
            case UNKNOWN_ABS_OR_ZP|UNKNOWN_ABX_OR_ZPX|UNKNOWN_ABY_OR_ZPY =>
              throw new IllegalStateException(s"Compiler internal error: invalid mode $mode")
            case _ =>
              val op = operand
              mem(pc) = (op & 0xFF).toByte
              mem(pc + 1) = (op >> 8).toByte
              pc += 2
          }
        }
    }
  }

  private def ascii2screenCode(c:Int) : Int = {
    if (c >= 64 && c <= 95) c - 64
    else if (c >= 96 && c <= 127) c - 32
    else if (c >= 128 && c <= 159) c + 64
    else if (c >= 160 && c <= 191) c - 64
    else if (c >= 192 && c <= 223) c - 128
    else if (c >= 224 && c <= 254) c - 128
    else if (c == 255) 94
    else if (c > 255) c & 0xFF
    else c
  }
}