package ucesoft.c64.cpu

import util.parsing.combinator._
import java.io.Reader
import java.io.InputStreamReader
import javax.swing.JFrame
import javax.swing.JTextArea
import java.awt.Font
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JDialog
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JFileChooser
import java.io.PrintWriter
import java.io.FileWriter
import java.io.StringReader
import javax.swing.JOptionPane
import java.awt.Color
import javax.swing.JScrollPane
import scala.language.postfixOps

class AssemblerException(msg: String) extends Exception(msg)

class Assembler {
  import CPU6510._
  import Mode._
  import Instruction._

  private val OPS = (Instruction.values.map { _.toString.toLowerCase }) ++ (Instruction.values map { _.toString.toUpperCase })
  private trait Step {
    var PC: Option[Int] = None
    var listeners: List[Step] = Nil
    val label: Option[String]

    def notify(labelAddress: Int) {}
    def addListener(l: Step) { listeners = l :: listeners }
    def notifyListeners = listeners foreach { _.notify(PC.get) }
  }
  private trait Operand
  private case class Label(label: String,fn:String="") extends Operand
  private case class Value(value: Int) extends Operand

  private case object REM extends Step { val label = None }
  private case class EQU(label: Option[String], value: Int) extends Step
  private case class ORG(value: Int) extends Step { val label = None }
  private case class BYTE(label: Option[String], bytes: List[Int]) extends Step
  private case class WORD(label: Option[String], bytes: List[Int]) extends Step
  private case class OP(label: Option[String], op: String, var mode: Mode.MODE, var operand: Option[Operand]) extends Step {
    override def notify(labelAddress: Int) {
      operand match {
        case Some(Label(_,fn)) => operand = Some(Value(applyFunctionOnLabel(fn,labelAddress)))
        case _ => println("Error while notifying address to this: " + toString)
      }
    }
  }

  def isZeroPage(a: Int) = a <= 0xff
  val BRANCHES = Set(BPL,BMI,BVC,BVS,BCC,BCS,BNE,BEQ) map { _.toString }

  private object Parser extends JavaTokenParsers {
    override val whiteSpace = "[ \t\f]+".r
    def label: Parser[String] = ident
    def number: Parser[Int] = wholeNumber ^^ { i => i.toInt }
    def hexWord: Parser[Int] = "\\$[0-9A-Fa-f]{1,4}".r ^^ { i => Integer.parseInt(i.substring(1), 16) }
    def binWord : Parser[Int] = "%[01]{1,16}".r ^^ { i => Integer.parseInt(i.substring(1), 2) }
    def address: Parser[Int] = number | hexWord | binWord

    // -------------------------------
    def rem: Parser[Step] = ";.*".r ^^ { r => REM }
    def org: Parser[ORG] = ("*=" | "org") ~> address ^^ { case a => ORG(a) }
    def equ: Parser[EQU] = (label <~ "=") ~ address ^^ { case l ~ n => EQU(Some(l), n) }
    def byte: Parser[BYTE] = (opt(label) <~ (".byte" | ".by")) ~ byteList ^^ { case l ~ bl => BYTE(l, bl) }
    def byteList: Parser[List[Int]] = repsep(byteType, ",") ^^ { l => l.flatten }
    def byteType: Parser[List[Int]] = address ^^ { a => List(a) } | "\"[^\"]*\"".r ^^ { s => s.toList map { _.toInt } }
    def word: Parser[WORD] = (opt(label) <~ (".word" | ".wd")) ~ wordList ^^ { case l ~ bl => WORD(l, bl) }
    def wordList: Parser[List[Int]] = repsep(address, ",")

    def opcode: Parser[String] = OPS.drop(1).foldLeft(OPS.head: Parser[String]) { (p, b) => p | b }
    def op: Parser[OP] = opt(label <~ ":") ~ opcode ~ opt(mode) ^^ {
      case l ~ o ~ None => OP(l, o, IMP, None)
      case l ~ o ~ Some(m) =>
        if (BRANCHES.contains(o.toUpperCase)) OP(l, o, REL, Some(m._2)) else OP(l, o, m._1, Some(m._2))
    }

    def mode: Parser[(Mode.MODE, Operand)] =
      "#" ~> address ^^ { a => (IMM, Value(a)) } |
        "#" ~> (opt("<" | ">") ~ label) ^^ { 
          case None~a => (IMM, Label(a))
          case Some(fn)~a => (IMM, Label(a,fn))
        } |
        address ~ opt(",x" | ",X" | ",y" | ",Y") ^^ {
          case a ~ None => (if (isZeroPage(a)) ZP else ABS, Value(a))
          case a ~ Some(v) => v.toUpperCase match {
            case ",X" => (if (isZeroPage(a)) ZPX else ABX, Value(a))
            case ",Y" => (if (isZeroPage(a)) ZPY else ABY, Value(a))
          }
        } |
        label ~ opt(",x" | ",X" | ",y" | ",Y") ^^ {
          case l ~ None => (UNKNOWN_ABS_OR_ZP, Label(l))
          case l ~ Some(v) => v.toUpperCase match {
            case ",X" => (UNKNOWN_ABX_OR_ZPX, Label(l))
            case ",Y" => (UNKNOWN_ABY_OR_ZPY, Label(l))
          }
        } |
        "(" ~> address <~ ")" ^^ { a => (IND, Value(a)) } |
        "(" ~> label <~ ")" ^^ { a => (IND, Label(a)) } |
        ("(" ~> address <~ (",x" | ",X")) <~ ")" ^^ { a => (IZX, Value(a)) } |
        ("(" ~> label <~ (",x" | ",X")) <~ ")" ^^ { l => (IZX, Label(l)) } |
        ("(" ~> address <~ ")") <~ (",y" | ",Y") ^^ { a => (IZY, Value(a)) } |
        ("(" ~> label <~ ")") <~ (",y" | ",Y") ^^ { l => (IZY, Label(l)) }

    def crsEnd: Parser[String] = "[\n\r]+".r
    def crsStart: Parser[String] = "[\n\r]*".r
    def steps: Parser[List[Step]] = crsStart ~> rep((rem | org<~opt(rem) | equ<~opt(rem) | byte<~opt(rem) | word<~opt(rem) | op<~opt(rem)) <~ crsEnd)
  }

  private def makeSymbolTable(steps: List[Step]) =
    steps.flatMap {
      case s @ EQU(Some(l), _) => Some(l -> s)
      case s @ BYTE(Some(l), _) => Some(l -> s)
      case s @ WORD(Some(l), _) => Some(l -> s)
      case s @ OP(Some(l), _, _, _) => Some(l -> s)
      case _ => None
    } toMap

  private def applyFunctionOnLabel(fn:String,v:Int) = fn match {
      case "<" => v & 0xFF
      case ">" => (v >> 8) & 0xFF
      case "" => v
    }
  private def resolve(steps: List[Step]) = {
    val sym = makeSymbolTable(steps)
    var PC = 0
    val filteredSteps = steps.view.filter {
      case REM => false
      case EQU(_, _) => false
      case _ => true
    }

    for (s <- filteredSteps) {
      s.PC = Some(PC)
      s match {
        case ORG(a) => PC = a
        case BYTE(_, bl) => PC += bl.length
        case WORD(_, wl) => PC += wl.length * 2
        case op @ OP(l, _, m, o) =>
          o match {
            case Some(Label(l,fn)) =>
              sym get l match {
                case Some(target) =>
                  target match {
                    case EQU(l, v) => op.operand = Some(Value(applyFunctionOnLabel(fn,v)))
                    case _ => target.PC match {
                      case None => target.addListener(s)
                      case Some(pc) => op.operand = Some(Value(applyFunctionOnLabel(fn,pc)))
                    }
                  }
                case None => throw new AssemblerException("Can't find label " + l)
              }
            case _ =>
          }
          m match {
            case IMP => PC += 1
            case IMM | ZP | ZPX | ZPY | IZX | IZY | REL => PC += 2
            case ABS | ABX | ABY | IND => PC += 3
            case UNKNOWN_ABS_OR_ZP => op.operand match {
              case Some(Value(v)) =>
                if (isZeroPage(v)) { op.mode = ZP; PC += 2 } else { op.mode = ABS; PC += 3 }
              case Some(Label(_,_)) =>
                op.mode = ABS; PC += 3
              case _ =>
            }
            case UNKNOWN_ABX_OR_ZPX => op.operand match {
              case Some(Value(v)) =>
                if (isZeroPage(v)) { op.mode = ZPX; PC += 2 } else { op.mode = ABX; PC += 3 }
              case Some(Label(_,_)) =>
                op.mode = ABX; PC += 3
              case _ =>
            }
            case UNKNOWN_ABY_OR_ZPY => op.operand match {
              case Some(Value(v)) =>
                if (isZeroPage(v)) { op.mode = ZPY; PC += 2 } else { op.mode = ABY; PC += 3 }
              case Some(Label(_,_)) =>
                op.mode = ABY; PC += 3
              case _ =>
            }
          }
      }
      s.notifyListeners
    }
  }
    
  private def encode(steps:List[Step],mem:Memory) {
    for(s <- steps) {
      s match {
        case BYTE(_,bl) =>
          val pc = s.PC.get
          for(i <- 0 until bl.length) mem.write(pc + i,bl(i) & 0xff)
        case WORD(_,bl) =>
          val pc = s.PC.get
          for(i <- 0 until bl.length) {
            val lh = lohi(bl(i))
            mem.write(pc + i * 2,lh._1)
            mem.write(pc + i * 2 + 1,lh._2)
          }
        case OP(_,op,m,o) =>
          val pc = s.PC.get
          findCode(op.toUpperCase,m) match {
            case None => throw new AssemblerException("Invalid opcode " + op + " with addressing mode " + m)
            case Some(code) =>
              mem.write(pc,code)
              m match {
                case IMP =>
                case IMM =>
                  val operand = o.get.asInstanceOf[Value].value & 0xff
                  mem.write(pc + 1,operand) 
                case REL =>
                  val target = o.get.asInstanceOf[Value].value & 0xffff
                  val diff = (target - (pc + 2)).asInstanceOf[Byte]
                  mem.write(pc + 1,diff & 0xff)
                case _ =>
                  val operand = lohi(o.get.asInstanceOf[Value].value & 0xffff)
                  mem.write(pc + 1,operand._1)
                  mem.write(pc + 2,operand._2)
              }
          }
        case _ =>
      }
    }
  }

  def assemble(in: Reader,mem:Memory) = {
    Parser.parseAll(Parser.steps, in) match {
      case Parser.Success(parsed, _) =>
        resolve(parsed)
        try {
          encode(parsed,mem)
          None
        }
        catch {
          case t:AssemblerException =>
            Some(t.toString)
        }
        //parsed foreach println        
      case Parser.Error(msg, _) =>
        Some(msg)
      case Parser.Failure(e, _) =>
        Some(e)
    }
  }
}

object Assembler {
  /*val mem = Memory.dummyWith(0xc000,Array.ofDim[Int](1024) : _*)
  println("Type code:")
  val asm = new Assembler
  asm.assemble(new InputStreamReader(System.in),mem)
  */
  def getAssemblerDialog(parent:JFrame,mem:Memory) = {
    val assembler = new Assembler
    val textArea = new JTextArea(20,50)
    val al = new ActionListener {
      def actionPerformed(e:ActionEvent) {
        e.getActionCommand match {
          case "LOAD" =>
            val fc = new JFileChooser
            fc.showOpenDialog(textArea) match {
              case JFileChooser.APPROVE_OPTION =>
                val src = io.Source.fromFile(fc.getSelectedFile)
                val text = src.getLines.mkString("\n")
                src.close
                textArea.setText(text)
              case _ =>
            }
          case "SAVE" =>
            val fc = new JFileChooser
            fc.showSaveDialog(textArea) match {
              case JFileChooser.APPROVE_OPTION =>
                val pw = new PrintWriter(new FileWriter(fc.getSelectedFile))
                pw.println(textArea.getText)
                pw.close
              case _ =>
            }
          case "COMPILE" =>
            val sr = new StringReader(textArea.getText + "\n")
            assembler.assemble(sr,mem) match {
              case None =>
                JOptionPane.showMessageDialog(textArea,"Code assembled successfully","Success",JOptionPane.INFORMATION_MESSAGE)
              case Some(e) =>
                JOptionPane.showMessageDialog(textArea,"Error while compiling:\n" + e,"Error",JOptionPane.ERROR_MESSAGE)
            }
        }
      }
    }    
    textArea.setFont(new Font(Font.MONOSPACED,Font.BOLD,12))
    textArea.setBackground(Color.BLACK)
    textArea.setForeground(Color.GREEN)
    textArea.setCaretColor(Color.YELLOW)
    val save = new JButton("Save")
    save.setActionCommand("SAVE")
    save.addActionListener(al)
    val load = new JButton("Load")
    load.setActionCommand("LOAD")
    load.addActionListener(al)
    val compile = new JButton("Compile")
    compile.setActionCommand("COMPILE")
    compile.addActionListener(al)
    val buttonPanel = new JPanel
    buttonPanel.add(save)
    buttonPanel.add(load)
    buttonPanel.add(compile)
    val dialog = new JDialog(parent,"Assembler")
    dialog.getContentPane.add("North",buttonPanel)
    dialog.getContentPane.add("Center",new JScrollPane(textArea))
    dialog.pack
    dialog
  }
}