package ucesoft.cbm.cpu.asm

import util.parsing.combinator._
import scala.util.parsing.input.Positional

object AsmParser {
  import ucesoft.cbm.cpu.CPU65xx._
  import Mode._
  
  private val OPCODES = (Instruction.values.map { _.toString.toLowerCase }) ++ (Instruction.values map { _.toString.toUpperCase })
  private val BRANCHES = OP_MATRIX flatMap { r => r.filter { o => o._2 == REL } } map { _._1.toString }
  private val IMP_OPCODES = {
    val op = (OP_MATRIX flatMap { r => r.filter { o => o._2 == IMP } } map { _._1.toString }).toSet
    op ++ (op map { _.toLowerCase })
  }
  
  sealed trait Statement extends Positional {
    var fileName : Option[String] = None
  }
  sealed trait ASMStatement extends Statement
  case class ModuleLabel(label:String,module:Option[String])
  case class EvalTarget(variable:ModuleLabel, fieldSelector: Option[List[String]])
  // Expressions
  sealed trait Expr extends Positional
  case object Null extends Expr
  case class Str(value: String) extends Expr
  case class Value(value: Double) extends Expr
  case class ListValue(elements: List[Expr], isRange: Boolean = false) extends Expr
  case class Label(label: String,module:Option[String]) extends Expr
  case class BinOp(op: String, op1: Expr, op2: Expr) extends Expr
  case class UnaryOp(op: String, op1: Expr, isPre: Boolean = true) extends Expr
  case class FunOp(fun: String,module:Option[String],args: List[Expr]) extends Expr
  case class NewStruct(name: String,module:Option[String],values: Array[Expr]) extends Expr
  case class FieldOrMethod(e:Expr,name:String,pars:List[Expr]) extends Expr
  // Statements
  case class LABELED(label: String) extends Statement
  case class CONST(name: String, value: Expr,isPrivate:Boolean) extends Statement
  case class VAR(name: String, initValue: Expr,isPrivate:Boolean) extends Statement
  case class PRINT(what: Expr) extends Statement
  case class EVAL(assignTo: Option[EvalTarget], value: Expr) extends Statement
  case class IF(cond: Expr, thenClause: List[Statement], elseClause: List[Statement]) extends Statement
  case class STRUCT(name: String, fields: List[String],isPrivate:Boolean) extends Statement
  case class ENUM(fields: List[(String, Option[Int])]) extends Statement
  case class WHILE(cond: Expr, body: List[Statement]) extends Statement
  case class FOR(vars: List[VAR], cond: Expr, post: List[Expr], body: List[Statement]) extends Statement
  case class MACRO(name:String,parameters:List[String],body:List[Statement]) extends Statement
  case class MACRO_CALL(name:String,actualParameters:List[Expr]) extends Statement
  case class FUNCTION(name:String,parameters:List[String],body:List[Statement],isPrivate:Boolean) extends Statement
  case class DECLARE_LABEL(name:String,value:Expr) extends Statement
  case object BREAK extends Statement
  case class DUP(times:Expr, body: List[Statement]) extends Statement
  case class ALIGN(alignTo:Expr) extends Statement
  case class ERROR(msg:Expr) extends Statement
  case class MODULE(name:String,body:List[Statement]) extends Statement
  case class INCLUDE(file:String) extends Statement
  // ASMStatement  
  case class ORG(org: Expr, sectionName: Option[String]) extends ASMStatement
  case class WORD(values: List[Expr], isByte: Boolean = true) extends ASMStatement
  case class WORD_GEN(gen: FunOp, isByte: Boolean = true) extends ASMStatement
  case class TEXT(text: Expr) extends ASMStatement
  case class ASM(opcode: String, var mode: Mode.MODE, operand: Option[Expr]) extends ASMStatement
}

class AsmParser(fileName:String) extends JavaTokenParsers {
  import AsmParser._
  import ucesoft.cbm.cpu.CPU65xx._
  import Mode._
  import Instruction._
  
  private val imp_opcodes: Parser[String] = IMP_OPCODES.drop(1).foldLeft(IMP_OPCODES.head: Parser[String]) { (p, b) => p | b }
  private val other_opcodes: Parser[String] = OPCODES.drop(1).foldLeft(OPCODES.head: Parser[String]) { (p, b) => p | b }

  override val whiteSpace = "[ \t]+".r
  // =================================================================

  private def lineComment: Parser[String] = "//.*".r ^^ { _.substring(2) }
  private def multilineComment: Parser[String] = """/\*[^\*]*\*(\*|[^\*/][^\*]*\*)*/""".r ^^ { c => c.substring(2, c.length - 2) }
  private def comment: Parser[String] = lineComment | multilineComment

  // =========================== EXPR ================================

  private def label: Parser[String] = ident
  private def moduleLabel : Parser[ModuleLabel] = label ~ opt("::" ~> label) ^^ { 
    case l ~ None => ModuleLabel(l,None)
    case m ~ Some(l) => ModuleLabel(l,Some(m))
  }
  private def decNumber: Parser[Int] = "[0-9]{1,5}".r ^^ { i => i.toInt }
  private def hexWord: Parser[Int] = "\\$[0-9A-Fa-f]{1,4}".r ^^ { i => Integer.parseInt(i.substring(1), 16) }
  private def binWord: Parser[Int] = "%[01]{1,16}".r ^^ { i => Integer.parseInt(i.substring(1), 2) }
  private def float: Parser[Double] = floatingPointNumber ^^ { _.toDouble }
  private def number: Parser[Double] = float | (decNumber | hexWord | binWord) ^^ { _.toFloat }
  private def string: Parser[String] = stringLiteral ^^ { s => s.substring(1, s.length - 1) }

  def expr: Parser[Expr] = positioned {
    logical ~ rep("==" ~ logical | "!=" ~ logical) ~ opt("?" ~> (expr ~ (":" ~> expr))) ^^ {
      case l1 ~ l ~ ifclause =>
        val cond = l.foldLeft(l1) { case (acc, op ~ f) => BinOp(op, acc, f) }
        ifclause match {
          case None =>
            cond
          case Some(t ~ f) =>
            FunOp("if",None,List(cond, t, f))
        }
    }
  }
  private def logical: Parser[Expr] =
    logical2 ~ rep("^" ~ logical2 | "&" ~ logical2 | "|" ~ logical2 | "^" ~ logical2) ^^ {
      case s ~ l => l.foldLeft(s) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }
        
  private def logical2: Parser[Expr] =
    not ~ rep("<<" ~ not | ">>" ~ not | ">=" ~ not | "<=" ~ not | ">" ~ not | "<" ~ not) ^^ {
      case s ~ l => l.foldLeft(s) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }

  private def not: Parser[Expr] =
    opt("!") ~ sum ^^ {
      case None ~ s => s
      case Some(n) ~ s => UnaryOp(n, s)
    }

  private def sum: Parser[Expr] =
    term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }

  private def term: Parser[Expr] = factorFieldOrMethod ~ rep("*" ~ factorFieldOrMethod | "/" ~ factorFieldOrMethod | "%" ~ factorFieldOrMethod) ^^ {
    case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
  }
  private def factorFieldOrMethod : Parser[Expr] =
    factor ~ rep("." ~> fieldOrMethod) ^^ { 
      case e~Nil => e
      case e~l => 
        l.foldLeft(e) { (acc,f) =>
          f match {
            case Label(l,None) =>
              FieldOrMethod(acc,l,Nil)
            case FunOp(fn,None,pars) =>
              FieldOrMethod(acc,fn,pars)
            case _ => throw new IllegalArgumentException // TODO
          }
        }
    }  
  private def fieldOrMethod : Parser[Expr] = function | label ^^ { Label(_,None) } 
  private def factor: Parser[Expr] =
    "null" ^^ { n => Null } |
    generate |
    list |
    "<" ~> factor ^^ { e => FunOp("lsb",None, e :: Nil) } |
    ">" ~> factor ^^ { e => FunOp("msb",None, e :: Nil) } |
    function |
    newStruct |
    string ^^ { Str(_) } |
    number ^^ { Value(_) } |
    "(" ~> expr <~ ")" ^^ { e => e } |
    "-" ~> expr ^^ { UnaryOp("-", _) } |
    ("++" | "--") ~ moduleLabel ^^ { case op ~ l => UnaryOp(op, Label(l.label,l.module)) } |
    //structField |
    moduleLabel ~ opt("++" | "--") ^^ {
      case l ~ None => Label(l.label,l.module)
      case l ~ Some(op) => UnaryOp(op, Label(l.label,l.module), false)
    }
  private def list: Parser[Expr] = "[" ~ "[\n\r]*".r ~ "]" ^^ { _ => ListValue(Nil) } |
    ("[" ~> expr <~ "..") ~ expr ~ opt("," ~> expr) <~ "]" ^^ {
      case (from ~ to) ~ step =>
        ListValue(List(from, to, step.getOrElse(Value(1))), true)
    } |
    "[" ~> repsep(expr, ("[\n\r]*".r ~ "," ~ "[\n\r]*".r)) <~ "]" ~ "[\n\r]*".r ^^ { ListValue(_) }
    
  private def function: Parser[FunOp] = moduleLabel ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case ModuleLabel(l,m) ~ args => FunOp(l,m, args) }
  private def newStruct: Parser[NewStruct] = moduleLabel ~ ("{" ~> repsep(expr, ",") <~ "}") ^^ { case ModuleLabel(l,m) ~ vals => NewStruct(l,m,vals.toArray) }
  private def generate: Parser[FunOp] = ("[" ~> label <~ "<-") ~ (expr ~ ("||" ~> expr <~ "]")) ^^ {
    case v ~ (l ~ e) => FunOp("gen",None, List(Str(v), l, e))
  }

  // ================ ASM STATEMENT ==================================
  private def labelOp: Parser[Option[String]] = opt(label <~ ":" <~ "[\n\r]*".r)

  private def org: Parser[ASMStatement] = ((".pc" | "*") ~ "=" | "org") ~> (expr ~ opt(string)) ^^ {
    case addr ~ l => ORG(addr, l)
  }
  private def text: Parser[ASMStatement] = (".text" | ".t") ~> expr ^^ { TEXT(_) }
  private def bytes: Parser[ASMStatement] = (".byte" | ".b") ~> (
    generate ^^ { g => WORD_GEN(g, true) } |
    repsep(expr, ",") ^^ { l => WORD(l, true) })

  private def words: Parser[ASMStatement] = (".word" | ".w") ~> (
    generate ^^ { g => WORD_GEN(g, false) } |
    repsep(expr, ",") ^^ { l => WORD(l, false) })
    
  private def asmMode: Parser[(Mode.MODE, Expr)] =
    "#" ~> expr ^^ { (IMM, _) } |
      "(" ~> expr <~ ")" <~ (",y" | ",Y") ^^ { (IZY, _) } |
      "(" ~> expr <~ ")" ^^ { (IND, _) } |
      "(" ~> expr <~ (",x" | ",X") <~ ")" ^^ { (IZX, _) } |
      expr ~ opt(",x" | ",X" | ",y" | ",Y") ^^ {
        case e ~ None => (UNKNOWN_ABS_OR_ZP, e) // we will decide later if it's ABS or ZP
        case e ~ Some(index) =>
          index.toUpperCase match {
            case ",X" => (UNKNOWN_ABX_OR_ZPX, e)
            case ",Y" => (UNKNOWN_ABY_OR_ZPY, e)
          }
      }

  private def asm: Parser[ASMStatement] = other_opcodes ~ opt(".z" | ".a") ~ opt(asmMode) <~ opt(comment) <~ ("\n" | ";") ^^ {
    case o ~ m ~ None => // m is ignored, IMP forced 
      ASM(o, IMP, None)
    case o ~ m ~ Some(mode) =>
      var mm = m match {
        case None => mode._1
        case Some(hm) =>
          (hm, mode._1) match {
            case (".z", UNKNOWN_ABS_OR_ZP) => ZP
            case (".a", UNKNOWN_ABS_OR_ZP) => ABS
            case (".z", UNKNOWN_ABX_OR_ZPX) => ZPX
            case (".a", UNKNOWN_ABX_OR_ZPX) => ABX
            case (".z", UNKNOWN_ABY_OR_ZPY) => ZPY
            case (".a", UNKNOWN_ABY_OR_ZPY) => ABY
            case (_,m) => m
          }
      }
      mm = mm match {
        case UNKNOWN_ABS_OR_ZP =>
          if (!hasMode(o,ZP)) ABS else mm
        case UNKNOWN_ABX_OR_ZPX =>
          if (!hasMode(o,ZPX)) ABX else mm
        case UNKNOWN_ABY_OR_ZPY =>
          if (!hasMode(o,ZPY)) ABY else mm
        case m => m
      }
      BRANCHES.contains(o.toUpperCase) match {
        case true =>
          ASM(o, REL, Some(mode._2))
        case false =>
          ASM(o, mm, Some(mode._2))
      }
  }
  // ===================== STATEMENT =================================       
  private def enum: Parser[Statement] = ("enum" ~ "{") ~> repsep(label ~ opt("=" ~> number), ",") <~ "}" ^^ {
    case l => ENUM(l map { case e ~ n => (e, n map { _.toInt }) })
  }
  private def struct: Parser[Statement] = opt("private") ~ ("struct" ~> label) ~ ("{" ~> repsep(label, ",") <~ "}") ^^ {
    case pr ~ name ~ fields => STRUCT(name, fields,pr.isDefined)
  }
  private def const: Parser[Statement] = opt("private") ~ ("val" ~> (label ~ ("=" ~> expr))) ^^ { case pr ~ (id ~ e) => CONST(id,e,pr.isDefined) }
  private def variable: Parser[Statement] = opt("private") ~ ("var" ~> (label ~ ("=" ~> expr))) ^^ { case pr ~ (id ~ e) => VAR(id,e,pr.isDefined) }
  private def print: Parser[Statement] = "print" ~> expr ^^ { PRINT }
  private def assignment : Parser[Statement] = moduleLabel ~ rep("." ~> label) ~ ("=" ~> expr) ^^ {
    case l ~ Nil ~ e => EVAL(Some(EvalTarget(l, None)), e)
    case l ~ fs ~ e => EVAL(Some(EvalTarget(l, Some(fs))), e)
  }
  private def eval: Parser[Statement] = "eval" ~> opt(moduleLabel ~ rep("." ~> label) <~ "=") ~ expr ^^ {
    case Some(l ~ Nil) ~ e => EVAL(Some(EvalTarget(l, None)), e)
    case Some(l ~ fs) ~ e => EVAL(Some(EvalTarget(l, Some(fs))), e)
    case None ~ e => EVAL(None, e)
  }
  private def singleBlock: Parser[List[Statement]] = (asmStatement | statement) ^^ { List(_) } 
  private def multipleBlock: Parser[List[Statement]] = "{" ~> statements(false,false,false,false) <~ "}"
  private def block: Parser[List[Statement]] = "[\n\r]*".r ~> (multipleBlock | singleBlock) <~ "[\n\r]*".r
  private def asmBlock: Parser[List[Statement]] = "[\n\r]*".r ~> "{" ~> statements(true,false,false,false) <~ "}" <~ "[\n\r]*".r
  private def funBlock: Parser[List[Statement]] = "[\n\r]*".r ~> "{" ~> statements(false,true,false,false) <~ "}" <~ "[\n\r]*".r
  private def moduleBlock: Parser[List[Statement]] = "[\n\r]*".r ~> "{" ~> statements(false,true,false,true) <~ "}" <~ "[\n\r]*".r
  
  private def ifStmt: Parser[Statement] = (("if" ~ "(") ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
    case cond ~ t ~ None => IF(cond, t, Nil)
    case cond ~ t ~ Some(e) => IF(cond, t, e)
  }
  private def whileStmt: Parser[Statement] = (("while" ~ "(") ~> expr <~ ")") ~ block ^^ { case c ~ b => WHILE(c, b) }
  private def forStmt: Parser[Statement] = ("for" ~ "(") ~> repsep((label <~ "=") ~ expr, ",") ~ (";" ~> expr <~ ";") ~ repsep(expr, ",") ~ (")" ~> block) ^^ {
    case vars ~ cond ~ post ~ body =>
      val vs = vars map { case n ~ e => VAR(n,e,false) }
      FOR(vs, cond, post, body)
  }
  
  private def declareLabel : Parser[Statement] = ("label" ~> label <~ "=") ~ expr ^^ {
    case l~e => DECLARE_LABEL(l,e)
  }
  
  private def break : Parser[Statement] = "break" ^^ { b => BREAK }
  
  private def dup : Parser[Statement] = "dup" ~> expr ~ block ^^ { case e~b => DUP(e,b) }
  
  private def align : Parser[Statement] = ("align" | ".align") ~> expr ^^ { case e => ALIGN(e) }
  
  private def error : Parser[Statement] = "error" ~> expr ^^ { case e => ERROR(e) }
  
  private def macroCall : Parser[Statement] = function ^^ { 
    case FunOp(n,None,pars) => MACRO_CALL(n,pars)
    case FunOp(n,_,pars) => throw new IllegalArgumentException // TODO
  } 
  
  private def macroStmt : Parser[Statement] = positioned {
    ("def" ~ "macro") ~> label ~ ("(" ~> repsep(label,",") <~ ")") ~ asmBlock ^^ {
      case name~pars~body => MACRO(name,pars,body)
    }
  }
  
  private def funStmt : Parser[Statement] = positioned { 
    opt("private") ~ ("def" ~> label ~ ("(" ~> repsep(label,",") <~ ")") ~ funBlock) ^^ {
      case pr~(name~pars~body) => FUNCTION(name,pars,body,pr.isDefined)
    }
  }
  
  private def moduleStmt : Parser[Statement] = positioned {
    "module" ~> label ~ moduleBlock ^^ { case l~b => MODULE(l,b) }
  }
  
  private def include : Parser[Statement] = "include" ~> expr ^? ({ case Str(fn) => INCLUDE(fn) }, _ => "Type mismatch: include statement expects a string as filename")
  
  private def asmStatement: Parser[Statement] = positioned { asm | org | bytes | words | text }
  private def statement: Parser[Statement] = positioned { include | error | align | dup | break | const | variable | print | eval | ifStmt | struct | enum | whileStmt | forStmt | macroCall | declareLabel | assignment}
  
  private def statements(asmOnly:Boolean,asmNotAllowed:Boolean,top:Boolean,module:Boolean): Parser[List[Statement]] = allStatements(asmOnly,asmNotAllowed,top,module) ^^ { _.flatten }

  private def allStatements(asmOnly:Boolean,asmNotAllowed:Boolean,top:Boolean,module:Boolean) : Parser[List[Option[Statement]]] = {
    rep(
      "[\n\r]*".r ~>
        (comment ^^ { _ => None } |
         label <~ ":" ^^ { case l => Some(LABELED(l)) } |
          (
              (
              if (module) (const | variable | struct | funStmt) | ((statement | asmStatement) <~ failure("In a module can be defined functions, vars/vals and structs only"))
              else
              if (top) (moduleStmt | macroStmt | funStmt| asmStatement | statement )
              else
              if (asmOnly) asmStatement|(statement| macroStmt | funStmt)<~failure("Only assembler statement are allowed here")               
              else
              if (asmNotAllowed) statement|asmStatement<~failure("Assembler statement are not allowed here")|macroStmt<~failure("Macros are not allowed here")|funStmt<~failure("Functions are not allowed here")
              else 
              (asmStatement | statement | macroStmt<~failure("Macros can be declared on outermost scope only")|funStmt<~failure("Functions can be declared on outermost scope only"))
              ) ^^ { s => s.fileName = Some(fileName) ; s }
          ) ^^ {
            case s => Some(s)
          }
        ) <~ "[\n\r]*".r)
  }
  
  def topStatements : Parser[List[Statement]] = statements(false,false,true,false)
}