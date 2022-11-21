package ucesoft.cbm.trace

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

object TracerConditionParser extends JavaTokenParsers {

  sealed trait Expr extends Positional {
    def size: Int = 0
  }

  case object TRUE extends Expr

  case object FALSE extends Expr

  case class Register(reg: String) extends Expr

  case class Value(value: Int) extends Expr

  case class Label(label: String) extends Expr

  case class BinOp(op: String, op1: Expr, op2: Expr) extends Expr {
    override def size: Int = 1 + op1.size + op2.size
  }

  case class UnaryOp(op: String, op1: Expr, isPre: Boolean = true) extends Expr {
    override def size: Int = 1 + op1.size
  }

  case class FunOp(fun: String, args: List[Expr]) extends Expr

  private def decNumber: Parser[Int] = "[0-9]+".r ^^ { i => i.toInt }
  private def hexWord: Parser[Int] = "0x[0-9A-Fa-f]+".r ^^ { i => Integer.parseInt(i.substring(2), 16) }
  private def binWord: Parser[Int] = "[01]+[bB]".r ^^ { i => Integer.parseInt(i.dropRight(1), 2) }
  private def number: Parser[Int] = binWord | hexWord | decNumber

  private def label: Parser[String] = ident

  private def register: Parser[Register] = ("." ~> ident) ^^ { s => Register(s.toUpperCase()) }

  def parseCondition(cond:String): Either[String,Expr] = {
    val in = new java.io.StringReader(cond)
    parseAll(expr, in) match {
      case Success(parsed: Expr, _) =>
        Right(reduce(parsed))
      case Failure(f, i) =>
        Left(s"Syntax error on $f [column ${i.pos.column}]")
      case Error(e, i) =>
        Left(s"Syntax error on $e [column ${i.pos.column}]")
    }
  }

  private def expr: Parser[Expr] = positioned {
    not ~ rep("&&" ~ not | "||" ~ not) ^^ {
      case s ~ l =>
        l.foldLeft(s) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }
  }

  private def not: Parser[Expr] = {
    opt("!") ~ logical ^^ {
      case None ~ l => l
      case Some(_) ~ l => UnaryOp("!",l)
    }
  }

  private def logical: Parser[Expr] =
    sum ~ rep1("==" ~ sum | "!=" ~ sum | ">" ~ sum | "<" ~ sum | ">=" ~ sum | "<=" ~ sum) ^^ {
      case l1 ~ l =>
        l.foldLeft(l1) { case (acc, op ~ f) => BinOp(op, acc, f) }
    } |
    function |
    "(" ~> expr <~ ")"

  private def sum: Parser[Expr] =
    term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }

  private def term: Parser[Expr] = bitwise ~ rep("*" ~ bitwise | "/" ~ bitwise | "%" ~ bitwise) ^^ {
    case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
  }

  private def bitwise: Parser[Expr] = factorShift ~ rep("&" ~ factorShift | "|" ~ factorShift | "^" ~ factorShift) ^^ {
    case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
  }

  private def factorShift: Parser[Expr] =
    neg ~ rep("<<" ~ neg | ">>" ~ neg) ^^ {
      case f ~ l => l.foldLeft(f) { case (acc, op ~ f) => BinOp(op, acc, f) }
    }

  private def neg: Parser[Expr] =
    opt("-") ~ factor ^^ {
      case None ~ s => s
      case Some(n) ~ s => UnaryOp(n, s)
    }

  private def factor: Parser[Expr] = {
    number ^^ { Value(_) } |
    function |
    label ^^ { Label(_) } |
    register |
    "(" ~> sum <~ ")"
  }

  private def function: Parser[Expr] = label ~ ("(" ~> repsep(sum, ",") <~ ")") ^^ {
    case l ~ args => FunOp(l,args)
  }

  def reduce(e: Expr): Expr = {
    if (e.size == 1) prune(e)
    else {
      val r = prune(e)
      if (r.size < e.size) reduce(r) else r
    }
  }

  def prune(e: Expr): Expr = {
    e match {
      case BinOp(op,left,right) =>
        val pe = BinOp(op, prune(left), prune(right))
        pe match {
          // + -
          case BinOp("+", Value(a), Value(b)) => Value(a + b)
          case BinOp("-", Value(a), Value(b)) => Value(a - b)
          // * / %
          case BinOp("*", Value(a), Value(b)) => Value(a * b)
          case BinOp("/", Value(a), Value(b)) => Value(a / b)
          case BinOp("%", Value(a), Value(b)) => Value(a % b)
          // << >>
          case BinOp("<<", Value(a), Value(b)) => Value(a << b)
          case BinOp(">>", Value(a), Value(b)) => Value(a >> b)
          // & | ^
          case BinOp("&", Value(a), Value(b)) => Value(a & b)
          case BinOp("|", Value(a), Value(b)) => Value(a | b)
          case BinOp("^", Value(a), Value(b)) => Value(a ^ b)
          case BinOp(">",Value(a),Value(b)) => if (a > b) TRUE else FALSE
          case BinOp(">=",Value(a),Value(b)) => if (a >= b) TRUE else FALSE
          case BinOp("<", Value(a), Value(b)) => if (a < b) TRUE else FALSE
          case BinOp("<=", Value(a), Value(b)) => if (a <= b) TRUE else FALSE
          case BinOp("==", Value(a), Value(b)) => if (a == b) TRUE else FALSE
          case BinOp("!=", Value(a), Value(b)) => if (a != b) TRUE else FALSE
          case BinOp("&&", e, TRUE) => e
          case BinOp("&&", _, FALSE) => FALSE
          case BinOp("&&", FALSE, _) => FALSE
          case BinOp("||", _, TRUE) => TRUE
          case BinOp("||", TRUE, _) => TRUE
          case BinOp("||", FALSE, e) => e
          case BinOp("||", e, FALSE) => e
          case _ => pe
        }
      case UnaryOp("-", Value(a), true) => Value(-a)
      case UnaryOp("!",TRUE,true) => FALSE
      case UnaryOp("!",FALSE,true) => TRUE
      case FunOp(fun, args) => FunOp(fun,args.map(prune))
      case UnaryOp(op,e,true) => UnaryOp(op,prune(e))
      case _ => e
    }
  }

  def parse(input:String): Either[String,Expr] = {
    val in = new java.io.StringReader(input)
    parseAll(expr, in) match {
      case Success(parsed: Expr, _) =>
        Right(parsed)
      case Failure(f, i) =>
        Left(s"Syntax error on $f at ${i.pos}")
      case Error(e, i) =>
        Left(s"Syntax error on $e at ${i.pos}")
    }
  }
}
