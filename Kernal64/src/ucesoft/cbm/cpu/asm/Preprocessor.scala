package ucesoft.cbm.cpu.asm

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex.Match

object Preprocessor extends App {
  case class Result(text:String,symbols:Map[String,String])

  while (true) {
    var keepReading = true
    val sb = new StringBuilder
    while (keepReading) {
      val line = io.StdIn.readLine(">")
      if (line == "\\") keepReading = false
      else sb.append(line + "\n")
    }

    println(s"Evaluating:\n$sb")
    val prep = new Preprocessor()
    prep.analyze(sb.toString(),Map("X" -> "1","Y" -> "ciao")) match {
      case Right(Result(text,map)) =>
        println(s"OK:\n$map\n$text")
      case Left(err) =>
        println(s"ERROR: $err")
    }
  }
}

class Preprocessor {
  import Preprocessor._
  private sealed trait PrepStat {
    val stmt : Match
  }
  private case class Define(stmt : Match) extends PrepStat
  private case class Undefine(stmt : Match) extends PrepStat
  private case class If(stmt : Match) extends PrepStat {
    private val clauses = Array(new ListBuffer[PrepStat],new ListBuffer[PrepStat])
    private var state = 0

    def incState : If = {
      state += 1
      this
    }
    def add(stmt:PrepStat) : Unit = clauses(state) += stmt
    def thenClauses : List[PrepStat] = clauses(0).toList
    def elseClauses : List[PrepStat] = clauses(1).toList
    def isClosed : Boolean = state == 1 || state == 2
    def isElseState : Boolean = state == 1

    override def toString = s"If($stmt,then=${thenClauses},else=${elseClauses})"
  }
  private case class Else(stmt : Match) extends PrepStat
  private case class Endif(stmt : Match) extends PrepStat
  private case class Text(text:String) extends PrepStat { val stmt = null }

  private case class PrepError(msg:String,stmt:PrepStat)

  private val REG = """#define[ \t]+([A-Z|a-z|0-9|_]+)([ \t]+[A-Z|a-z|0-9|_]+)?|#undef[ \t]+([A-Z|a-z|0-9|_]+)|#if[ \t]+(!?[A-Z|a-z|0-9|_]+)([ \t]+(=|>|<|>=|<=)[ \t]+[a-z|A-Z|0-9|_]+)?|#else|#endif""".r
  private val DEFINE_IDENT_POS = 1
  private val DEFINE_VALUE_POS = 2
  private val UNDEF_IDENT_POS = 3
  private val IF_IDENT_POS = 4
  private val IF_COND_POS = 5

  def analyze(source:String,symbols:Map[String,String]) : Either[String,Result] = {
    prep(source) match {
      case Left(err) =>
        Left(err)
      case Right(pp) =>
        val newSymbols = new collection.mutable.HashMap[String,String]
        newSymbols.addAll(symbols)
        eval(pp,newSymbols) match {
          case Left(err) =>
            Left(err)
          case Right(r) =>
            Right(Result(r,newSymbols.toMap))
        }
    }
  }

  private def eval(pp:List[PrepStat],symbols:collection.mutable.HashMap[String,String]) : Either[String,String] = {
    def parse(s:String) : AnyRef = {
      if (s == null) None
      else
      try {
        Integer.valueOf(s)
      }
      catch {
        case nfe:NumberFormatException =>
          s
      }
    }
    val sb = new StringBuilder
    for(p <- pp) p match {
      case Text(txt) =>
        if (txt.length > 0) sb.append(txt)
      case Define(m) =>
        symbols += m.group(DEFINE_IDENT_POS) -> m.group(DEFINE_VALUE_POS).trim
      case Undefine(m) =>
        symbols -= m.group(UNDEF_IDENT_POS)
      case i@If(m) =>
        var symbol = m.group(IF_IDENT_POS)
        val negate = symbol.charAt(0) == '!'
        val cond = m.group(IF_COND_POS)
        var evaluatedCond = false
        if (negate) symbol = symbol.substring(1)
        // evaluate
        if (cond == null) evaluatedCond = if (negate) !symbols.contains(symbol) else symbols.contains(symbol)
        else {
          val Array(op,opValue) = cond.trim.split("[ \t]")
          (parse(symbols.getOrElse(symbol,null)),parse(symbols.getOrElse(opValue,opValue))) match {
            case (null,_) =>
            case (_,null) =>
            case (s1:String,s2:String) =>
              evaluatedCond = op match {
                case "=" => s1 == s2
                case ">" => s1 > s2
                case "<" => s1 < s2
                case ">=" => s1 >= s2
                case "<=" => s1 <= s2
              }
            case (i1:Integer,i2:Integer) =>
              evaluatedCond = op match {
                case "=" => i1 == i2
                case ">" => i1 > i2
                case "<" => i1 < i2
                case ">=" => i1 >= i2
                case "<=" => i1 <= i2
              }
            case _ =>
          }
        }

        val text = if (evaluatedCond) eval(i.thenClauses,symbols) else eval(i.elseClauses,symbols)
        text match {
          case l@Left(_) =>
            return l
          case Right(t) if t.length > 0 =>
            sb.append(t)
          case _ =>
        }
    }
    Right(sb.toString)
  }

  private def prep(source:String) : Either[String,List[PrepStat]] = {
    val stmts = REG.findAllMatchIn(source).toList map {
      case m if m.matched.startsWith("#define") => Define(m)
      case m if m.matched.startsWith("#undef") => Undefine(m)
      case m if m.matched.startsWith("#if") => If(m)
      case m if m.matched.startsWith("#else") => Else(m)
      case m if m.matched.startsWith("#endif") => Endif(m)
    }

    process(addText(stmts,source)) match {
      case Right(ss) =>
        Right(ss)
      case Left(err) =>
        Left(err.msg)
    }
  }

  private def addText(stmts:List[PrepStat],source:String) : List[PrepStat] = {
    val buffer = new ListBuffer[PrepStat]
    var lastPos = 0
    for(s <- stmts) {
      buffer += Text(source.substring(lastPos,s.stmt.start))
      buffer += s
      lastPos = s.stmt.`end`
    }
    buffer += Text(source.substring(lastPos))
    buffer.toList
  }

  private def process(stmts:List[PrepStat]) : Either[PrepError,List[PrepStat]] = {
    val buffer = new ListBuffer[PrepStat]
    var stack : List[PrepStat] = Nil

    def push(e:PrepStat) : Unit = stack ::= e
    def pop : Option[PrepStat] = stack match {
      case e :: tail =>
        stack = tail
        Some(e)
      case Nil =>
        None
    }
    def peek : Option[PrepStat] = stack.headOption

    for(s <- stmts) {
      s match {
        case Define(_)|Undefine(_) =>
          peek match {
            case Some(i@If(_)) =>
              i.add(s)
            case None =>
              buffer += s
          }
        case If(_) =>
          push(s)
        case Else(_) =>
          peek match {
            case Some(i@If(_)) =>
              if (!i.incState.isElseState) return Left(PrepError("Bad #else position",s))
            case _ =>
              return Left(PrepError("Bad #else position",s))
          }
        case Endif(_) =>
          pop match {
            case Some(i@If(_)) =>
              if (!i.incState.isClosed) return Left(PrepError("Bad #endif position",s))
              peek match {
                case Some(in@If(_)) =>
                  in.add(i)
                case None =>
                  buffer += i
              }
            case _ =>
              return Left(PrepError("Bad #endif position",s))
          }
        case Text(_) =>
          peek match {
            case Some(i@If(_)) =>
              i.add(s)
            case None =>
              buffer += s
          }
      }
    }
    if (!stack.isEmpty) Left(PrepError(s"#If without #endif",stack.head))
    else Right(buffer.toList)
  }
}
