package ucesoft.cbm.trace

import ucesoft.cbm.trace.TraceListener.TraceRegister
import ucesoft.cbm.trace.TracerConditionParser._

object TracerConditionEvaluator {
  def main(args:Array[String]): Unit = {
    val txt = scala.io.StdIn.readLine(">")
    TracerConditionParser.parseCondition(txt) match {
      case Right(expr) =>
        print(expr)
        eval(expr,List(TraceRegister("X","20",20)),Map.empty) match {
          case Right(b) => println(s" => $b")
          case Left(err) => println(s" error $err")
        }
      case Left(err) =>
        println(s"Error $err")
    }
  }

  def eval(expr:Expr,registers:List[TraceRegister],labels:Map[String,Int]): Either[String,Boolean] = {
    val regsMap = registers map { r =>
      val name = r.conditionName.getOrElse(r.name)
      name -> r.intValue
    } toMap

    try {
      eval(expr,regsMap,labels) match {
        case TRUE =>
          Right(true)
        case FALSE =>
          Right(false)
        case e =>
          Left(s"Invalid expression: $e")
      }
    }
    catch {
      case i:IllegalArgumentException =>
        Left(i.getMessage)
    }
  }

  private def eval(expr:Expr,regs:Map[String,Int],labels:Map[String,Int]): Expr = {
    expr match {
      case TRUE|FALSE|Value(_) => expr
      case Register(r) =>
        regs.get(r) match {
          case Some(v) =>
            Value(v)
          case None =>
            throw new IllegalArgumentException(s"Register $r not defined")
        }
      case Label(l) =>
        labels.get(l) match {
          case Some(v) =>
            Value(v)
          case None =>
            throw new IllegalArgumentException(s"Label $l not defined")
        }
      case FunOp(f,args) =>
        val vargs = args.map(eval(_,regs,labels))
        applyFun(f,vargs,regs, labels)
      case UnaryOp("-",op1,_) =>
        eval(op1,regs,labels) match {
          case Value(v) =>
            Value(-v)
          case _ =>
            throw new IllegalArgumentException(s"Invalid unary operator -")
        }
      case UnaryOp("!", op1, _) =>
        eval(op1, regs, labels) match {
          case TRUE =>
            FALSE
          case FALSE =>
            TRUE
          case _ =>
            throw new IllegalArgumentException(s"Invalid unary operator !")
        }
      case BinOp(op,a,b) if op == "&&" || op == "||" =>
        val va = eval(a, regs, labels) match {
          case TRUE => true
          case FALSE => false
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments on operator $op")
        }
        val vb = eval(b, regs, labels) match {
          case TRUE => true
          case FALSE => false
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments on operator $op")
        }
        op match {
          case "&&" => if (va && vb) TRUE else FALSE
          case "||" => if (va || vb) TRUE else FALSE
        }
      case BinOp(op,a,b) =>
        val va = eval(a,regs, labels) match {
          case v@Value(_) => v.value
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments on operator $op")
        }
        val vb = eval(b,regs, labels) match {
          case v@Value(_) => v.value
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments on operator $op")
        }
        op match {
          case "+" => Value(va + vb)
          case "-" => Value(va - vb)
          case "*" => Value(va * vb)
          case "/" => Value(va / vb)
          case "%" => Value(va % vb)
          case "&" => Value(va & vb)
          case "|" => Value(va | vb)
          case "^" => Value(va ^ vb)
          case "<<" => Value(va << vb)
          case ">>" => Value(va >> vb)
          case "==" => if (va == vb) TRUE else FALSE
          case "!=" => if (va != vb) TRUE else FALSE
          case ">" => if (va > vb) TRUE else FALSE
          case "<" => if (va < vb) TRUE else FALSE
          case ">=" => if (va >= vb) TRUE else FALSE
          case "<=" => if (va <= vb) TRUE else FALSE
          case _  =>
            throw new IllegalArgumentException(s"Unknown operator $op")
        }
    }
  }

  private def applyFun(f:String,args:List[Expr],regs:Map[String,Int],labels:Map[String,Int]): Expr = {
    f match {
      case "bit" if args.length == 2 =>
        val va = eval(args.head, regs, labels) match {
          case v@Value(_) => v.value
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments for function $f")
        }
        val vb = eval(args.tail.head, regs, labels) match {
          case v@Value(_) => v.value
          case _ =>
            throw new IllegalArgumentException(s"Bad arguments for function $f")
        }
        if ((va & (1 << vb)) > 0) TRUE else FALSE
      case _ =>
        throw new IllegalArgumentException(s"Unknown function $f/${args.length}")
    }
  }
}
