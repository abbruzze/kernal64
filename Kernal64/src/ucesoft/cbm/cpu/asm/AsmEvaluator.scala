package ucesoft.cbm.cpu.asm

import scala.collection.mutable.Buffer

object AsmEvaluator {
  import AsmParser._
  import Formats._

  import Ordering.Double.TotalOrdering

  // ====================== EVALUATION ===============================
  sealed trait RuntimeValue {

    protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      val matched : List[RuntimeValue] => RuntimeValue = f match {
        case ("isnull",0) => _ => NumberVal(if (this != NullVal) 1 else 0)
        case ("toString",0) => _ => StringVal(toString)
        case _ => null
      }

      Option(matched)
    }
    
    def handleFieldOrMethod(name:String,pars:List[RuntimeValue]) : Option[RuntimeValue] = fieldsOrMethods((name,pars.size)) map { _(pars) }
  }
  case object NullVal extends RuntimeValue {
    override def toString = "null"
  }

  case class NumberVal(n: Double) extends RuntimeValue {
    override protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      super.fieldsOrMethods(f) match {
        case None =>
          val matched : List[RuntimeValue] => RuntimeValue = f match {
            case ("toHexString",0) => _ => StringVal(n.toInt.toHexString)
            case ("toBinaryString",0) => _ => StringVal(n.toInt.toBinaryString)
            case _ => null
          }
          Option(matched)
        case s@Some(_) => s
      }
    }
    override def toString: String = if (n.isValidInt) n.toInt.toString else n.toString
  }
  case class StringVal(s: String) extends RuntimeValue {
    override def toString: String = s
    override protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      super.fieldsOrMethods(f) match {
        case None =>
          val matched: List[RuntimeValue] => RuntimeValue = f match {
            case ("length",0) => _ => NumberVal(s.length)
            case ("toInt",0) => _ => NumberVal(s.toInt)
            case ("toHexInt",0) => _ => NumberVal(Integer.parseInt(s,16))
            case ("at",1) => l => at(l.head)
            case ("toChar",0) => _ => NumberVal(s.charAt(0))
            case ("substring",2) => l => StringVal(s.substring(asInt(l,0),asInt(l,1)))
            case ("size",0) => _ => NumberVal(s.length)
            case _ => null
          }
          Option(matched)
        case s@Some(_) => s
      }
    }
    private def at(i:RuntimeValue) : RuntimeValue = {
      i match {
        case NumberVal(index) => StringVal(s.charAt(index.toInt).toString)
        case _ => throw new EvaluationError(s"Type mismatch on method string::at: expected int")
      }
    }
    private def asInt(l:List[RuntimeValue],pos:Int) : Int = {
      if (pos >= l.size) throw new EvaluationError(s"Invalid index on $pos on string $s")
      l(pos) match {
        case NumberVal(n) => n.toInt
        case _ => throw new EvaluationError(s"Invalid index on $pos on string $s. Expected an int.")
      }
    }
  }
  case class ListVal(list:Buffer[RuntimeValue]) extends RuntimeValue {
    override def toString: String = list.mkString("[", ",", "]")
    override protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      super.fieldsOrMethods(f) match {
        case None =>
          val matched: List[RuntimeValue] => RuntimeValue = f match {
            case ("get",1) => l => get(l.head)
            case ("set",2) => l => set(l(0),l(1))
            case ("head",0) => _ => list.head
            case ("tail",0) => l => ListVal(list.tail)
            case ("isEmpty",0) => _ => NumberVal(if (list.isEmpty) 1 else 0)
            case ("add",1) => l => ListVal(list.addOne(l.head))
            case ("size",0) => _ => NumberVal(list.length)
            case ("contains",1) => l => NumberVal(if (list.contains(l.head)) 1 else 0)
            // sid
            case ("asSID",0) => _ => analyzeSID(this)
            case _ => null
          }
          Option(matched)
        case s@Some(_) => s
      }
    }
    private def get(i:RuntimeValue) : RuntimeValue = {
      i match {
        case NumberVal(index) =>
          if (index < list.size) list(index.toInt)
          else throw new EvaluationError(s"Index out of bound on list::get: $index (${list.size})")
        case _ => throw new EvaluationError(s"Type mismatch on method list::get: expected int")
      }
    }
    private def set(i:RuntimeValue,v:RuntimeValue) : RuntimeValue = {
      i match {
        case NumberVal(index) =>
          if (index < list.size) {
            list(index.toInt) = v
            NullVal
          }
          else throw new EvaluationError(s"Index out of bound on list::set: $index (${list.size})")
        case _ => throw new EvaluationError(s"Type mismatch on method list::set: expected int")
      }
    }
  }
  case class MapVal(list:List[RuntimeValue]) extends RuntimeValue {
    private val map : collection.mutable.HashMap[RuntimeValue,RuntimeValue] = new collection.mutable.HashMap[RuntimeValue,RuntimeValue]

    map.addAll(list map { case ListVal(l) => (l.head,l.tail.head) })

    override def toString: String = map.map { kv => s"[${kv._1},${kv._2}]" } mkString("#[", ",", "]")
    override protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      super.fieldsOrMethods(f) match {
        case None =>
          val matched: List[RuntimeValue] => RuntimeValue = f match {
            case ("get",1) => l => get(l.head)
            case ("put",2) => l => put(l(0),l(1))
            case ("keys",0) => _ => keys
            case ("values",0) => _ => values
            case ("isEmpty",0) => _ => NumberVal(if (list.isEmpty) 1 else 0)
            case ("size",0) => _ => NumberVal(list.length)
            case ("contains",1) => l => NumberVal(if (map.contains(l.head)) 1 else 0)
            case (field,0) => _ => map.getOrElse(StringVal(field),NullVal)
            case _ => null
          }
          Option(matched)
        case s@Some(_) => s
      }
    }
    private def get(i:RuntimeValue) : RuntimeValue = map.getOrElse(i,NullVal)
    private def put(k:RuntimeValue,v:RuntimeValue) : RuntimeValue = {
      map += k -> v
      this
    }
    private def keys : RuntimeValue = ListVal(map.keys.toBuffer)
    private def values : RuntimeValue = ListVal(map.values.toBuffer)
  }
  case class StructVal(structName: String,module:Option[String],fieldNames:List[String],map:collection.mutable.Map[String,RuntimeValue]) extends RuntimeValue {
    override def toString: String = structName + fieldNames.flatMap(map.get).mkString("{",",","}")

    override protected def fieldsOrMethods(f:(String,Int)) : Option[List[RuntimeValue] => RuntimeValue] = {
      super.fieldsOrMethods(f) match {
        case None =>
          val matched: List[RuntimeValue] => RuntimeValue = f match {
            case ("get",1) => l => get(l.head)
            case ("length",0) => _ => NumberVal(fieldNames.size)
            case ("set",2) => l => set(l(0),l(1))
            case ("fields",0) => _ => ListVal(fieldNames map StringVal toBuffer)
            case ("has",1) => l => NumberVal(if (fieldNames.contains(l.head.toString)) 1 else 0)
            case (field,0) if fieldNames.contains(field) => _ => map(field)
            case _ => null
          }
          Option(matched)
        case s@Some(_) => s
      }
    }
    private def get(i:RuntimeValue) : RuntimeValue = {
      i match {
        case NumberVal(index) =>
          if (index < fieldNames.size) map.getOrElse(fieldNames(index.toInt),NullVal)
          else throw new EvaluationError(s"Index out of bound on struct::get: $index (${fieldNames.size})")
        case _ => throw new EvaluationError(s"Type mismatch on method struct::get: expected int")
      }
    }
    private def set(i:RuntimeValue,v:RuntimeValue) : RuntimeValue = {
      i match {
        case NumberVal(index) =>
          if (index < fieldNames.size) {
            map += fieldNames(index.toInt) -> v
            NullVal
          }
          else throw new EvaluationError(s"Index out of bound on struct::set: $index (${fieldNames.size})")
        case _ => throw new EvaluationError(s"Type mismatch on method struct::set: expected int")
      }
    }
  }
  case class WillBeVal(e: Expr, label: String) extends RuntimeValue

  trait ModuleAware {
    val name : String
    val module : String
    val isPrivate : Boolean
    val ctx : EvaluationContext
  }
  case class Variable(value:RuntimeValue,isConstant:Boolean,isPrivate:Boolean,module:String,ctx:EvaluationContext,name:String) extends ModuleAware
  case class Struct(name:String,module:String,ctx:EvaluationContext,isPrivate:Boolean,fields:List[String]) extends ModuleAware
  
  object VariableUpdate extends Enumeration {
    val OK: VariableUpdate.Value = Value
    val CANNOT_MODIFY_CONSTANT: VariableUpdate.Value = Value
    val VARIABLE_NOT_EXISTS: VariableUpdate.Value = Value
  }

  class EvaluationException(val msg: String) extends Exception(msg)
  class EvaluationError(msg: String) extends EvaluationException(msg)
  private class LabelNotAvailableException(val label: String) extends EvaluationException(label)

  trait EvaluationContext {
    def console: String => Unit
    
    def newStack(name:String,isLocal:Boolean,module:Option[String],applyCounter:Boolean) : EvaluationContext
    def pop : EvaluationContext

    def findLabel(label:String) : Option[Int]
    def defineLabel(label:String,address:Int) : Boolean
    
    def findVar(varName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Variable]
    def defineVar(varName:String,module:Option[String],value:RuntimeValue,isConstant:Boolean,isPrivate:Boolean) : Boolean
    def updateVar(varName:String,module:Option[String],value:RuntimeValue) : VariableUpdate.Value
    
    def findStruct(structName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Struct]
    def defineStruct(structName:String,module:Option[String],fields:List[String],isPrivate:Boolean) : Boolean
    
    def evalUserFunction(name:String,module:Option[String],pars:List[RuntimeValue]) : Option[RuntimeValue]
  }

  object Evaluator {
    var importDir = "."
    
    def evalExpr(e: Expr)(implicit ctx: EvaluationContext) : RuntimeValue = {
      try {
        eval(e)
      } catch {
        case lna: LabelNotAvailableException =>
          WillBeVal(e,lna.label)
      }
    }
    private def eval(e: Expr)(implicit ctx: EvaluationContext): RuntimeValue = {
      import ctx._
      e match {
        case Null => NullVal
        case Value(v) => NumberVal(v)
        case Str(s) => StringVal(s)
        case MapValue(keyVal) =>
          for(kv <- keyVal) {
            if (kv.elements.size != 2) throw new EvaluationError("Each map's element must be a list of size 2")
          }
          val elements = keyVal map eval
          MapVal(elements)
        case ListValue(l, isRange) if !isRange => ListVal((l map eval).toBuffer)
        case ListValue(l, _) =>
          val min = eval(l(0))
          val max = eval(l(1))
          val step = eval(l(2))
          (min, max, step) match {
            case (NumberVal(from), NumberVal(to), NumberVal(step)) =>
              val range = for (i <- from.toInt to to.toInt by step.toInt) yield NumberVal(i)
              ListVal(range.toBuffer)
            case _ => throw new EvaluationError("Range mismatch: range expressions must be numbers")
          }
        case NewStruct(name,module,values) =>
          findStruct(name,module) match {
            case Some(Struct(_,_,_,_,fields)) =>
              if (values.length <= fields.size) {
                val map = new collection.mutable.HashMap[String,RuntimeValue]
                for(fv <- fields.zip(values)) {
                  eval(fv._2) match {
                    case WillBeVal(_,e) => throw new LabelNotAvailableException(e)
                    case ev => map += fv._1 -> ev
                  }                                    
                }
                StructVal(name,module,fields, map)
              }
              else throw new EvaluationError(s"Too many fields in creation of struct $name")              
            case None => throw new EvaluationError(s"Struct $name not found")
          }
        case FieldOrMethod(e,fn,ps) =>
          eval(e) match {
            case WillBeVal(_,e) => throw new LabelNotAvailableException(e)
            case ev =>
              val pars = ps map eval
              pars find { _.isInstanceOf[WillBeVal]} match {
                case None =>
                  ev.handleFieldOrMethod(fn, pars) match {
                    case Some(v) => v
                    case None =>
                      if (pars.size == 0) {
                        ctx.evalUserFunction(fn,None,List(ev)) match {
                          case Some(rv) => rv
                          case None =>
                            findFunction(fn,1) match {
                              case Some(f) =>
                                f(List(ev))
                              case None =>
                                throw new EvaluationError(s"Cannot apply method or field $fn")
                            }                            
                        }
                      }
                      else throw new EvaluationError(s"Cannot apply method or field $fn")
                  }
                case Some(WillBeVal(_,e)) => throw new LabelNotAvailableException(e)
              }              
          }
        case UnaryOp(op, e, true) =>
          if (op == "++" || op == "--") handlePrePostIncDec(e, op,true)
          else
            eval(e) match {
              case NumberVal(n) if op == "-" => NumberVal(-n)
              case NumberVal(n) if op == "!" => if (n == 0) NumberVal(1) else NumberVal(0)
              case nv @ NumberVal(_) if op == "+" => nv
              case _ => throw new EvaluationError(s"Unary operator $op must be used on numbers only")
            }
        case UnaryOp(op, e, false) =>
          handlePrePostIncDec(e, op,false)
        case Label(l,m) =>
          findVar(l,m) match {
            case Some(v) => v.value
            case None if !m.isDefined =>
              findLabel(l) match {
                case Some(addr) => NumberVal(addr)
                case None => throw new LabelNotAvailableException(if (!m.isDefined) l else s"${m.get}::$l" )
              }           
            case _ => throw new LabelNotAvailableException(if (!m.isDefined) l else s"${m.get}::$l" )
          }
        case BinOp(op, a, b) =>
          val av = eval(a)
          val bv = eval(b)
          (av,bv) match {
            case (WillBeVal(_,e),_) => throw new LabelNotAvailableException(e)
            case (_,WillBeVal(_,e)) => throw new LabelNotAvailableException(e)
            case (av,bv) =>
              try {
                handleBinOp(op, av, bv)
              } catch {
                case _: EvaluationError =>
                  throw new EvaluationError(s"Type mismatch: cannot apply operator $op on $av and $bv")
              }   
          }          
        case FunOp(name,module,args) =>
          if (name == "gen") genFun(args)
          else {
            val ea = args map eval
            ea find { _.isInstanceOf[WillBeVal]} match {
              case None =>
                findFunction(name,args.size) match {
                  case Some(f) =>
                    try f(ea)
                    catch {
                      case e: EvaluationError =>
                        throw new EvaluationError(s"Type mismatch: cannot apply function $name/${args.size}: " + e.msg)
                    }
                  case None =>
                    evalUserFunction(name,module,ea) match {
                      case Some(rv) => rv
                      case None => throw new EvaluationError(s"Unknown function ${if (module.isDefined) module.get + "::" else ""}$name/${args.size}")
                    }                  
                }              
              case Some(WillBeVal(_,e)) => throw new LabelNotAvailableException(e)
            }
          }
      }
    }
    
    private def findFunction(name:String,args:Int) : Option[List[RuntimeValue] => RuntimeValue] = {
      functions get (name,-1) match {
        case s@Some(_) => s
        case None =>
          functions get (name,args)
      }
    }
    
    private def genFun(args:List[Expr])(implicit ctx: EvaluationContext) : RuntimeValue = {
      args match {
        case List(Str(x),l,e) =>    
          eval(l) match {
            case w@WillBeVal(_,_) => w
            case ListVal(l) =>
              val result = for(v <- l) yield {
                ctx.newStack("gen",false,None,false)
                try {
                  ctx.defineVar(x,None,v,true,false)
                  eval(e)
                }
                finally ctx.pop                
              }
              ListVal(result)
            case _ => throw new EvaluationException(s"Type mismatch on list generator")
          }
      }
    }
    
    private val operators: Map[String, (RuntimeValue, RuntimeValue) => RuntimeValue] = Map(
      "%" -> mod,
      "+" -> plus,
      "-" -> minus,
      "*" -> times,
      "/" -> div,
      "==" -> eq,
      "!=" -> noteq,
      ">" -> gte(true),
      ">=" -> gte(false),
      "<" -> lte(true),
      "<=" -> lte(false),
      "&" -> logical(_ & _),
      "|" -> logical(_ | _),
      "^" -> logical(_ ^ _),
      "<<" -> shift(true),
      ">>" -> shift(false))

    private def handleBinOp(op: String, a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      operators get op match {
        case Some(f) => f(a, b)
        case None => throw new EvaluationException(s"Unknown operator $op")
      }
    }

    private val rnd = new util.Random(System.currentTimeMillis)
    private val functions: Map[(String, Int), List[RuntimeValue] => RuntimeValue] = Map(
      ("lsb",1) -> lohi(true),
      ("msb",1) -> lohi(false),
      ("if",3) -> ifFun,
      ("time",0) -> (_ => NumberVal(System.currentTimeMillis)),
      ("rnd",0) -> (_ => NumberVal(rnd.nextDouble)),
      ("rnd",1) -> (args => NumberVal(rnd.nextInt(args2Numbers(args)(0).n.toInt))),
      ("cons",1) -> (args => ListVal(Array(args.head).toBuffer)),
      ("cons",2) -> (args => {
        args(1) match {
          case l@ListVal(tail) =>
            ListVal((args.head :: tail.toList).toBuffer)
          case _ => throw new EvaluationException(s"Type mismatch: cons/2 function accepts a head and a tail elements") 
        }
      }),
      // math
      ("rad",1) -> (args => NumberVal(args2Numbers(args)(0).n * 2 * math.Pi / 360)),
      ("cos",1) -> (args => NumberVal(math.cos(args2Numbers(args)(0).n))),
      ("sin",1) -> (args => NumberVal(math.sin(args2Numbers(args)(0).n))),
      ("floor",1) -> (args => NumberVal(math.floor(args2Numbers(args)(0).n))),
      ("ceil",1) -> (args => NumberVal(math.ceil(args2Numbers(args)(0).n))),
      ("max",-1) -> (args => NumberVal(args2Numbers(args) map { _.n } max)),
      ("min",-1) -> (args => NumberVal(args2Numbers(args) map { _.n } min)),
      ("round",1) -> (args => NumberVal(math.round(args2Numbers(args)(0).n).toDouble)),
      ("pow",2) -> (args => NumberVal(math.pow(args2Numbers(args)(0).n,args2Numbers(args)(1).n))),
      ("sqrt",1) -> (args => NumberVal(math.sqrt(args2Numbers(args)(0).n))),
      //
      ("import",2) -> (args => importFile(args)),
      ("import",3) -> (args => importFile(args)),
      ("import",4) -> (args => importFile(args))
    )
    // ==================== FUNCTIONS ==================================
    private def importFile(args:List[RuntimeValue]) : ListVal = {
      import java.io._      
      args.head match {
        case StringVal(fileName) =>
          val sourceFile = new File(new File(importDir),fileName)
          if (!sourceFile.exists)
            throw new EvaluationException(s"Import file $fileName not found in directory $importDir")
          
          val from = if (args.size == 2) NumberVal(0) else args(2)
          val size = if (args.size == 3 || args.size == 2) NumberVal(sourceFile.length) else args(3)
          
          from match {
            case NumberVal(start) =>
              size match {
                case NumberVal(length) =>
                  val buffer = Array.ofDim[Byte](length.toInt)
                  val in = new FileInputStream(sourceFile)
                  try {                        
                    in.skip(start.toInt)
                    in.read(buffer)                        
                  }
                  catch {
                    case _:IOException =>
                      throw new EvaluationException(s"Error while loading file $fileName of import statement")
                  }
                  finally in.close()
                  ListVal(buffer map { b => NumberVal(b.toInt & 0xFF) } toBuffer)
                case _ =>
                  throw new EvaluationException(s"Cannot determine the size of import statement")
              }
            case _ => 
              throw new EvaluationException(s"Cannot determine the start address of import statement")
          }
        case _ => 
          throw new EvaluationException(s"Cannot determine the filename of import statement")
      } 
    }
    // maths
    private def args2Numbers(args: List[RuntimeValue]) : Array[NumberVal] = {
      args map {
        case n@NumberVal(_) => n
        case _ => throw new EvaluationError("")
      } toArray
    }
    private def ifFun(args: List[RuntimeValue]) = {
      (args(0),args(1),args(2)) match {
        case (NumberVal(cond),thenC,elseC) =>
          if (cond == 0) elseC else thenC
        case _ => throw new EvaluationError("")
      }
    }
    // general purpose
    private def lohi(lo:Boolean)(args: List[RuntimeValue]) = args(0) match {
      case NumberVal(n) => if (lo) NumberVal(n.toInt & 0xFF) else NumberVal((n.toInt >> 8) & 0xFF)
      case _ => throw new EvaluationError("")
    }
    // ==================== OPERATORS ==================================
    private def mod(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(x.toInt % y.toInt)
        case (a, b) => throw new EvaluationError("")
      }
    }
    private def shift(left: Boolean)(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(if (left) x.toInt << y.toInt else x.toInt >> y.toInt)
        case _ => throw new EvaluationError("")
      }
    }
    private def logical(op: (Int, Int) => Int)(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(op(x.toInt, y.toInt))
        case _ => throw new EvaluationError("")
      }
    }
    private def noteq(a: RuntimeValue, b: RuntimeValue): RuntimeValue = if (eq(a,b) == NumberVal(0)) NumberVal(1) else NumberVal(0)
    private def eq(a: RuntimeValue, b: RuntimeValue): RuntimeValue = if (a == b) NumberVal(1) else NumberVal(0)
    private def gte(gt: Boolean)(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) =>
          if (gt) NumberVal(if (x > y) 1 else 0)
          else NumberVal(if (x >= y) 1 else 0)
        case (StringVal(x), StringVal(y)) =>
          if (gt) NumberVal(if (x > y) 1 else 0)
          else NumberVal(if (x >= y) 1 else 0)
        case _ => throw new EvaluationError("")
      }
    }
    private def lte(gt: Boolean)(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) =>
          if (gt) NumberVal(if (x < y) 1 else 0)
          else NumberVal(if (x <= y) 1 else 0)
        case (StringVal(x), StringVal(y)) =>
          if (gt) NumberVal(if (x < y) 1 else 0)
          else NumberVal(if (x <= y) 1 else 0)
        case _ => throw new EvaluationError("")
      }
    }
    private def plus(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(x + y) // int int
        case (NumberVal(x), StringVal(y)) => StringVal(x + y) // int string
        case (StringVal(x), NumberVal(y)) => StringVal(x + y) // string int
        case (StringVal(x), StringVal(y)) => StringVal(x + y) // string string
        case (ListVal(l), a) => ListVal(l ++ Buffer(a)) // list any
        case (a, ListVal(l)) => ListVal(l ++ Buffer(a)) // any list
        case _ => throw new EvaluationError("")
      }
    }
    private def minus(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(x - y) // int int        
        case (ListVal(l), a) => ListVal(l filterNot { _ == a }) // list any
        case _ => throw new EvaluationError("")
      }
    }
    private def times(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(x * y) // int int  
        case (StringVal(y),NumberVal(x)) if y.length == 1 => StringVal((y.charAt(0).toInt + x).toChar.toString) // string int
        case (ListVal(l), n @ NumberVal(_)) => ListVal(l map { times(_, n) }) // list int
        case (ListVal(l1), ListVal(l2)) => ListVal(l1 ++ l2) // list list
        case (n @ NumberVal(_), ListVal(l)) => ListVal(l map { times(_, n) }) // int list
        case _ => throw new EvaluationError("")
      }
    }
    private def div(a: RuntimeValue, b: RuntimeValue): RuntimeValue = {
      (a, b) match {
        case (NumberVal(x), NumberVal(y)) => NumberVal(x / y) // int int        
        case (ListVal(l), n @ NumberVal(_)) => ListVal(l map { div(_, n) }) // list int
        case (n @ NumberVal(_), ListVal(l)) => ListVal(l map { div(_, n) }) // int list
        case _ => throw new EvaluationError("")
      }
    }

    private def handlePrePostIncDec(e: Expr, op: String,pre: Boolean)(implicit ctx: EvaluationContext) : RuntimeValue = {
      import ctx._
      val label = e.asInstanceOf[Label].label
      val module = e.asInstanceOf[Label].module
      findVar(label,module) match {
        case Some(Variable(n @ NumberVal(v), isConstant,_,_,_,_)) =>
          if (!isConstant) {
            if (pre) {
              val newValue = NumberVal(if (op == "++") v + 1 else v - 1)
              updateVar(label,module,newValue) match {
                case VariableUpdate.OK =>
                  newValue
                case VariableUpdate.CANNOT_MODIFY_CONSTANT =>
                  throw new EvaluationError(s"Cannot modify constant $label")
                case VariableUpdate.VARIABLE_NOT_EXISTS =>
                  throw new EvaluationError(s"Cannot find variable $label")
              }              
            } else {
              val newValue = NumberVal(if (op == "++") v + 1 else v - 1)
              updateVar(label,module,newValue) match {
                case VariableUpdate.OK =>
                  newValue
                case VariableUpdate.CANNOT_MODIFY_CONSTANT =>
                  throw new EvaluationError(s"Cannot modify constant $label")
                case VariableUpdate.VARIABLE_NOT_EXISTS =>
                  throw new EvaluationError(s"Cannot find variable $label")
              }
              n
            }
          } else throw new EvaluationError(s"Cannot modify constant $v")
        case Some(Variable(_,_,_,_,_,_)) => throw new EvaluationError(s"Type mismatch: cannot apply $op on $label")
        case None => throw new LabelNotAvailableException(label)
      }
    }
  }
}