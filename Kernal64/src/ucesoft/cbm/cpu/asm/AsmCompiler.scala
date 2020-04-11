package ucesoft.cbm.cpu.asm

import java.io.PrintStream
import AsmParser._
import ucesoft.cbm.cpu.CPU65xx._
import Mode._
import AsmEvaluator._
import scala.util.parsing.input.Position
import java.io.File
import java.io.IOException

class CompilerException(val msg:String,val statement:Option[AsmParser.Statement]) extends Exception(msg)

case class ByteCodeStatement(pc:Int,asm:ASMStatement,operandValue:Option[RuntimeValue],source:Position)
  
class ByteCodeBlock(_org:Int,var name:Option[String]) {
  private val asmList = new collection.mutable.ListBuffer[ByteCodeStatement]
  private var org = _org
  private var PC = _org
  
  def setNewOrg(newOrg:Int) {
    org = newOrg
    PC = org
  }
  
  def getOrg = org
  
  def pc = PC
  
  def addSizeOf(asm:ASMStatement)(implicit ctx:EvaluationContext) {
    PC += sizeAndCheck(asm)
  }
  
  def addAsm(asm:ASMStatement,operandValue:Option[RuntimeValue])(implicit ctx:EvaluationContext) {
    asmList += ByteCodeStatement(PC,asm,operandValue,asm.pos)
    PC += sizeAndCheck(asm)             
  }
  
  def alignTo(alignment:Int) {
    val offset = alignment - PC % alignment
    PC += offset
  }
  
  def size = PC - org
  
  def asmStatements : List[ByteCodeStatement] = asmList.toList
  
  private def isZeroPage(asm:ASM)(implicit ctx:EvaluationContext) : Boolean = Evaluator.evalExpr(asm.operand.get) match {
    case NumberVal(n) => n < 0x100      
    case _ =>
      false
      //throw new CompilerException(s"Cannot determine if the operand has zero page or absolute mode",Some(asm))
  }
  
  private def sizeAndCheck(asm:ASMStatement)(implicit ctx:EvaluationContext) : Int = asm match {
    case a@ASM(opcode,mode,operand) =>
      val (mmode,size) = mode match {
        case IMP => 
          (mode,1)
        case IMM | ZP | ZPX | ZPY | IZX | IZY | REL => 
          (mode,2)
        case ABS|ABX|ABY|IND => 
          (mode,3)
        case UNKNOWN_ABS_OR_ZP =>
          if (isZeroPage(a)) (ZP,2)
          else (ABS,3)
        case UNKNOWN_ABX_OR_ZPX =>
          if (isZeroPage(a)) (ZPX,2)
          else (ABX,3)
        case UNKNOWN_ABY_OR_ZPY =>
          if (isZeroPage(a)) (ZPY,2)
          else (ABY,3)
      }
      a.mode = mmode
      size
    case ORG(_,_) => 0
    case TEXT(e) => 
      Evaluator.evalExpr(e) match {
        case StringVal(s) => s.length
        case WillBeVal(_,e) => throw new CompilerException(s"Cannot determine the length of the text: Missing $e",Some(asm))
        case _ => throw new CompilerException(s"Type mismatch: TEXT expected a string",Some(asm))
      }
    case WORD_GEN(FunOp("gen",None,name :: l :: _ :: Nil),isByte) => 
      Evaluator.evalExpr(l) match {
        case ListVal(lv) => if (isByte) lv.size else lv.size * 2
        case WillBeVal(_,e) => 
          throw new CompilerException(s"Cannot determine the length of the ${if (isByte) "byte" else "word"} statement: Missing $e",Some(asm))
        case _ => 
          throw new CompilerException(s"Cannot determine the length of the ${if (isByte) "byte" else "word"} statement: type mismatch",Some(asm))
      }      
    case WORD(l,true) => l.length
    case WORD(l,false) => l.length * 2
    case WORD_GEN(f,_) => 0 // cannot happen
  }
  
  override def toString = {
    val sb = new StringBuffer(s"ByteCodeBlock[${org.toHexString},$name,$size] {\n")
    for(s <- asmList) sb.append(s"\t$s\n")
    sb.append("}")
    sb.toString
  }
}

class AsmCompiler(console:PrintStream,importDir:String) {  
  private val STD_MODULE_NAME = "std"
  private case class ModuleRef(ref:String,enclosedModule:String,pars:Int = 0) {
    override def toString = s"$enclosedModule::$ref/$pars"
  }
  
  private class Context(val name:String,val isLocal:Boolean,val enclosedModule:String = STD_MODULE_NAME) extends EvaluationContext {
    val console = (s : String) => AsmCompiler.this.console.println(s)
    private val vars = new collection.mutable.HashMap[ModuleRef, Variable]
    private val labels = new collection.mutable.HashMap[String, Int]
    private val structs = new collection.mutable.HashMap[ModuleRef,Struct]
    private var stackCounter = 0
    
    def incAndGetCounter : Int = {
      stackCounter += 1
      stackCounter
    }
    
    def newStack(name:String,isLocal:Boolean,module:Option[String]) : EvaluationContext = ??? // not implemented
    def pop : EvaluationContext = ??? // not implemented
        
    def findLabel(label:String) : Option[Int] = labels get label
    def defineLabel(label:String,address:Int) = labels.put(label,address).isDefined      
    
    private def findRef[T <: ModuleAware](refName:String,module:Option[String],map:collection.mutable.Map[ModuleRef,T],forceEnclosingModule:Option[String] = None) : Option[T] = {
      //println(s"Finding $module::$refName on $map")
      val enclosingModule = forceEnclosingModule.getOrElse(enclosedModule)
      module match {
        case None =>
          // try with enclosingModule
          map get ModuleRef(refName,enclosingModule) match {
            case s@Some(_) => s
            case None =>
              // try with standard
              map get ModuleRef(refName,STD_MODULE_NAME) match {
                case s@Some(_) => s
                case None => None
              }
          }
        case Some(m) =>
          map get ModuleRef(refName,m) match {
            case None =>
              None
            case s@Some(v) =>
              if (enclosingModule == v.module || !v.isPrivate) s else None
          }
      }
    }
    
    def findVar(varName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Variable] = findRef(varName,module,vars,forceEnclosingModule)
    def defineVar(varName:String,module:Option[String],value:RuntimeValue,isConstant:Boolean,isPrivate:Boolean) = {
      val moduleName = module.getOrElse(enclosedModule)
      vars.put(ModuleRef(varName,moduleName),Variable(value,isConstant,isPrivate,moduleName,this,varName)).isDefined
    }
    def updateVar(varName:String,module:Option[String],value:RuntimeValue) = {
      //println(s"Updating variable $varName $module on $vars")
      findVar(varName,module) match {
        case Some(v@Variable(_,false,_,m,_,_)) =>
          vars.put(ModuleRef(varName,m),v.copy(value = value))
          VariableUpdate.OK
        case Some(Variable(n,true,_,_,_,_)) =>
          VariableUpdate.CANNOT_MODIFY_CONSTANT
        case None =>
          VariableUpdate.VARIABLE_NOT_EXISTS
      }
    }
    def updateVar(v:Variable,value:RuntimeValue) : VariableUpdate.Value = {
      if (v.isConstant) VariableUpdate.CANNOT_MODIFY_CONSTANT
      else {
        vars.put(ModuleRef(v.name,v.module),v.copy(value = value))
        VariableUpdate.OK
      }
    }
    
    def clearVars = {
      vars.clear
      structs.clear
      stackCounter = 0
    }
    
    def findStruct(structName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Struct] = findRef(structName,module,structs)
    def defineStruct(structName:String,module:Option[String],fields:List[String],isPrivate:Boolean) = {
      val moduleName = module.getOrElse(enclosedModule)
      structs.put(ModuleRef(structName,moduleName),Struct(structName,moduleName,this,isPrivate,fields)).isDefined
    }
    
    def evalUserFunction(name:String,module:Option[String],pars:List[RuntimeValue]) : Option[RuntimeValue] = None // not implemented
    
    def content = {
      val sb = new StringBuffer(s"Context($name,$isLocal) {\n")
      sb.append(s"\tVARS=$vars\n")
      sb.append(s"\tLABS=$labels\n")
      sb.append(s"\tSTRS=$structs\n")
      sb.append("}")
      sb.toString
    }
    
    override def toString = s"Context($name,$isLocal)"
  }
  
  class StackEvaluationContext(runFun:(FUNCTION,String,List[RuntimeValue]) => RuntimeValue,pc:() => Int) extends EvaluationContext {
    import collection.mutable.Stack
    private val stack = new Stack[Context]
    private val userFunctions = new collection.mutable.HashMap[ModuleRef,FUNCTION]
    private val userMacros = new collection.mutable.HashMap[(String,Int),MACRO]
    private val top : EvaluationContext = newStack("top",false,Some(STD_MODULE_NAME))
    private var _linking = false
    
    def linking = _linking
    def linking_=(l:Boolean) {
      require(isTop)
      _linking = true
      if (l) top.asInstanceOf[Context].clearVars
    }
    
    def isTop : Boolean = stack.size == 1
    def level : Int = stack.size
    def newStack(name:String,isLocal:Boolean,module:Option[String]) : EvaluationContext = {
      val stackName = if (level == 0) name else s"$name(${ctx.incAndGetCounter})"
      val ns = new Context(stackName,isLocal,module.getOrElse(STD_MODULE_NAME))
      stack.push(ns)
      ns
    }
    def peek : EvaluationContext = stack.head
    def pop : EvaluationContext = stack.pop
    
    private[AsmCompiler] def ctx : Context = stack.head
    
    private def find[T](find:Context => Option[T]) : Option[T] = {
      val it = stack.iterator
      while (it.hasNext) {
        val s = it.next
        find(s) match {
          case s@Some(_) => return s
          case None =>
        }
      }
      None
    }
    
    private def currentPathOf(stack:Stack[Context]) : String = stack map { _.name} mkString(".")
    private def paths : List[String] = {
      (for(i <- 0 until stack.size) yield currentPathOf(stack.drop(i))) toList
    }
    private def labelPathOf(path:String,label:String) = label + "." + path
    private def labelPaths(label:String) : List[String] = paths map { labelPathOf(_,label) }
    
    def console = ctx.console
    def findLabel(label:String) : Option[Int] = {
      if (label == "_") Some(pc())
      else
      if (ctx.isLocal) {
        val cp = currentPathOf(stack)
        labelPaths(label) filter { p => p.endsWith(cp) } flatMap top.findLabel headOption
      }
      else labelPaths(label) flatMap top.findLabel headOption
    }
    def defineLabel(label:String,address:Int) = {
      top.defineLabel(labelPathOf(currentPathOf(stack),label),address)
    }
    
    def currentLabel(label:String) = labelPathOf(currentPathOf(stack),label)
    
    private def findVarInLocal(ctx:Context,name:String,module:Option[String]) : Option[Variable] = {
      // find in local & global only
      ctx.findVar(name,module) match {
        case s@Some(_) => s // ok, local
        case None =>
          top.findVar(name,module,Some(ctx.enclosedModule)) match {
            case s@Some(_) => 
              s // ok, module
            case None =>
              top.findVar(name,module)
          }
      }
    }
    private def findStructInLocal(ctx:Context,name:String,module:Option[String]) : Option[Struct] = {
      // find in local & global only
      ctx.findStruct(name,module) match {
        case s@Some(_) => s // ok, local
        case None =>
          top.findStruct(name,module,Some(ctx.enclosedModule)) match {
            case s@Some(_) => 
              s // ok, module
            case None =>
              top.findStruct(name,module)
          }
      }
    }
    
    def findVar(varName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Variable] = {
      if (ctx.isLocal) findVarInLocal(ctx,varName,module)
      else find ( ctx => {
          if (ctx.isLocal) findVarInLocal(ctx,varName,module)
          else ctx.findVar(varName,module)
        }
      )      
    }
    def defineVar(varName:String,module:Option[String],value:RuntimeValue,isConstant:Boolean,isPrivate:Boolean) = ctx.defineVar(varName,module,value,isConstant,isPrivate)
    def updateVar(varName:String,module:Option[String],value:RuntimeValue) = {
      if (ctx.isLocal) {
        findVarInLocal(ctx,varName,module) match {
          case Some(v) =>
            v.ctx.asInstanceOf[Context].updateVar(v, value)
          case None =>
            VariableUpdate.VARIABLE_NOT_EXISTS
        }
      }
      else {
        val v = find ( ctx => {
          if (ctx.isLocal) findVarInLocal(ctx,varName,module)
          else ctx.findVar(varName,module)
        })
        v match {
          case Some(v) =>
            v.ctx.asInstanceOf[Context].updateVar(v, value)
          case None =>
            VariableUpdate.VARIABLE_NOT_EXISTS
        }
      }
    }
    
    def findStruct(structName:String,module:Option[String],forceEnclosingModule:Option[String] = None) : Option[Struct] = {
      //find(ctx => ctx.findStruct(structName))
      if (ctx.isLocal) findStructInLocal(ctx,structName,module)
      else find ( ctx => {
          if (ctx.isLocal) findStructInLocal(ctx,structName,module)
          else ctx.findStruct(structName,module)
        }
      )      
    }
    
    def defineStruct(structName:String,module:Option[String],fields:List[String],isPrivate:Boolean) = {
      ctx.defineStruct(structName,module,fields,isPrivate)
    }
    
    def evalUserFunction(name:String,module:Option[String],pars:List[RuntimeValue]) : Option[RuntimeValue] = {
      val moduleName = module.getOrElse(ctx.enclosedModule)
      //println(s"Calling function $name $module from ${ctx.enclosedModule}")
      userFunctions get ModuleRef(name,moduleName,pars.size) flatMap { f =>
        //println(s"Found function $f")
        if (!f.isPrivate || moduleName == ctx.enclosedModule) Some(runFun(f,moduleName,pars))
        else None
      }
    }
    
    def addUserFunction(f:FUNCTION,module:Option[String],isPrivate:Boolean) {
      if (userFunctions.put(ModuleRef(f.name,module.getOrElse(STD_MODULE_NAME),f.parameters.size),f).isDefined) throw new CompilerException(s"Function ${f.name}/${f.parameters} already declared in module $module",Some(f))
    }
    
    def addUserMacro(m:MACRO) {
      if (userMacros.put((m.name,m.parameters.size),m).isDefined) throw new CompilerException(s"Macro ${m.name}/${m.parameters} already declared",Some(m))
    }
    
    def findMacro(name:String,pars:Int) : Option[MACRO] = userMacros get (name,pars)
    
    override def toString = s"${ctx.name}[${stack.size}]"
  }
  
  // ======================================================================================  
  private var byteCodeBlock : ByteCodeBlock = _ 
  private val byteCodeBlocks = new collection.mutable.ListBuffer[ByteCodeBlock]
  implicit private val ctx = new StackEvaluationContext(runFunction,() => byteCodeBlock.pc)

  @inline private def checkCondition(cond:Expr,statementType:String,s:Statement) : Boolean = {
    Evaluator.evalExpr(cond) match {          
      case NumberVal(n) => n > 0        
      case WillBeVal(_,e) => throw new CompilerException(s"Cannot determine the $statementType condition. Missing $e",Some(s))
      case _ => throw new CompilerException(s"Type mismatch: $statementType condition must be a number",Some(s))
    }
  }
  
  private def runFunction(f:FUNCTION,module:String,actualPars:List[RuntimeValue]) : RuntimeValue = {
    ctx.newStack(f.name,true,Some(module))
    try {
      for((pn,pv) <- f.parameters.zip(actualPars)) ctx.defineVar(pn,Some(module),pv,true,false)
      var retValue : Option[RuntimeValue] = None
      for(s <- f.body) retValue = compile(s)
      retValue match {
        case Some(rv) => rv
        case None => NullVal
      }
    }
    finally ctx.pop
  }
  
  private class BreakException extends Exception
  
  private def compile(s:Statement,overrideModule:Option[String] = None) : Option[RuntimeValue] = {
    try {
      s match {
        // =============== ASM STATEMENTS ====================
        case ORG(address,name) =>
          Evaluator.evalExpr(address) match {
            case NumberVal(address) =>
              if (byteCodeBlocks.exists(_.getOrg == address)) throw new CompilerException(s"Invalid org ${Integer.toHexString(address.toInt)}: already defined",Some(s))
              if (byteCodeBlock.size == 0) {
                byteCodeBlock.setNewOrg(address.toInt)
                byteCodeBlock.name = name
              }
              else {
                byteCodeBlock = new ByteCodeBlock(address.toInt,name)
                byteCodeBlocks += byteCodeBlock
              }
            case WillBeVal(_,e) => throw new CompilerException(s"Cannot determine the ORG initial address. Missing $e",Some(s))
            case _ => throw new CompilerException(s"Type mismatch: ORG expected a int address",Some(s))
          }
          None
        // =================================================
        case asm:ASMStatement =>
          if (ctx.linking) {
            // link
            val eop = asm match {
              case ASM(_,_,Some(op)) =>
                Evaluator.evalExpr(op) match {
                  case WillBeVal(_,e) => 
                    throw new CompilerException(s"Cannot evaluate asm statement $asm. Missing $e",Some(s))
                  case ev =>
                    Some(ev)
                }
              case TEXT(e) => 
                Evaluator.evalExpr(e) match {
                  case s@StringVal(_) => 
                    Some(s)
                  case WillBeVal(_,e) => throw new CompilerException(s"Cannot evaluate text statement $asm. Missing $e",Some(s))
                }
              case WORD(l,isByte) =>
                val list = l map { e =>
                  Evaluator.evalExpr(e) match {
                    case WillBeVal(_,e) => throw new CompilerException(s"Cannot evaluate ${if (isByte) "byte" else "word"} statement $asm. Missing $e",Some(s))
                    case ev => ev
                  }
                }
                Some(ListVal(list.toBuffer))
              case WORD_GEN(f,isByte) =>
                Evaluator.evalExpr(f) match {
                    case WillBeVal(_,e) => throw new CompilerException(s"Cannot evaluate ${if (isByte) "byte" else "word"} generator statement $asm. Missing $e",Some(s))
                    case ev => 
                      Some(ev)
                  }
              case _ => 
                None
            }
            byteCodeBlock.addAsm(asm,eop)
          }
          else byteCodeBlock.addSizeOf(asm)
          None
        // =================================================
        case LABELED(label) =>
          if (!ctx.linking) {
            if (ctx.defineLabel(label, byteCodeBlock.pc)) throw new CompilerException(s"Label $label already defined",Some(s))            
          }
          None
        // =================================================
        case ENUM(fields) =>
          var id = 0
          for((name,opId) <- fields) {
            val eid = opId match {
              case None => 
                val i = id
                id += 1
                i
              case Some(cid) =>
                cid
            }
            if (ctx.defineVar(name,Some(ctx.ctx.enclosedModule),new NumberVal(eid),true,false)) throw new CompilerException(s"Enum $name cannot be defined, label already exists",Some(s))    
          }
          None
        // =================================================
        case STRUCT(name,fields,isPrivate) =>
          val module = overrideModule.getOrElse(ctx.ctx.enclosedModule)
          if (ctx.defineStruct(name,Some(module),fields,isPrivate)) throw new CompilerException(s"Struct $name already defined",Some(s))
          None
        // =================================================
        case CONST(name,e,isPrivate) =>
          val module = overrideModule.getOrElse(ctx.ctx.enclosedModule)
          if (ctx.defineVar(name,Some(module),Evaluator.evalExpr(e),true,isPrivate)) throw new CompilerException(s"Constant $name already defined",Some(s))
          None
        // =================================================
        case VAR(name,e,isPrivate) =>
          val module = overrideModule.getOrElse(ctx.ctx.enclosedModule)
          if (ctx.defineVar(name,Some(module),Evaluator.evalExpr(e),false,isPrivate)) throw new CompilerException(s"Variable $name already defined",Some(s))
          None
        // =================================================
        case EVAL(None,e) => 
          Some(Evaluator.evalExpr(e))
        case EVAL(Some(EvalTarget(ModuleLabel(name,module),None)),e) =>
          ctx.updateVar(name,module,Evaluator.evalExpr(e)) match {
            case VariableUpdate.OK =>
            case VariableUpdate.CANNOT_MODIFY_CONSTANT =>
              throw new CompilerException(s"Cannot modify constant var $name",Some(s))
            case VariableUpdate.VARIABLE_NOT_EXISTS =>
              throw new CompilerException(s"Cannot find var $name",Some(s))
          }
          None
        case EVAL(Some(EvalTarget(ModuleLabel(name,module),Some(selectors))),e) =>
          ctx.findVar(name,module) match {
            case Some(Variable(st@StructVal(_,_,_,_),const,_,_,_,_)) =>
              if (const) throw new CompilerException(s"Cannot modify constant struct $name",Some(s))
              val ev = Evaluator.evalExpr(e)
              var struct = st
              for((sel,i) <- selectors.zipWithIndex) {
                struct.map get sel match {
                  case Some(inner@StructVal(_,_,_,_)) if i < selectors.size - 1 =>
                    struct = inner
                  case Some(_) if i == selectors.size - 1 =>
                    struct.map += sel -> ev
                  case None =>
                    throw new CompilerException(s"Field $sel is not in the struct ${struct.structName}",Some(s))
                }
              }
              None
            case Some(_) => throw new CompilerException(s"Variable $name is not a struct",Some(s))
            case None => throw new CompilerException(s"Variable $name not found",Some(s))
          }
        // =================================================
        case IF(cond,thenStmts,elseStmts) =>
          ctx.newStack("if",false,Some(ctx.ctx.enclosedModule))
          try {
            var last : Option[RuntimeValue] = None
            if (checkCondition(cond,"if",s)) thenStmts foreach { s => last = compile(s) }
            else elseStmts foreach { s => last = compile(s) }
            last
          }
          finally ctx.pop
        // =================================================
        case WHILE(cond,stmts) =>
          var broken = false
          while (!broken && checkCondition(cond,"while",s)) 
          try {
            ctx.newStack("while",false,Some(ctx.ctx.enclosedModule))
            stmts foreach { s => compile(s) }
          }
          catch {
            case _:BreakException => 
              broken = true
          }
          finally ctx.pop
          None
        // =================================================
        case FOR(vars,cond,post,stmts) =>
          ctx.newStack("forVar",false,Some(ctx.ctx.enclosedModule))
          try {
            // vars
            for(v <- vars) compile(v)
            var broken = false
            while (!broken && checkCondition(cond,"for",s))
            try {
              ctx.newStack("for",false,Some(ctx.ctx.enclosedModule))
              stmts foreach { s => compile(s) }
              // post
              for(p <- post) compile(EVAL(None,p))
            }
            catch {
              case _:BreakException =>
                broken = true
            }
            finally ctx.pop
          }
          finally ctx.pop          
          None
        // =================================================
        case MACRO_CALL(name,pars) =>
          ctx.findMacro(name,pars.size) match {
            case Some(m) =>
              ctx.newStack("macro",true,Some(ctx.ctx.enclosedModule))
              try {
                val epars = pars map { Evaluator.evalExpr }
                for((pn,pv) <- m.parameters.zip(epars)) ctx.defineVar(pn,None,pv,true,false)
                for(s <- m.body) compile(s)
              }
              finally ctx.pop
            case None =>
              throw new CompilerException(s"Cannot find macro $name",Some(s))
          }          
          None
        // =================================================
        case PRINT(e) =>
          if (ctx.linking) {
            Evaluator.evalExpr(e) match {
              case WillBeVal(_,e) =>
                throw new CompilerException(s"Cannot print: missing $e",Some(s))
              case v => console.println(v)
            }            
          }
          None
        // =================================================
        case DECLARE_LABEL(label,e) =>
          if (!ctx.linking) {
            val addr = Evaluator.evalExpr(e) match {
              case NumberVal(address) => address.toInt
              case _ => throw new CompilerException(s"Cannot evaluate label declaration of $label",Some(s))
            }
            if (ctx.defineLabel(label,addr)) throw new CompilerException(s"Label $label already defined",Some(s))            
          }
          None
        // =================================================
        case BREAK =>
          throw new BreakException
        // =================================================
        case DUP(e,body) =>
          val times = Evaluator.evalExpr(e) match {
            case NumberVal(n) => n.toInt
            case _ => throw new CompilerException(s"Cannot evaluate dup declaration",Some(s))
          }
          ctx.newStack("dup",false,Some(ctx.ctx.enclosedModule))
          try {
            for(i <- 1 to times;s <- body)
              compile(s)
          }
          finally ctx.pop
          None
        // =================================================
        case ALIGN(n) =>
          val alignment = Evaluator.evalExpr(n) match {
            case NumberVal(align) => align.toInt
            case _ => throw new CompilerException(s"Cannot evaluate alignment declaration",Some(s))
          }
          byteCodeBlock.alignTo(alignment)
          None
        // =================================================
        case ERROR(msg) =>
          Evaluator.evalExpr(msg) match {
            case WillBeVal(_,e) =>  throw new CompilerException(s"Cannot evaluate error argument",Some(s))
            case err => throw new CompilerException(err.toString,Some(s))
          }
          None
      }
    }
    catch {
      case ee:EvaluationException =>
        throw new CompilerException(s"Error while evaluating expression: " + ee.msg,Some(s))
    }
  }
  
  private def init {
    byteCodeBlock = new ByteCodeBlock(0,Some("default"))
    byteCodeBlocks.clear
    byteCodeBlocks += byteCodeBlock
    // constants
    ctx.defineVar("true",None,new NumberVal(1),true,false)
    ctx.defineVar("false",None,new NumberVal(0),true,false)
    ctx.defineVar("nil",None,new ListVal(Nil.toBuffer),true,false)
  }
  
  private def include(includeStmt:INCLUDE) : List[Statement] = {
    console.println("Including file " + includeStmt.file)
    val includeFile = new File(new File(importDir),includeStmt.file)
    if (!includeFile.exists) throw new CompilerException("Cannot find include file " + includeStmt.file + " in " + importDir,Some(includeStmt))
    val source = io.Source.fromFile(includeFile)
    try {
      val txt = source.getLines.mkString("\n") + "\n"
      val in = new java.io.StringReader(txt)
      println("Including \n" + txt)
      val parser = new AsmParser(includeStmt.file)
      parser.parseAll(parser.topStatements, in) match {
       case parser.Success(parsed, _) =>
         parsed
       case parser.Failure(f,i) =>
         throw new CompilerException("Parsing error while processing include file " + includeStmt.file + " in " + importDir + " : " + f + " " + i.pos,Some(includeStmt))
       case parser.Error(e,i) =>
         throw new CompilerException("Parsing error while processing include file " + includeStmt.file + " in " + importDir + " : " + e + " " + i.pos,Some(includeStmt))
      }
    }
    catch {
      case io:IOException =>
        throw new CompilerException("Cannot find include file " + includeStmt.file + " in " + importDir + " : " + io,Some(includeStmt))
    }
    finally {
      source.close
    }
  }
  
  def compile(sts:List[Statement]) : List[ByteCodeBlock] = {
    init
    // include includes
    var stillContainsInclude = true
    var included = sts
    while (stillContainsInclude) {
      stillContainsInclude = false
      included = included flatMap {
        case i@INCLUDE(_) =>
          stillContainsInclude = true
          include(i)
        case s => 
          List(s)
      }
    }
    print("Statements after include:\n" + included.mkString("\n"))
    // filter user functions & macros
    val filteredSts = included filter { 
      case f@AsmParser.FUNCTION(_,_,_,isPrivate) =>
        ctx.addUserFunction(f,None,isPrivate)
        false
      case m@AsmParser.MACRO(_,_,_) =>
        ctx.addUserMacro(m)
        false
      case _ => true
    }
    for(s <- filteredSts) try {
      s match {
        case MODULE(module,stats) =>
          console.println(s"Declaring module $module")
          for(s <- stats) s match {
            case f@AsmParser.FUNCTION(_,_,_,isPrivate) =>
              ctx.addUserFunction(f,Some(module),isPrivate)
            case s =>
              compile(s,Some(module))
          }
        case s =>
          compile(s)
      }      
    }
    catch {
      case _:BreakException => 
        throw new CompilerException(s"Break without for or while statement",Some(s))
    }

    ctx.defineLabel("__",byteCodeBlock.pc)
    printStack
    println(s"========= 1st pass completed ============== [${ctx.level}]")
    ctx.linking = true
    init
    
    for(s <- filteredSts) s match {
      case MODULE(module,stats) =>
        for(s <- stats) s match {
          case f@AsmParser.FUNCTION(_,_,_,_) =>
          case s =>
            compile(s,Some(module))
        }
      case s =>
        compile(s)
    }
    
    byteCodeBlocks.toList
  }
  
  def printStack {
    println(ctx.peek.asInstanceOf[Context].content)
  }
}