package ucesoft.cbm.cpu.asm

object AsmTest extends App {
  println("Enter expression to evaluate [eval to evaluate]")
  val sb = new StringBuilder
  val parser = new AsmParser("test.asm")
  AsmEvaluator.Evaluator.importDir = "/Users/ealeame/Desktop"
  while (true) {            
    print(">")
    val line = io.StdIn.readLine
    if (line == "eval") {        
      val in = new java.io.StringReader(sb.toString)
      println("Evaluating:" + sb.toString)
      parser.parseAll(parser.topStatements, in) match {
       case parser.Success(parsed, _) =>
         println("Success:\n" + parsed.mkString("\n"))
         try {
           val cc = new AsmCompiler(System.out,"/Users/ealeame/Desktop")
           val bl = cc.compile(parsed)
           for(b <- bl) println(b)
           cc.printStack
           
           println("Emitting...")
           val emitter = new AsmBytecodeEmitter(System.out,bl)
           val AsmEncoding(org,mem) = emitter.encode
           println("Mem size = " + mem.size)
           /*
           var i = 0
           var b = 0
           while (i < mem.length) {
             val bb = mem(i).toInt & 0xFF
             val v = bb.toHexString
             print((if (v.length == 1) "0" + v else v) + " ")
             b += 1
             if (b == 16) {
               println
               b = 0
             }
             i += 1
           }
           * 
           */
           
           val out = new java.io.FileOutputStream("/Users/ealeame/Desktop/fra.prg")
           out.write(org & 0xFF)
           out.write(org >> 8)
           out.write(mem)
           out.close
           
         }
         catch {
           case cc:CompilerException =>
             println("ERROR: " + cc.msg + " on " + cc.statement + " => " + (cc.statement map { _.pos }) + " fileName:" + (cc.statement map { _.fileName }))
         }
       case x =>
         println("Failure:" + x)
      }
      sb.clear
    }
    else sb.append(line + "\n")
  }
}