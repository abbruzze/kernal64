package ucesoft.cbm.peripheral.rs232

import java.io._
import java.net.{ServerSocket, Socket}

import ucesoft.cbm.Log

import scala.util.matching.Regex

trait ModemCommandListener {
  def hangUp : Unit
  def commandMode(on:Boolean)
  def connectTo(address:String)
  def ring(ringing:Boolean) : Unit
}

object HayesResultCode extends Enumeration {
  val OK          = Value(0)
  val CONNECT     = Value(1)
  val RING        = Value(2)
  val NO_CARRIER  = Value(3)
  val ERROR       = Value(4)
  val CONNECT1200 = Value(5)
  val CONNECT2400 = Value(10)
  val CONNECT4800 = Value(11)
  val CONNECT9600 = Value(12)
  val CONNECT14400= Value(13)
  val CONNECT19200= Value(14)
  val CONNECT38400= Value(28)
}

class Modem(mcl:ModemCommandListener,welcomeMessage:String = null) extends Runnable {
  // ===================== Hayes stuff =============================
  private case class HayesResult(matches:Boolean,rest:String,skipResultCode:Boolean = false)

  private abstract class HayesCommand {
    val RE1 : Option[Regex] = None
    val RE2 : Option[Regex] = None
    def matches(cmd:String) : HayesResult = {
      RE1 match {
        case Some(re1) =>
          cmd match {
            case re1(p1) =>
              cmd1(cmd,p1)
            case _ => HayesResult(false,cmd)
          }
        case None =>
          RE2 match {
            case Some(re2) =>
              cmd match {
                case re2(p1,p2) =>
                  cmd2(cmd,p1,p2)
              }
            case None =>
              HayesResult(false,cmd)
          }
      }
    }

    protected def cmd1(cmd:String,p1:String) : HayesResult = HayesResult(false,cmd)
    protected def cmd2(cmd:String,p1:String,p2:String) : HayesResult = HayesResult(false,cmd)
  }

  private class FastHayesCommand(re:String,handle:(String,String) => HayesResult) extends HayesCommand {
    override val RE1 = Some(re.r)
    override def cmd1(cmd:String,p1:String) : HayesResult = handle(cmd,p1)
  }
  // ========================= SUPPORTED HAYES COMMANDS ============================
  private object ATDT extends FastHayesCommand("DT?(.*)", (_,p1) => {
    if (p1.trim.length > 0) {
      mcl.connectTo(p1.trim)
      HayesResult(true, "", true)
    }
    else HayesResult(true,"")
  })
  private object ATI extends FastHayesCommand("(I)", (_,_) => {
    modemIn.append("KERNAL64 INTERNAL MODEM EMULATOR" + 13.toChar)
    HayesResult(true,"")
  })
  private object ATE extends FastHayesCommand("(E0?).*", (cmd,p1) => {
    echoMode = false
    HayesResult(true,cmd.substring(p1.length))
  })
  private object ATE1 extends FastHayesCommand("(E1).*", (cmd,p1) => {
    echoMode = true
    HayesResult(true,cmd.substring(p1.length))
  })
  private object ATH extends FastHayesCommand("(H0?).*", (cmd,p1) => {
    mcl.hangUp
    HayesResult(true,cmd.substring(p1.length))
  })
  private object ATO extends FastHayesCommand("(O)", (_,_) => {
    commandMode = false
    mcl.commandMode(false)
    HayesResult(true,"")
  })
  private object Q1 extends FastHayesCommand("(Q1).*", (cmd,p1) => {
    quiteMode = true
    HayesResult(true,cmd.substring(p1.length))
  })
  private object Q extends FastHayesCommand("(Q0?).*", (cmd,p1) => {
    quiteMode = false
    HayesResult(true,cmd.substring(p1.length))
  })
  private object X1 extends FastHayesCommand("(X1).*", (cmd,p1) => {
    addConnectionSpeed = true
    HayesResult(true,cmd.substring(p1.length))
  })
  private object X0 extends FastHayesCommand("(X0).*", (cmd,p1) => {
    addConnectionSpeed = false
    HayesResult(true,cmd.substring(p1.length))
  })
  private object Dummy extends FastHayesCommand("([XM]\\d).*", (cmd,p1) => {
    // do nothing
    HayesResult(true,cmd.substring(p1.length))
  })
  private object Z extends FastHayesCommand("(Z).*", (cmd,p1) => {
    reset
    HayesResult(true,cmd.substring(p1.length))
  })
  private object Select extends FastHayesCommand("(S\\d{1,3}).*", (cmd,p1) => {
    currentS = p1.substring(1).toInt
    HayesResult(true,cmd.substring(p1.length))
  })
  private object QuestionM extends FastHayesCommand("(\\?).*", (cmd,p1) => {
    modemIn.append(S(currentS).toString + 13.toChar)
    HayesResult(true,cmd.substring(p1.length),true)
  })
  private object GetS extends FastHayesCommand("(=\\d{1,3}).*", (cmd,p1) => {
    S(currentS) = p1.substring(1).toInt
    HayesResult(true,cmd.substring(p1.length))
  })
  private object V extends FastHayesCommand("(V0?).*", (cmd,p1) => {
    englishResultCodes = false
    HayesResult(true,cmd.substring(p1.length))
  })
  private object V1 extends FastHayesCommand("(V1).*", (cmd,p1) => {
    englishResultCodes = true
    HayesResult(true,cmd.substring(p1.length))
  })
  private object ATAn extends FastHayesCommand("(A\\d{1,5}).*", (cmd,p1) => {
    allowListening(true,p1.substring(1).toInt)
    HayesResult(true,cmd.substring(p1.length))
  })
  private object ATA extends FastHayesCommand("(A).*", (cmd,p1) => {
    if (ringing) answerCall = true
    HayesResult(true,cmd.substring(p1.length))
  })
  // ===============================================================================
  private object HayesCommand {
    private val commands = List(ATDT, ATI, ATE1, ATE, ATH, ATO, Q1, Q, X1, X0, Dummy, Z, Select, QuestionM, GetS, V1, V, ATAn, ATA).view

    def processCommands(_cmds:String) : Option[Boolean] = {
      var processing = true
      var cmds = _cmds.replaceAll("\\s+","")
      var skipResult = false
      while (cmds.length > 0 && processing) {
        commands map { _.matches(cmds) } find { case HayesResult(true,_,_) => true case HayesResult(false,_,_) => false } match {
          case Some(HayesResult(_,rest,sr)) =>
            cmds = rest
            skipResult = sr
          case None =>
            processing = false
        }
      }
      if (skipResult) None else Some(processing)
    }
  }
  // ===============================================================
  private class CommandDetector {
    private[this] val lastThree = new StringBuilder

    def addAndCheck(c:Char) : Boolean = {
      lastThree.append(c)
      if (lastThree.length == 4) lastThree.deleteCharAt(0)
      val s2 = S(2)
      val commandModeReq = lastThree.length == 3 &&
                           lastThree.charAt(0) == s2 &&
                           lastThree.charAt(1) == s2 &&
                           lastThree.charAt(2) == s2
      if (commandModeReq) lastThree.clear
      commandModeReq
    }

    def reset = lastThree.clear
  }

  private[this] class ModemCommandStream(welcomeMessage:String = "") extends InputStream {
    private[this] var msg = welcomeMessage

    override def available = msg.length

    def read = synchronized {
      if (msg.length > 0) {
        val b = msg.charAt(0).toInt
        msg = msg.substring(1)
        b
      }
      else -1
    }
    def append(s:String) = synchronized { msg += s }
  }

  private class WrappedInputStream extends InputStream {
    override def available = {
      if (commandMode) modemIn.available
      else
      if (in != null) in.available
      else 0
    }

    def read = {
      if (commandMode) modemIn.read
      else {
        if (in != null) {
          val b = in.read
          if (inCommandDetector.addAndCheck(b.toChar)) {
            mcl.commandMode(true)
            commandMode = true
          }
          b
        }
        else -1
      }
    }
  }

  private class WrappedOutputStream extends OutputStream {
    def write(b:Int): Unit = {
      if (commandMode) {
        if (echoMode) modemIn.append(b.toChar.toString)
        if (b == 13) {
          try {
            processCommand(commandOutBuffer)
          }
          finally {
            commandOutBuffer.clear
          }
        }
        else
        if (b == S(5)) commandOutBuffer.delete(commandOutBuffer.length - 1,commandOutBuffer.length)
        else commandOutBuffer.append(b.toChar)
      }
      else
      if (out != null) {
        out.write(b)
        if (outCommandDetector.addAndCheck(b.toChar)) {
          mcl.commandMode(true)
          commandMode = true
        }
      }
    }
    override def flush: Unit = {
      if (out != null) out.flush
    }
  }

  private def processCommand(commandBuffer:StringBuilder): Unit = {
    import HayesResultCode._
    val cmd = commandBuffer.toString.toUpperCase.trim
    println("Processing command " + cmd)

    if (cmd.length == 0) return
    if (cmd == "AT") modemIn.append("OK" + 13.toChar)
    else
    if (cmd.startsWith("AT")) {
      HayesCommand.processCommands(cmd.substring(2)) match {
        case Some(true) =>
          if (!quiteMode) commandModeMessage(OK)
        case Some(false) =>
          if (!quiteMode) commandModeMessage(ERROR)
        case None =>
      }
    }
  }

  private[this] var in : InputStream = _
  private[this] var out : OutputStream = _
  private[this] val win = new WrappedInputStream
  private[this] val wout = new WrappedOutputStream
  private[this] val commandOutBuffer = new StringBuilder
  private[this] var commandMode = true
  private[this] val inCommandDetector, outCommandDetector = new CommandDetector
  private[this] val modemIn = new ModemCommandStream(if (welcomeMessage == null) "WELCOME TO KERNAL64 RS-232. ATDT<HOST:PORT> TO CONNECT" + 13.toChar else welcomeMessage)
  private[this] var echoMode = true
  private[this] var serverSocket : ServerSocket = _
  private[this] var socket : Socket = _
  private[this] var listeningThread : Thread = _
  private[this] var listeningPort = 0
  private[this] var quiteMode = false
  private[this] var englishResultCodes = true
  private[this] var currentBaud = 1200
  private[this] var addConnectionSpeed = false
  private[this] var currentS = 0
  private[this] val S = Array.ofDim[Int](256)
  @volatile private[this] var ringing,answerCall = false

  reset

  def setBaud(baud:Int) = {
    currentBaud = baud
  }

  def setStreams(in:InputStream,out:OutputStream): Unit = {
    this.in = in
    this.out = out
    commandMode = in == null
    Log.info(s"Modem switched to ${if (commandMode) "command" else "internet"} mode")
    if (in == null && socket != null) {
      socket.close()
      socket = null
    }
  }

  def commandModeMessage(_code:HayesResultCode.Value): Unit = {
    import HayesResultCode._
    val code = _code match {
      case CONNECT =>
        currentBaud match {
          case 1200 => CONNECT1200
          case 2400 => CONNECT2400
          case 4800 => CONNECT4800
          case 9600 => CONNECT9600
          case 14400 => CONNECT14400
          case 19200 => CONNECT19200
          case 38400 => CONNECT38400
          case _ => _code
        }
      case _ => _code
    }
    if (englishResultCodes) {
      val codeString = if (code == NO_CARRIER) "NO CARRIER" else code.toString
      modemIn.append(codeString + 13.toChar)
    }
    else modemIn.append(code.id.toString + 13.toChar)
  }

  def commandModeMessage(msg:String): Unit = {
    if (commandMode) modemIn.append(msg)
  }

  def inputStream : InputStream = win
  def outputStream : OutputStream = wout

  def reset: Unit = {
    commandMode = true
    commandOutBuffer.clear
    inCommandDetector.reset
    outCommandDetector.reset
    allowListening(false)
    quiteMode = false
    englishResultCodes = true
    currentBaud = 1200
    addConnectionSpeed = false
    // init S
    java.util.Arrays.fill(S,0)
    S(2) = 43
    S(3) = 13
    S(4) = 10
    S(5) = 20
    ringing = false
    answerCall = false
  }

  def allowListening(allowed:Boolean,port:Int = -1): Unit = {
    if (allowed) {
      if (listeningThread != null && listeningThread.isAlive) listeningThread.interrupt()
      listeningThread = new Thread(this,"ModemListener")
      listeningPort = port
      listeningThread.start
    }
    else {
      if (listeningThread != null) {
        listeningThread.interrupt()
        if (in != null) in.close
        if (out != null) out.close
      }
    }
  }

  def run: Unit = {
    try {
      serverSocket = new ServerSocket(listeningPort)
      Log.info(s"Modem listening on port $listeningPort ...")
    }
    catch {
      case io:IOException =>
        Log.info(s"Modem: Cannot listen on port $listeningPort: " + io)
        return
    }
    var running = true
    while (running) {
      try {
        val newSocket = serverSocket.accept()
        if (socket == null) {
          socket = newSocket
          Log.info(s"Modem: new incoming connection (${socket.getInetAddress.getHostAddress}) accepted. Sending RING")
          S(1) = 0
          ringing = true
          val autoAnswer = S(0) > 0
          val maxRings = if (autoAnswer) S(0) else 10
          mcl.ring(true)
          commandModeMessage(HayesResultCode.RING)
          while (S(1) < maxRings && !answerCall) {
            Thread.sleep(1000)
            S(1) += 1
          }
          mcl.ring(false)
          if (!autoAnswer && !answerCall) {
            newSocket.getOutputStream.write(new String("SERVER NOT ANSWERED+++").getBytes)
            newSocket.getOutputStream.flush
            newSocket.close()
            socket = null
            ringing = false
            answerCall = false
          }
          else {
            ringing = false
            answerCall = false
            commandModeMessage(HayesResultCode.CONNECT)
            Thread.sleep(1000)
            Log.info("Answered call")
            setStreams(new BufferedInputStream(socket.getInputStream),new BufferedOutputStream(socket.getOutputStream))
          }
        }
        else {
          Log.info("Modem busy, cannot answer new call")
          newSocket.getOutputStream.write(new String("SERVER BUSY+++").getBytes)
          newSocket.getOutputStream.flush
          newSocket.close()
        }
      }
      catch {
        case _:InterruptedException =>
          running = false
        case io:IOException =>
          Log.info(s"Modem: Error while listening on port $listeningPort: " + io)
      }
    }
    Log.info("Modem: listener closed")
  }
}
