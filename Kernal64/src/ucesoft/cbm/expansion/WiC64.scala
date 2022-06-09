package ucesoft.cbm.expansion

import org.apache.commons.net.telnet.TelnetClient
import ucesoft.cbm.{CBMComponent, CBMComponentType, Clock, ClockEvent}

import java.io.{BufferedInputStream, InputStream, ObjectInputStream, ObjectOutputStream}
import java.net._
import java.text.SimpleDateFormat
import java.util.Properties
import java.util.concurrent.{CountDownLatch, LinkedBlockingDeque}

object WiC64 extends CBMComponent with Runnable {
  override val componentID: String = "WiC64"
  override val componentType: CBMComponentType.Type = CBMComponentType.USER_PORT

  trait WiC64Listener {
    def onCMD(cmdDescr:String): Unit
    def turnGreenLed(on:Boolean): Unit
    def log(info:String): Unit
    def newFirmwareAvaiilable(newVer:Int,oldVer:Int): Unit
  }

  var flag2Action : () => Unit = _

  private var _enabled = false

  final val RECEIVING_MODE = 4
  final val SENDING_MODE = 0

  private var networkif : NetworkInterface = _
  private var MAC_ADDRESS = getMacAddress()

  private final val WIC64_REAL_FIRMWARE_VERSION = 30
  private final val FIRMWARE_VERSION = "WIC64FWV:K1.0.2"
  final val SSID = "KERNALSID"
  private final val DEFAULT_SERVER = "http://www.wic64.de/prg/"

  private final val RECEIVING_MODE_WAITING_W = 0
  private final val RECEIVING_MODE_WAITING_LEN_LO = 1
  private final val RECEIVING_MODE_WAITING_LEN_HI = 2
  private final val RECEIVING_MODE_WAITING_CMD = 3
  private final val RECEIVING_MODE_WAITING_DATA = 4
  private final val RECEIVING_MODE_WAITING_LAST = 5

  private final val SENDING_MODE_WAITING_DUMMY = 0
  private final val SENDING_MODE_LO = 1
  private final val SENDING_MODE_HI = 2
  private final val SENDING_MODE_WAITING_END = 3
  private final val SENDING_MODE_STREAMING = 4

  private var mode = SENDING_MODE
  private var sendMode = SENDING_MODE_WAITING_DUMMY
  private var recMode = RECEIVING_MODE_WAITING_W
  private var bufferLen = 0
  private var adjustBufferLenForPRG = 0
  private var recCmd = 0
  @volatile private var buffer,responseBuffer : Array[Int] = _
  private var writeIndex,readIndex = 0

  private final val TOKEN_NAME = "sectokenname"
  private var token = ""
  private val prefMap = new Properties()

  private var telnetClient : TelnetClient = _
  private val dateFormatter = new SimpleDateFormat("HH:mm:ss dd/MM/yyyy")

  // UDP
  private var udpPort = 8080
  private var udp : DatagramSocket = _
  private var udpThread : Thread = _
  private var udpThreadRunning = false
  private var udpThreadShutdown = new CountDownLatch(1)
  private case class UDPPacket(from:Array[Byte],data:Array[Byte])
  private val udpReceivedQueue = new LinkedBlockingDeque[UDPPacket](10)

  private var server = DEFAULT_SERVER

  private var listener : WiC64Listener = _
  private var totalCmdIssued = 0

  private var logEnabled = false

  private var streamingIn : InputStream = _
  private var streamingSize = 0

  //private val executor = Executors.newSingleThreadExecutor()
  private val clk = Clock.systemClock

  private final val CMD_CLOCKS_WAIT = (clk.PAL_CLOCK_HZ / 1000).toInt

  private final val CMD_DESCR = Map(
    0x0 -> "get FW version",
    0x01 -> "loading http",
    0x02 -> "config wifi",
    0x03 -> "FW update 1",
    0x04 -> "FW update 2",
    0x05 -> "FW update 3",
    0x06 -> "get ip",
    0x07 -> "get stats",
    0x08 -> "set server",
    0x09 -> "REM",
    0x0A -> "get upd",
    0x0B -> "send udp",
    0x0C -> "scanning wlan",
    0x0D -> "config wifi id",
    0x0E -> "change udp port",
    0x0F -> "loading httpchat",
    0x10 -> "get ssid",
    0x11 -> "get rssi",
    0x12 -> "get server",
    0x13 -> "get external ip",
    0x14 -> "get mac",
    0x15 -> "get time and date",
    0x16 -> "set timezone",
    0x17 -> "get timezone",
    0x18 -> "check update",
    0x19 -> "read prefs",
    0x1A -> "save prefs",
    0x1E -> "get tcp",
    0x1F -> "send tcp",
    0x20 -> "set tcp port",
    0x21 -> "connect tcp1",
    0x22 -> "get tcp1",
    0x23 -> "send tcp1",
    0x24 -> "http post",
    0x25 -> "streaming", // ?
    0x63 -> "factory reset"
  )

  override def getProperties: Properties = {
    val properties = super.getProperties
    properties.setProperty("Enabled",_enabled.toString)
    properties.setProperty("Command counts:",totalCmdIssued.toString)
    properties.setProperty("Local IP",getIPAddress())
    properties.setProperty("MAC",getMacAddress())
    properties.setProperty("Mode",mode.toString)
    properties.setProperty("Read mode",recMode.toString)
    properties.setProperty("Write mode",sendMode.toString)
    properties
  }

  def enabled : Boolean = _enabled
  def enabled_=(on:Boolean): Unit = {
    _enabled = on
    if (!on) reset
    else {
      // check firmware version
      sendHttpGET("http://sk.sx-64.de/wic64/version.txt",false)
      if (responseBuffer != null && responseBuffer.length == 4) {
        val version = responseBuffer.map(_.toChar).mkString.substring(2).toInt
        if (version > WIC64_REAL_FIRMWARE_VERSION && listener != null) listener.newFirmwareAvaiilable(version,WIC64_REAL_FIRMWARE_VERSION)
      }
    }
  }

  def setLogEnabled(enabled:Boolean): Unit = logEnabled = enabled

  def getTotalCmdIssued: Int = totalCmdIssued

  def setListener(l:WiC64Listener): Unit = listener = l

  def setNetworkInterface(ni:NetworkInterface): Unit = {
    networkif = ni
    MAC_ADDRESS = getMacAddress()
  }

  def reset: Unit = {}

  def resetWiC64(): Unit = {
    if (telnetClient != null && telnetClient.isConnected) {
      try {
        telnetClient.disconnect()
        telnetClient = null
      }
      catch {
        case _:Exception =>
      }
    }

    totalCmdIssued = 0
    token = ""
    setMode(SENDING_MODE)
    recMode = RECEIVING_MODE_WAITING_W
    sendMode = SENDING_MODE_WAITING_DUMMY
    server = DEFAULT_SERVER

    udpPort = 8080
    if (udpThreadRunning) {
      udpThreadRunning = false
      udpThread.interrupt()
    }

    if (listener != null) listener.onCMD(CMD_DESCR(0x63))
    log("Resetting ...")
  }

  def setMode(mode:Int): Unit = {
    if (mode != this.mode) {
      this.mode = mode
      //println(s"SET MODE = $mode")

      recMode = RECEIVING_MODE_WAITING_W
      sendMode = SENDING_MODE_WAITING_DUMMY

      if (listener != null) listener.turnGreenLed(mode == RECEIVING_MODE)
    }
  }

  /*
  private def background(task: => Unit): Unit = {
    executor.submit(new Runnable() {
      override def run(): Unit = task
    })
  }
   */

  def write(byte:Int): Unit = {
    //println(s"WRITE: ${byte.toHexString} '${byte.toChar}' recMode = $recMode writeIndex=$writeIndex len=$bufferLen")
    if (mode != RECEIVING_MODE) {
      //println("NOT IN RECEIVING MODE ...")
      return
    }

    //println(s"WRITE: ${byte.toHexString} '${byte.toChar}' recMode = $recMode writeIndex=$writeIndex len=$bufferLen")

    recMode match {
      case RECEIVING_MODE_WAITING_W if byte == 0x57 =>
        recMode = RECEIVING_MODE_WAITING_LEN_LO
        flag2Action()
      case RECEIVING_MODE_WAITING_LEN_LO =>
        bufferLen = byte
        recMode = RECEIVING_MODE_WAITING_LEN_HI
        flag2Action()
      case RECEIVING_MODE_WAITING_LEN_HI =>
        bufferLen = (bufferLen | byte << 8) - 4
        if (bufferLen < 0) bufferLen = 0
        buffer = Array.ofDim[Int](bufferLen)
        writeIndex = 0
        recMode = RECEIVING_MODE_WAITING_CMD
        flag2Action()
      case RECEIVING_MODE_WAITING_CMD =>
        if (bufferLen == 0) {
          recCmd = byte
          flag2Action()
          bufferLen = -1
          clk.schedule(new ClockEvent("WiC64",clk.currentCycles + CMD_CLOCKS_WAIT,_ => executeCmd()))
        }
        else if (bufferLen > 0) {
          recCmd = byte
          recMode = RECEIVING_MODE_WAITING_DATA
          flag2Action()
        }
      case RECEIVING_MODE_WAITING_DATA =>
        if (writeIndex < buffer.length) {
          flag2Action()
          buffer(writeIndex) = byte
          writeIndex += 1
          if (writeIndex == bufferLen) {
            clk.schedule(new ClockEvent("WiC64",clk.currentCycles + CMD_CLOCKS_WAIT,_ => executeCmd()))
          }
        }
      case _ =>
    }
  }

  private def prepareResponse(resp:String): Unit = {
    sendMode = SENDING_MODE_WAITING_DUMMY
    responseBuffer = resp.toArray.map(_.toInt)
    flag2Action()
  }
  private def prepareResponse(resp:Array[Int]): Unit = {
    sendMode = SENDING_MODE_WAITING_DUMMY
    responseBuffer = resp
    flag2Action()
  }

  @inline private def log(s:String): Unit = if (logEnabled && listener != null) listener.log(s)

  private def checkHttpResponse(rcode:Int,content:Array[Int]): Option[String] = {
    if (rcode == 201) {
      // search var name and value
      var retCode = "0"
      content.map(_.toChar).mkString.split("\\u0001") match {
        case Array(_, name, value, rtCode) =>
          prefMap.setProperty(name, value)
          log(s"PREF $name = $value")
          retCode = rtCode

          if (prefMap.getProperty(TOKEN_NAME, "") == name) {
            token = value
            log(s"TOKEN = $token")
          }
        case _ =>
      }
      //responseBuffer = retCode.toArray.map(_.toInt)
      Some(retCode)
    }
    else None
  }

  private def dump(toDump:Array[Int]): Unit = {
    val dumped = toDump.sliding(16,16)
    var offset = 0
    val sb = new StringBuilder
    log("-" * (16 * 4 + 5))
    for (row <- dumped) {
      var i = 0
      var offsetHex = offset.toHexString
      offsetHex = ("0" * (4 - offsetHex.length)) + offsetHex
      sb.append(s"$offsetHex ")
      while (i < row.length) {
        val hex = if (row(i) < 0xF) s"0${row(i).toHexString}" else row(i).toHexString
        sb.append(s"$hex ")
        i += 1
      }
      i = 0
      while (i < (48 - 3 * row.length)) {
        sb.append(' ')
        i += 1
      }
      i = 0
      while (i < row.length) {
        if (row(i) >= 32 && row(i) < 127) sb.append(row(i).toChar) else sb.append('.')
        i += 1
      }
      while (i < (16 - row.length)) {
        sb.append('.')
        i += 1
      }
      offset += 16
      log(sb.toString)
      sb.clear()
    }
    log("-" * (16 * 4 + 5))
  }

  private def sendHttpPOST(target:String,fileName:Option[String],postContent:Array[Int],streaming:Boolean = false): Unit = {
    log(s"Opening connection[POST/${postContent.length} bytes ${fileName.getOrElse("")} ${if (streaming) " streaming" else ""}]: $target")
    try {
      val url = new URL(target)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("POST")
      connection.setInstanceFollowRedirects(true)
      connection.setRequestProperty("User-Agent","ESP32HTTPClient")
      if (fileName.isDefined)
        connection.setRequestProperty("Content-Type","multipart/form-data; boundary=--jamaica")
      connection.setDoOutput(true)
      val out = connection.getOutputStream
      val buf = postContent.map(_.toByte)
      fileName match {
        case Some(fn) =>
          out.write("----jamaica\r\n".getBytes)
          out.write(s"""Content-Disposition: form-data; name="imageFile"; filename="$fn"\r\n""".getBytes)
          out.write("Content-Type: application/octet-stream\r\n\r\n".getBytes)
        case None =>
      }
      out.write(buf)
      if (fileName.isDefined)
        out.write("\r\n----jamaica\r\n".getBytes)
      out.close()
      //connection.connect()
      val rcode = connection.getResponseCode
      log(s"HTTP code=$rcode")
      if (rcode != 200 && rcode != 201) {
        log(s"Returning error code for http result code: $rcode")
        sendMode = SENDING_MODE_WAITING_DUMMY
        buffer = "!0".toArray.map(_.toInt)
      }
      else {
        val in = connection.getInputStream
        if (streaming) {
          streamingIn = new BufferedInputStream(in)
          prepareResponse(null:Array[Int])
          return
        }

        var content = in.readAllBytes().map(_.toInt & 0xFF)
        log(s"HTTP POST response[${content.length}]:")
        dump(content)
        checkHttpResponse(rcode,content) match {
          case None =>
          case Some(code) => content = code.toArray.map(_.toInt)
        }
        in.close()
        if (target.endsWith(".prg")) {
          adjustBufferLenForPRG = 2
          log("Adjusting PRG size ...")
        }
        prepareResponse(content)
      }
    }
    catch {
      case io:Exception =>
        log(s"Http post I/O error: $io")
        prepareResponse("!0")
    }
  }

  private def sendHttpGET(target:String,streaming:Boolean = false): Unit = {
    log(s"Opening connection[GET${if (streaming) " streaming" else ""}]: $target")
    try {
      val url = new URL(target)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection.setInstanceFollowRedirects(true)
      connection.setRequestProperty("User-Agent","ESP32HTTPClient")
      connection.connect()
      val rcode = connection.getResponseCode
      log(s"HTTP code=$rcode")
      if (rcode != 200 && rcode != 201) {
        log(s"Returning error code for http result code: $rcode")
        prepareResponse("!0")
      }
      else {
        val in = connection.getInputStream
        if (streaming) {
          streamingIn = new BufferedInputStream(in)
          streamingSize = connection.getContentLength
          log(s"HTTP Streaming Content length: $streamingSize")
          prepareResponse(null:Array[Int])
          return
        }

        var content = in.readAllBytes().map(_.toInt & 0xFF)
        log(s"HTTP GET response[${content.length}]:")
        dump(content)
        checkHttpResponse(rcode,content) match {
          case None =>
          case Some(code) => content = code.toArray.map(_.toInt)
        }
        in.close()
        if (target.endsWith(".prg")) {
          adjustBufferLenForPRG = 2
          log("Adjusting PRG size ...")
        }
        prepareResponse(content)
      }
    }
    catch {
      case io:Exception =>
        log(s"Http get I/O error: $io")
        prepareResponse("!0")
    }
  }

  private def encodeURL(urlString:String): String = {
    urlString
  }

  private def en_code(s:String): String = {
    val sb = new StringBuilder
    var i = 0
    while (i < s.length) {
      if (s.charAt(i) == '<' && i + 1 < s.length && s.charAt(i + 1) == '$') {
        val len = s.charAt(i + 2).toInt + (s.charAt(i + 3).toInt << 8)
        for (j <- 0 until len) {
          val hex = s.charAt(i + j + 4).toInt
          if (hex <= 0xF) {
            sb += '0'
            sb.append(hex.toHexString)
          }
          else {
            sb.append(hex.toHexString)
          }
        }
        i += 4 + len
      }
      else {
        sb += s.charAt(i)
        i += 1
      }
    }
    sb.toString()
  }

  private def resolve(urlString:String): String = {
    if (urlString.nonEmpty) {
      var url = urlString
      if (url.charAt(0) == '!') url = server + urlString.substring(1)
      url.replaceAll("%mac", MAC_ADDRESS.replaceAll(":", "") + token).replaceAll("%ser", server)
    }
    else urlString
  }

  private def getMacAddress(): String = {
    import scala.jdk.CollectionConverters._
    val ni = if (networkif == null) NetworkInterface.getNetworkInterfaces.asScala.find(_.getHardwareAddress != null).map(_.getHardwareAddress)
    else Option(networkif.getHardwareAddress)

    ni match {
      case Some(addr) => addr.map(_.toInt & 0xFF).map(a => if (a > 0x0F) a.toHexString else s"0${a.toHexString}").mkString(":")
      case None => "N/A"
    }
  }

  def getIPAddress(): String = {
    import scala.jdk.CollectionConverters._
    if (networkif == null) InetAddress.getLocalHost.getHostAddress
    else {
      networkif.getInetAddresses.asScala.toList.headOption match {
        case None => "0.0.0.0"
        case Some(n) => n.getHostAddress
      }
    }
  }
  private def executeCmd(): Unit = {
    totalCmdIssued += 1

    // TO BE CHECKED
    if (streamingIn != null) {
      streamingIn.close()
      streamingIn = null
    }

    adjustBufferLenForPRG = 0
    recMode = RECEIVING_MODE_WAITING_W
    responseBuffer = null
    log(s"CMD=[${recCmd.toHexString} ${CMD_DESCR.getOrElse(recCmd,"???")}]: ${if (bufferLen == 0) "EMPTY" else ""}")
    if (bufferLen > 0) dump(buffer)
    recCmd match {
      case 0 => // GET FIRMWARE VERSION
        prepareResponse(FIRMWARE_VERSION)
      case 1 => // LOAD HTTP / HTTPS
        //background {
          sendHttpGET(encodeURL(resolve(buffer.map(_.toChar).mkString)))
        //}
      case 2 => // CONFIG WIFI
        log(s"CONFIG WIFI: ${buffer.map(_.toChar).mkString}")
        prepareResponse("Wlan config not changed")
      case 3|4|5 => // FIRMWARE UPDATE STANDARD/DEVELOPER/ESP32 DEVELOPER
        prepareResponse("OK")
      case 6 => // GET IP ADDRESS OF WIC64
        prepareResponse(getIPAddress())
      case 7 => // GET FIRMWARE VERSION STATUS
        prepareResponse(s"${ucesoft.cbm.Version.BUILD_DATE} ${ucesoft.cbm.Version.VERSION}")
      case 8 => // SET DEFAULT SERVER
        server = String.valueOf(buffer.map(_.toChar))
        prefMap.setProperty("server",server)
        log(s"SERVER = $server")
      case 9 => // REM COMMAND FOR DEVELOPER CONSOLE
        log(s"WiC64 Serial: ${buffer.map(_.toChar).mkString}")
      case 0xA => // GET UDP PACKAGE
        receiveUDP()
      case 0xB => // SEND UDP PACKAGE
        if (buffer.length > 4) {
          val ip = s"${buffer(0)}.${buffer(1)}.${buffer(2)}.${buffer(3)}"
          val data = buffer.drop(4)
          log(s"UDP SEND to $ip data:")
          dump(data)
          val ipAddress = Array(buffer(0).toByte,buffer(1).toByte,buffer(2).toByte,buffer(3).toByte)
          sendUDP(ipAddress, udpPort, data)
        }
      case 0xC => // WLAN SCAN SSID
        val scan = s"0\u0001${SSID}\u0001255\u0001"
        prepareResponse(scan)
      case 0xD => // CONFIG WLAN SCAN SSID
        log(s"CONFIG WIFI (SCAN): ${buffer.map(_.toChar).mkString}")
        prepareResponse("Wlan config not changed")
      case 0xE => // CHANGE UDP PORT
        if (buffer.length == 2) {
          udpPort = buffer(0) | buffer(1) << 8
          try {
            checkUDP()
            log(s"UDP PORT set to $udpPort")
          }
          catch {
            case e:Exception =>
              log(s"Error while creating UDP: $e")
              udp = null
          }
        }
      case 0xF => // HTTP STRING CONVERSION FOR HTTP CHAT
        //background {
          sendHttpGET(encodeURL(resolve(en_code(buffer.map(_.toChar).mkString))))
        //}
      case 0x10 => // GET ACTUAL CONNECTED SSID
        prepareResponse(SSID)
      case 0x11 => // GET ACTUAL WIFI SIGNAL RSSI
        prepareResponse("255")
      case 0x12 => // GET DEFAULT SERVER
        prepareResponse(server)
      case 0x13 => // GET EXTERNAL IP OF WIC64 INTERNET CONNECTION
        //background {
          sendHttpGET("http://sk.sx-64.de/wic64/ip.php")
        //}
      case 0x14 => // GET MAC ADDRESS OF WIC64
        prepareResponse(MAC_ADDRESS)
      case 0x15 => // GET TIME AND DATE FROM NTP SERVER
        prepareResponse(dateFormatter.format(new java.util.Date))
      case 0x16 => // SET TIMEZONE OF NTP SERVER
        log(s"TIME ZONE: ${buffer.mkString(",")}")
        prepareResponse("Time zone set")
      case 0x17 => // GET TIMEZONE OF NTP SERVER
        prepareResponse(Array(2,0))
      case 0x18 => // CHECK IF DEVELOPER/STANDARD FIRMWARE UPDATE IS AVAILABLE
        prepareResponse(Array('0'.toInt))
      case 0x19 => // READ PREFS STRING FROM EEPROM
        // TODO CHECK IF MUST BE SKIPPED SEPARATOR (1)
        val prefName = buffer.map(_.toChar).mkString
        val value = prefMap.getProperty(prefName,"")
        log(s"READING PREF $prefName = '$value'")
        prepareResponse(value)
      case 0x1A => // SAVE PREFS STRING TO EEPROM
        val pref = buffer.map(_.toChar).mkString
        pref.split("\u00001") match {
          case Array(_,name,value,retVal) =>
            log(s"WRITING PREF $name = '$value")
            prefMap.setProperty(name,value)
            prepareResponse(retVal)
          case _ =>
            log(s"WRITING PREF ERROR: $pref")
        }
      // case 0x1E => // GET TCP
      // case 0x1F => // SEND TCP
      // case 0x20 => // SET TCP PORT
      case 0x21 => // CONNECT TO SERVER:PORT VIA TCP (TELNET)
        //background {
          if (telnetClient != null && telnetClient.isConnected) telnetClient.disconnect()
          telnetClient = new TelnetClient
          telnetClient.setDefaultTimeout(5000)
          val (h, p) = buffer.map(_.toChar).mkString.split(":") match {
            case Array(h,p) => (h,p)
            case Array(h) => (h,"23")
          }
          try {
            telnetClient.connect(h, p.toInt)
            prepareResponse("0")
          }
          catch {
            case e: Exception =>
              log(s"Telnet error: $e")
              prepareResponse("!E")
          }
        //}
      case 0x22 => // GET DATA FROM TCP CONNECTION
        try {
          val in = telnetClient.getInputStream
          var av = in.available()
          if (av > 0xFFFF) av = 0xFFFF
          val bf = Array.ofDim[Byte](av)
          in.read(bf)
          prepareResponse(bf.map(_.toInt & 0xFF))
        }
        catch {
          case e: Exception =>
            e.printStackTrace()
            log(s"Telnet error: $e")
            prepareResponse(Array[Int]())
        }
      case 0x23 =>
        val out = telnetClient.getOutputStream
        val bf = buffer.map(_.toByte)
        try {
          out.write(bf)
          out.flush()
          prepareResponse("0")
        }
        catch {
          case e: Exception =>
            log(s"Telnet error: $e")
            prepareResponse("!E")
        }
      case 0x24 => // HTTP POST
        // find url
        var i = 0
        var found = false
        val url = new StringBuilder
        while (i < buffer.length - 1 && !found) {
          if (buffer(i) == 1 && buffer(i + 1) == 0) found = true
          else {
            url.append(buffer(i).toChar)
            i += 1
          }
        }
        if (!found) {
          prepareResponse("!0")
        }
        else {
          val postContent = buffer.drop(i + 1)
          // extract filename
          val mark = url.indexOf('?')
          val (fn,target) = mark match {
            case -1 =>
              (None,url.toString())
            case mark =>
              (Some(url.substring(mark + 1)),url.substring(0,mark))
          }
          //background {
            sendHttpPOST(target, fn, postContent) // http://sk.sx-64.de:80/up/up.php http://10.42.145.148:8000
          //}
        }
      case 0x25 => // STREAMING
        //background {
          sendHttpGET(encodeURL(resolve(buffer.map(_.toChar).mkString)), true)
        //}
      case 99 =>
        reset
      case _ =>
        log(s"Command ${recCmd.toHexString} not implemented")
    }

    if (listener != null) listener.onCMD(CMD_DESCR.getOrElse(recCmd,"???"))
  }

  def read(): Int = {
    if (mode != SENDING_MODE) {
      //println("NOT IN SENDING MODE ...")
      return 0
    }

    var rd = 0

    if (responseBuffer != null || streamingIn != null) {
      sendMode match {
        case SENDING_MODE_WAITING_DUMMY =>
          if (streamingIn != null) sendMode = SENDING_MODE_STREAMING else sendMode = SENDING_MODE_HI
          readIndex = 0
          flag2Action()
        case SENDING_MODE_HI =>
          sendMode = SENDING_MODE_LO
          rd = (responseBuffer.length - adjustBufferLenForPRG) >> 8
          flag2Action()
        case SENDING_MODE_LO =>
          rd = (responseBuffer.length - adjustBufferLenForPRG) & 0xFF
          if (responseBuffer.length > 0) {
            sendMode = SENDING_MODE_WAITING_END
            flag2Action()
          }
          else {
            sendMode = SENDING_MODE_WAITING_DUMMY
            responseBuffer = null
          }
        case SENDING_MODE_WAITING_END =>
          if (readIndex < responseBuffer.length) {
            rd = responseBuffer(readIndex)
            //println(s"READ $readIndex/${responseBuffer.length - 1}")
            readIndex += 1
            if (readIndex == responseBuffer.length) {
              sendMode = SENDING_MODE_WAITING_DUMMY
              responseBuffer = null
            }
            else flag2Action()
          }
        case SENDING_MODE_STREAMING =>
          rd = streamingIn.read()
          streamingSize -= 1
          if (streamingSize == 0) {
            streamingIn.close()
            streamingIn = null
            responseBuffer = null
            sendMode = SENDING_MODE_WAITING_DUMMY
            //rd = 0
          }
          flag2Action()
      }
    }

    rd

  }

  private def checkUDP(): DatagramSocket = {
    if (udp == null || udp.getPort != udpPort) {
      if (udpThread != null) {
        udpThreadRunning = false
        udp.close()
        udpThreadShutdown.await()
        udpThreadShutdown = new CountDownLatch(1)
      }

      if (udp != null) udp.close()
      udp = new DatagramSocket(udpPort)

      udpThread = new Thread(this,"WiC64UDP")
      udpThread.start()
    }

    udp
  }

  private def sendUDP(ip: Array[Byte], port: Int, data:Array[Int]): Unit = {
    val packet = new DatagramPacket(data.map(_.toByte),data.length,InetAddress.getByAddress(ip),port)
    try {
      checkUDP()
      udp.send(packet)
    }
    catch {
      case e:Exception =>
        log(s"UDP: can't send packet to ${ip.map(_.toInt & 0xFF).mkString(".")}:$udpPort : $e")
    }
  }

  private def receiveUDP(): Unit = {
    val head = udpReceivedQueue.poll()
    if (head != null) {
      val buf = Array.ofDim[Int](4 + head.data.length)
      var i = 0
      while (i < 4) {
        buf(i) = head.from(i).toInt & 0xFF
        i += 1
      }
      i = 0
      while (i < head.data.length) {
        buf(4 + i) = head.data(i).toInt & 0xFF
        i += 1
      }
      //log(s"UDP received from ${head.from.map(_.toInt & 0xFF).mkString(".")} : ${head.data.map(_.toInt & 0xFF).mkString(" ")}")
      log(s"UDP received from ${head.from.map(_.toInt & 0xFF).mkString(".")}:")
      dump(head.data.map(_.toInt & 0xFF))
      prepareResponse(buf)
    }
    else {
      prepareResponse(Array[Int]())
    }
  }

  override def run(): Unit = {
    log("UDP Thread started")
    val buffer = Array.ofDim[Byte](1024)
    val packet = new DatagramPacket(buffer,0,buffer.length)
    udpThreadRunning = true
    while (udpThreadRunning) {
      try {
        val socket: Unit = udp.receive(packet)
        val data = Array.ofDim[Byte](packet.getLength)
        System.arraycopy(packet.getData, 0, data, 0, data.length)
        if (!udpReceivedQueue.offer(UDPPacket(packet.getAddress.getAddress, data))) {
          log(s"UDP queue overflow: ${udpReceivedQueue.size()}")
        }
      }
      catch {
        case t:SocketException =>
          udpThreadRunning = false
          log(s"Unexpected error while receiving data: $t")
      }
    }
    udpThread = null
    udpThreadShutdown.countDown()
    log("UPD Thread stopped")
  }

  override def init: Unit = {}

  override protected def saveState(out: ObjectOutputStream): Unit = {}

  override protected def loadState(in: ObjectInputStream): Unit = {}

  override protected def allowsStateRestoring: Boolean = false
}
