package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.peripheral.bus.IECBus
import ucesoft.cbm.peripheral.bus.BusDataIterator
import java.io.File
import ucesoft.cbm.formats.D64
import java.net.InetAddress
import scala.collection.mutable.ListBuffer
import java.io.InputStream
import java.io.OutputStream
import java.net.HttpURLConnection
import java.io.IOException
import java.net.Socket
import ucesoft.cbm.Log
import java.net.ServerSocket
import java.net.URL
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.net.URLEncoder

class FlyerIEC(bus: IECBus,attachDrive: (File) => Unit) extends AbstractDrive(bus, 7) {
  val componentID = "FlyerIEC"
  override val busid = "FlyerIEC"
  
  private class ConnectionDataIterator extends BusDataIterator {
    private val queue = new collection.mutable.Queue[Int]
    def enqueue(e:Int) { queue.enqueue(e) }
    def isLast = queue.size == 1
    def getPerc = 0
    def goto(pos:Int) { throw new IllegalArgumentException }
    def hasNext = queue.size > 0
    def next = queue.dequeue
    def queueSize = queue.size
  }
  
  private class ClientConnection(val channel:Int) {
    private[this] var in : Option[InputStream] = None
    private[this] var out : Option[OutputStream] = None
    private[this] var httpConnection : Option[HttpURLConnection] = None
    private[this] var remoteIPAndPort : Option[(String,Int)] = None
    private[this] var listener : Option[Thread] = None
    private[this] var httpGenericDataWritten = false
    private[this] var httpFirstPost = true
    
    private class ThreadListener extends Thread(s"Flyer-Thread-$channel") {
      override def run {
        try {
          var closed = false
          while (!isInterrupted && !closed) {
            val r = in.get.read
            if (r != -1) {
              if (!channels(channel).dataToSend.isDefined) channels(channel).dataToSend = Some(new ConnectionDataIterator)
              channels(channel).dataToSend.get.asInstanceOf[ConnectionDataIterator].enqueue(r)
            }
            else closed = true
          }
        }
        catch {
          case _:InterruptedException =>
          case io:IOException =>
            sendStatus(STATUS_CONNECTION_ERROR)
        }
        in = None
        out = None
      }
    }
    
    private def fromPetASCII(s:String) : String = {
      s map { c => 
        if (c >= 65 && c <= 90) c.toLower
        else
        if (c >= 129 && c <= 154) c.toUpper
        else c
      }
    }
    
    private def listen(port:Int) {
      if (isConnected) close
      
      Log.info(s"Flyer: listening on $port ...")
      try {
        val ss = new ServerSocket(port)
        listener = Some(new Thread {
          override def run {
            try {
              val socket = ss.accept
              ss.close
              in = Some(socket.getInputStream)
              out = Some(socket.getOutputStream)
              Log.info(s"Flyer: connection accepted from ${ss.getInetAddress.getHostName}")
              remoteIPAndPort = Some((socket.getInetAddress.getHostAddress,socket.getPort))
              listener = Some(new ThreadListener)
              listener.get.start
            }
            catch {
              case _:InterruptedException =>
              case io:IOException =>
                Log.info("Flyer connection error: " + io)
                sendStatus(STATUS_CONNECTION_ERROR)
            }
          }
        })
        listener.get.start
      }
      catch {
        case io:IOException =>
          Log.info("Flyer connection error: " + io)
          sendStatus(STATUS_CONNECTION_ERROR)
      }
    }
    
    private def httpLoad(disk:Option[String]) {
      val buffer = Array.ofDim[Byte](1024)
      val loadBuffer = new ListBuffer[Int]
      var read = in.get.read(buffer)
      while (read != -1) {
        for(i <- 0 until read) loadBuffer += buffer(i)
        read = in.get.read(buffer)
      }
      disk match {
        case None =>
          channels(0).dataToSend = Some(new BusDataIterator.ArrayIntDataIterator(loadBuffer.toArray))
        case Some(d) =>
          val name = d.split("=")
          if (name.length == 2) {
            val file = new BufferedOutputStream(new FileOutputStream(new File(floppyRepository,name(1))))
            for(i <- 0 until loadBuffer.length) file.write(loadBuffer(i))
            file.close
            channels(0).dataToSend = Some(loadLocalDiskDirectory)
          }
          else sendStatus(STATUS_SYNTAX_ERROR)
      }      
      in.get.close
      in = None
    }
    
    private def http(httpUrl:String) {
      Log.info(s"Flyer: http connecting to $httpUrl ...")
      try {
        val urlAndDisk = httpUrl.split(",")
        val url = new URL(s"http://${urlAndDisk(0)}")
        val conn = url.openConnection.asInstanceOf[HttpURLConnection]
        //conn.connect        
        if (channel == 0) {
          conn.connect
          in = Some(conn.getInputStream)
          httpLoad(if (urlAndDisk.length == 2) Some(urlAndDisk(1)) else None)
        }
        else {
          httpConnection = Some(conn)          
        }
      }
      catch {
        case io:IOException =>
          Log.info("Flyer http connection error: " + io)
          sendStatus(STATUS_CONNECTION_ERROR)
      }
    }
    
    def connect(url:String) {
      if (isConnected) close
      
      Log.info(s"Flyer: connecting to ${url} ...")
      val protocol = url.split(":")
      
      protocol(0).toUpperCase match {
        case "HTTP" =>
          http(fromPetASCII(protocol(1)))
        case "LISTEN" =>
          listen(protocol(1).toInt)
        case "TCP" =>
          try {
            val addressPort = protocol(1).split(",")
            if (addressPort.length != 2) {
              sendStatus(STATUS_SYNTAX_ERROR)
              return
            }            
            val socket = new Socket(addressPort(0),addressPort(1).toInt)
            in = Some(socket.getInputStream)
            out = Some(socket.getOutputStream)
            sendStatus(STATUS_OK)
            listener = Some(new ThreadListener)
            listener.get.start
          }
          catch {
            case io:Throwable =>
              Log.info("Flyer connection error: " + io)
              sendStatus(STATUS_CONNECTION_ERROR)
          }
        case _ =>
          channel match {
            case 0 =>
              if (protocol(0) == "$$") channels(0).dataToSend = Some(loadLocalDiskDirectory)
              else setStatus(STATUS_SYNTAX_ERROR)
            case 1 =>
              setStatus(STATUS_SYNTAX_ERROR)
            case _ =>
              setStatus(STATUS_SYNTAX_ERROR)
          }          
      }
    }
    def available : Int = in match {
      case None => -1
      case Some(in) => //in.available
        channels(channel).dataToSend match {
          case Some(bi:ConnectionDataIterator) => bi.queueSize            
          case _ => 0
        }
    }
    
    def isConnected = in.isDefined
    def close {
      in foreach { _.close }
      out foreach { _.close }
      listener foreach { _.interrupt }
      httpConnection = None
      in = None
      out = None
      listener = None
      remoteIPAndPort = None
      httpGenericDataWritten = false
    }
    def write {
      httpConnection match {
        case None =>
          out match {
            case Some(out) =>
              val buffer = channels(channel).buffer
              for(i <- 0 until buffer.length) out.write(buffer(i))
              channels(channel).buffer.clear
            case None =>
          }
        case Some(hc) =>
          out match {
            case None =>
              hc.setDoOutput(true)
              try {
                hc.connect
                out = Some(hc.getOutputStream)
              }
              catch {
                case io:IOException =>
                  Log.info(s"Flyer: error while connecting: $io")
                  setStatus(STATUS_PROTOCOL_ERROR)
              }
            case Some(_) =>              
          }
          if (!httpGenericDataWritten) {
            httpGenericDataWritten = true
            out.get.write("filename=data.bin\n".getBytes)
            out.get.write("data=".getBytes)
          }
          val buffer = channels(channel).buffer
          for(i <- 0 until buffer.length) out.get.write(buffer(i))
          channels(channel).buffer.clear
      }      
    }
    def netstat {
      setStatus(STATUS_OK)
      in match {
        case Some(_) =>          
          status_s = 1
          status_t = available // ??          
        case None =>
          status_s = 0
          status_t = -1 // ??
      }
      //println(s"netstat on channel $channel => conn = $status_s avail = $status_t")
      //sendStatus
    }
    def netavail {
      setStatus(STATUS_OK)
      status_t = available
      status_s = 0
      //println(s"netavail on channel $channel = $status_t")
      //sendStatus
    }
    def connectioninfo {
      setStatus(STATUS_OK)
      remoteIPAndPort match {
        case Some((ip,port)) =>
          statusDescr = Some(ip)
          status_t = port
          status_s = if (isConnected) 1 else 0
        case None =>
          statusDescr = Some("")
          status_t = -1
          status_s = 0
      }
    }
    def httptransact {
      httpConnection match {
        case Some(hc) =>
          in match {
            case None =>
              out match {
                case Some(out) =>
                  out.flush
                  out.close
                case None =>                                  
              }
              try {
                hc.connect
                in = Some(hc.getInputStream)
                listener = Some(new ThreadListener)
                listener.get.start
              }
              catch {
                case io:IOException =>
                  Log.info(s"Flyer: error while connecting: $io")
                  setStatus(STATUS_PROTOCOL_ERROR)
              }
            case Some(_) =>
              setStatus(STATUS_PROTOCOL_ERROR)
          }
        case None =>
          setStatus(STATUS_PROTOCOL_ERROR)
      }
    }
    def httppost(post:String) {
      httpConnection match {
        case Some(hc) =>
          out match {
            case None =>
              try {
                hc.setDoOutput(true)
                hc.connect
                out = Some(hc.getOutputStream)
                httpFirstPost = true
              }
              catch {
                case io:IOException =>
                  Log.info(s"Flyer: error while connecting: $io")
                  setStatus(STATUS_PROTOCOL_ERROR)
              }              
            case Some(_) =>              
          }
          val kv = post.split("=")          
          if (kv.length == 2) {
            val kve = kv map { x => URLEncoder.encode(fromPetASCII(x),"UTF-8") }
            var toPost = kve(0) + "=" + kve(1)
            if (httpFirstPost) httpFirstPost = false
            else toPost = "&" + toPost
            out.get.write(toPost.getBytes)
          } 
        case None =>
          setStatus(STATUS_PROTOCOL_ERROR)
      }
    }
  }

  final private[this] val FIRMWARE_VERSION = "1.2.1"
  final private[this] val STATUS_OK = 0
  final private[this] val STATUS_FILE_NOT_FOUND = 62
  final private[this] val STATUS_SYNTAX_ERROR = 30
  final private[this] val STATUS_PROTOCOL_ERROR = 1
  final private[this] val STATUS_CONNECTION_ERROR = 2

  protected val ERROR_CODES = Map(
    STATUS_OK -> "OK",
    STATUS_SYNTAX_ERROR -> "SYNTAX ERROR",
    STATUS_FILE_NOT_FOUND -> "FILE NOT FOUND",
    STATUS_PROTOCOL_ERROR -> "PROTOCOL ERROR",
    STATUS_CONNECTION_ERROR -> "CONNECTION ERROR"
    )

  private[this] var statusDescr : Option[String] = None
  private[this] var status_t, status_s = 0
  private[this] var floppyRepository : File = new File("./")
  private[this] val IP = InetAddress.getLocalHost.getHostAddress
  private[this] val connections : Array[ClientConnection] = {
    val cs = Array.ofDim[ClientConnection](15)
    for(i <- 0 until 15) cs(i) = new ClientConnection(i)
    cs
  }

  // register itself to bus
  bus.registerListener(this)
  //sendStatus(STATUS_OK)
  
  def setFloppyRepository(repository:File) = floppyRepository = repository
  def getFloppyRepository = floppyRepository
  
  def reset {
    status_t = 0
    status_s = 0
    statusDescr = None
    sendStatus(STATUS_OK)
    connections foreach { _.close }
  }

  override protected def sendStatus {
    import BusDataIterator._
    channels(15).dataToSend = Some(new StringDataIterator("%02d,%s,%02d,%02d".format(status,statusDescr.getOrElse(ERROR_CODES(status)), status_t, status_s) + 13.toChar))
    statusDescr = None
  }
  
  private def sendStatus(code:Int) {
    setStatus(code)
    sendStatus
  }

  protected def getDirectoryEntries(path: String): List[DirEntry] = Nil

  protected def loadData(fileName: String) : Option[BusDataIterator] = None
  
  protected def loadLocalDiskDirectory : BusDataIterator = {
    val disks = listDisks
    
    val out = new ListBuffer[Int]
    // set start address to $0801
    out.append(0x01)
    out.append(0x08)
    var ptr = 0x801    
    // write next line address
    ptr += 30
    out.append(ptr & 0xFF)  // L
    out.append(ptr >> 8)  // H
    // write label
    out.append(0) // drive L
    out.append(0) // drive H
    out.append(0x12) // RVS ON
    out.append(0x22) // "
    val dir = "LOCAL DISKS"
    for(i <- 0 until 16) {
      if (i < dir.length) out.append(dir.charAt(i)) else out.append(0x20)
    }
    out.append(0x22) // "
    out.append(0x20)
    out.append(54)
    out.append(52)
    out.append(0x20)
    out.append(48)
    out.append(48)
    out.append(0x00)  // EOL
    var idx = 0
    while (idx < disks.length) {
      val fileName = disks(idx).take(16)
      val sizeInSector = idx + 1
      val blanks = if (sizeInSector < 10) 1 else 0
      // write next line address
      ptr += blanks + 2 + 2 + 18 + 5
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      out.append(ptr & 0xFF)  // L
      out.append(ptr >> 8)  // H
      // write blocks
      out.append(sizeInSector & 0xFF)
      out.append(sizeInSector >> 8)
      // blanks after blocks      
      for(i <- 1 to blanks) out.append(0x20)
      out.append(0x22) // "
      for(i <- 0 until fileName.length) out.append(fileName.charAt(i))
      out.append(0x22) // "
      for(i <- 1 to 16 - fileName.length) out.append(0x20)
      out.append(0x20) // "
      val fileType = "D64"
      for(i <- 0 until fileType.length) out.append(fileType.charAt(i))
      for(i <- 1 to endBlanks) out.append(0x20)
      out.append(0x00) // EOL
      idx += 1
    }
    
    out.append(0x00)
    out.append(0x00)
    import BusDataIterator._
    new ArrayIntDataIterator(out.toArray)
  }
  
  override protected def setStatus(code: Int) {
    super.setStatus(code)
    if (code == STATUS_OK) {
      status_t = 0
      status_s = 0
    }
  }
  
  override def unlisten {}
  
  override protected def fileNameReceived {
    super.fileNameReceived
    if (channel == 15) handleChannel15 else openChannel
  }

  override protected def untalk {
    resetSignals
  }

  override def open_channel {
    super.open_channel
    channels(channel).open
    //if (channel == 15 && channels(channel).fileName.length == 0) sendStatus
  }
  
  override def close {
    super.close
    if (channel > 1 && channel < 15) connections(channel).close
  }
  
  override def byteJustWritten(isLast: Boolean) {
    super.byteJustWritten(isLast)    
//    println(s"WRITE BYTE ON $channel")
  }
  
  override protected def byteJustRead(byte:Int,isLast:Boolean) {
    super.byteJustRead(byte, isLast)
    if (channel == 15 && channels(channel).fileName.length > 0) channels(channel).fileName.clear
    if (isLast) {
      if (channel == 15) handleChannel15 else writeOnChannel
    }
  }
  
  private def matchesWith(cmd:String,cmds:String*) : Boolean = cmds.exists { _ == cmd }  
  
  protected def handleChannel15 {
    val cmd = if (channels(channel).fileName.length > 0) channels(channel).fileName.toString else channels(channel).bufferToString
    val command = (if (cmd.charAt(cmd.length - 1) == 13) cmd.substring(0, cmd.length - 1) else cmd).split(":")
    try {
      //println(s"CMD '${command.mkString(":")}'")
      // DISK MANAGEMENT
      if (matchesWith(command(0),"DISK-COUNT","D-C")) diskCountCommand
      else
      if (matchesWith(command(0),"DISK-QUERY","D-Q") && command.length == 2) diskQuery(command(1).trim)
      else
      if (matchesWith(command(0),"DISK-SCRATCH","D-S") && command.length == 2) diskScratch(command(1).trim)
      else
      if (matchesWith(command(0),"DISK-RELABEL","D-R") && command.length == 2) diskRelabel(command(1).trim)
      else
      if (matchesWith(command(0),"DISK-ADD","D-A") && command.length == 2) diskAdd(command(1).trim)
      else
      if (matchesWith(command(0),"DISK-MOUNT","D-M") && command.length == 2) diskMount(command(1).trim)
      else
      if (matchesWith(command(0),"DISK-WIPE","D-W") && command.length == 1) diskWipe
      else
      // CONFIGURATION MANAGEMENT
      if (matchesWith(command(0),"CONFIG","C","STATUS","S") && command.length == 2) configure(command(1).trim)
      else
      // NETWORK MANAGEMENT
      if (matchesWith(command(0),"NET-AVAIL","N-A") && command.length == 2) netavail(command(1).trim)
      else
      if (matchesWith(command(0),"NET-STAT","N-S") && command.length == 2) netstat(command(1).trim)
      else
      if (matchesWith(command(0),"CONNECTION-INFO","C-I") && command.length == 2) connectioninfo(command(1).trim)
      else
      if (matchesWith(command(0),"HTTP-TRANSACT","H-T") && command.length == 2) httptransact(command(1).trim)
      else
      if (matchesWith(command(0),"HTTP-POST","H-P") && command.length == 2) httppost(command(1).trim)
      else
      setStatus(STATUS_SYNTAX_ERROR)
    }
    finally {
      channels(channel).clear
      sendStatus
    }
  }
  
  private def listDisks : Array[String] = {
    floppyRepository.listFiles filter { f => f.getName.endsWith(".d64") || f.getName.endsWith(".D64") } map { name => name.getName.substring(0,name.getName.lastIndexOf(".")) }
  }
  
  private def httppost(ch:String) {
    val channelAndPost = ch.split(",")
    if (channelAndPost.length != 2) {
      setStatus(STATUS_SYNTAX_ERROR)
      return
    }
    val channel = try {
      channelAndPost(0).toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    connections(channel).httppost(channelAndPost(1))
  }
  
  private def httptransact(ch:String) {
    val channel = try {
      ch.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    connections(channel).httptransact
  }
  
  private def connectioninfo(ch:String) {
    val channel = try {
      ch.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    connections(channel).connectioninfo
  }
  
  private def netstat(ch:String) {
    val channel = try {
      ch.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    connections(channel).netstat
  }
  
  private def netavail(ch:String) {
    val channel = try {
      ch.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    connections(channel).netavail
  }
  
  private def configure(setting:String) {
    val setPar = setting.split("=")
    val write = setPar.length == 2
    
    if (matchesWith(setPar(0),"DI")) {  // DRIVE ID
      if (!write) statusDescr = Some("8")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"JIFFYDOS","JI")) {  // JIFFY DOS
      if (!write) statusDescr = Some("0")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"IP")) {  // IP
      if (!write) statusDescr = Some(IP)
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"SUBNETMASK","SU")) {  // SUBNETMASK
      if (!write) statusDescr = Some("255.255.255.0")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"GATEWAY","GA")) {  // GATEWAY
      if (!write) statusDescr = Some("192.168.1.1")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"DNS","DN")) {  // DNS
      if (!write) statusDescr = Some("8.8.8.8")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"FIRMWARESERVER","FI")) {  // FIRMWARESERVER
      if (!write) statusDescr = Some("RETROSWITCH%2ECOM")
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"FIRMWARE")) {  // FIRMWARE VERSION
      if (!write) statusDescr = Some(FIRMWARE_VERSION)
      setStatus(STATUS_OK)
    }
    else
    if (matchesWith(setPar(0),"DHCP","DH")) {  // DHCP
      if (!write) statusDescr = Some("0")
      setStatus(STATUS_OK)
    }
  }
  
  private def diskWipe {
    floppyRepository.listFiles map { _.delete }
    setStatus(STATUS_OK)
  }
  
  private def diskMount(index:String) {
    val idx = try {
      index.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    val disks = floppyRepository.listFiles
    if (idx > disks.length) {
      setStatus(STATUS_SYNTAX_ERROR)
      return
    }
    setStatus(STATUS_OK)
    attachDrive(disks(idx - 1))
  }
  
  private def diskAdd(disk:String) {
    val typeName = disk.split(",")
    if (typeName.length != 2) {
      setStatus(STATUS_SYNTAX_ERROR)
      return
    }
    val newDisk = new File(floppyRepository,typeName(1) + ".d64")
    val emptyD64 = new D64(newDisk.toString,true)
    emptyD64.format(s"N:FLYER,00")
    emptyD64.close
  }
  
  private def diskRelabel(indexRen:String) {
    val indexName = indexRen.split("=")
    if (indexName.length != 2) {
      setStatus(STATUS_SYNTAX_ERROR)
      return
    }
    val idx = try {
      indexName(0).toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    val disks = floppyRepository.listFiles
    val diskToRename = disks(idx - 1)
    val newDisk = new File(diskToRename.getParentFile,indexName(1) + ".d64")
    diskToRename.renameTo(newDisk)
    setStatus(STATUS_OK)
  }
  
  private def diskScratch(index:String) {
    val idx = try {
      index.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    val disks = floppyRepository.listFiles
    if (idx > disks.length) {
      setStatus(STATUS_SYNTAX_ERROR)
      return
    }
    setStatus(STATUS_OK)
    disks(idx - 1).delete
  }
  
  private def diskQuery(index:String) {
    val idx = try {
      index.toInt
    }
    catch {
      case _:NumberFormatException =>
        setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    val disks = listDisks
    if (idx > disks.length) {
      setStatus(STATUS_SYNTAX_ERROR)
        return
    }
    setStatus(STATUS_OK)
    statusDescr = Some(disks(idx - 1))
    status_t = 0 // d64
  }
  
  private def diskCountCommand {
    setStatus(STATUS_OK)
    status_t = listDisks.length
  }  
  
  private def openChannel {
    //println(s"OPEN CHANNEL $channel with ${channels(channel).fileName}")
    channel match {
      //case 0 => load(channels(channel).fileName.toString)
      case _ =>
        connections(channel).connect(channels(channel).fileName.toString)
    }
  }
  
  private def writeOnChannel {
    //println(s"WRITING IN CHANNEL $channel ${channels(channel).bufferToString}")
    connections(channel).write
  }
}