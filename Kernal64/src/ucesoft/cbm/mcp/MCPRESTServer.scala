package ucesoft.cbm.mcp

import ucesoft.cbm.Clock
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.trace.TraceListener
import ucesoft.cbm.trace.TraceListener.{CpuStepInfo, StepIn, TraceRegister}

import java.util.concurrent.{CountDownLatch, TimeUnit}

private class PauseTool(var getIsPaused:() => Boolean, var doSetPaused:Boolean => Unit)
private class KeyboardTool(var typeText:String => Unit)
private class LoadPRGTool(var loadPRG:String => Unit)
private class ResetTool(var reset:Boolean => Unit)
private class SystemDescTool(var getSystemDesc:() => String)

class DisplayInfo(val name:String, val description: String,val display:Display)
class CPUInfo(val name:String, val description: String,val cpu:TraceListener)

class MCPRESTServer private (clock:Clock,displayConfig: Array[DisplayInfo],cpuInfo:Array[CPUInfo],memory: Memory,_host:String,_port: Int) extends cask.MainRoutes {
  private var pauseTool   = new PauseTool(() => false, _ => {})
  private var keyboardTool = new KeyboardTool(_ => {})
  private var loadPRGTool = new LoadPRGTool(_ => {})
  private var resetTool = new ResetTool(_ => {})
  private var systemDescTool = new SystemDescTool(() => "N/A")
  private var display = displayConfig(0).display
  private var cpu = cpuInfo(0).cpu
  private var isTracing = false

  def configurePause(isPaused:() => Boolean, setPaused:Boolean => Unit): Unit =
    pauseTool = new PauseTool(isPaused, setPaused)

  def configureKeyboard(typeText: String => Unit): Unit =
    keyboardTool = new KeyboardTool(typeText)

  def configureLoadPRG(loadPRG: String => Unit): Unit =
    loadPRGTool = new LoadPRGTool(loadPRG)

  def configureReset(reset: Boolean => Unit): Unit =
    resetTool = new ResetTool(reset)

  def configureSystemDesc(getSystemDesc: () => String): Unit =
    systemDescTool = new SystemDescTool(getSystemDesc)

  override def host: String = _host
  override def port: Int = _port

  @cask.get("/ping")
  def ping(): ujson.Value = ujson.Obj("status" -> "ok")

  @cask.get("/peek/:address/:length")
  def peek(address: Int, length: Int): ujson.Value =
    if (address < 0 || address > 0xFFFF)
      ujson.Obj("error" -> s"address out of range: $address")
    else if (length < 1 || address + length > 0x10000)
      ujson.Obj("error" -> s"length out of range: $length")
    else
      ujson.Obj(
        "address" -> address,
        "bytes"   -> ujson.Arr((0 until length).map(i => ujson.Num(memory.read(address + i))): _*)
      )

  @cask.post("/poke")
  def poke(request: cask.Request): ujson.Value = {
    val json    = ujson.read(request.text())
    val address = json("address").num.toInt
    val bytes   = json("bytes").arr.map(_.num.toInt)
    if (address < 0 || address > 0xFFFF)
      ujson.Obj("error" -> s"address out of range: $address")
    else if (address + bytes.length > 0x10000)
      ujson.Obj("error" -> s"write exceeds memory bounds")
    else if (bytes.exists(v => v < 0 || v > 0xFF))
      ujson.Obj("error" -> s"one or more values out of range")
    else {
      bytes.zipWithIndex.foreach { case (v, i) => memory.write(address + i, v) }
      ujson.Obj("address" -> address, "bytes" -> bytes)
    }
  }

  @cask.post("/typeText")
  def typeText(request: cask.Request): ujson.Value = {
    val text = ujson.read(request.text())("text").str
    keyboardTool.typeText(text)
    ujson.Obj("text" -> text)
  }

  @cask.post("/loadPRG")
  def loadPRG(request: cask.Request): ujson.Value = {
    val path = ujson.read(request.text())("path").str
    val file = new java.io.File(path)
    if (!file.exists())
      ujson.Obj("error" -> s"file not found: $path")
    else {
      loadPRGTool.loadPRG(path)
      ujson.Obj("path" -> path)
    }
  }

  @cask.post("/reset")
  def doReset(request: cask.Request): ujson.Value = {
    val hard = ujson.read(request.text())("hard").bool
    resetTool.reset(hard)
    ujson.Obj("hard" -> hard)
  }

  @cask.get("/getPerformance")
  def getPerformance(): ujson.Value =
    ujson.Obj(
      "speedPercent"  -> clock.getLastPerformancePerc,
      "fps"           -> display.lastFramePerSecondCounter,
      "totalFrames"   -> display.getFrameCounter
    )

  @cask.post("/takePicture")
  def takePicture(request: cask.Request): ujson.Value = {
    val path = ujson.read(request.text())("path").str
    val file = new java.io.File(path)
    val paused = pauseTool.getIsPaused()
    pauseTool.doSetPaused(true)
    display.saveSnapshot(file)
    pauseTool.doSetPaused(paused)
    ujson.Obj("path" -> path)
  }

  @cask.get("/getSystemDesc")
  def getSystemDesc(): ujson.Value =
    ujson.Obj("description" -> systemDescTool.getSystemDesc())

  @cask.get("/getDisplayList")
  def getDisplayList(): ujson.Value =
    ujson.Arr(
      displayConfig.map(d => ujson.Obj("name" -> d.name, "description" -> d.description)).toIndexedSeq: _*
    )

  @cask.post("/selectDisplay")
  def selectDisplay(request: cask.Request): ujson.Value = {
    val name = ujson.read(request.text())("name").str
    displayConfig.find(_.name == name) match {
      case Some(info) =>
        display = info.display
        ujson.Obj("name" -> info.name, "description" -> info.description)
      case None =>
        ujson.Obj("error" -> s"display not found: $name")
    }
  }

  @cask.get("/getCPUList")
  def getCPUList(): ujson.Value =
    ujson.Arr(
      cpuInfo.map(c => ujson.Obj("name" -> c.name, "description" -> c.description)).toIndexedSeq: _*
    )

  @cask.post("/selectCPU")
  def selectCPU(request: cask.Request): ujson.Value = {
    val name = ujson.read(request.text())("name").str
    cpuInfo.find(_.name == name) match {
      case Some(info) =>
        cpu = info.cpu
        ujson.Obj("name" -> info.name, "description" -> info.description)
      case None =>
        ujson.Obj("error" -> s"cpu not found: $name")
    }
  }

  @cask.get("/getRegisters")
  def getRegisters(): ujson.Value =
    registersToJson(cpu.getRegisters())

  private def registersToJson(regs: List[TraceRegister]): ujson.Value =
    ujson.Arr(
      regs.map(r => ujson.Obj(
        "name"     -> r.name,
        "value"    -> r.value,
        "intValue" -> r.intValue
      )): _*
    )

  @cask.post("/step")
  def step(): ujson.Value = {
    if (!isTracing) {
      return ujson.Obj("error" -> "tracing mode is off. set tracing mode to true before stepping")
    }
    val latch = new CountDownLatch(1)
    @volatile var stepInfo: CpuStepInfo = null
    cpu.step(info => { stepInfo = info; latch.countDown() }, StepIn)
    if (!latch.await(5, TimeUnit.SECONDS) || stepInfo == null)
      ujson.Obj("error" -> "step timed out: the CPU did not advance (is the emulator running/paused appropriately?)")
    else
      ujson.Obj(
        "pc"           -> stepInfo.pc,
        "disassembled" -> stepInfo.disassembled,
        "registers"    -> registersToJson(stepInfo.registers)
      )
  }

  @cask.get("/disassemble/:address/:length")
  def disassemble(address: Int, length: Int): ujson.Value =
    if (address < 0 || address > 0xFFFF)
      ujson.Obj("error" -> s"address out of range: $address")
    else if (length < 1 || address + length > 0x10000)
      ujson.Obj("error" -> s"length out of range: $length")
    else {
      val sb  = new StringBuilder
      var pc  = address
      val end = address + length
      while (pc < end) {
        val info = cpu.disassemble(pc)
        sb.append(s"${info.dis}\n")
        pc += info.bytes
      }
      ujson.Obj(
        "address"      -> address,
        "length"       -> length,
        "disassembled" -> sb.toString
      )
    }

  @cask.post("/setCPUTraceMode")
  def setCPUTraceMode(request: cask.Request): ujson.Value = {
    val traceOn = ujson.read(request.text())("traceOn").bool
    cpu.setTrace(traceOn)
    //if (traceOn) {
      cpu.step(_ => {}, StepIn)
    //}
    isTracing = traceOn
    ujson.Obj("traceOn" -> traceOn)
  }

  @cask.get("/isPaused")
  def isPaused(): ujson.Value =
    ujson.Obj("paused" -> pauseTool.getIsPaused())

  @cask.post("/setPaused")
  def setPaused(request: cask.Request): ujson.Value = {
    val paused = ujson.read(request.text())("paused").bool
    pauseTool.doSetPaused(paused)
    ujson.Obj("paused" -> paused)
  }

  initialize()
  println(s"MCP server started on localhost and port $port")
}

object MCPRESTServer {
  def start(clock:Clock,displayConfig: Array[DisplayInfo],cpuInfo:Array[CPUInfo],memory: Memory,host:String = "localhost", port: Int = 8080): MCPRESTServer = {
    println(s"Starting MCP server on $host:$port ...")
    val server = new MCPRESTServer(clock,displayConfig,cpuInfo,memory,host, port)
    val t = new Thread(() => server.main(Array.empty), "MCP-REST-Server")
    t.setDaemon(true)
    t.start()
    server
  }
}
