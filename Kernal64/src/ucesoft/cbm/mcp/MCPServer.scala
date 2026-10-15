package ucesoft.cbm.mcp

import ujson._

import java.io.{BufferedReader, InputStreamReader, PrintStream}

/**
 * MCP server that communicates with an AI agent via stdin/stdout using JSON-RPC 2.0.
 * It forwards tool calls (ping, peek, poke) to the emulator's REST server.
 */
object MCPServer {

  private var restBase = s"http://localhost:8080"

  // ---- minimal HTTP helpers using JDK HttpURLConnection ----

  private def httpGet(url: String): String = {
    val conn = java.net.URI.create(url).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
    conn.setConnectTimeout(3000)
    conn.setReadTimeout(3000)
    conn.setRequestMethod("GET")
    val src = scala.io.Source.fromInputStream(conn.getInputStream)
    try src.mkString finally src.close()
  }

  private def httpPost(url: String, body: String): String = {
    val conn = java.net.URI.create(url).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
    conn.setConnectTimeout(3000)
    conn.setReadTimeout(3000)
    conn.setRequestMethod("POST")
    conn.setDoOutput(true)
    conn.setRequestProperty("Content-Type", "application/json")
    conn.getOutputStream.write(body.getBytes("UTF-8"))
    val src = scala.io.Source.fromInputStream(conn.getInputStream)
    try src.mkString finally src.close()
  }

  // ---- tool definitions advertised to the agent ----

  private val toolList = ujson.Arr(
    ujson.Obj(
      "name"        -> "ping",
      "description" -> "Check if the emulator is alive",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "getSystemDesc",
      "description" -> "Returns a description of the emulated system",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "getDisplayList",
      "description" -> "Returns the list of available displays as <name, description> pairs",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "selectDisplay",
      "description" -> "Selects the active display by its name",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("name" -> ujson.Obj("type" -> "string")),
        "required"   -> ujson.Arr("name")
      )
    ),
    ujson.Obj(
      "name"        -> "getCPUList",
      "description" -> "Returns the list of available CPUs as <name, description> pairs",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "selectCPU",
      "description" -> "Selects the active CPU type by its name",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("name" -> ujson.Obj("type" -> "string")),
        "required"   -> ujson.Arr("name")
      )
    ),
    ujson.Obj(
      "name"        -> "getRegisters",
      "description" -> "Returns the registers of the currently selected CPU as a list of {name, value, intValue}",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "disassemble",
      "description" -> "Disassembles a chunk of memory starting at the given address (0-65535) for the given length in bytes, using the selected CPU. Returns a string with the disassembly",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj(
          "address" -> ujson.Obj("type" -> "integer"),
          "length"  -> ujson.Obj("type" -> "integer")
        ),
        "required" -> ujson.Arr("address", "length")
      )
    ),
    ujson.Obj(
      "name"        -> "setCPUTraceMode",
      "description" -> "Enables or disables the step-by-step trace mode of the selected CPU",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("traceOn" -> ujson.Obj("type" -> "boolean")),
        "required"   -> ujson.Arr("traceOn")
      )
    ),
    ujson.Obj(
      "name"        -> "step",
      "description" -> "Executes a single step of the selected CPU and returns the current registers (pc, disassembled, registers)",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "getPerformance",
      "description" -> "Returns emulator performance: speedPercent (100=C64 original speed), fps (frames per second), totalFrames (total frames since start)",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "reset",
      "description" -> "Reset the emulator. hard=true for hard reset, hard=false for soft reset",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("hard" -> ujson.Obj("type" -> "boolean")),
        "required"   -> ujson.Arr("hard")
      )
    ),
    ujson.Obj(
      "name"        -> "loadPRG",
      "description" -> "Load a PRG file into the emulator given its path",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string")),
        "required"   -> ujson.Arr("path")
      )
    ),
    ujson.Obj(
      "name"        -> "takePicture",
      "description" -> "Take a screenshot of the emulator screen and save it as PNG to the given path",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string")),
        "required"   -> ujson.Arr("path")
      )
    ),
    ujson.Obj(
      "name"        -> "peek",
      "description" -> "Read N bytes from emulator memory starting at the given address (0-65535)",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj(
          "address" -> ujson.Obj("type" -> "integer"),
          "length"  -> ujson.Obj("type" -> "integer")
        ),
        "required" -> ujson.Arr("address", "length")
      )
    ),
    ujson.Obj(
      "name"        -> "typeText",
      "description" -> "Insert a string into the emulator keyboard buffer",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("text" -> ujson.Obj("type" -> "string")),
        "required"   -> ujson.Arr("text")
      )
    ),
    ujson.Obj(
      "name"        -> "isPaused",
      "description" -> "Returns whether the emulator is currently paused",
      "inputSchema" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj(), "required" -> ujson.Arr())
    ),
    ujson.Obj(
      "name"        -> "setPaused",
      "description" -> "Pause or resume the emulator",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj("paused" -> ujson.Obj("type" -> "boolean")),
        "required"   -> ujson.Arr("paused")
      )
    ),
    ujson.Obj(
      "name"        -> "poke",
      "description" -> "Write a list of bytes to emulator memory starting at the given address",
      "inputSchema" -> ujson.Obj(
        "type"       -> "object",
        "properties" -> ujson.Obj(
          "address" -> ujson.Obj("type" -> "integer"),
          "bytes"   -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("type" -> "integer"))
        ),
        "required" -> ujson.Arr("address", "bytes")
      )
    )
  )

  // ---- JSON-RPC helpers ----

  private def response(id: Value, result: Value): String =
    ujson.write(ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "result" -> result))

  private def errorResponse(id: Value, code: Int, message: String): String =
    ujson.write(ujson.Obj(
      "jsonrpc" -> "2.0",
      "id"      -> id,
      "error"   -> ujson.Obj("code" -> code, "message" -> message)
    ))

  private def send(out: PrintStream, msg: String): Unit = {
    // MCP stdio transport: newline-delimited JSON (one JSON-RPC message per line).
    // The message must not contain embedded newlines; ujson.write produces compact
    // single-line JSON, so it is safe to terminate with a single '\n'.
    out.print(msg)
    out.print('\n')
    out.flush()
  }

  // ---- request dispatch ----

  private def handle(req: ujson.Obj, out: PrintStream): Unit = {
    val id     = req.value.getOrElse("id", ujson.Null)
    val method = req("method").str

    method match {
      case "initialize" =>
        send(out, response(id, ujson.Obj(
          "protocolVersion" -> "2024-11-05",
          "capabilities"    -> ujson.Obj("tools" -> ujson.Obj()),
          "serverInfo"      -> ujson.Obj("name" -> "kernal64-mcp", "version" -> "1.0")
        )))

      case "notifications/initialized" => // no response needed

      case "tools/list" =>
        send(out, response(id, ujson.Obj("tools" -> toolList)))

      case "tools/call" =>
        val params   = req("params").obj
        val toolName = params("name").str
        val args     = params.getOrElse("arguments", ujson.Obj()).obj

        val resultJson = try { toolName match {
          case "ping" =>
            httpGet(s"$restBase/ping")
          case "getSystemDesc" =>
            httpGet(s"$restBase/getSystemDesc")
          case "getDisplayList" =>
            httpGet(s"$restBase/getDisplayList")
          case "selectDisplay" =>
            val name = args("name").str
            httpPost(s"$restBase/selectDisplay", ujson.write(ujson.Obj("name" -> name)))
          case "getCPUList" =>
            httpGet(s"$restBase/getCPUList")
          case "selectCPU" =>
            val name = args("name").str
            httpPost(s"$restBase/selectCPU", ujson.write(ujson.Obj("name" -> name)))
          case "getRegisters" =>
            httpGet(s"$restBase/getRegisters")
          case "disassemble" =>
            val address = args("address").num.toInt
            val length  = args("length").num.toInt
            httpGet(s"$restBase/disassemble/$address/$length")
          case "setCPUTraceMode" =>
            val traceOn = args("traceOn").bool
            httpPost(s"$restBase/setCPUTraceMode", ujson.write(ujson.Obj("traceOn" -> traceOn)))
          case "step" =>
            httpPost(s"$restBase/step", "{}")
          case "getPerformance" =>
            httpGet(s"$restBase/getPerformance")
          case "reset" =>
            val hard = args("hard").bool
            httpPost(s"$restBase/reset", ujson.write(ujson.Obj("hard" -> hard)))
          case "loadPRG" =>
            val path = args("path").str
            httpPost(s"$restBase/loadPRG", ujson.write(ujson.Obj("path" -> path)))
          case "takePicture" =>
            val path = args("path").str
            httpPost(s"$restBase/takePicture", ujson.write(ujson.Obj("path" -> path)))
          case "typeText" =>
            val text = args("text").str
            httpPost(s"$restBase/typeText", ujson.write(ujson.Obj("text" -> text)))
          case "isPaused" =>
            httpGet(s"$restBase/isPaused")
          case "setPaused" =>
            val paused = args("paused").bool
            httpPost(s"$restBase/setPaused", ujson.write(ujson.Obj("paused" -> paused)))
          case "peek" =>
            val address = args("address").num.toInt
            val length  = args("length").num.toInt
            httpGet(s"$restBase/peek/$address/$length")
          case "poke" =>
            val address = args("address").num.toInt
            val bytes   = args("bytes").arr.map(_.num.toInt).toList
            httpPost(s"$restBase/poke", ujson.write(ujson.Obj("address" -> address, "bytes" -> bytes)))
          case unknown =>
            send(out, errorResponse(id, -32601, s"Unknown tool: $unknown"))
            return
        }} catch { case e: Exception => ujson.write(ujson.Obj("error" -> e.getMessage)) }

        send(out, response(id, ujson.Obj(
          "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> resultJson))
        )))

      case unknown =>
        send(out, errorResponse(id, -32601, s"Method not found: $unknown"))
    }
  }

  // ---- stdin reader (newline-delimited JSON framing) ----

  private def readMessage(reader: BufferedReader): Option[String] = {
    // MCP stdio transport: each message is a single line of JSON terminated by '\n'.
    // Skip any blank lines that may appear between messages.
    var line = reader.readLine()
    while (line != null && line.trim.isEmpty)
      line = reader.readLine()
    Option(line)
  }

  // ---- main ----

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      // the argument is the emulator REST server endpoint as address:port
      // (e.g. localhost:8080); build the base URL from it
      restBase = s"http://${args(0)}"
    }

    val in  = new BufferedReader(new InputStreamReader(System.in,  "UTF-8"))
    val out = new PrintStream(System.out, true, "UTF-8")

    Iterator.continually(readMessage(in))
      .takeWhile(_.isDefined)
      .flatten
      .foreach { raw =>
        try handle(ujson.read(raw).obj, out)
        catch { case e: Exception => System.err.println(s"MCP error: ${e.getMessage}") }
      }
  }
}
