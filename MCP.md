# Kernal64 MCP Server (starting from version 1.9.0)

## Overview

Kernal64 can expose its running emulator to an AI agent through the
[Model Context Protocol (MCP)](https://modelcontextprotocol.io/). This lets an
LLM-based agent inspect and drive the emulated Commodore machine
(C64, C128, SuperCPU C64, CBM-II, VIC-20) in real time: read/write memory,
read CPU registers, disassemble code, single-step the CPU, type on the
keyboard, load PRG files, take screenshots, pause/resume, reset, and query
performance.

The feature is made of two cooperating pieces:

1. **In-emulator REST server** (`ucesoft.cbm.mcp.MCPRESTServer`)
   A small HTTP server (built on the `cask` framework) embedded in the
   emulator. It is started on demand from the command line and exposes the
   emulator state over plain HTTP (default `localhost:8080`). It runs on a
   daemon thread, so it does not keep the JVM alive by itself.

2. **MCP bridge** (`ucesoft.cbm.mcp.MCPServer`, packaged as
   `kernal64-mcp-server.jar`)
   A standalone process that speaks the MCP **stdio** transport
   (newline-delimited JSON-RPC 2.0 over stdin/stdout) with the AI agent, and
   translates each tool call into an HTTP request against the emulator's REST
   server. This is the process the AI agent spawns and talks to.

```
+------------------+   stdio JSON-RPC 2.0   +---------------------------+   HTTP (localhost:8080)   +-----------------------+
|     AI agent     | <--------------------> | kernal64-mcp-server.jar   | <-----------------------> |  Kernal64 emulator    |
| (MCP client)     |   (stdin/stdout)       | (ucesoft.cbm.mcp.MCPServer)|                          |  MCPRESTServer (cask) |
+------------------+                        +---------------------------+                           +-----------------------+
```

The bridge jar is intentionally minimal: it only bundles the Scala standard
library plus the `ujson` JSON chain (`ujson`, `upickle-core`, `geny`). All HTTP
is done with the JDK's built-in `HttpURLConnection`, so no HTTP client library
is required. The jar is produced by the sbt `assembly` task and included in the
distribution zip under the `mcp/` directory.

The bridge accepts a single optional command-line argument: the emulator REST
server endpoint as `address:port` (for example `localhost:8080`). It builds the
base URL `http://<address>:<port>` from it. If no argument is given, it defaults
to `http://localhost:8080`.

## Exposed tools

The bridge advertises the following MCP tools (via `tools/list`). Each tool maps
to a REST endpoint on the emulator.

| Tool              | Arguments                                   | Description |
|-------------------|---------------------------------------------|-------------|
| `ping`            | –                                           | Check if the emulator is alive. |
| `getSystemDesc`   | –                                           | Return a textual description of the emulated system. |
| `getDisplayList`  | –                                           | List available displays as `{name, description}` pairs. |
| `selectDisplay`   | `name: string`                              | Select the active display by name. |
| `getCPUList`      | –                                           | List available CPUs as `{name, description}` pairs. |
| `selectCPU`       | `name: string`                              | Select the active CPU by name. |
| `getRegisters`    | –                                           | Return the registers of the selected CPU as `{name, value, intValue}`. |
| `disassemble`     | `address: int (0-65535)`, `length: int`     | Disassemble `length` bytes starting at `address` using the selected CPU. |
| `setCPUTraceMode` | `traceOn: bool`                             | Enable/disable step-by-step trace mode of the selected CPU. |
| `step`            | –                                           | Execute a single CPU step; returns `pc`, `disassembled`, `registers`. Requires trace mode on. |
| `getPerformance`  | –                                           | Return `speedPercent` (100 = original C64 speed), `fps`, `totalFrames`. |
| `reset`           | `hard: bool`                                | Reset the emulator (`true` = hard reset, `false` = soft reset). |
| `loadPRG`         | `path: string`                              | Load a PRG file into the emulator from the given path. |
| `takePicture`     | `path: string`                              | Save a PNG screenshot of the emulator screen to the given path. |
| `peek`            | `address: int (0-65535)`, `length: int`     | Read `length` bytes from emulator memory starting at `address`. |
| `poke`            | `address: int`, `bytes: int[]`              | Write a list of byte values to memory starting at `address`. |
| `typeText`        | `text: string`                              | Insert a string into the emulator keyboard buffer. |
| `isPaused`        | –                                           | Return whether the emulator is currently paused. |
| `setPaused`       | `paused: bool`                              | Pause or resume the emulator. |

### Notes on behaviour

- `step` only works when trace mode is enabled; call `setCPUTraceMode` with
  `traceOn: true` first, otherwise `step` returns an error.
- `peek`/`poke`/`disassemble` validate the address range (`0x0000`–`0xFFFF`) and
  return an `{ "error": ... }` object on invalid input.
- `takePicture` momentarily pauses the emulator to grab a clean frame and then
  restores the previous pause state.

## Launching the emulator with the MCP server enabled

The REST server is **off by default**. Enable it with the command-line option:

```
--mcp-server <host>:<port>
```

The option takes a single `host:port` argument. The same `address:port` value is
passed to the MCP bridge so the two match. For a standard local setup launch the
emulator with:

```
--mcp-server localhost:8080
```

### Using the launcher scripts

The distribution ships per-machine launcher scripts (in the root of the zip):

| Script            | Machine            | Main class                 |
|-------------------|--------------------|----------------------------|
| `k64` (.bat/.sh)  | Commodore 64       | `ucesoft.cbm.c64.C64`      |
| `k128` (.bat/.sh) | Commodore 128      | `ucesoft.cbm.c128.C128`    |
| `kscpu64`         | SuperCPU C64       | `ucesoft.cbm.scpu.SCPUC64` |
| `k20`             | VIC-20             | `ucesoft.cbm.vic20.VIC20`  |
| `kcbm2`           | CBM-II             | `ucesoft.cbm.cbm2.CBMII`   |

The scripts pass through any extra arguments, so you simply append the option.

Windows:

```bat
k64.bat --mcp-server localhost:8080
```

Linux / macOS:

```bash
./k64.sh --mcp-server localhost:8080
```

### Launching the emulator directly with java

If you prefer to invoke the JVM yourself (e.g. from the unpacked distribution
`kernal64/` folder):

Windows:

```bat
java -server -Xms64M -Xmx128M -cp "lib\*" -Djava.library.path=lib ucesoft.cbm.c64.C64 --mcp-server localhost:8080
```

Linux / macOS:

```bash
java -server -Xms64M -Xmx128M -cp "lib/*" -Djava.library.path=lib ucesoft.cbm.c64.C64 --mcp-server localhost:8080
```

When the server starts you will see on the console:

```
Starting MCP server on localhost:8080 ...
MCP server started on localhost and port 8080
```

## Running the MCP bridge

The AI agent normally spawns the bridge itself (see the configuration below),
but you can also run it manually to test:

```bash
java -jar mcp/kernal64-mcp-server.jar localhost:8080
```

The bridge reads JSON-RPC requests from stdin (one JSON object per line) and
writes responses to stdout. The optional `address:port` argument selects the
emulator REST endpoint; without it the bridge defaults to `localhost:8080`.

## Integrating with an AI agent (`mcp.json`)

MCP-capable agents (Claude Desktop, IDE assistants, custom MCP clients, etc.)
are configured through an `mcp.json`-style file that declares how to start each
MCP server. For the Kernal64 bridge the server is launched as a `java -jar`
subprocess communicating over stdio.

```json
{
  "mcpServers": {
    "kernal64": {
      "command": "java",
      "args": [
        "-jar",
        "C:\\kernal64\\mcp\\kernal64-mcp-server.jar",
        "localhost:8080"
      ]
    }
  }
}
```

On Linux / macOS adjust the path:

```json
{
  "mcpServers": {
    "kernal64": {
      "command": "java",
      "args": [
        "-jar",
        "/opt/kernal64/mcp/kernal64-mcp-server.jar",
        "localhost:8080"
      ]
    }
  }
}
```

Key points:

- `command` + `args` tell the agent how to spawn the bridge process. The
  transport is **stdio**, so no port is configured on the agent side.
- The last argument is the emulator REST endpoint as `address:port`. Omit it to
  use the default `localhost:8080`. Set it to the hostname/IP and port of the
  machine running the emulator if the bridge and the emulator run on different
  hosts; it must match the `--mcp-server` value passed to the emulator.
- The emulator must already be running with `--mcp-server localhost:8080`
  before the agent calls any tool; otherwise tool calls return a connection
  error.

## End-to-end usage example

1. Start the emulator with the MCP REST server enabled:

   ```bat
   k64.bat --mcp-server localhost:8080
   ```

2. Point your AI agent at the `mcp.json` shown above and start a session.

3. Ask the agent something like:

   > "Read 16 bytes of C64 memory starting at $0400 (the screen RAM) and tell
   > me what characters are on the top-left of the screen."

   The agent will issue an MCP `tools/call` for `peek`:

   ```json
   {
     "jsonrpc": "2.0",
     "id": 1,
     "method": "tools/call",
     "params": {
       "name": "peek",
       "arguments": { "address": 1024, "length": 16 }
     }
   }
   ```

   The bridge forwards this to `GET http://localhost:8080/peek/1024/16` and
   returns the result to the agent:

   ```json
   {
     "jsonrpc": "2.0",
     "id": 1,
     "result": {
       "content": [
         {
           "type": "text",
           "text": "{\"address\":1024,\"bytes\":[32,32,32,...]}"
         }
       ]
     }
   }
   ```

4. Other typical requests the agent can fulfil:
   - "Type `LOAD\"$\",8` and press return" -> `typeText`
   - "Load this PRG and tell me when it's running" -> `loadPRG`, `getPerformance`
   - "Enable trace mode and single-step 5 instructions from the current PC" ->
     `setCPUTraceMode`, then repeated `step`
   - "Disassemble 32 bytes at $C000" -> `disassemble`
   - "Take a screenshot to C:\\temp\\screen.png" -> `takePicture`
   - "Pause the machine" / "Reset hard" -> `setPaused`, `reset`
