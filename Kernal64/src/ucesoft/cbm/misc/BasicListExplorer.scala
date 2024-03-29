package ucesoft.cbm.misc

import ucesoft.cbm.cbm2.CBM2MMU
import ucesoft.cbm.cpu.Memory

import java.awt.Desktop
import java.io.{FileWriter, PrintWriter}

object BasicListExplorer {
  private[this] val TOKEN_MAP = Map(
      0x80 -> "END",
      0x81 -> "FOR",
      0x82 -> "NEXT",
      0x83 -> "DATA",
      0x84 -> "INPUT#",
      0x85 -> "INPUT",
      0x86 -> "DIM",
      0x87 -> "READ",
      0x88 -> "LET",
      0x89 -> "GOTO",
      0x8a -> "RUN",
      0x8b -> "IF",
      0x8c -> "RESTORE",
      0x8d -> "GOSUB",
      0x8e -> "RETURN",
      0x8f -> "REM",
      0x90 -> "STOP",
      0x91 -> "ON",
      0x92 -> "WAIT",
      0x93 -> "LOAD",
      0x94 -> "SAVE",
      0x95 -> "VERIFY",
      0x96 -> "DEF",
      0x97 -> "POKE",
      0x98 -> "PRINT#",
      0x99 -> "PRINT",
      0x9a -> "CONT",
      0x9b -> "LIST",
      0x9c -> "CLR",
      0x9d -> "CMD",
      0x9e -> "SYS",
      0x9f -> "OPEN",
      0xa0 -> "CLOSE",
      0xa1 -> "GET",
      0xa2 -> "NEW",
      0xa3 -> "TAB(",
      0xa4 -> "TO",
      0xa5 -> "FN",
      0xa6 -> "SPC(",
      0xa7 -> "THEN",
      0xa8 -> "NOT",
      0xa9 -> "STEP",
      0xaa -> "+",
      0xab -> "-",
      0xac -> "*",
      0xad -> "/",
      0xae -> "^",
      0xaf -> "AND",
      0xb0 -> "OR",
      0xb1 -> ">",
      0xb2 -> "=",
      0xb3 -> "<",
      0xb4 -> "SGN",
      0xb5 -> "INT",
      0xb6 -> "ABS",
      0xb7 -> "USR",
      0xb8 -> "FRE",
      0xb9 -> "POS",
      0xba -> "SQR",
      0xbb -> "RND",
      0xbc -> "LOG",
      0xbd -> "EXP",
      0xbe -> "COS",
      0xbf -> "SIN",
      0xc0 -> "TAN",
      0xc1 -> "ATN",
      0xc2 -> "PEEK",
      0xc3 -> "LEN",
      0xc4 -> "STR$",
      0xc5 -> "VAL",
      0xc6 -> "ASC",
      0xc7 -> "CHR$",
      0xc8 -> "LEFT$",
      0xc9 -> "RIGHT$",
      0xca -> "MID$",
      0xcb -> "GO",
      0xff -> "{pi}",
      0xcc -> "RGR",
      0xcd -> "RCLR",
      0xce -> "*PREFIX*",
      0xcf -> "JOY",
      0xd0 -> "RDOT",
      0xd1 -> "DEC",
      0xd2 -> "HEX$",
      0xd3 -> "ERR$",
      0xd4 -> "INSTR",
      0xd5 -> "ELSE",
      0xd6 -> "RESUME",
      0xd7 -> "TRAP",
      0xd8 -> "TRON",
      0xd9 -> "TROFF",
      0xda -> "SOUND",
      0xdb -> "VOL",
      0xdc -> "AUTO",
      0xdd -> "PUDEF",
      0xde -> "GRAPHIC",
      0xdf -> "PAINT",
      0xe0 -> "CHAR",
      0xe1 -> "BOX",
      0xe2 -> "CIRCLE",
      0xe3 -> "GSHAPE",
      0xe4 -> "SSHAPE",
      0xe5 -> "DRAW",
      0xe6 -> "LOCATE",
      0xe7 -> "COLOR",
      0xe8 -> "SCNCLR",
      0xe9 -> "SCALE",
      0xea -> "HELP",
      0xeb -> "DO",
      0xec -> "LOOP",
      0xed -> "EXIT",
      0xee -> "DIRECTORY",
      0xef -> "DSAVE",
      0xf0 -> "DLOAD",
      0xf1 -> "HEADER",
      0xf2 -> "SCRATCH",
      0xf3 -> "COLLECT",
      0xf4 -> "COPY",
      0xf5 -> "RENAME",
      0xf6 -> "BACKUP",
      0xf7 -> "DELETE",
      0xf8 -> "RENUMBER",
      0xf9 -> "KEY",
      0xfa -> "MONITOR",
      0xfb -> "USING",
      0xfc -> "UNTIL",
      0xfd -> "WHILE",
      0xfe -> "*PREFIX*"
  )
  private[this] val EXTENDED_TOKEN_MAP = Map(
      0xce -> Map (
          0x02 -> "POT",
          0x03 -> "BUMP",
          0x04 -> "PEN",
          0x05 -> "RSPPOS",
          0x06 -> "RSSPRITE",
          0x07 -> "RSPCOLOR",
          0x08 -> "XOR",
          0x09 -> "RWINDOW",
          0x0A -> "POINTER"),
      0xfe -> Map(
          0x02 -> "BANK",
          0x03 -> "FILTER",
          0x04 -> "PLAY",
          0x05 -> "TEMPO",
          0x06 -> "MOVSPR",
          0x07 -> "SPRITE",
          0x08 -> "SPRCOLOR",
          0x09 -> "RREG",
          0x0a -> "ENVELOPE",
          0x0b -> "SLEEP",
          0x0c -> "CATALOG",
          0x0d -> "DOPEN",
          0x0e -> "APPEND",
          0x0f -> "DCLOSE",
          0x10 -> "BSAVE",
          0x11 -> "BLOAD",
          0x12 -> "RECORD",
          0x13 -> "CONCAT",
          0x14 -> "DVERIFY",
          0x15 -> "DCLEAR",
          0x16 -> "SPRSAV",
          0x17 -> "COLLISION",
          0x18 -> "BEGIN",
          0x19 -> "BEND",
          0x1a -> "WINDOW",
          0x1b -> "BOOT",
          0x1c -> "WIDTH",
          0x1d -> "SPRDEF",
          0x1e -> "QUIT",
          0x1f -> "STASH",
          0x21 -> "FETCH",
          0x23 -> "SWAP",
          0x24 -> "OFF",
          0x25 -> "FAST",
          0x26 -> "SLOW"
      )
  )
  
  private def findToken(token:Int,nextByte:Int) : (String,Int) = {
    TOKEN_MAP get token match {
      case None => "{Token not found}" -> 1
      case Some(t) if t == "*PREFIX*" =>
        EXTENDED_TOKEN_MAP get token match {
          case None => "{Ext. token not found}" -> 1
          case Some(emap) =>
            emap get nextByte match {
              case None => "{Ext. byte token not found}" -> 1
              case Some(t) => t -> 2
            }
        }
      case Some(t) => t -> 1
    }
  }
  // taken from C64Studio
  private def mapChar(c:Int) : String = {
    c match {
      case 5 => "{white}"
      case 10 => "\n"
      case 17 => "{down}"
      case 18 => "{rvon}"
      case 19 => "{home}"
      case 20 => "{del}"
      case 28 => "{red}"
      case 29 => "{right}"
      case 30 => "{green}"
      case 31 => "{blue}"
      case 92 => "{pound}"
      case 94 => "^"
      case 95 => "{Shift-ArrowLeft}"
      case 96 => "{Shift-*}"
      case 97 => "{Shift-A}"
      case 98 => "{Shift-B}"
      case 99 => "{Shift-C}"
      case 100 => "{Shift-D}"
      case 101 => "{Shift-E}"
      case 102 => "{Shift-F}"
      case 103 => "{Shift-G}"
      case 104 => "{Shift-H}"
      case 105 => "{Shift-I}"
      case 106 => "{Shift-J}"
      case 107 => "{Shift-K}"
      case 108 => "{Shift-L}"
      case 109 => "{Shift-M}"
      case 110 => "{Shift-N}"
      case 111 => "{Shift-O}"
      case 112 => "{Shift-P}"
      case 113 => "{Shift-Q}"
      case 114 => "{Shift-R}"
      case 115 => "{Shift-S}"
      case 116 => "{Shift-T}"
      case 117 => "{Shift-U}"
      case 118 => "{Shift-V}"
      case 119 => "{Shift-W}"
      case 120 => "{Shift-X}"
      case 121 => "{Shift-Y}"
      case 122 => "{Shift-Z}"
      case 123 => "{Shift-+}"
      case 124 => "{CBM--}"
      case 125 => "{Shift--}"
      case 127 => "{CBM-*}"
      case 129 => "{orange}"
      case 133 => "{F1}"
      case 134 => "{F3}"
      case 135 => "{F5}"
      case 136 => "{F7}"
      case 137 => "{F2}"
      case 138 => "{F4}"
      case 139 => "{F6}"
      case 140 => "{F8}"
      case 144 => "{black}"
      case 145 => "{up}"
      case 146 => "{rvof}"
      case 147 => "{clr}"
      case 148 => "{ins}"
      case 149 => "{brown}"
      case 150 => "{lred}"
      case 151 => "{gry1}"
      case 152 => "{gry2}"
      case 153 => "{lgrn}"
      case 154 => "{lblu}"
      case 155 => "{gry3}"
      case 156 => "{purple}"
      case 157 => "{left}"
      case 158 => "{yellow}"
      case 159 => "{cyn}"
      case 161 => "{CBM-K}"
      case 162 => "{CBM-I}"
      case 163 => "{CBM-T}"
      case 164 => "{CBM-@}"
      case 165 => "{CBM-G}"
      case 166 => "{CBM-+}"
      case 167 => "{CBM-N}"
      case 168 => "{CBM-£}"
      case 169 => "{Shift-£}"
      case 170 => "{CBM-M}"
      case 171 => "{CBM-Q}"
      case 172 => "{CBM-D}"
      case 173 => "{CBM-Z}"
      case 174 => "{CBM-S}"
      case 175 => "{CBM-P}"
      case 176 => "{CBM-A}"
      case 177 => "{CBM-E}"
      case 178 => "{CBM-R}"
      case 179 => "{CBM-W}"
      case 180 => "{CBM-H}"
      case 181 => "{CBM-J}"
      case 182 => "{CBM-L}"
      case 183 => "{CBM-Y}"
      case 184 => "{CBM-U}"
      case 185 => "{CBM-O}"
      case 187 => "{CBM-F}"
      case 188 => "{CBM-C}"
      case 189 => "{CBM-X}"
      case 190 => "{CBM-V}"
      case 191 => "{CBM-B}"
      case 255 => "{Shift-Arrowup}"
      case p if p >= 32 && p < 95 => p.toChar.toString
      case x => s"{$x}"
    }
  }

  def createSource(ram:Memory,startAddress:Int): StringBuilder = {
    val sb = new StringBuilder
    var adr = startAddress

    var keepListing = true
    while (keepListing && adr < 0x10000) {
      val nextAdr = ram.read(adr) | ram.read(adr + 1) << 8
      adr += 2
      keepListing = nextAdr != 0
      if (keepListing) {
        val line = ram.read(adr) | ram.read(adr + 1) << 8
        var stringMode = false
        adr += 2
        sb.append(s"$line ")
        var token = ram.read(adr)
        while (token != 0 && adr < 0x10000) {
          if (token == 0x22) stringMode = !stringMode
          val nextByte = ram.read(adr + 1)
          if (!stringMode && (token & 0x80) > 0) {
            val (text,length) = findToken(token,nextByte)
            adr += length
            sb.append(text)
          }
          else {
            sb.append(mapChar(token))
            adr += 1
          }
          token = ram.read(adr)
        }
        adr = nextAdr
        sb.append("\n")
      }
    }
    sb
  }

  def createSourceCBM2(ram:CBM2MMU, startAddress:Int): StringBuilder = {
    val sb = new StringBuilder
    var adr = startAddress

    var keepListing = true
    while (keepListing && adr < 0x10000) {
      val nextAdr = ram.readBank(adr,1) | ram.readBank(adr + 1,1) << 8
      adr += 2
      keepListing = nextAdr != 0
      if (keepListing) {
        val line = ram.readBank(adr,1) | ram.readBank(adr + 1,1) << 8
        var stringMode = false
        adr += 2
        sb.append(s"$line ")
        var token = ram.readBank(adr,1)
        while (token != 0 && adr < 0x10000) {
          if (token == 0x22) stringMode = !stringMode
          val nextByte = ram.readBank(adr + 1,1)
          if (!stringMode && (token & 0x80) > 0) {
            val (text,length) = findToken(token,nextByte)
            adr += length
            sb.append(text)
          }
          else {
            sb.append(mapChar(token))
            adr += 1
          }
          token = ram.readBank(adr,1)
        }
        adr = nextAdr
        sb.append("\n")
      }
    }
    sb
  }

  def list(ram:Memory,startAddress:Int) : Unit = {
    val sb = createSource(ram, startAddress)
    if (Desktop.isDesktopSupported) {
      val file = java.io.File.createTempFile("kernal64",".txt")
      file.deleteOnExit()
      val pw = new PrintWriter(new FileWriter(file))
      pw.println(sb.toString)
      pw.close()
      Desktop.getDesktop.edit(file)
    }
    else println(sb)
  }

  def listCBM2(ram:CBM2MMU,startAddress:Int) : Unit = {
    val sb = createSourceCBM2(ram, startAddress)
    if (Desktop.isDesktopSupported) {
      val file = java.io.File.createTempFile("kernal64",".txt")
      file.deleteOnExit()
      val pw = new PrintWriter(new FileWriter(file))
      pw.println(sb.toString)
      pw.close()
      Desktop.getDesktop.edit(file)
    }
    else println(sb)
  }
}