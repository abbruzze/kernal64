package ucesoft.c64.util

import ucesoft.c64.cpu.Memory
import javax.swing._
import ucesoft.c64.cpu.CPU6510Mems
import java.awt.Dimension
import scala.collection.mutable.ListBuffer
import ucesoft.c64.peripheral.vic.Palette
import java.awt.Graphics

object CBMCanvas extends App {  
  val f = new JFrame
  val rom = new CPU6510Mems.CHARACTERS_ROM(null)
  rom.init
  val c = new CBMCanvas(rom).enhanceWidth
  c rep (32, 40) newLine;
  c << "ALESSANDRO" newLine;
  c.rvsOn
  c.green
  c.lowCase
  c << "ABBRUZZETTI" newLine;
  c.rvsOff
  c.red
  c << "0123456789" end

  f.getContentPane.add("Center", c)
  f.pack
  f.setVisible(true)
  
  c.upCase
  c.yellow
  for (i <- 1 to 5) {
    for (y <- 1 to 8) {
      c yscroll -y
      c.repaint()
      Thread.sleep(100)
    }
    c yscroll 0
    c.dropFirst
    c << "LAST " + i end

    c.repaint()
  }
  c.cyan
  for (i <- 1 to 5) {
    for (y <- 1 to 8) {
      c yscroll y
      c.repaint()
      Thread.sleep(100)
    }
    c yscroll 0
    c.dropLast
    c << "FIRST " + i << 209 firstLine

    c.repaint()
  }
}

class CBMCanvas(charRom: Memory) extends JComponent {
  private[this] case class Char(fgColor: Int, charCode: Int,double:Boolean)

  private[this] val lines = new ListBuffer[ListBuffer[Char]]
  private[this] var currentLine = new ListBuffer[Char]
  private[this] var backgroundColor = 0
  private[this] var foregroundColor = 1
  private[this] var lowerCase = false
  private[this] var reverseOn = false
  private[this] var scrollY = 0
  private[this] var doubleWidth = false

  def clear = {
    lines.clear
    currentLine = new ListBuffer[Char]
    this
  }

  def black = fgColor(0)
  def white = fgColor(1)
  def red = fgColor(2)
  def cyan = fgColor(3)
  def purple = fgColor(4)
  def green = fgColor(5)
  def blue = fgColor(6)
  def yellow = fgColor(7)
  def orange = fgColor(8)
  def brown = fgColor(9)
  def lightRed = fgColor(10)
  def darkGray = fgColor(11)
  def gray = fgColor(12)
  def lightGreen = fgColor(13)
  def lightBlue = fgColor(14)
  def lightGray = fgColor(15)

  def enhanceWidth = { doubleWidth = true ; this }
  def standardWidth = { doubleWidth = false ; this }
  def yscroll(value: Int) = { scrollY = value; this }
  def fgColor(fgColor: Int) = { foregroundColor = fgColor; this }
  def bgColor(bgColor: Int) = { backgroundColor = bgColor; this }
  def rvsOn = { reverseOn = true; this }
  def rvsOff = { reverseOn = false; this }
  def lowCase = { lowerCase = true; this }
  def upCase = { lowerCase = false; this }
  def newLine = !!
  def firstLine = {
    lines.insert(0,currentLine)
    currentLine = new ListBuffer[Char]
    this
  }
  def !! = {
    lines += currentLine
    currentLine = new ListBuffer[Char]
    this
  }
  def <<(ch: scala.Char) = add(ch)
  def <<(txt: String) = add(txt)
  def <<(code: Int) = add(code)
  def rep(code: Int, times: Int) = {
    for (i <- 1 to times) add(code)
    this
  }
  def add(ch: scala.Char): CBMCanvas = add(ch.toString)
  def add(txt: String) = {
    for (c <- txt) {
      currentLine += Char(foregroundColor, withModifiers(convertCode(c)),doubleWidth)
    }
    this
  }
  def add(code: Int) = {
    currentLine += Char(foregroundColor, withModifiers(convertCode(code)),doubleWidth)
    this
  }
  def end = { !!; checkSize; this }
  def dropFirst = { lines.remove(0); checkSize; this }
  def dropLast = { lines.remove(lines.length - 1); checkSize; this }
  
  def center(s:String,width:Int) = {
    val delta = width - (if (doubleWidth) s.length * 2 else s.length)
    if (delta <= 0) s
    else {
      val left = delta / 2
      if (doubleWidth) (" " * (left / 2)) + s + (" " * (delta - left / 2))
      else (" " * left) + s + (" " * (delta - left))
    }
  }

  private def convertCode(c: Int) =
//    if (c >= 64 && c <= 95) c - 64
//    else if (c >= 96 && c <= 127) c - 32
//    else if (c >= 128 && c <= 191) c - 64
//    else if (c >= 192 && c <= 223) c - 192 + 96 - 32
//    else if (c >= 224 && c <= 254) c - 224 + 160 - 32
//    /*
//    else if (c >= 192 && c <= 254) c - 128
//    */
    if (c >= 64 && c <= 95) c - 64
    else if (c >= 96 && c <= 127) c - 32
    else if (c >= 128 && c <= 159) c + 64
    else if (c >= 160 && c <= 191) c - 64
    else if (c >= 192 && c <= 223) c - 128
    else if (c >= 224 && c <= 254) c - 128
    else if (c == 255) 94
    else if (c > 255) c & 0xFF
    else c

  private def withModifiers(code: Int) = {
    var c = code
    if (reverseOn) c += 0x80
    if (lowerCase) c += 0x100
    c
  }

  def checkSize {
    val maxWidth = lines map { _.length } max

    setPreferredSize(new Dimension(maxWidth * 8, lines.length * 8))
  }

  override def paint(g: Graphics) {
    val size = getSize()
    g.setColor(Palette.VIC_COLORS(backgroundColor))
    g.fillRect(0, 0, size.width - 1, size.height - 1)

    g.translate(0, scrollY)
    var y = 0
    for (line <- lines) {
      for (rc <- 0 to 7) {
        var x = 0
        for (char <- line) {
          val byte = charRom.read(charRom.startAddress | (char.charCode << 3) | rc)
          g.setColor(Palette.VIC_COLORS(char.fgColor))
          var bit = 0x80
          var dx = 0          
          while (bit > 0) {
            var width = if (char.double) 2 else 1
            while (width > 0) {
              if ((byte & bit) > 0) g.drawRect(x << 3 | dx, y << 3 | rc, 0, 0)
              dx += 1
              width -= 1
            }
            bit >>= 1
          }
          x += (if (char.double) 2 else 1)
        }
      }
      y += 1
    }
  }
}