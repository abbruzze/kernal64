package ucesoft.cbm.misc

import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.peripheral.vic.Palette

import java.awt.{Dimension, Graphics}
import javax.swing._
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class CBMCanvas(charRom: Memory,romCharBytes:Int,romCharHeight:Int) extends JComponent {
  private[this] case class Char(fgColor: Int, charCode: Int,double:Boolean)

  private[this] val lines = new ListBuffer[ListBuffer[Char]]
  private[this] var currentLine = new ListBuffer[Char]
  private[this] var backgroundColor = 0
  private[this] var foregroundColor = 1
  private[this] var lowerCase = false
  private[this] var reverseOn = false
  private[this] var scrollY = 0
  private[this] var doubleWidth,doubleHeight = false
  private[this] var rowSelected = -1

  protected def selectRow(rs:Int): Unit = rowSelected = rs
  protected def selectedRow : Int = rowSelected

  def isDoubleWidth : Boolean = doubleWidth
  def isDoubleHeight : Boolean = doubleHeight
  def linesCount : Int = lines.length

  def clear: CBMCanvas = {
    lines.clear
    currentLine = new ListBuffer[Char]
    this
  }

  def black: CBMCanvas = fgColor(0)
  def white: CBMCanvas = fgColor(1)
  def red: CBMCanvas = fgColor(2)
  def cyan: CBMCanvas = fgColor(3)
  def purple: CBMCanvas = fgColor(4)
  def green: CBMCanvas = fgColor(5)
  def blue: CBMCanvas = fgColor(6)
  def yellow: CBMCanvas = fgColor(7)
  def orange: CBMCanvas = fgColor(8)
  def brown: CBMCanvas = fgColor(9)
  def lightRed: CBMCanvas = fgColor(10)
  def darkGray: CBMCanvas = fgColor(11)
  def gray: CBMCanvas = fgColor(12)
  def lightGreen: CBMCanvas = fgColor(13)
  def lightBlue: CBMCanvas = fgColor(14)
  def lightGray: CBMCanvas = fgColor(15)

  def enhanceWidth: CBMCanvas = { doubleWidth = true ; this }
  def standardWidth: CBMCanvas = { doubleWidth = false ; this }
  def enhanceHeight: CBMCanvas = { doubleHeight = true ; this }
  def standardHeight: CBMCanvas = { doubleHeight = false ; this }
  def yscroll(value: Int): CBMCanvas = { scrollY = value; this }
  def fgColor(fgColor: Int): CBMCanvas = { foregroundColor = fgColor; this }
  def bgColor(bgColor: Int): CBMCanvas = { backgroundColor = bgColor; this }
  def rvsOn: CBMCanvas = { reverseOn = true; this }
  def rvsOff: CBMCanvas = { reverseOn = false; this }
  def lowCase: CBMCanvas = { lowerCase = true; this }
  def upCase: CBMCanvas = { lowerCase = false; this }
  def newLine: CBMCanvas = !!
  def firstLine: CBMCanvas = {
    lines.insert(0,currentLine)
    currentLine = new ListBuffer[Char]
    this
  }
  def !! : CBMCanvas = {
    lines += currentLine
    currentLine = new ListBuffer[Char]
    this
  }
  def <<(ch: scala.Char): CBMCanvas = add(ch)
  def <<(txt: String): CBMCanvas = add(txt)
  def <<(code: Int): CBMCanvas = add(code)
  def rep(code: Int, times: Int): CBMCanvas = {
    for (i <- 1 to times) add(code)
    this
  }
  def add(ch: scala.Char): CBMCanvas = add(ch.toString)
  def add(txt: String): CBMCanvas = {
    for (c <- txt) {
      currentLine += Char(foregroundColor, withModifiers(convertCode(c)),doubleWidth)
    }
    this
  }
  def add(code: Int): CBMCanvas = {
    currentLine += Char(foregroundColor, withModifiers(convertCode(code)),doubleWidth)
    this
  }
  def end: CBMCanvas = { !!; checkSize; this }
  def dropFirst: CBMCanvas = { lines.remove(0); checkSize; this }
  def dropLast: CBMCanvas = { lines.remove(lines.length - 1); checkSize; this }
  
  def center(s:String,width:Int): String = {
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

  def checkSize()  : Unit = {
    val maxWidth = lines map { _.length } max

    setPreferredSize(new Dimension(maxWidth * (if (doubleWidth) 16 else 8),lines.length * (if (doubleHeight) 16 else 8)))
  }

  override def paint(g: Graphics) : Unit = {
    val size = getSize()
    g.setColor(Palette.VIC_COLORS(backgroundColor))
    g.fillRect(0, 0, size.width - 1, size.height - 1)

    g.translate(0, scrollY)
    var y = 0
    for ((line,row) <- lines.zipWithIndex) {
      var rcy = 0
      for (rc <- 0 to 7) {
        var x = 0
        for (char <- line) {
          val byte = charRom.read(charRom.startAddress | (char.charCode * romCharBytes) | rc)
          g.setColor(Palette.VIC_COLORS(char.fgColor))
          var bit = 0x80
          var dx = 0
          while (bit > 0) {
            var width = if (char.double) 2 else 1
            while (width > 0) {
              var pixel = (byte & bit) > 0
              if (rowSelected == row) pixel = !pixel
              if (pixel) {
                g.drawRect(x << 3 | dx, y * romCharHeight | rcy, 0, 0)
                if (doubleHeight) g.drawRect(x << 3 | dx, y * romCharHeight | (rcy + 1), 0, 0)
              }
              dx += 1
              width -= 1
            }
            bit >>= 1
          }
          x += (if (char.double) 2 else 1)
        }
        if (doubleHeight) rcy += 2 else rcy += 1
      }
      y += (if (doubleHeight) 2 else 1)
    }
  }
}