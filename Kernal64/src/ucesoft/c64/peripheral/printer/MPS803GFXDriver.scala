package ucesoft.c64.peripheral.printer

import javax.swing._
import ucesoft.c64.cpu.Memory
import scala.collection.mutable.ListBuffer
import java.awt.Dimension
import java.awt.Graphics
import ucesoft.c64.peripheral.vic.Palette
import ucesoft.c64.cpu.CPU6510Mems
import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Color

object MPS803GFXDriver extends App {
  val f = new JFrame
  val canvas = new MPS803GFXDriver(new MPS803ROM)
  "012345678901234567890" foreach { c => canvas.print(c) }
  canvas.print(13)
  canvas.print(18)
  //canvas.moveHeadTab(10)
  canvas.print(14)
  "CIAO" foreach { c => canvas.print(c) }
  canvas.print(0x71)
  canvas.print(13)
  canvas.print(17)
  canvas.print(15)
  "PIPPO" foreach { c => canvas.print(c) }
  canvas.print(13)
  canvas.checkSize
  f.getContentPane.add("Center", canvas)
  f.pack
  f.setVisible(true)
}

class MPS803GFXDriver(charRom: Memory) extends JComponent with PrinterDriver {
  private[this] val COLUMNS = 80
  private[this] val CHRWIDTH = 6
  private[this] val CHRHEIGHT = 7
  private[this] val LINE_DOTS = COLUMNS * CHRWIDTH
  // --------------- SPECIAL CHARACTERS -------------
  private[this] val QUOTE = 34
  private[this] val ENHANCE_ON = 14
  private[this] val ENHANCE_OFF = 15
  private[this] val RVS_ON = 18
  private[this] val RVS_OFF = 146
  private[this] val CR = 13
  private[this] val LINE_FEED = 10
  private[this] val BUSINESS_MODE = 17
  private[this] val GRAPHIC_MODE = 145
  private[this] val TAB = 16
  private[this] val REPEAT = 26
  private[this] val DOT_ADDRESS = 27
  private[this] val BIT_IMAGE_PRINTING = 8

  private trait Operation
  private case class Print(ch: Int) extends Operation
  private trait Special extends Operation
  private object RvsOn extends Special
  private object RvsOff extends Special
  private object EnhanceOn extends Special
  private object EnhanceOff extends Special
  private object CarrigeReturn extends Special
  private object LineFeed extends Special
  private object GraphicMode extends Special
  private object BusinessMode extends Special
  private object BitmapMode extends Special
  private case class MoveHeadTo(x: Int) extends Operation
  private case class PosTo(x: Int) extends Operation
  //private case class Repeat(n:Int,bitmap:Int) extends Operation

  private object State extends Enumeration {
    val WAITING_CHAR = Value
    val POS_WAITING_L = Value
    val POS_WAITING_H = Value
    val DOT_ADDRESS_WAITING_16 = Value
    val DOT_ADDRESS_WAITING_L = Value
    val DOT_ADDRESS_WAITING_H = Value
    val REPEAT_WAITING_N = Value
    val REPEAT_WAITING_BITMAP = Value
  }

  import State._

  private[this] val operations = new ListBuffer[Operation]
  private[this] var quote = 0
  private[this] var state = WAITING_CHAR
  private[this] var posL,posH,rep = 0
  private[this] var bitmapActive = false
  
  def clearPages {
    operations.clear
    quote = 0
    state = WAITING_CHAR
    bitmapActive = false
    checkSize
    repaint()
  }

  def print(ch: Int) {
    state match {
      case POS_WAITING_L =>
        posL = ch
        state = POS_WAITING_H
      case POS_WAITING_H =>
        val x = (posL - 48) * 10 + ch - 48
        operations += PosTo(x)
        state = WAITING_CHAR
      case DOT_ADDRESS_WAITING_16 =>
        state = if (ch == TAB) DOT_ADDRESS_WAITING_L else WAITING_CHAR
      case DOT_ADDRESS_WAITING_L =>
        posH = ch
        state = DOT_ADDRESS_WAITING_H
      case DOT_ADDRESS_WAITING_H =>
        val x = posH << 8 | ch
        operations += MoveHeadTo(x)
        state = WAITING_CHAR
      case REPEAT_WAITING_N =>
        rep = ch
        state = REPEAT_WAITING_BITMAP
      case REPEAT_WAITING_BITMAP =>
        for(i <- 1 to rep) operations += Print(ch)
        state = WAITING_CHAR
      case WAITING_CHAR =>
        if (ch == QUOTE) {
          quote += 1
          operations += Print(ch)
        } else {
          if (quote % 2 == 1 && ((ch >= 0x00 && ch <= 0x1F) || (ch >= 0x80 && ch <= 0x9F))) { // odd number of " with special char
            operations += RvsOn
            if (ch >= 0x00 && ch <= 0x1F) operations += Print(ch + 0x40)
            else operations += Print(ch - 0x20)
            operations += RvsOff
            return
          }          
          ch match {
            case ENHANCE_ON => 
              operations += EnhanceOn
              bitmapActive = false
            case ENHANCE_OFF => 
              operations += EnhanceOff
              bitmapActive = false
            case GRAPHIC_MODE => operations += GraphicMode
            case BUSINESS_MODE => operations += BusinessMode
            case BIT_IMAGE_PRINTING => 
              operations += BitmapMode
              bitmapActive = true
            case CR =>
              operations += CarrigeReturn
              quote = 0
            case LINE_FEED => operations += LineFeed
            case RVS_ON => operations += RvsOn
            case RVS_OFF => 
              if (!bitmapActive) operations += RvsOff
              else operations += Print(ch)
            case DOT_ADDRESS =>
              state = DOT_ADDRESS_WAITING_16
            case TAB =>
              state = POS_WAITING_L
            case REPEAT =>
              if (bitmapActive) state = REPEAT_WAITING_N
              else operations += Print(ch)
            case _ => operations += Print(ch)
          }
        }
    }
    repaint()      
  }

  def checkSize {
    val lines = 1 + (operations count { op => op == CarrigeReturn || op == LineFeed })
    val width = COLUMNS * CHRWIDTH
    setPreferredSize(new Dimension(width, lines * CHRHEIGHT))
  }

  @inline private def readChar(code: Int, line: Int) = charRom.read(charRom.startAddress + (code * CHRHEIGHT) + line)

  override def paintComponent(g: Graphics) {
    val size = getPreferredSize
    g.setColor(Palette.VIC_COLORS(1)) // white background
    g.fillRect(0, 0, size.width - 1, size.height - 1)
    g.setColor(Palette.VIC_COLORS(0)) // black foreground

    val INTER_LINE_PIXEL = 2
    val INTER_LINE_PIXEL_BITMAP = 0
    var x, y = 0
    var graphicMode = true
    var rvsOn = false
    var doubleWidth = false
    var bitmapMode = false
    var interLinePixels = INTER_LINE_PIXEL

    val opIterator = operations.iterator
    while (opIterator.hasNext) {
      opIterator.next match {
        case Print(ch) =>
          bitmapMode match {
            case false =>
              var char = ch
              if (!graphicMode) char += 0x100
              for (line <- 0 to CHRHEIGHT - 1) { // char line
                var byte = readChar(char, line)
                if (rvsOn) byte = ~byte
                var bit = 0x80
                var dx = 0
                while (bit > 1) {
                  var width = if (doubleWidth) 2 else 1
                  while (width > 0) {
                    if ((byte & bit) > 0) g.drawRect(x + dx, y + line, 0, 0)
                    width -= 1
                    dx += 1
                  }
                  bit >>= 1
                }
              }
              x += CHRWIDTH * (if (doubleWidth) 2 else 1)
              if (x >= LINE_DOTS) {
                x = 0
                y += CHRHEIGHT + interLinePixels
                if (y > getPreferredSize.height) {
                  setPreferredSize(new Dimension(getPreferredSize.width, y + 2 * (CHRHEIGHT + interLinePixels)))
                  invalidate
                }
              }
            case true => // bitmap mode
              val byte = ch & 0x7F // clear 8th bit
              for(dy <- 1 to 7) {
                if ((byte & (1 << dy)) > 0) g.drawRect(x, y + dy - 1, 0, 0)
              }
              x += 1
          }
        case CarrigeReturn =>
          x = 0
          y += CHRHEIGHT + interLinePixels
          if (y > getPreferredSize.height) {
            setPreferredSize(new Dimension(getPreferredSize.width, y + 2 * (CHRHEIGHT + interLinePixels)))
            invalidate
          }
          rvsOn = false
        case LineFeed =>
          x = 0
          y += CHRHEIGHT + interLinePixels
          if (y > getPreferredSize.height) {
            setPreferredSize(new Dimension(getPreferredSize.width, y + 2 * (CHRHEIGHT + interLinePixels)))
            invalidate
          }
        case GraphicMode =>
          graphicMode = true
        case BusinessMode =>
          graphicMode = false
        case RvsOn =>
          rvsOn = true
        case RvsOff =>
          rvsOn = false
        case EnhanceOn =>
          doubleWidth = true
          bitmapMode = false
          interLinePixels = INTER_LINE_PIXEL
        case EnhanceOff =>
          doubleWidth = false
          bitmapMode = false
          interLinePixels = INTER_LINE_PIXEL
        case MoveHeadTo(targetX) =>
          x = targetX
        case PosTo(targetX) =>
          x = targetX * CHRWIDTH
        case BitmapMode =>
          bitmapMode = true
          interLinePixels = INTER_LINE_PIXEL_BITMAP
      }
    }
    g.setColor(Color.RED)
    g.drawRect(x,y,CHRWIDTH - 1,CHRHEIGHT - 1)    
  }
  
  def saveAsPNG(file: File) {
    val snap = createImage(getPreferredSize.width, getPreferredSize.height).asInstanceOf[BufferedImage]
    paint(snap.getGraphics)
    ImageIO.write(snap, "png", file)
  }
}