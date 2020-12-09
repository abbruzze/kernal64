package ucesoft.cbm.peripheral.drive

import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.DataOutputStream
import ucesoft.cbm.formats.Diskette
import ucesoft.cbm.misc.FloppyFlushListener

object Floppy {
  def load(in:ObjectInputStream) : Option[Floppy] = {
    in.readBoolean match {
      case true =>
        val file = in.readObject.asInstanceOf[String]
        val content = in.readObject.asInstanceOf[Array[Byte]]
        val tmpFile = new File(new File(System.getProperty("java.io.tmpdir")),new File(file).getName)
        tmpFile.deleteOnExit
        val out = new DataOutputStream(new FileOutputStream(tmpFile))
        out.write(content)
        out.close
        val fileName = file.toUpperCase
        val floppy : Floppy = Diskette(tmpFile.toString)
        floppy.load(in)
        Some(floppy)
      case false =>
        None
    }    
  }  
  def save(out:ObjectOutputStream,floppy:Option[Floppy]) : Unit = {
    floppy match {
      case Some(f) =>
        out.writeBoolean(true)
        f.saveFile(out)
        f.save(out)
      case None =>
        out.writeBoolean(false)
    }    
  }
}

/**
 * @author ealeame
 */
trait Floppy {
  type TrackListener = (Int,Boolean,Option[Int]) => Unit
  var canWriteOnDisk = true
  var flushListener : FloppyFlushListener = _
  
  val isReadOnly : Boolean
  val totalTracks : Int
  val file : String
  lazy val singleSide = true
  val isEmpty = false
  
  def isOnIndexHole : Boolean = false
  
  def minTrack = 1
  def maxTrack = totalTracks
  
  def side : Int = 0
  def side_=(newSide:Int) : Unit = {}
  
  def nextBit : Int
  def writeNextBit(bit:Boolean) : Unit
  def nextByte : Int
  def writeNextByte(b:Int) : Unit
  
  def currentTrack : Int
  def currentSector : Option[Int]
  def changeTrack(trackSteps:Int) : Unit
  def setTrackChangeListener(l:TrackListener) : Unit
  def notifyTrackSectorChangeListener : Unit
    
  def defaultZoneFor(track:Int) : Int = {
    if (track <= 17) 0
    else if (track <= 24) 1
    else if (track <= 30) 2
    else 3
  }
  
  def flush : Unit = {}
  def close : Unit
  def reset : Unit
  // state
  def save(out:ObjectOutputStream) : Unit
  def load(in:ObjectInputStream) : Unit
  
  protected def saveFile(out:ObjectOutputStream) : Unit = {
    flush
    val len = new File(file).length.toInt
    val f = new DataInputStream(new FileInputStream(file))
    try {
      out.writeObject(file)
      val content = Array.ofDim[Byte](len)
      f.readFully(content)
      out.writeObject(content)
    }
    finally {
      f.close
    }
  }
}

object EmptyFloppy extends EmptyFloppy
class EmptyFloppy extends Floppy {
  val isReadOnly = false
  val totalTracks = 35
  val file = ""
  override val isEmpty = true
  
  protected var track = 1
  private[this] var listener : TrackListener = _
  
  def nextBit = 0
  def writeNextBit(bit:Boolean) : Unit = {}
  def nextByte = 0
  def writeNextByte(b:Int) : Unit = {}
  
  def currentTrack = track
  def currentSector = None
  def changeTrack(trackSteps:Int) : Unit = {
    val isOnTrack = (trackSteps & 1) == 0
    if (isOnTrack) {
      track = trackSteps >> 1
    }
    notifyTrackSectorChangeListener
  }
  def setTrackChangeListener(l:TrackListener) = listener = l
  def notifyTrackSectorChangeListener : Unit = {
    if (listener != null) listener(track,false,None)
  }
    
  def close : Unit = {}
  def reset : Unit = {
    track = 1
  }
  // state
  def save(out:ObjectOutputStream) : Unit = {}
  def load(in:ObjectInputStream) : Unit = {}
  override def toString = "Empty Floppy"
}