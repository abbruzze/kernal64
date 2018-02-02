package ucesoft.cbm.peripheral.drive

import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.DataOutputStream
import ucesoft.cbm.formats.Diskette

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
        val floppy : Floppy = Diskette(fileName)
        floppy.load(in)
        Some(floppy)
      case false =>
        None
    }    
  }  
  def save(out:ObjectOutputStream,floppy:Option[Floppy]) {
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
  val isReadOnly : Boolean
  val isFormattable : Boolean
  val totalTracks : Int
  val file : String
  lazy val singleSide = true
  val isEmpty = false
  
  def minTrack = 1
  def maxTrack = totalTracks
  
  def side : Int = 0
  def side_=(newSide:Int) {}
  
  def nextBit : Int
  def writeNextBit(bit:Boolean)
  
  def currentTrack : Int
  def currentSector : Option[Int]
  def changeTrack(trackSteps:Int)
  def setTrackChangeListener(l:TrackListener)
  def notifyTrackSectorChangeListener
  
  def format(diskName:String)
  
  def defaultZoneFor(track:Int) : Int = {
    if (track <= 17) 0
    else if (track <= 24) 1
    else if (track <= 30) 2
    else 3
  }
  
  def close
  def reset
  // state
  def save(out:ObjectOutputStream)
  def load(in:ObjectInputStream)
  
  protected def saveFile(out:ObjectOutputStream) {
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

object EmptyFloppy extends Floppy {
  val isReadOnly = false
  val isFormattable = false
  val totalTracks = 35
  val file = ""
  override val isEmpty = true
  
  private[this] var track = 1
  private[this] var listener : TrackListener = _
  
  def nextBit = 0
  def writeNextBit(bit:Boolean) {}
  
  def currentTrack = track
  def currentSector = None
  def changeTrack(trackSteps:Int) {
    val isOnTrack = (trackSteps & 1) == 0
    if (isOnTrack) {
      track = trackSteps >> 1
    }
    notifyTrackSectorChangeListener
  }
  def setTrackChangeListener(l:TrackListener) = listener = l
  def notifyTrackSectorChangeListener {
    if (listener != null) listener(track,false,None)
  }
  
  def format(diskName:String) = throw new IllegalArgumentException("Empty disk. Can't format")
  
  def close {}
  def reset {
    track = 1
  }
  // state
  def save(out:ObjectOutputStream) {}
  def load(in:ObjectInputStream) {}
  override def toString = "Empty Floppy"
}