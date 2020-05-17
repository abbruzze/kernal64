package ucesoft.cbm.game

import java.net.URL
import scala.concurrent.Future
import java.security.MessageDigest
import java.awt.Dimension
import java.io.File

final case class Game(name:String,imageURL:Option[URL],var downloadPageURL:Option[URL],date:String,genre:String,softwareHouse:String) {
  lazy val id = {
    val sign = name + imageURL+ downloadPageURL + date + genre + softwareHouse
    val md = MessageDigest.getInstance("SHA")
    val hash = md.digest(sign.getBytes)
    hash map { c => "%02X".format(c) } mkString
  }
}

trait GamePlayer {
  def play(file:File) : Unit
  def attachDevice(file:File) : Unit
}

trait GameLoadingProgressListener {
  def update(perc:Int) : Unit
}

trait SyncConstraint
case object SyncAll extends SyncConstraint {
  override def toString = "All"
}

trait GameProvider {
  val url : Option[URL]
  val iconURL : Option[URL]
  val gameIconPreferredSize : Dimension
  val name : String
  val version : String
  val repository : Repository
  
  def existsNewerVersion : Boolean
  def setProgressListener(l:GameLoadingProgressListener) : Unit
  def games(constraint:Option[SyncConstraint]) : Future[List[Game]]
  def syncConstraints : List[SyncConstraint]
  def interrupt : Unit
}