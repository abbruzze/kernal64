package ucesoft.cbm.game

import java.net.URL
import java.awt.Dimension
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import concurrent.ExecutionContext.Implicits.global

abstract class PouetSpi(genre:String) extends GameProvider {
  private case object LastConstraint extends SyncConstraint {
    override def toString = s"Last added"
  }  
  
  val url = Some(new URL("http://www.pouet.net/"))
  val iconURL = Some(new URL("http://content.pouet.net/logos/ewery_pouet2.png"))
  val gameIconPreferredSize = new Dimension(384,272)
  val version = "N/A"
  lazy val repository = new Repository(this)
  
  val existsNewerVersion = false
  
  private var progressListener : Option[GameLoadingProgressListener] = None
  private var currentLoading : Option[Future[List[Game]]] = None
  @volatile private var totalToDownload = 0
  private val currentDownloaded = new AtomicInteger(0)
  @volatile private var interrupted = false
  private val constraints : List[SyncConstraint] = List(SyncAll,LastConstraint)
  
  def setProgressListener(l:GameLoadingProgressListener) = progressListener = Some(l)
  
  private def makeUrl(page:Int) = s"http://www.pouet.net/prodlist.php?type%5B%5D=$genre&order=release&platform%5B%5D=Commodore+64&page=$page"
  
  private def openURL[T](url:String)(action: Iterator[String] => T) : T = {
    val src = io.Source.fromURL(url,"UTF-8")
    try {
      action(src.getLines)
    }
    finally {
      src.close
    }
  }
  
  private def getTotalPages : Int = {
    val OPTION_BEGIN = """.*<select name='page'>.*""".r
    openURL(makeUrl(1)) { lines =>
      while (lines.hasNext) {
        var line = lines.next
        line match {
          case OPTION_BEGIN() =>
            var optionCount = 0
              line = lines.next
              while (line.indexOf("<option value=") != -1) {
                optionCount += 1
                line = lines.next
              }
            return optionCount
          case _ =>
        }
      }
    }
    1
  }
   
  private def makeGame(name:String,url:String,group:String,date:String) : Game = {
    val IMAGE = """.*<td.+id='screenshot'><img src='([^']+)'.*""".r
    val GAME_LINK = """.*\[<a id='mainDownloadLink' href='(.+)'>.*""".r
    openURL(url) { lines =>
      var found = false
      var imageURL : Option[URL] = None
      var downloadURL : Option[URL] = None
      
      while (lines.hasNext && !found && !interrupted) {
        lines.next match {
          case IMAGE(imageUrl) => imageURL = Some(new URL(imageUrl))
          case GAME_LINK(gameLink) =>
            found = true
            downloadURL = Some(new URL(gameLink))
          case _ =>
        }
      }
      Game(name,imageURL,downloadURL,date,genre,group)
    }
  } 
  
  private def getContent(page:Int) : List[Game] = {
    val BEGIN_GAME = s""".*<span class='typeiconlist'><span class='.+' title='$genre'>.*""".r
    val GAME_LOC= """.*</span><span class='prod'><a href='(.+)'>(.+)</a></span>.*""".r
    val GROUP = """.*<a href='groups\.php\?which=.+'>(.+)</a>.*""".r
    val DATE = """.*<td class='date'>(.+)</td>.*""".r
    
    val buffer = new collection.mutable.ListBuffer[Game]
    try {
      openURL(makeUrl(page)) { lines =>
        while (lines.hasNext && !interrupted) {
          val line = lines.next
          line match {
            case BEGIN_GAME() =>
              var found = false
              while (lines.hasNext && !interrupted && !found) {
                lines.next match {
                  case GAME_LOC(location,name) =>
                    var innerFound = false
                    var group = "N/A"
                    var date = ""
                    while (lines.hasNext && !innerFound && !interrupted) {
                      lines.next match {
                        case GROUP(g) => group = g
                        case DATE(d) =>
                          date = d
                          innerFound = true
                        case _ =>
                      }
                    }
                    val gameURL = s"http://www.pouet.net/$location"
                    try {
                      buffer += makeGame(name,gameURL,group,date)
                    }
                    catch {
                      case t:Throwable =>
                        println(s"Can't download info for game $name and url $gameURL")
                    }
                    found = true
                  case _ =>                    
                }  
              }
            case _ =>
          }
        }
      }
      
      buffer.toList
    }
    finally {
      val cd = currentDownloaded.addAndGet(1)
      if (interrupted && cd == totalToDownload) {
        interrupted = false
        currentLoading = None
      }
      progressListener match {
        case Some(pl) if totalToDownload > 0 =>
          val perc = (cd / totalToDownload.toDouble * 100.0).toInt
          pl.update(perc)          
        case _ =>
      }
    }
  }
  
  def games(constraint:Option[SyncConstraint]) : Future[List[Game]] = {
    currentLoading match {
      case Some(cl) => cl
      case None =>
        val future = Future {
          val lastPage = constraint match {
            case None|Some(SyncAll) => getTotalPages
            case Some(LastConstraint) => 10
            case _ => return Future.failed(new IllegalArgumentException("Bad constraint: " + constraint))
          }
          
          totalToDownload = lastPage
          val futureList = for(p <- (1 to lastPage).toList) yield Future { getContent(p) }
          val futures = Future.sequence(futureList)
          currentDownloaded.set(0)  
          currentLoading = Some(futures map { _.flatten })
          currentLoading.get
        }
        future flatMap { x => x }
    }
  }
  def interrupt  : Unit = {
    interrupted = true
  }
  val syncConstraints : List[SyncConstraint] = constraints
}

class PouetDemoSpi extends PouetSpi("demo") { val name = "Pouet - Demo" }