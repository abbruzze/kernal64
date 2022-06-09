package ucesoft.cbm.game

import java.awt.Dimension
import java.net.URL
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.Source

class GameBaseSpi extends GameProvider {
  private case class Games(index:String,pages:Int,games:Future[List[Game]])
  private case class LetterConstraint(letter:Char) extends SyncConstraint {
    override def toString = s"Group $letter"
  }
  
  val url: Option[URL] = Some(new URL("http://www.gb64.com"))
  val iconURL: Option[URL] = Some(new URL("http://www.gb64.com/images/c64top/gamebase64.jpg"))
  val name = "GameBase"   
  val gameIconPreferredSize = new Dimension(320,200)
  private val MAX_PER_PAGE = 45
  private val gamebaseVersionUrl = "http://www.gb64.com/search.php"
  private val gamebaseUrl = "http://www.gb64.com"
  private val gamebaseDownloadUrl = "ftp://8bitfiles.net/gamebase_64/Games"
  private val pages = (65 to 90).toList ++ List(48)
  private val constraints : List[SyncConstraint] = SyncAll :: (pages map { l => LetterConstraint(l.toChar) })
  
  lazy val version: String = {
    try getVersion.getOrElse("??")
    catch {
      case t:Throwable =>
        "N/A"
    }
  }
  
  val repository = new Repository(this)
  
  private var progressListener : Option[GameLoadingProgressListener] = None
  private var currentLoading : Option[Future[List[Game]]] = None
  @volatile private var totalToDownload = 0
  private val currentDownloaded = new AtomicInteger(0)
  @volatile private var interrupted = false
  
  def existsNewerVersion : Boolean = {
    def ver(v:String) = v.split("""\.""").reverse.zipWithIndex.foldLeft(0)((acc,e) => acc + math.pow(e._1.toInt,e._2 + 1).toInt)
    try {
      ver(repository.currentVersion) < ver(version)
    }
    catch {
      case t:NumberFormatException => t.printStackTrace() ; false
    }
  }
  def setProgressListener(l:GameLoadingProgressListener): Unit = progressListener = Some(l)
  
  private def url(index:String,page:Int) = s"$gamebaseUrl/search.php?a=4&l=$index&d=45&h=0&p=$page"
  private def getVersion : Option[String] = {
    val VERSION = """.*Database Version[^<]+<b>([^<]+)</b>.*""".r
    val src = Source.fromURL(gamebaseVersionUrl)
    val lines = src.getLines
    try {
      while (lines.hasNext) {
        lines.next match {
          case VERSION(ver) => return Some(ver)
          case _ =>
        }
      }
      None
    }
    finally {
      src.close
    }
  }
  private def getFiles(index:String) : Int = {
  	val FOUND = """.*Found[^\d]+(\d+).*""".r
  	val src = Source.fromURL(url(index,0))
  	val lines = src.getLines
  	while (lines.hasNext) {
  		val line = lines.next
  		line match {
  			case FOUND(pages) =>
  				src.close
  				return pages.toInt
  			case _ =>
  		}
  	}
  	src.close
  	0
  }
  private def makeGame(name:String,imageUrl:Option[URL],downloadUrl:String) : Game = {
    val PUBLISHED_SH = """.*Published:.*<b>(.+)</b>.*<b>(.+)""".r
    val FILE = """.*GB64-Filename:.*<b>(.+)</b>.*""".r
    val GENRE = """.*Genre:.*<b>(.+)""".r
    val src = Source.fromURL(downloadUrl)
    val lines = src.getLines
    var year : Option[String] = None
    var file : Option[URL] = None
    var genre : Option[String] = None
    var softwareHouse : Option[String] = None
    var found = false
    while (lines.hasNext && !found && !interrupted) {
      val line = lines.next
      line match {
        case PUBLISHED_SH(y,sh) => 
          year = Some(y)
          softwareHouse = Some(sh)
        case FILE(f) if f != "None" => 
          file = Some(new URL(s"$gamebaseDownloadUrl/$f"))
          found = true
        case FILE(f) =>
          found = true
        case GENRE(g) => 
          genre = Some(g)                    
        case _ =>
      }
    }    
    src.close
    Game(name,imageUrl,file,year.getOrElse("??"),genre.getOrElse("??"),softwareHouse.getOrElse("??"))        
  }
  private def getContent(index:String,page:Int) : List[Game] = {
  	//println(s"Getting ${index.toInt.toChar}/$page ...")
    try {
    	val IMAGE = """.*<img src="(http://www.gb64.com/Screenshots/.*\.png).*""".r
    	val NO_IMAGE = """.*<img src="images/game/nosssmall.gif".*""".r
    	val NAME = """.*<a href="(.*)"><b>(.*)</b>.*""".r
    	val src = Source.fromURL(url(index,page))
    	val lines = src.getLines  	
    	val list = new collection.mutable.ListBuffer[Game]
    	while (lines.hasNext && !interrupted) {
    		val line = lines.next
    		line match {
    			case IMAGE(imageUrl) =>
    				val nameLine = lines.next
    				nameLine match {
    					case NAME(url,name) =>
    						list += makeGame(name,Some(new URL(imageUrl)),gamebaseUrl + "/" + url)
    					case _ =>
    				}
    			case NO_IMAGE() =>
    			  val nameLine = lines.next
    				nameLine match {
    					case NAME(url,name) =>
    						list += makeGame(name,None,gamebaseUrl + "/" + url)
    					case _ =>
    				}
    			case _ =>
    		}
    	}  	
    	src.close
    	list.toList
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
  private def getGames(index:String) : Games = {
  	val totalFiles = getFiles(index)
  	val pages = totalFiles / MAX_PER_PAGE + (if (totalFiles % MAX_PER_PAGE == 0) 0 else 1)
  	//println(s"[${index.toInt.toChar}]Total files to retrieve: $totalFiles [$pages pages]")  	
  	val files = for(p <- 0 until pages) yield Future { getContent(index,p) }
  	//files.toList.flatten
  	val games = Future.sequence(files.toList) map { x => x.flatten }
  	Games(index,pages,games)
  }
  
  def games(constraint:Option[SyncConstraint]) : Future[List[Game]] = {
    currentLoading match {
      case Some(cl) => cl
      case None =>
        val future = Future {
          val pages = constraint match {
            case None|Some(SyncAll) => this.pages
            case Some(LetterConstraint(l)) => List(l.toInt)
            case _ => return Future.failed(new IllegalArgumentException("Bad constraint: " + constraint))
          }
          val gamesFuture = for(p <- pages) yield getGames(p.toString) 
          val futures = Future.sequence(gamesFuture map { _.games })
          totalToDownload = gamesFuture map { _.pages } sum ;
          //println(s"Total pages = $totalToDownload")
          currentDownloaded.set(0)  
          currentLoading = Some(futures map { _.flatten })
          currentLoading.get
        }
        future.flatMap { x => x }
    }
  }
  def interrupt  : Unit = {
    interrupted = true
  }  
  def syncConstraints : List[SyncConstraint] = constraints
}