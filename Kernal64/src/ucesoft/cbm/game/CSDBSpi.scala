package ucesoft.cbm.game

import java.awt.Dimension
import java.util.concurrent.atomic.AtomicInteger
import java.net.URL
import org.jsoup.Jsoup
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CSDBSpi extends GameProvider {
  private case object Last100Constraint extends SyncConstraint {
    override def toString = s"Latest 100 releases"
  }
  private case object Last10Constraint extends SyncConstraint {
    override def toString = s"Latest 10 releases"
  }
  private case object Last1000Constraint extends SyncConstraint {
    override def toString = s"Latest 1000 releases"
  }

  val name = "CSDB"
  val url = Some(new URL("https://csdb.dk/"))
  val iconURL = Some(new URL("https://csdb.dk/gfx/csdb-frontlogo.gif"))
  val gameIconPreferredSize = new Dimension(384,272)
  val version = "N/A"
  lazy val repository = new Repository(this)

  val existsNewerVersion = false

  private var progressListener : Option[GameLoadingProgressListener] = None
  private var currentLoading : Option[Future[List[Game]]] = None
  @volatile private var totalToDownload = 0
  private val currentDownloaded = new AtomicInteger(0)
  @volatile private var interrupted = false

  def setProgressListener(l:GameLoadingProgressListener) = progressListener = Some(l)

  def games(constraint:Option[SyncConstraint]) : Future[List[Game]] = {
    currentLoading match {
      case Some(cl) => cl
      case None =>
        val future = Future {
          val total = constraint match {
            case Some(Last10Constraint) => 10
            case Some(Last100Constraint) => 100
            case Some(Last1000Constraint) => 1000
          }

          val root = Jsoup.parse(new URL(s"https://csdb.dk/latestreleases.php?count=$total"),20000)
          val trTable = root.select(s"b:matches(The latest $total.*)~table tr")
          import scala.jdk.CollectionConverters._
          totalToDownload = trTable.size - 2 // first row is the header, last is a form
          val futures = for(tr <- trTable.asScala.drop(1)) yield {
            try {
              if (tr.select("form").size > 0) None
              else {
                val cols = tr.select("td")
                val link = cols.get(1).select("a").attr("href")
                val title = cols.get(1).text()
                val _type = cols.get(2).text()
                val authors = cols.get(3).text()
                val date = cols.get(4).text()
                var imgLink = ""
                var downLink = ""

                Some(Future {
                  val game = Jsoup.parse(new URL(s"https://csdb.dk/$link"), 20000)
                  try {
                    val img = game.select(s"""img[alt=\"$title\"]""")
                    if (img.size() > 0) {
                      imgLink = s"https://csdb.dk/${img.get(0).attr("src")}"
                    }
                    val download = game.select("""b:matches(Download :)~table tr a:matches(http://csdb.*(PRG|prg|ZIP|zip|CRT|crt|(d|D)(64|71|81)))""")
                    if (download.size() > 0) {
                      downLink = download.get(0).text().replaceAll(" ", "%20")
                    }
                  }
                  catch {
                    case t:Throwable =>
                      println(s"CSDB: Unable to download information for game $title: $t")
                      println(game)
                  }

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

                  Game(title, if (imgLink.isEmpty) None else Some(new URL(imgLink)), if (downLink.isEmpty) None else Some(new URL(downLink)), date, _type, authors)
                })
              }
            }
            catch {
              case t:Throwable =>
                println("Error while loading game from CSDB:")
                t.printStackTrace()
                None
            }
          }
          currentLoading = Some(Future.sequence(futures.flatten.toList))

          currentDownloaded.set(0)
          currentLoading.get
        }
        future flatMap { x => x }
    }
  }
  def interrupt  : Unit = {
    interrupted = true
  }
  val syncConstraints : List[SyncConstraint] = List(Last10Constraint,Last100Constraint,Last1000Constraint)
}
