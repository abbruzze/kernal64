package ucesoft.cbm.misc

import java.util.Properties
import scala.collection.mutable

object Preferences {
  final val TOTALDRIVES       = 4
  // KEYS ========================================================
  val PREF_WARP               = "warp"
  val PREF_HEADLESS           = "headless"
  val PREF_TESTCART           = "testcart"
  val PREF_LIMITCYCLES        = "limitcycles"
  val PREF_RUNFILE            = "run-file"
  val PREF_SCREENSHOT         = "screenshot"
  val PREF_CPUJAMCONTINUE     = "cpujam-continue"
  val PREF_CIAMODEL           = "cia-model"
  val PREF_LOADSTATE          = "load-state"
  val PREF_SCREENDIM          = "screen-dim"
  val PREF_FULLSCREEN         = "fullscreen"
  val PREF_IGNORE_CONFIG_FILE = "ignore-config-file"
  val PREF_KERNEL             = "kernel"
  val PREF_BASIC              = "basic"
  val PREF_CHARROM            = "charrom"
  val PREF_1541DOS            = "1541dos"
  val PREF_1571DOS            = "1571dos"
  val PREF_1581DOS            = "1581dos"
  val PREF_VICIINEW           = "viciinew"
  val PREF_WARPONLOAD         = "warponload"
  val PREF_PRGASDISK          = "prg-as-disk"
  val PREF_DRIVE_X_ENABLED: Array[String] = (for(d <- 0 until TOTALDRIVES) yield s"drive${8 + d}-enabled").toArray
  val PREF_DRIVE_X_FILE: Array[String] = (for(d <- 0 until TOTALDRIVES) yield s"drive${8 + d}-file").toArray
  val PREF_DRIVE_X_TYPE: Array[String] = (for(d <- 0 until TOTALDRIVES) yield s"drive${8 + d}-type").toArray
  val PREF_DRIVE12LOCALPATH   = "drive12-local-path"
  val PREF_WRITEONDISK        = "write-on-disk"
  val PREF_CART               = "cart"
  val PREF_RAW_CART           = "raw-cart"
  val PREF_PREFAUTOSAVE       = "pref-auto-save"
  val PREF_TRACE              = "trace"
  val PREF_VICPALETTE         = "vic-palette"
  val PREF_RENDERINGTYPE      = "rendering-type"
  val PREF_PRINTERENABLED     = "printer-enabled"
  val PREF_SID8580            = "sid-8580"
  val PREF_DUALSID            = "dual-sid"
  val PREF_SIDCYCLEEXACT      = "sid-cycle-exact"
  val PREF_REUTYPE            = "reu-type"
  val PREF_GEORAM             = "geo-ram"
  val PREF_CPMCARTENABLED     = "cpm64-enabled"
  val PREF_BEAMRACERENABLED   = "beam-racer-enabled"
  val PREF_NTSC               = "ntsc"
  val PREF_VICBORDEROFF       = "vic-border-off"
  val PREF_CUSTOMGLUELOGIC    = "custom-glue-logic"
  val PREF_VDC80STARTUP       = "vdc-80-startup"
  val PREF_VDCDISABLED        = "vdc-disabled"
  val PREF_MMUPANELENABLED    = "mmupanel-enabled"
  val PREF_VDCSCREENSHOT      = "vdcscreenshot"
  val PREF_128EXTROM          = "ext-rom"
  val PREF_128INTROM          = "int-rom"
  val PREF_128GO64            = "go64"
  val PREF_VDCFULLSCREEN      = "vdc-fullscreen"
  val PREF_KERNEL128          = "kernel128"
  val PREF_CHARROM128         = "charrom128"
  val PREF_SCPURAM            = "scpu-ram"
  val PREF_SCPUJIFFYDOSENABLED= "scpu-jiffydos-enabled"
  val PREF_WIC64_NETWORK      = "wic64-network-name"
  val PREF_WIC64_ENABLED      = "wic64-enabled"
  val PREF_MOUSE_DELAY_MILLIS = "mouse-delay-millis"
  // ================== VIC 20 ===================================
  val VIC20_PREF_KERNEL_PAL   = "kernel-pal"
  val VIC20_PREF_KERNEL_NTSC  = "kernel-ntsc"
  val PREF_VIC20_MEM_CONFIG   = "exp"
  val PREF_VIC20_IO2_ENABLED  = "io2"
  val PREF_VIC20_IO3_ENABLED  = "io3"
  val PREF_VIC20_8K_EXP       = "8k"
  val PREF_VIC20_16K_EXP      = "16k"
  val PREF_VIC20_24K_EXP      = "24k"
  val PREF_VIC20_32K_EXP      = "32k"
  val PREF_VIC20_ULTIMEM      = "ultimem"
  // =============================================================
  class PreferenceIllegalArgumentException(msg:String) extends Exception(msg)

  trait PreferenceChangeListener {
    def preferenceHasChanged(pref:Preference[_]) : Unit
  }

  trait PreferenceConv[T] {
    def convert(value:String) : T
    val consume : Int
    val default : T
  }

  implicit object StringPreferenceConv extends PreferenceConv[String] {
    def convert(value:String): String = value
    val default = ""
    val consume = 1
  }
  implicit object IntPreferenceConv extends PreferenceConv[Int] {
    def convert(value:String): Int = value.toInt
    val default = 0
    val consume = 1
  }
  implicit object BooleanPreferenceConv extends PreferenceConv[Boolean] {
    def convert(value:String): Boolean = value.toBoolean
    val default = false
    val consume = 0
  }

  case class Preference[T](cmdLine:String,description:String,var value:T,enumerated:Set[T] = Set.empty[T],canBeSaved : Boolean = true)(listener : T => Unit)(implicit prefConv:PreferenceConv[T]) {
    private var listeners : List[PreferenceChangeListener] = Nil
    private[Preferences] var adjusting = true
    private var loaded = false

    def isAdjusting : Boolean = adjusting
    def isLoaded : Boolean = loaded

    listeners ::= new PreferenceChangeListener {
      override def preferenceHasChanged(pref: Preference[_]): Unit = listener(value)
    }

    def addChangeListener(l:PreferenceChangeListener) : Unit = if (!listeners.contains(l)) listeners ::= l

    def notifyListeners() : Unit = for(l <- listeners) l.preferenceHasChanged(this)

    def load(p:Properties) : Unit = {
      p.getProperty(cmdLine) match {
        case null =>
        case v => load(v)
      }
    }

    def load(from:String) : Unit = load(prefConv.convert(from))

    def load(v:T,notify:Boolean = true) : Unit = {
      loaded = true
      val prevValue = value
      value = v
      if (!enumerated.isEmpty && !enumerated.contains(value)) {
        val badValue = value
        value = prevValue
        throw new PreferenceIllegalArgumentException(s"Bad value '$badValue' for option '$cmdLine'. Expected: ${enumerated.mkString(",")}")
      }
      if (prevValue != value && notify) notifyListeners
    }

    def save(p:Properties) : Unit = {
      if (loaded && canBeSaved) p.setProperty(cmdLine,if (value != null) value.toString else "")
    }

    def consume : Int = prefConv.consume
  }
}

class Preferences {
  import Preferences._

  private[this] val prefs = new collection.mutable.ListBuffer[Preference[_]]

  def add[T](cmdLine:String,description:String,value:T,enumerated:Set[T] = Set.empty[T],canBeSaved:Boolean = true)(listener : T => Unit)(implicit prefConv:PreferenceConv[T]) : Preference[T] = {
    val p = Preference(cmdLine,description,value,enumerated,canBeSaved)(listener)
    prefs += p
    p
  }

  def save(p:Properties) : Unit = {
    for(s <- preferences) s.save(p)
  }

  def remove(cmdLine:String) : Unit = {
    prefs.find(_.cmdLine == cmdLine).foreach( prefs -= _ )
  }

  def apply[T](cmdLine:String) : Option[T] = {
    prefs.find(_.cmdLine == cmdLine) match {
      case Some(p) if p.isLoaded => Some(p.value.asInstanceOf[T])
      case _ => None
    }
  }

  def get[T](cmdLine:String) : Option[Preference[T]] = prefs.find(_.cmdLine == cmdLine).asInstanceOf[Option[Preference[T]]]

  def update[T](cmdLine:String,value:T) : Unit = prefs.find(_.cmdLine == cmdLine).foreach(_.asInstanceOf[Preference[T]].load(value))

  def updateWithoutNotify[T](cmdLine:String,value:T) : Unit = prefs.find(_.cmdLine == cmdLine).foreach(_.asInstanceOf[Preference[T]].load(value,false))

  def checkForHelp(args:Array[String]) : Boolean = args.length == 1 && (args(0) == "--help" || args(0) == "-h" || args(0) == "-help")

  def printUsage(fileDescr:String)  : Unit = {
    println(s"Usage: [settings] [$fileDescr]")
    for(s <- prefs) {
      val opt = if (s.cmdLine.length > 20) s.cmdLine else s.cmdLine + (" " * (20 - s.cmdLine.length))
      println("--" + opt + s.description)
    }
  }

  def preferences : List[Preference[_]] = prefs.toList

  def parseAndLoad(args:Array[String],props:Properties) : Option[String] = {
    var p = 0
    val found : collection.mutable.Set[String] = new mutable.HashSet[String]
    while (p < args.length && args(p).startsWith("--")) {
      try {
        prefs find {
          _.cmdLine == args(p).substring(2)
        } match {
          case Some(s) if s.consume == 0 =>
            if (p + 1 < args.length && !args(p + 1).startsWith("--")) {
              s.load(args(p + 1))
              p += 2
            }
            else {
              s.load("true")
              p += 1
            }
            found += s.cmdLine
          case Some(s) if s.consume == 1 =>
            if (p + 1 < args.length) s.load(args(p + 1))
            else throw new PreferenceIllegalArgumentException("Value for setting " + args(p) + " not found")
            p += 2
            found += s.cmdLine
          case None =>
            throw new PreferenceIllegalArgumentException("Invalid setting: " + args(p))
        }
      }
      catch {
        case e:Throwable =>
          throw new PreferenceIllegalArgumentException(s"error while applying command option ${args(p)}: ${e.getMessage}")
      }
    }
    // check props
    val filtered = prefs.filterNot(p => found.contains(p.cmdLine))
    filtered.foreach(_.load(props))

    // stop adjusting
    for(p <- prefs) p.adjusting = false

    if (p < args.length) Some(args(p)) else None
  }
}
