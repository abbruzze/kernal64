package ucesoft.cbm.peripheral.keyboard

import ucesoft.cbm.{C128Model, C64Model, CBMComputerModel, CBMIIModel, Log, VIC20Model}
import ucesoft.cbm.peripheral.keyboard.CKey.{Key, L_SHIFT}

import java.awt.event.KeyEvent
import java.io._

trait KeyboardMapper {
  val configuration : Option[String]
  val locale : Option[String]
  val content: String
	val map : Map[HostKey,List[CKey.Key]]
	val keypad_map : Map[HostKey,List[CKey.Key]]
}

object KeyboardMapperStore {
  private val KEY_EVENT_MAP = getKeyEventMap
  private val KEY_EVENT_REV_MAP = getKeyEventMap map { kv => (kv._2,kv._1) }

  def getKey(code:Int) : String = KEY_EVENT_MAP get code match {
    case None =>
      KeyEvent.getKeyText(code)
    case Some(k) =>
      k.substring(3)
  }

  def isExtendedKey(code:Int) : Boolean = !KEY_EVENT_MAP.contains(code)
  
  def store(km:KeyboardMapper,out:PrintWriter,model:CBMComputerModel) : Unit = {
    for(m <- 1 to 2) {
      val map = m match {
        case 1 =>
          out.println("[map]")
          km.map
        case 2 =>
          out.println("[keypad_map]")
          km.keypad_map
      }
      for (kv <- map) {
        val VK = if (kv._1.isNumberCode()) s"!${kv._1.code.toString}" else KEY_EVENT_MAP.getOrElse(kv._1.code, "??")
        out.println(s"$VK\t\t\t=${kv._2.map(k => CKey.getKeyWithoutPrefix(k,model)).mkString(",")}")
      }
    }
  }
  
  private def loadFromResource(name:String,model:CBMComputerModel) : Option[KeyboardMapper] = {
    val in = getClass.getResourceAsStream(name)
    if (in == null) None
    else {
      try {
        val map = Some(load(new BufferedReader(new InputStreamReader(in)),model,None,Some(name.takeRight(2))))
        in.close()
        map
      }
      catch {
        case t:Throwable =>
          t.printStackTrace()
          Log.info(s"Can't load keyboard mapping '$name': " + t)
          None
      }
    }
  }

  private def load(in:BufferedReader,model:CBMComputerModel,file:Option[String],_locale:Option[String]) : KeyboardMapper = {
    val e_map = new collection.mutable.HashMap[HostKey,List[CKey.Key]]
    val e_keypad_map = new collection.mutable.HashMap[HostKey,List[CKey.Key]]
    
    var line = in.readLine
    var map : collection.mutable.HashMap[HostKey,List[CKey.Key]] = null

    val VIRTUAL_SHIFT = model match {
      case C64Model | C128Model => CKey.L_SHIFT
      case VIC20Model => CKey.VIC20_L_SHIFT
      case CBMIIModel => CKey.CBM2_SHIFT
    }

    val fileContent = new StringBuilder
    while (line != null) {
      fileContent.append(line)
      fileContent.append('\n')

      line = line.trim
      //println(line)
      if (!line.startsWith("#") && !line.isEmpty) {
        if (line == "[map]") map = e_map
        else if (line == "[keypad_map]") map = e_keypad_map
        else {
          val Array(n, v) = line.split("=")
          val k = n.trim
          val lineComment = v.indexOf("#")
          val emulatedKeys = if (lineComment == -1) v else v.substring(0,lineComment)
          val ckeys = emulatedKeys.split(",").map(k => CKey.getKey(k.trim,model)).toList
          HostKey.parse(k,s => KEY_EVENT_REV_MAP.get(if (!s.startsWith("VK_")) "VK_" + s else s)) match {
            case Some(hk) =>
              if (!hk.mustBeFilteredByOS()) { // check if the key must not be configured for this OS
                map += hk -> ckeys
                if (!hk.isNoShift() && !hk.shifted) map += hk.copy(shifted = true) -> (VIRTUAL_SHIFT :: ckeys)
              }
            case None =>
              throw new IllegalArgumentException
          }
        }
      }
      line = in.readLine
    }
    
    if (map == null) throw new IllegalArgumentException

    e_map += HostKey(KeyEvent.VK_SHIFT,true,false) -> List(VIRTUAL_SHIFT)
    
    new KeyboardMapper {
      override val configuration = file
      override val locale = _locale
      override val content = fileContent.toString
      override val map: Map[HostKey, List[Key]] = e_map.toMap
      override val keypad_map: Map[HostKey, List[Key]] = e_keypad_map.toMap
    }
  }
  
  private def getKeyEventMap : Map[Int,String] = {
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields filter { _.getName.startsWith("VK_") } map { f => (f.get(null).asInstanceOf[Int],f.getName) } toMap
  }

  private def convertLayoutName(name:String): String = {
    name.toUpperCase match {
      case "US" => "EN"
      case _ => name
    }
  }

  private def checkLinuxKeyboardLayout(): Option[String] = {
    import sys.process._
    try {
      /*
        Example of output:
        rules:      evdev
        model:      pc105
        layout:     it
      */
      "setxkbmap -query".!!.split("\n").filter(_.trim.toUpperCase().startsWith("LAYOUT:")).headOption match {
        case Some(layout) =>
          layout.toUpperCase().substring("LAYOUT:".length).trim.split(",").headOption
        case None =>
          None
      }
    }
    catch {
      case _:Throwable =>
        None
    }
  }

  private def getDefaultKeyboardByInputContext(): String = Option(java.awt.im.InputContext.getInstance().getLocale).map(_.getLanguage).getOrElse("IT")

  /*
    Check keyboard.layout env variabile first.
    If not set:
      1. If OS is linux try to use setxkbmap command to extract layout
      2. If command fails or OS is not linux try to use InputContext
   */
  private def findDefaultKeyboardLayoutForLocale(internalResource:String) : String = {
    val _layout = if (System.getProperty("keyboard.layout") != null) System.getProperty("keyboard.layout") else {
      if (System.getProperty("os.name").toUpperCase().startsWith("LINUX")) {
        checkLinuxKeyboardLayout() match {
          case Some(layout) => layout
          case None =>
            getDefaultKeyboardByInputContext()
        }
      }
      else getDefaultKeyboardByInputContext()
    }
    val layout = convertLayoutName(_layout)
    println(s"Using '${layout.toUpperCase()}' keyboard layout")
    s"${internalResource}_${layout.toUpperCase}"
  }
  
  def loadMapper(externalFile:Option[String],_internalResource:String,model:CBMComputerModel) : KeyboardMapper = {
    externalFile match {
      case None =>
        val internalResource = findDefaultKeyboardLayoutForLocale(_internalResource)
        loadFromResource(internalResource,model) match {
          case None =>
            println(s"Cannot find internal default layout '$internalResource'. Using IT layout.")
            // layout not found, switching to IT
            loadFromResource(s"${_internalResource}_IT",model) match {
              case None =>
                throw new FileNotFoundException(s"Can't find default keyboard file: ${_internalResource}")
              case Some(m) =>
                Log.info(s"Loaded keyboard configuration file from $internalResource")
                m
            }
          case Some(m) =>
            Log.info(s"Loaded keyboard configuration file from $internalResource")
            m
        }
      case Some(file) =>
        try {
          val in = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
          val m = load(in,model,Some(file),None)
          in.close()
          Log.info(s"Loaded keyboard configuration file from $file")
          println(s"Loaded keyboard layout from $file")
          m
        }
        catch {
          case t:Throwable =>
            Log.info(s"Cannot load keyboard file $file: " + t)
            println(s"Cannot load keyboard file $file: ")
            t.printStackTrace()
            loadMapper(None,_internalResource,model)
        }
    }
  }
}