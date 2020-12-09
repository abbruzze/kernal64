package ucesoft.cbm.peripheral.keyboard

import java.io.{BufferedReader, FileInputStream, FileNotFoundException, IOException, InputStreamReader, PrintWriter}
import java.awt.event.KeyEvent

import ucesoft.cbm.Log

trait KeyboardMapper {
	val map : Map[Int,CKey.Key]
	val keypad_map : Map[Int,CKey.Key]
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
  
  def store(km:KeyboardMapper,out:PrintWriter) : Unit = {
    out.println("[map]")
    for(kv <- km.map) {
      KEY_EVENT_MAP get kv._1 match {
        case Some(k) =>
          out.println("%20s = %s".format(s"$k",s"${kv._2}"))
        case None =>
          out.println("%20s = %s".format(s"#${kv._1}",s"${kv._2}"))
      }
    }
    out.println("[keypad_map]")
    for(kv <- km.keypad_map) {
      out.println("%20s = %s".format(s"${KEY_EVENT_MAP(kv._1)}",s"${kv._2}"))
    }
  }
  
  def loadFromResource(name:String) : Option[KeyboardMapper] = {
    val in = getClass.getResourceAsStream(name)
    if (in == null) None
    else {
      try {
        val map = Some(load(new BufferedReader(new InputStreamReader(in))))
        in.close
        map
      }
      catch {
        case t:Throwable =>
          Log.info(s"Can't load keyboard mapping '$name': " + t)
          None
      }
    }
  }
  
  def load(in:BufferedReader) : KeyboardMapper = {
    val e_map = new collection.mutable.HashMap[Int,CKey.Key]
    val e_keypad_map = new collection.mutable.HashMap[Int,CKey.Key]
    
    var line = in.readLine
    var section = 0
    while (line != null) {
      line = line.trim
      section match {
        case 0 =>  
          if (line == "[map]") section += 1
        case 1 => // map
          if (line == "[keypad_map]") section += 1
          else {
            val Array(n,v) = line.split("=")
            val k = n.trim
            val key = if (k.charAt(0) == '#') k.substring(1).toInt else KEY_EVENT_REV_MAP(k)
            e_map += key -> CKey.withName(v.trim)
          }
        case 2 => // keypad_map
          val Array(n,v) = line.split("=")
          e_keypad_map += KEY_EVENT_REV_MAP(n.trim) -> CKey.withName(v.trim)
          
      }
      line = in.readLine
    }
    
    if (section == 0) throw new IllegalArgumentException

    // add l-shift button
    e_map += KeyEvent.VK_SHIFT -> CKey.L_SHIFT
    
    new KeyboardMapper {
      val map = e_map.toMap
      val keypad_map = e_keypad_map.toMap
    }
  }
  
  private def getKeyEventMap : Map[Int,String] = {
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields filter { _.getName.startsWith("VK_") } map { f => (f.get(null).asInstanceOf[Int],f.getName) } toMap
  }

  private def findDefaultKeyboardLayoutForLocale(internalResource:String) : String = {
    Option(java.awt.im.InputContext.getInstance().getLocale) match {
      case None =>
        Log.info("Cannot find any keyboard layout for current locale. Switching to IT")
        s"${internalResource}_IT"
      case Some(loc) =>
        s"${internalResource}_${loc.getLanguage.toUpperCase}"
    }
  }
  
  def loadMapper(externalFile:Option[String],_internalResource:String) : KeyboardMapper = {
    externalFile match {
      case None =>
        val internalResource = findDefaultKeyboardLayoutForLocale(_internalResource)
        loadFromResource(internalResource) match {
          case None =>
            // layout not found, switching to IT
            loadFromResource(s"${_internalResource}_IT") match {
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
          val m = load(in)
          in.close
          Log.info(s"Loaded keyboard configuration file from $file")
          m
        }
        catch {
          case t:Throwable =>
            Log.info(s"Cannot load keyboard file $file: " + t)
            println(s"Cannot load keyboard file $file: " + t)
            loadMapper(None,_internalResource)
        }
    }
  }
}