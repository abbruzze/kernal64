package ucesoft.cbm.peripheral.keyboard

import java.io.PrintWriter
import java.awt.event.KeyEvent
import java.io.BufferedReader
import java.io.InputStreamReader
import ucesoft.cbm.Log
import java.io.FileInputStream

trait KeyboardMapper {
	val map : Map[Int,CKey.Key]
	val cmap : Map[Char,(CKey.Key,Boolean)]
	val keypad_map : Map[Int,CKey.Key]
}

object KeyboardMapperStore {
  val KEY_EVENT_MAP = getKeyEventMap
  private val KEY_EVENT_REV_MAP = getKeyEventMap map { kv => (kv._2,kv._1) }
  
  def store(km:KeyboardMapper,out:PrintWriter) {
    out.println("[map]")
    for(kv <- km.map) {
      out.println("%20s = %s".format(s"${KEY_EVENT_MAP(kv._1)}",s"${kv._2}"))
    }
    out.println("[cmap]")
    for(kv <- km.cmap) {
      out.println("%20s = %s".format(s"${kv._1}${if (kv._2._2) "/shifted" else ""}",s"${kv._2._1}"))
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
    val e_cmap = new collection.mutable.HashMap[Char,(CKey.Key,Boolean)]
    val e_keypad_map = new collection.mutable.HashMap[Int,CKey.Key]
    
    var line = in.readLine
    var section = 0
    while (line != null) {
      line = line.trim
      section match {
        case 0 =>  
          if (line == "[map]") section += 1
        case 1 => // map
          if (line == "[cmap]") section += 1
          else {
            val Array(n,v) = line.split("=")
            e_map += KEY_EVENT_REV_MAP(n.trim) -> CKey.withName(v.trim)
          }
        case 2 => // cmap
          if (line == "[keypad_map]") section += 1
          else {
            val Array(n,v) = line.split("=")
            val (ch,shifted) = if (n.trim.endsWith("/shifted")) (n.split("/")(0).charAt(0),true) else (n.charAt(0),false)
            e_cmap += ch -> (CKey.withName(v.trim),shifted)
          }
        case 3 => // keypad_map
          val Array(n,v) = line.split("=")
          e_keypad_map += KEY_EVENT_REV_MAP(n.trim) -> CKey.withName(v.trim)
          
      }
      line = in.readLine
    }
    
    if (section == 0) throw new IllegalArgumentException
    
    new KeyboardMapper {
      val map = e_map.toMap
      val cmap = e_cmap.toMap
      val keypad_map = e_keypad_map.toMap
    }
  }
  
  private def getKeyEventMap : Map[Int,String] = {
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields filter { _.getName.startsWith("VK_") } map { f => (f.get(null).asInstanceOf[Int],f.getName) } toMap
  }
  
  def loadMapper(externalFile:Option[String],internalResource:String,defaultMapper:KeyboardMapper) : KeyboardMapper = {
    externalFile match {
      case None =>
        loadFromResource(internalResource) match {
          case None => defaultMapper
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
            defaultMapper
        }
    }
  }
  
  def main(args:Array[String]) {
    store(ucesoft.cbm.c128.C128KeyboardMapper,new java.io.PrintWriter(System.out,true))
  }
}