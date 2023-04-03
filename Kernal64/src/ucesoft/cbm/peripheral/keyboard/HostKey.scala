package ucesoft.cbm.peripheral.keyboard

case class HostKey(code:Int,shifted:Boolean,altG:Boolean) {
  private var noshift = false
  private var numberCode = false
  private var osFilter : Set[String] = Set.empty

  def isNoShift(): Boolean = noshift
  def isNumberCode(): Boolean = numberCode
  def mustBeFilteredByOS(): Boolean = {
    if (osFilter.isEmpty) return false

    val os = System.getProperty("os.name").toUpperCase()
    !osFilter.exists(os.startsWith)
  }

  def flags : String = {
    val sb = new StringBuilder()
    if (noshift) sb.append("-")
    if (shifted) sb.append("+")
    if (altG) sb.append("^")
    if (numberCode) sb.append("!")
    sb.toString()
  }

  override def toString: String = {
    val keyText = if (!numberCode) java.awt.event.KeyEvent.getKeyText(code) else code.toString
    s"$flags$keyText"
  }
}

object HostKey {
  def parse(key:String,keyMap: String => Option[Int]): Option[HostKey] = {
    var shifted = false
    var altg = false
    var code = false
    var noshift = false
    var k = key
    val osSet = new collection.mutable.HashSet[String]

    if (k.startsWith("[") && k.indexOf("]") != -1) {
      var p = 1
      var c = k.charAt(p)
      while (c != ']') {
        c.toUpper match {
          case 'W' => osSet.add("WINDOWS")
          case 'X' => osSet.add("LINUX")
          case 'M' => osSet.add("MAC")
          case _ =>
            return None
        }
        p += 1
        c = k.charAt(p)
      }
      k = k.substring(p + 1)
    }

    while (!k.charAt(0).isLetterOrDigit && k.length > 0) {
      if (k.startsWith("+")) {
        shifted = true
        k = k.substring(1)
      }
      else if (k.startsWith("^")) {
        altg = true
        k = k.substring(1)
      }
      else if (k.startsWith("!")) {
        code = true
        k = k.substring(1)
      }
      else if (k.startsWith("-")) {
        noshift = true
        k = k.substring(1)
      }
      else return None
    }
    val keyCode = code match {
      case true =>
        try {
          if (k.startsWith("0x")) Integer.parseInt(k.substring(2),16) else k.toInt
        }
        catch {
          case _:NumberFormatException =>
            return None
        }
      case false =>
        keyMap(k) match {
          case Some(c) => c
          case None => return None
        }
    }
    val hk = HostKey(keyCode,shifted,altg)
    hk.noshift = noshift
    hk.numberCode = code
    hk.osFilter = osSet.toSet
    Some(hk)
  }
}
