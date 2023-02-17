package ucesoft.cbm.peripheral.keyboard

case class HostKey(code:Int,shifted:Boolean,altG:Boolean) {
  private var noshift = false
  private var numberCode = false
  private var osFilter : String = _

  def isNoShift(): Boolean = noshift
  def isNumberCode(): Boolean = numberCode
  def mustBeFilteredByOS(): Boolean = osFilter != null && !System.getProperty("os.name").toUpperCase().startsWith(osFilter)

  def flags : String = {
    val sb = new StringBuilder()
    if (shifted) sb.append("+")
    if (altG) sb.append("^")
    if (noshift) sb.append("-")
    if (numberCode) sb.append("!")
    sb.toString()
  }
}

object HostKey {
  def parse(key:String,keyMap: String => Option[Int]): Option[HostKey] = {
    var shifted = false
    var altg = false
    var code = false
    var noshift = false
    var k = key
    var os : String = null

    if (k.startsWith("[x]")) {
      k = k.substring(3)
      os = "LINUX"
    }
    else if (k.startsWith("[w]")) {
      k = k.substring(3)
      os = "WINDOWS"
    }
    else if (k.startsWith("[m]")) {
      k = k.substring(3)
      os = "MAC"
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
    hk.osFilter = os
    Some(hk)
  }
}
