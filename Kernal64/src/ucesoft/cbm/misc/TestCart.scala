package ucesoft.cbm.misc

object TestCart {
  private[this] final val EXITCODE_LOCATION = 0xD7FF
  var enabled = false
  var screenshotFile : Option[String] = None
  var screeshotHandler : (java.io.File,() => Unit) => Unit = _

  final def write(address:Int,value:Int) : Unit = {
    if (enabled && address == EXITCODE_LOCATION) exit(value)
  }

  final def exit(value:Int): Unit = {
    screenshotFile match {
      case None =>
        sys.exit(value)
      case Some(file) =>
        screeshotHandler(new java.io.File(file),() => sys.exit(value))
    }
  }
}