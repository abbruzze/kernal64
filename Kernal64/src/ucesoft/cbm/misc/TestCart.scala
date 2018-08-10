package ucesoft.cbm.misc

object TestCart {
  private[this] final val EXITCODE_LOCATION = 0xD7FF
  var enabled = false  
  
  final def write(address:Int,value:Int) {
    if (enabled && address == EXITCODE_LOCATION) sys.exit(value)
  }
}