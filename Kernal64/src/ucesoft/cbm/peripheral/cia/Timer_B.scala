package ucesoft.cbm.peripheral.cia

class Timer_B(ciaName: String,
              id: Int,
              irqAction: (Int) => Unit,
              idleAction : (Boolean) => Unit) extends Timer(ciaName,id,irqAction,idleAction) {
  override val componentID: String = ciaName + "_TB"
  final protected def underflow : Unit = {
    irqAction(id)
  }

  override def writeCR(cr: Int): Unit = {
    super.writeCR(cr | (cr & 0x40) >> 1)
  }
}
