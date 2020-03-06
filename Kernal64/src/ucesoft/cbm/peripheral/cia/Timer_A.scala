package ucesoft.cbm.peripheral.cia

class Timer_A(ciaName: String,
              id: Int,
              irqAction: (Int) => Unit,
              timerB : Timer,
              idleAction : (Boolean) => Unit) extends Timer(ciaName,id,irqAction,idleAction) {
  override val componentID = ciaName + "_TA"
  final protected def underflow : Unit = {
    irqAction(id)
    if ((timerB.readCR & 0x41) == 0x41) {
      if ((timerB.getState & CIAT_CR_START) != 0) timerB.setStep
    }
  }
}
