package ucesoft.cbm.peripheral.cia

class TimerA(ciaName: String,
             id: Int,
             irqAction: (Int) => Unit,
             timerB : TimerB) extends TimerB(ciaName,id,irqAction) {
  override val componentID = ciaName + "_TA"

  final override protected def setCountMode(cr:Int) : Unit = {
    newCountMode = if ((cr & 0x20) == 0) COUNT_CLOCK else COUNT_CNT
    hasNewCountMode = true
  }

  final override protected def underflow: Unit = {
    super.underflow
    if (timerB.getCountMode == COUNT_A)
      timerB.externalUnderflow
  }
}
