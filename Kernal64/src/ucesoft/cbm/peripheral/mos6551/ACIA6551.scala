package ucesoft.cbm.peripheral.mos6551

import java.io.{InputStream, OutputStream}

class ACIA6551(irqLow: Boolean => Unit) {
  private var commandRegister,controlRegister = 0
  private var dcd,dsr,irq = false
  private var inputStream : InputStream = null
  private var outputStream : OutputStream = null
  private var irqMasterEnabled,irqReceiverEnabled,irqTransmitterEnabled = false

  /*
  new Thread() {
    override def run(): Unit = {
      Thread.sleep(10000)
      dcd = true
      dsr = true

      val s = new Socket("8bit.hoyvision.com",6400)
      inputStream = s.getInputStream
      outputStream = s.getOutputStream
      println("ACIA ready.")
    }
  }.start()
  */
  def write(register:Int,value:Int): Unit = {
    //println(s"ACIA write to ${register & 3} = $value")
    register & 3 match {
      case 0 =>
        writeTransmitDataRegister(value)
      case 1 =>
        reset()
      case 2 =>
        writeCommandRegister(value)
      case 3 =>
        writeControlRegister(value)
    }
  }

  def read(register:Int): Int = {
    val r = register & 3 match {
      case 0 =>
        readReceiverDataRegister()
      case 1 =>
        statusRegister()
      case 2 =>
        commandRegister
      case 3 =>
        controlRegister
    }
    //println(s"ACIA read from ${register & 3} = $r")
    r
  }

  private def reset(): Unit = {
    controlRegister = 0
    commandRegister = 2
    dcd = false
    dsr = false
    irq = false
    irqMasterEnabled = false
    irqReceiverEnabled = false
    irqTransmitterEnabled = false
  }

  private def statusRegister(): Int = {
    var st = 0x10 // trasmitter data register always empty
    if (inputStream != null && inputStream.available() > 0) st |= 0x8
    if (!dcd) st |= 0x20
    if (!dsr) st |= 0x40
    if (irq) st |= 0x80
    irq = false
    checkIRQ()
    st
  }
  private def readReceiverDataRegister(): Int = {
    val rec = if (inputStream != null) inputStream.read() else 0
    if (inputStream != null && inputStream.available() > 0 && irqReceiverEnabled) {
      irq = true
      checkIRQ()
    }
    //println(s"ACIA read data ${rec.toChar}")
    rec.toChar.toUpper.toInt
  }
  private def writeTransmitDataRegister(data:Int): Unit = {
    println(s"ACIA Transmitting $data")
    if (outputStream != null) {
      outputStream.write(data)
      outputStream.flush()
      if (irqTransmitterEnabled) {
        irq = true
        checkIRQ()
      }
    }
  }
  private def writeCommandRegister(data:Int): Unit = {
    //println(s"ACIA write command reg = $data")
    commandRegister = data
    irqMasterEnabled = (commandRegister & 1) == 1
    irqReceiverEnabled = (commandRegister & 2) == 0
    irqTransmitterEnabled = ((commandRegister >> 2) & 1) == 1
    if (irqReceiverEnabled && inputStream != null && inputStream.available() > 0) irq = true
    checkIRQ()
  }
  private def writeControlRegister(data:Int): Unit = {
    controlRegister = data
  }
  private def checkIRQ(): Unit = {
    irqLow(irqMasterEnabled && irq)
  }
}
