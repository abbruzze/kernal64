package ucesoft.cbm.peripheral.mos6551

import ucesoft.cbm.Log

import java.io.{IOException, InputStream, OutputStream}

class ACIA6551(irqLow: Boolean => Unit) {
  private var commandRegister,controlRegister = 0
  private var dcd,dsr,irq = false
  private var inputStream : InputStream = null
  private var outputStream : OutputStream = null
  private var irqMasterEnabled,irqReceiverEnabled,irqTransmitterEnabled = false
  private var connectionDownHandler: () => Unit = _
  private var convertToUpper = false

  def setStreams(in:InputStream,out:OutputStream,connectionDownHandler: () => Unit): Unit = {
    inputStream = in
    outputStream = out
    this.connectionDownHandler = connectionDownHandler
    dcd = in != null
    dsr = in != null
  }

  def setConvertToUpperCase(convert:Boolean): Unit = convertToUpper = convert

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
    try {
      val rec = if (inputStream != null) inputStream.read() else 0
      if (inputStream != null && inputStream.available() > 0 && irqReceiverEnabled) {
        irq = true
        checkIRQ()
      }
      //println(s"ACIA read data ${rec.toChar}")
      if (convertToUpper) rec.toChar.toUpper.toInt else rec
    }
    catch {
      case io:IOException =>
        Log.info(s"ACIA connection error: $io")
        connectionDownHandler()
        0
    }
  }
  private def writeTransmitDataRegister(data:Int): Unit = {
    //println(s"ACIA Transmitting $data")
    try {
      if (outputStream != null) {
        outputStream.write(data)
        outputStream.flush()
        if (irqTransmitterEnabled) {
          irq = true
          checkIRQ()
        }
      }
    }
    catch {
      case io:IOException =>
        Log.info(s"ACIA connection error: $io")
        connectionDownHandler()
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
