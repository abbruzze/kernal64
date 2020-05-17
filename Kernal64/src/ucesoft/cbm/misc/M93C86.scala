package ucesoft.cbm.misc

import java.util.Arrays
import java.io.File
import java.io.IOException
import ucesoft.cbm.Log
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.DataOutputStream

/**
 * Programmable EEPROM, x16 or x8 organization
 */
class M93C86(x16:Boolean) {
  private[this] final val IDLE = 0
  private[this] final val READ_ADDRESS = 1
  private[this] final val READ_OUT = 2
  private[this] final val WRITE_OR_ERASE = 3
  private[this] final val WRITE_ADDRESS = 4
  private[this] final val WRITE_DATA = 5
  private[this] final val WRAL_DATA = 6
  private[this] final val ERASE_ADDRESS = 7
  
  private[this] final val ADDRESS_BIT_SIZE = if (x16) 10 else 11
  private[this] final val DATA_BIT_SIZE = if (x16) 16 else 8
  private[this] final val ADDRESS_MASK = if (x16) 0x3FF else 0x7FF
  private[this] final val DATA_BIT_MASK = if (x16) 0x10000 else 0x100
  private[this] final val DATA_MASK = if (x16) 0xFFFF else 0xFF
  
  private[this] val eeprom = Array.ofDim[Int](if (x16) 1024 else 2048)
  private[this] var serialBus = 0
  private[this] var out = 0
  private[this] var in = 0
  private[this] var clk = false
  private[this] var cs = false
  private[this] var state = IDLE
  
  private[this] var bitCounter = 0
  private[this] var writeEnabled = false
  private[this] var data = 0
  private[this] var address = 0
  private[this] var firstRead = true
  
  def input(b:Int) = in = b
  def output = out
  def chipSelect(select:Boolean) : Unit = {
    cs = select
    if (!cs) {
      firstRead = true
      state = IDLE
      bitCounter = 0
      out = 1
    }
  }
  
  def load(file:File) : Unit = {
    try {
      val f = new DataInputStream(new FileInputStream(file))
      for(i <- 0 until eeprom.length) {
        eeprom(i) = f.readInt
      }
      f.close
    }
    catch {
      case io:IOException =>
        Log.info("Can't load eeprom file " + file + " " + io)
    }
  }
  
  def save(file:File) : Unit = {
    try {
      val f = new DataOutputStream(new FileOutputStream(file))
      eeprom foreach { v =>
        f.writeInt(v)
      }
      f.close
    }
    catch {
      case io:IOException =>
        Log.info("Can't load eeprom file " + file + " " + io)
    }
  }
  
  private def serialIn  : Unit = {
    serialBus <<= 1
    serialBus |= in
    bitCounter += 1
  }
  
  @inline private def decodeAddress = serialBus & ADDRESS_MASK
  @inline private def decodeData = serialBus & DATA_MASK
  
  def clock(clk:Boolean) : Unit = {
    val oldClk = this.clk
    this.clk = clk
    if (cs && !oldClk && clk) { // rising edge
      state match {
        case IDLE =>
          serialIn
          if (bitCounter == 1) out = 1 // READY
          else
          if (bitCounter == 3) {
            bitCounter = 0
            (serialBus & 7) match {
              case 6 => // READ
                state = READ_ADDRESS
                //println("READ CMD")
              case 4 => // WRITE ENABLE OR DISABLE
                state = WRITE_OR_ERASE
                //println("WRITE_OR_ERASE CMD")
              case 5 => // WRITE
                state = WRITE_ADDRESS
                //println("WRITE CMD")
              case 7 => // ERASE
                state = ERASE_ADDRESS
                //println("ERASE CMD")
            }
          }
        case ERASE_ADDRESS =>
          serialIn
          if (bitCounter == ADDRESS_BIT_SIZE) {
            state = IDLE
            bitCounter = 0
            address = decodeAddress
            if (writeEnabled) eeprom(address) = DATA_MASK
            //println(s"ERASE ADDRESS = $address")            
          }
        case WRAL_DATA =>
          serialIn
          if (bitCounter == DATA_BIT_SIZE - 1) {
            out = 0 // simulate write beginning => BUSY
          }
          else
          if (bitCounter == DATA_BIT_SIZE) {
            data = decodeData
            //println("WRAL DATA = " + data)
            if (writeEnabled) Arrays.fill(eeprom,data)
            out = 1 // READY
          }
        case WRITE_DATA =>
          serialIn
          //println("WRITE DATA BIT" + bitCounter)
          if (bitCounter == DATA_BIT_SIZE - 1) {
            out = 0 // simulate write beginning => BUSY
          }
          else
          if (bitCounter == DATA_BIT_SIZE) {
            data = decodeData
            //println("WRITE DATA = " + data + " at ADDRESS " + address)
            if (writeEnabled) eeprom(address) = data
            out = 1 // READY
          }
        case WRITE_ADDRESS =>
          serialIn
          if (bitCounter == ADDRESS_BIT_SIZE) {
            state = WRITE_DATA
            bitCounter = 0
            address = decodeAddress
            //println(s"WRITE ADDRESS = $address")            
          }
        case WRITE_OR_ERASE =>
          serialIn
          if (bitCounter == ADDRESS_BIT_SIZE) {
            val adr = (serialBus >> 8) & 3
            adr match {
              case 3 => // WEN
                writeEnabled = true
                state = IDLE
                //println("WRITE ENABLED")
              case 0 => // WDS
                writeEnabled = false
                state = IDLE
                //println("WRITE DISABLED")
              case 2 => // ERAL
                if (writeEnabled) Arrays.fill(eeprom,DATA_MASK) // fills with 1
                out = 1 // READY
                state = IDLE
                //println("ERAL")                
              case 1 => // WRAL
                state = WRAL_DATA
                //println("WRAL")
            }
            bitCounter = 0            
          }
        case READ_ADDRESS =>
          serialIn
          if (bitCounter == ADDRESS_BIT_SIZE) {
            state = READ_OUT
            bitCounter = 0
            address = decodeAddress
            data = eeprom(address)
            //println(s"READ ADDRESS = $address DATA = $data")            
          }
        case READ_OUT =>
          data <<= 1
          out = if ((data & DATA_BIT_MASK) == 0) 0 else 1 
          bitCounter += 1
          val bitsOut = DATA_BIT_SIZE
          
          if (bitCounter == bitsOut) {
            bitCounter = 0
            firstRead = false
            address = (address + 1) & ADDRESS_MASK
            data = eeprom(address)
            //println("Next Address to READ " + address + " with DATA " + data)
          }
      }
    }
  }
}