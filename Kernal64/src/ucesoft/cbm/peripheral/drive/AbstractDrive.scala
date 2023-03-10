package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.peripheral.bus.{BusDataIterator, IECBus, IECBusDevice}

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.mutable.ListBuffer

abstract class AbstractDrive(bus: IECBus, device: Int = 9) extends IECBusDevice(bus, device) with Drive {
  protected var status = 0
  protected val ERROR_CODES : Map[Int,String] 
  
  protected case class DirEntry(name:String,byteSize:Int,isDirectory:Boolean)
  
  // register itself to bus
  //bus.registerListener(this)
  
  def isDeviceReady = true
  def init(): Unit = {}
  def setDriveReader(driveReader: Floppy,emulateInserting:Boolean) : Unit = {}
  def getFloppy: Floppy = EmptyFloppy
  
  protected def setStatus(code: Int): Unit = status = code
  protected def sendStatus() : Unit = {
    import BusDataIterator._
    channels(15).dataToSend = Some(new StringDataIterator("%02d,%s,00,00".format(status, ERROR_CODES(status)) + 13.toChar))
  }
  
  override def resetSignals() : Unit = {
    super.resetSignals()
  }
  
  protected def getDirectoryEntries(path:String) : List[DirEntry]
  
  protected def handleChannel15() : Unit
  
  protected def loadDirectory(path:String,pathName:String) : BusDataIterator = {
    val out = new ListBuffer[Int]
    // set start address to $0801
    out.append(0x01)
    out.append(0x08)
    var ptr = 0x801    
    // write next line address
    ptr += 30
    out.append(ptr & 0xFF)  // L
    out.append(ptr >> 8)  // H
    // write label
    out.append(0) // drive L
    out.append(0) // drive H
    out.append(0x12) // RVS ON
    out.append(0x22) // "
    val dir = pathName.toUpperCase
    for(i <- 0 until 16) {
      if (i < dir.length) out.append(dir.charAt(i)) else out.append(0x20)
    }
    out.append(0x22) // "
    out.append(0x20)
    out.append(54)
    out.append(52)
    out.append(0x20)
    out.append(48)
    out.append(48)
    out.append(0x00)  // EOL
    for(file <- getDirectoryEntries(path)) {
      val fileName = file.name.take(16).toUpperCase
      val sectors = file.byteSize / 256
      val sizeInSector = math.min(if (sectors == 0) 1 else sectors,9999)
      val blanks = if (sizeInSector < 10) 3 
        else
        if (sizeInSector < 100) 2
        else
        if (sizeInSector < 1000) 1
        else 0
      // write next line address
      ptr += blanks + 2 + 2 + 18 + 5
      val endBlanks = 32 - (blanks + 2 + 2 + 18 + 5)
      out.append(ptr & 0xFF)  // L
      out.append(ptr >> 8)  // H
      // write blocks
      out.append(sizeInSector & 0xFF)
      out.append(sizeInSector >> 8)
      // blanks after blocks      
      for(i <- 1 to blanks) out.append(0x20)
      out.append(0x22) // "
      for(i <- 0 until fileName.length) out.append(fileName.charAt(i))
      out.append(0x22) // "
      for(i <- 1 to 16 - fileName.length) out.append(0x20)
      out.append(0x20) // "
      val fileType = if (file.isDirectory) "DIR" else "PRG"
      for(i <- 0 until fileType.length) out.append(fileType.charAt(i))
      for(i <- 1 to endBlanks) out.append(0x20)
      out.append(0x00) // EOL
    }
    val blocksFreeText = "BLOCKS FREE."
    // write next line address
    ptr += 2 + 2 + blocksFreeText.length + 1
    out.append(ptr & 0xFF)  // L
    out.append(ptr >> 8)  // H
    val blocksFree = 0
    // write block free
    out.append(blocksFree & 0xFF)   // L
    out.append(blocksFree >> 8)   // H    
    for(i <- 0 until blocksFreeText.length) out.append(blocksFreeText.charAt(i))
    out.append(0x00) // EOL
    
    out.append(0x00)
    out.append(0x00)
    import BusDataIterator._
    new ArrayIntDataIterator(out.toArray)
  }
  
  override protected def untalk() : Unit = {
    resetSignals()
  }
  
  override def unlisten() : Unit = {
    super.unlisten()
    if (channel == 15) handleChannel15()
    resetSignals()
  }
  
  // state
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
}