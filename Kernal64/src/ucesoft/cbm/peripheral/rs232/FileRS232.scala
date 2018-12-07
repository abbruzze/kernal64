package ucesoft.cbm.peripheral.rs232

import ucesoft.cbm.Log
import java.io._

object FileRS232 extends StreamRS232 {
  val componentID = "File RS-232"
  
  private[this] var inFile,outFile = ""
  
  def getDescription = "Get input stream and output stream from files. Connection String syntax: input file path,output file path,baud,bits,parity,stops"
  
  /**
   * Syntax: input file path,output file path,bits,parity,stops
   */
  override def setConfiguration(conf:String) {
    val parts = conf.split(",")
    if (parts.length != 6) throw new IllegalArgumentException("Bad File RS-232 configuration string. Expected <input file path>,<output file path>,<baud>,<bits>,<parity>,<stops>")
    
    inFile = parts(0)
    outFile = parts(1)
    
    super.setConfiguration(parts.drop(2).mkString(","))
  }
  
  override def setEnabled(enabled:Boolean) {
    val lastEnabled = isEnabled    
    
    if (enabled) {
      if (lastEnabled) {
        Log.info(s"Closing streams...")
        val (in,out) = getStreams
        in.close
        out.close
        super.setEnabled(false)
      }
      
      Log.info(s"Opening files ...")
      setStreams(new FileInputStream(inFile),new FileOutputStream(outFile),"N/A")
    }
    else {
      val (in,out) = getStreams
      in.close
      out.close
      Log.info(s"Closing streams...")
    }
    
    super.setEnabled(enabled)
  }
  
  override def toString = componentID + (if (isEnabled) "(enabled)" else "")
}