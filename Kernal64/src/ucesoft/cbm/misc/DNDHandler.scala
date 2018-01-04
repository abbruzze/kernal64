package ucesoft.cbm.misc

import javax.swing.TransferHandler
import java.awt.datatransfer.DataFlavor
import java.io.File
import ucesoft.cbm.formats.ZIP
import scala.util.Success

class DNDHandler(handleDND:(File) => Unit) extends TransferHandler {
  override def canImport(support:TransferHandler.TransferSupport) : Boolean = support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)

  override def importData(support: TransferHandler.TransferSupport) : Boolean = {
      if (!canImport(support)) {
          return false
      }
      
      val t = support.getTransferable();

      try {
        import scala.collection.JavaConverters._
        t.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]].asScala.headOption match {
          case None =>
            false
          case Some(f) =>
            val name = f.getName.toUpperCase
            if (name.endsWith(".D64") ||
                name.endsWith(".G64") ||
                name.endsWith(".TAP") || 
                name.endsWith(".PRG") || 
                name.endsWith(".CRT") ||
                name.endsWith(".T64")) {
              handleDND(f)
              true
            }
            else
            if (name.endsWith(".ZIP")) {
              ZIP.zipEntries(f) match {
                case Success(entries) if entries.size > 0 =>
                  handleDND(f)
                  true
                case _ =>
                  false
              }                
            }
            else false
        }
      } 
      catch {
        case t:Throwable =>
          false
      }
  }
}