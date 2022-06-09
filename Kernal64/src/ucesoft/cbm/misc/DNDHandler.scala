package ucesoft.cbm.misc

import ucesoft.cbm.formats.ZIP
import ucesoft.cbm.peripheral.drive.DriveType

import java.awt.datatransfer.DataFlavor
import java.io.File
import javax.swing.TransferHandler
import scala.util.Success

class DNDHandler(handleDND:(File) => Unit,extOnly:Option[() => DriveType.Value] = None) extends TransferHandler {
  override def canImport(support:TransferHandler.TransferSupport) : Boolean = support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)

  override def importData(support: TransferHandler.TransferSupport) : Boolean = {
    if (!canImport(support)) return false
    val t = support.getTransferable

    try {
      import scala.jdk.CollectionConverters._
      t.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]].asScala.headOption match {
        case None =>
          false
        case Some(f) =>
          val name = f.getName.toUpperCase
          extOnly match {
            case None =>
              if (name.endsWith(".D71") ||
                name.endsWith(".D81") ||
                name.endsWith(".D64") ||
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
            case Some(ext)  =>
              ext() match {
                case DriveType._1541 if List(".D64",".G64").exists( name.endsWith ) =>
                  handleDND(f)
                  true
                case DriveType._1571 if List(".D64",".G64","D71").exists( name.endsWith ) =>
                  handleDND(f)
                  true
                case DriveType._1581 if List(".D81").exists( name.endsWith ) =>
                  handleDND(f)
                  true
                case _ =>
                  false
              }
            case _ =>
              false
          }
      }
    }
    catch {
      case t:Throwable =>
        t.printStackTrace()
        false
    }
  }
}