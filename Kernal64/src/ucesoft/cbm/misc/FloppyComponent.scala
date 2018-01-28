package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.Drive
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.formats.Diskette
import ucesoft.cbm.peripheral.drive.Floppy

class FloppyComponent(device:Int,attachedDisks : Array[Option[Diskette]],drives : Array[Drive],driveLeds:Array[DriveLed]) extends CBMComponent {
    val componentID = "Mounted floppy " + device
    val componentType = CBMComponentType.FLOPPY
    final private[this] val deviceID = device - 8 
    
    override def getProperties = {
      val attachedDisk = attachedDisks(deviceID)
      properties.setProperty("Floppy",if (attachedDisk.isDefined) attachedDisk.get.toString else "-")
      properties.setProperty("Track",if (attachedDisk.isDefined) attachedDisk.get.currentTrack.toString else "-")
      properties.setProperty("Sector",if (attachedDisk.isDefined && attachedDisk.get.currentSector.isDefined) attachedDisk.get.currentSector.get.toString else "N/A")
      properties.setProperty("Total tracks",if (attachedDisk.isDefined) attachedDisk.get.totalTracks.toString else "-")
      properties.setProperty("Side",if (attachedDisk.isDefined) attachedDisk.get.side.toString else "-")
      properties.setProperty("Single side",if (attachedDisk.isDefined) attachedDisk.get.singleSide.toString else "-")
      properties
    }
    
    def init {}
    def reset = attachedDisks(deviceID) match {
      case Some(d) => d.reset
      case None =>
    }
    // state
    protected def saveState(out:ObjectOutputStream) {
      Floppy.save(out,attachedDisks(deviceID))
    }
    protected def loadState(in:ObjectInputStream) {
      Floppy.load(in) match {
        case Some(floppy) =>
          attachedDisks(deviceID) match {
            case Some(oldDisk) => oldDisk.close
            case None =>
          }
          attachedDisks(deviceID) = Some(floppy.asInstanceOf[Diskette])
          drives(deviceID).setDriveReader(floppy,false)
          driveLeds(deviceID).setToolTipText(floppy.file)
        case None =>
      }
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }