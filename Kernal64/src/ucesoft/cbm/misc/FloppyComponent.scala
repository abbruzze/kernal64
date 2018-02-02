package ucesoft.cbm.misc

import ucesoft.cbm.peripheral.drive.Drive
import ucesoft.cbm.CBMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.peripheral.drive.Floppy

class FloppyComponent(device:Int,drive:Drive,driveLed:DriveLed) extends CBMComponent {
    val componentID = "Mounted floppy " + device
    val componentType = CBMComponentType.FLOPPY
    final private[this] val deviceID = device - 8 
    
    override def getProperties = {
      val attachedDisk = drive.getFloppy      
      properties.setProperty("Floppy",if (!attachedDisk.isEmpty) attachedDisk.toString else "-")
      properties.setProperty("Track",if (!attachedDisk.isEmpty) attachedDisk.currentTrack.toString else "-")
      properties.setProperty("Sector",if (!attachedDisk.isEmpty && attachedDisk.currentSector.isDefined) attachedDisk.currentSector.get.toString else "N/A")
      properties.setProperty("Total tracks",if (!attachedDisk.isEmpty) attachedDisk.totalTracks.toString else "-")
      properties.setProperty("Side",if (!attachedDisk.isEmpty) attachedDisk.side.toString else "-")
      properties.setProperty("Single side",if (!attachedDisk.isEmpty) attachedDisk.singleSide.toString else "-")
      properties
    }
    
    def init {}
    def reset = drive.getFloppy.reset
    // state
    protected def saveState(out:ObjectOutputStream) {
      Floppy.save(out,if (drive.getFloppy.isEmpty) None else Some(drive.getFloppy))
    }
    protected def loadState(in:ObjectInputStream) {
      Floppy.load(in) match {
        case Some(floppy) =>
          drive.getFloppy.close
          drive.setDriveReader(floppy,false)
          driveLed.setToolTipText(floppy.file)
        case None =>
      }
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }