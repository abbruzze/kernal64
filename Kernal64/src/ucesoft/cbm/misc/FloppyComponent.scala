package ucesoft.cbm.misc

import ucesoft.cbm.{CBMComponent, CBMComponentType}
import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.peripheral.drive.{Drive, Floppy}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

class FloppyComponent(device:Int,var drive:Drive,driveLed:DriveLed) extends CBMComponent {
    val componentID: String = "Mounted floppy " + device
    val componentType: Type = CBMComponentType.FLOPPY
    final private[this] val deviceID = device - 8 
    
    override def getProperties: Properties = {
      val attachedDisk = drive.getFloppy      
      properties.setProperty("Floppy",if (!attachedDisk.isEmpty) attachedDisk.toString else "-")
      properties.setProperty("Track",if (!attachedDisk.isEmpty) attachedDisk.currentTrack.toString else "-")
      properties.setProperty("Sector",if (!attachedDisk.isEmpty && attachedDisk.currentSector.isDefined) attachedDisk.currentSector.get.toString else "N/A")
      properties.setProperty("Total tracks",if (!attachedDisk.isEmpty) attachedDisk.totalTracks.toString else "-")
      properties.setProperty("Side",if (!attachedDisk.isEmpty) attachedDisk.side.toString else "-")
      properties.setProperty("Single side",if (!attachedDisk.isEmpty) attachedDisk.singleSide.toString else "-")
      properties
    }
    
    def init(): Unit = {}
    def reset(): Unit = drive.getFloppy.reset()
    
    // state
    protected def saveState(out:ObjectOutputStream) : Unit = {
      Floppy.save(out,if (drive.getFloppy.isEmpty) None else Some(drive.getFloppy))
    }
    protected def loadState(in:ObjectInputStream) : Unit = {
      Floppy.load(in) match {
        case Some(floppy) =>
          drive.getFloppy.close()
          drive.setDriveReader(floppy,false)
          driveLed.setToolTipText(floppy.file)
        case None =>
      }
    }
    protected def allowsStateRestoring : Boolean = true
  }