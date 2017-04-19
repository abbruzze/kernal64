package ucesoft.cbm.cpu

import ucesoft.cbm.CBMComponentType
import ucesoft.cbm.Log
import java.io._
import ucesoft.cbm.ChipID
import javax.swing.JFrame

class ROM(ram: Memory, 
          val name: String, 
          val startAddress: Int, 
          val length: Int, 
          val resourceName: String,
          initialOffset:Int = 0) extends RAMComponent {
    val componentID = "ROM " + name
    val componentType = CBMComponentType.MEMORY 
    
    val isRom = true
    private[this] var mem : Array[Int] = _
    private[this] var active = false
    
    final def isActive = active
    def setActive(active:Boolean) = this.active = active

    def init {
      mem = Array.fill(length)(0)
      Log.info(s"Initialaizing ${name} memory ...")
      Option(ClassLoader.getSystemClassLoader.getResourceAsStream(resourceName)) match {
        case None => throw new IOException(s"Can't find resource ${resourceName} for ROM ${name}")
        case Some(in) =>
          val buffer = Array.ofDim[Byte](length)
          var read = 0 //in.read(buffer)
          var offset = initialOffset
          do {            
            read = in.read(buffer, offset, length - offset)
            offset += read
          } while (read > 0)
          in.close
          for (i <- 0 until length) mem(i) = buffer(i) & 0xff
      }
    }
    
    def reset {}

    final def read(address: Int, chipID: ChipID.ID = ChipID.CPU): Int = mem(address - startAddress)    
    def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) = ram.write(address,value,chipID)
    final def patch(address:Int,value:Int) = mem(address - startAddress) = value
    // state
    protected def saveState(out:ObjectOutputStream) {
      out.writeBoolean(active)
      out.writeObject(mem)
    }
    protected def loadState(in:ObjectInputStream) {
      active = in.readBoolean
      loadMemory[Int](mem,in)
    }
    protected def allowsStateRestoring(parent:JFrame) : Boolean = true
  }
