package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.CBMComponentType
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JFrame
import ucesoft.cbm.ChipID
import ucesoft.cbm.Clock
import ucesoft.cbm.formats.MFM

class WD1770(rwh:RWHeadController,wd1772:Boolean = false) extends RAMComponent {
  val componentID = "WD1770"
  val componentType = CBMComponentType.INTERNAL
  val isRom = false
  val length = 4
  val startAddress = 0x2000
  val name = componentID
  val isActive = true
  
  /*
   * Status bits
   * Bit 0: Busy flag. Indicates that the command is being executed.
   * Bit 1: Data request/index For all other commands this bit signals that data can be taken from register $2003 or can be written in the register.
   * Bit 2: Lost data/track0 For commands of type 1 this bit indicates that the head is on track 0. For all other commands this bit indicates that the data in register $2003 was not read or written by the program in time.
   * Bit 3: CRC error. The checksum bytes of the header or the data block decoded an error.
   * Bit 4: Record not found. The specified track or sector was not found.
   * Bit 5: Spin-up/Record type For commands of type 1 this bit specifies that 6 diskette rotations have taken place. For commands of type 2 and 3 this bit was the value of the "data mark."
   * Bit 6: Write protect. This bit indicates when writing that the write protect tab is in place.
   * Bit 7: Motor on. This bit gives the status of the motor. 0= motor off 1= motor on
   */
  private[this] final val CMD_H = 1 << 3
  private[this] final val CMD_V = 1 << 2
  private[this] final val CMD_R0R1 = 3
  private[this] final val CMD_U = 1 << 4
  private[this] final val CMD_M = 1 << 4
  private[this] final val CMD_E = 1 << 2
  private[this] final val CMD_P = 1 << 1
  private[this] final val CMD_A = 1 << 0
  private[this] final val CMD_K = 1 << 1
  private[this] final val CMD_L = 1 << 0
  //
  private[this] final val BUSY_FLAG = 1 << 0
  private[this] final val DRQ_FLAG = 1 << 1
  private[this] final val LOSTDATA_TRACK0_FLAG = 1 << 2
  private[this] final val CRC_ERROR_FLAG = 1 << 3
  private[this] final val RECORD_NOT_FOUND_FLAG = 1 << 4
  private[this] final val DATA_MARK_FLAG = 1 << 5
  private[this] final val WP_FLAG = 1 << 6
  private[this] final val MOTORON_FLAG = 1 << 7
  
  private[this] final val RESTORE_CMD = 0
  private[this] final val SEEK_CMD = 1
  private[this] final val STEP_CMD = 2
  private[this] final val STEP_IN_CMD = 3
  private[this] final val STEP_OUT_CMD = 4
  private[this] final val READ_SECTOR_CMD = 5
  private[this] final val WRITE_SECTOR_CMD = 6
  private[this] final val READ_ADDRESS_CMD = 7
  private[this] final val READ_TRACK_CMD = 8
  private[this] final val WRITE_TRACK_CMD = 9
  private[this] final val FORCE_INT_CMD = 10
  
  private[this] final val clk_hz = if (Clock.systemClock == null) 1000000 else Clock.systemClock.getClockHz
  private[this] final val CMD_TYPEI_R0R1_TIME_1770_MS = Array(6,12,20,30)
  private[this] final val CMD_TYPEI_R0R1_TIME_1772_MS = Array(2,3,5,6)
  private[this] final val CMD_TYPEI_R0R1_TIME_CLOCKS = (if (wd1772) CMD_TYPEI_R0R1_TIME_1772_MS else CMD_TYPEI_R0R1_TIME_1770_MS) map millis2clocks
  
  @inline private def millis2clocks(m:Int) : Int = (m * clk_hz / 1000).toInt
  
  @inline private def sf(flag:Int) {
    status |= flag
  }
  @inline private def cf(flag:Int) {
    status &= ~flag & 0xFF
  }
  @inline private def isf(flag:Int) = (status & flag) == flag 
  
  private abstract class Step(name:String) {
    def apply()
    
    override def toString = name
  }
  private object NoStep extends Step("Idle") { def apply {} }
  private case class Command(cmdType : Int,mask : Int,value : Int,cmd:Int,label:String,mainStep:Step = NoStep) {
    val isIdle = false
    var flags = 0
    def is(flag:Int) = (flags & flag) == flag
    
    override def toString = s"$label($flags)"
  }
  private[this] val IdleCommand = new Command(0,0,0,0,"IDLE") {
    override val isIdle = true
  }
  
  private final val COMMANDS : Array[Command] = Array(
      Command(1,0xF0,0,RESTORE_CMD,"RESTORE",TypeIMainStep),
      Command(1,0xF0,0x10,SEEK_CMD,"SEEK",TypeIMainStep),
      Command(1,0xE0,0x20,STEP_CMD,"STEP",TypeIMainStep),
      Command(1,0xE0,0x40,STEP_IN_CMD,"STEP_IN",TypeIMainStep),
      Command(1,0xE0,0x60,STEP_OUT_CMD,"STEP_OUT",TypeIMainStep),
      Command(2,0xE0,0x80,READ_SECTOR_CMD,"READ_SECTOR"),
      Command(2,0xE0,0xA0,WRITE_SECTOR_CMD,"WRITE_SECTOR"),
      Command(3,0xF0,0xC0,READ_ADDRESS_CMD,"READ_ADDRESS",TypeIIIMainStep),
      Command(3,0xF0,0xE0,READ_TRACK_CMD,"READ_TRACK",TypeIIIMainStep),
      Command(3,0xF0,0xF0,WRITE_TRACK_CMD,"WRITE_TRACK",TypeIIIMainStep),
      Command(4,0xF0,0xD0,FORCE_INT_CMD,"FORCE_INT")
  )
  
  private def decodeCommand(byte:Int) : Option[Command] = {
    var i = 0
    while (i < COMMANDS.length) {
      val cmd = COMMANDS(i)
      if ((byte & cmd.mask) == cmd.value) return Some(cmd)
      i += 1
    }
    None
  }
  
  private[this] var cmd : Command = IdleCommand
  private[this] var track,sector,dr = 0
  private[this] var status = 0
  private[this] var direction = 1 // 1 = step in , 0 = step out
  private[this] var step : Step = NoStep
  private[this] val idfield = Array(0,0,0,0,0,0) // track,side,sector,sector len,crc1,crc2
  
  @inline private def setDirection = direction = 1
  @inline private def clearDirection = direction = 0
  
  override def getProperties = {
    properties.setProperty("Command",cmd.toString)
    properties.setProperty("Sector",sector.toString)
    properties.setProperty("Track",track.toString)
    properties.setProperty("Data",dr.toString)
    properties.setProperty("Status",stat.toString)
    properties.setProperty("Step",step.toString)
    super.getProperties
  }
  
  final def clock {
    if (!cmd.isIdle) step()
  }
  // ==============================================================================
  private abstract class CountIndexPulses(name:String) extends Step(name) {
    protected var indexPulses = 0
    private var lastIndexPulse = false
    def apply {
      if (!lastIndexPulse && rwh.isOnIndexHole) {
        lastIndexPulse = true
        indexPulses += 1        
      }
      else
      if (lastIndexPulse && !rwh.isOnIndexHole) 
        lastIndexPulse = false
    }
    
    override def toString = s"$name($indexPulses)"
  }
  private class WaitIndexPulses(qt:Int,nextStep:Step,setDataMark:Boolean = true) extends CountIndexPulses("WaitIndexPulses") {
    println(s"Begin waiting $qt pulses")
    override def apply {
      super.apply
      if (indexPulses == qt) {
        if (setDataMark) sf(DATA_MARK_FLAG)
        step = nextStep
      }
    }
  }
  private class WaitCycles(millis:Int,nextStep:Step) extends Step("WaitCycles") {
    private var cycles = millis2clocks(millis)
    def apply {
      cycles -= 1
      if (cycles <= 0) {
        step = nextStep
        //sf(DATA_MARK_FLAG)
      }
    }
    
    override def toString = s"$name($millis)"
  }
  private object CommandCompleted extends Step("CommandCompleted") {
    def apply {
      // TODO
      cf(BUSY_FLAG)
      step = NoStep
      println("Command completed")
    }
  }
  private class FindIDField(maxIndexPulses:Int,findStep:Step,trackToVerify:Int = -1,sectorToVerify:Int = -1) extends CountIndexPulses("FindIDField") {
    private var A1FEcount = 0
    private var idCount = 0
    private var crc = 45616 // 45616 = crc of A1 x 3, FE x 1
    private var crcToCheck = 0
    
    override def apply {
      super.apply
      if (indexPulses >= maxIndexPulses) {
        sf(RECORD_NOT_FOUND_FLAG)
        step = CommandCompleted
        println(s"Max index pulses..$maxIndexPulses")
      }
      else {
        if (rwh.getLastByteReady) { // ok, byte ready
          val byte = rwh.getLastRead
          if (A1FEcount < 3) {
            if (byte == MFM.SYNC_MARK) A1FEcount += 1            
            else A1FEcount = 0
          }
          else
          if (A1FEcount == 3) {
            if (byte == MFM.SYNC_MARK_HEADER_NEXT) A1FEcount += 1
            else A1FEcount = 0
          }
          else
          if (idCount < 4) {
            idfield(idCount) = byte
            crc = MFM.crc(byte,crc)
            idCount += 1
          }
          else
          if (idCount == 4) {
            idfield(idCount) = byte
            crcToCheck = byte << 8
            idCount += 1
          }
          else {
            idfield(idCount) = byte
            crcToCheck |= byte
            if (trackToVerify != -1) {
              if (trackToVerify == idfield(0)) {
                if (sectorToVerify != -1) {
                  if (sectorToVerify == idfield(2)) verifyCrc                                        
                }
                else verifyCrc
              }
            }
            else verifyCrc
          }          
        }
      }
    }
    private def verifyCrc {
      if (crc == crcToCheck) {
        cf(CRC_ERROR_FLAG)
        step = findStep
      }
      else sf(CRC_ERROR_FLAG)
    }
    
    override def toString = s"$name($A1FEcount,$idCount,$crc,$crcToCheck)"
  }
  // ========================= Type I =============================================    
  private object TypeIMainStep extends Step("TypeIMainStep") {
    def apply {
      sf(BUSY_FLAG)
      cf(CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG)
      println("Begin Type I command")
      step = if (!cmd.is(CMD_H) && !rwh.isMotorOn) {
        // rwh.setMotor(true) MO line is not connected
        new WaitIndexPulses(6,step_1)      
      }
      else step_1
    }
    
    private val step_1 = new Step("TypeIMainStep_1") {
      def apply {
        val command = cmd.cmd
        if (command == STEP_CMD || command == STEP_IN_CMD || command == STEP_OUT_CMD) {
          if (command == STEP_IN_CMD) setDirection
          else
          if (command == STEP_OUT_CMD) clearDirection
          
          if (cmd.is(CMD_U)) {
            if (direction == 1) track += 1 else track -= 1
            track &= 0xFF
          }
          
          if (rwh.getTrack == 0 && direction == 0) {
            track = 0
            step = step_2
          }
          else {
            rwh.moveHead(moveOut = direction == 0)
            val delay = cmd.flags & CMD_R0R1
            step = new WaitCycles(CMD_TYPEI_R0R1_TIME_CLOCKS(delay),step_2)
          }
        }
        else {
          if (command == RESTORE_CMD) {
            track = 0xFF
            dr = 0
          }          
          
          step = step_1_1        
        }
      }
    }
    
    private val step_1_1 = new Step("TypeIMainStep_1_1") {
      def apply {
        println(s"step_1_1 $track $dr $direction")
        if (track == dr) step = step_2
        else {
          if (dr > track) setDirection else clearDirection
          if (direction == 1) track += 1 else track -= 1
          track &= 0xFF
          
          if (rwh.getTrack == 0 && direction == 0) {
            track = 0
            step = step_2
          }
          else {
            rwh.moveHead(moveOut = direction == 0)
            val delay = cmd.flags & CMD_R0R1
            step = new WaitCycles(CMD_TYPEI_R0R1_TIME_CLOCKS(delay),this)
          }
        }
      }
    }
    
    private val step_2 = new Step("TypeIMainStep_2") {
      def apply {
        step = if (cmd.is(CMD_V)) new FindIDField(6,CommandCompleted,track) else CommandCompleted
      }
    }
  }
  // ========================= Type III ===========================================
  private object TypeIIIMainStep extends Step("TypeIIIMainStep") {
    def apply {
      sf(BUSY_FLAG)
      cf(CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG | LOSTDATA_TRACK0_FLAG)
      println("Begin Type III command")
      
      step = if (!cmd.is(CMD_H) /*&& !rwh.isMotorOn*/) {
        // rwh.setMotor(true) MO line is not connected
        new WaitIndexPulses(6,step_1)      
      }
      else step_1            
    }
    
    private val step_1 = new Step("TypeIIIMainStep_1") {
      def apply {
        step = if (cmd.is(CMD_E)) new WaitCycles(30,step_2) else step_2
      }
    }
    
    private val step_2 = new Step("TypeIIIMainStep_2") {
      def apply {
        step = cmd.cmd match {
          case READ_ADDRESS_CMD => 
            new FindIDField(6,step_3)
          case READ_TRACK_CMD => 
            step_5
          case _ =>
            println("Unimplemented command")
            CommandCompleted
        }
      }
    }
    
    private val step_3 = new Step("TypeIIIMainStep_3") {      
      def apply {
        sector = idfield(0) // track
        dr = idfield(0)
        sf(DRQ_FLAG)
        step = step_4
      }
    }
    
    private val step_4 = new Step("TypeIIIMainStep_4") {      
      private var byteIndex = 0
      def apply {
        if (!isf(DRQ_FLAG)) { // ok, byte read from computer
          byteIndex += 1
          if (byteIndex == 6) step = CommandCompleted
          else {
            dr = idfield(byteIndex)
            sf(DRQ_FLAG)
          }
        }
      }
    }
    
    private val step_5 = new Step("TypeIIIMainStep_5") {
      private var indexHoleFound = false
      def apply {
        if (!indexHoleFound) {
          if (rwh.isOnIndexHole) indexHoleFound = true // wait the first index hole          
        }
        else          
        if (!rwh.isOnIndexHole) step = step_6 // wait the first non-index hole byte        
      }
    }
    
    private val step_6 = new Step("TypeIIIMainStep_6") {
      def apply {        
        if (rwh.getLastByteReady) { // ok, first byte ready
          val byte = rwh.getLastRead
          sf(DRQ_FLAG)
          dr = byte & 0xFF
          step = step_7
        }
      }
    }
    
    private val step_7 = new Step("TypeIIIMainStep_7") {
      def apply {
        if (rwh.isOnIndexHole) step = CommandCompleted // on next index hole exit
        else
        if (rwh.getByteReadySignal == 0) { // ok, byte ready
          rwh.resetByteReadySignal
          val byte = rwh.getLastRead
          if (isf(DRQ_FLAG)) sf(LOSTDATA_TRACK0_FLAG)
          else {
            sf(DRQ_FLAG)
            dr = byte & 0xFF
          }
        }
      }
    }
  }
  // ==============================================================================
  
  def reset {
    // TODO
  }
  def init {
    // TODO
  }
  
  // ==============================================================================
  @inline private def stat : Int = {
    var st = status | (if (rwh.isMotorOn) MOTORON_FLAG else 0)
    if (cmd.cmdType == 1) {
      st |= (if (rwh.isOnIndexHole) DRQ_FLAG else 0) // index hole
      st |= (if (rwh.getTrack != 0) LOSTDATA_TRACK0_FLAG else 0)
    }
    st
  }
  
  def read(address: Int, chipID: ChipID.ID = ChipID.CPU) : Int = {
    address & 3 match {
      case 0 =>
        var st = stat
        cf(DATA_MARK_FLAG)
        st
      case 1 =>
        track        
      case 2 =>
        sector
      case 3 =>
        cf(DRQ_FLAG)
        dr
    }
  }
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) {
    address & 3 match {
      case 0 =>
        decodeCommand(value) match {
          case Some(c) if !isf(BUSY_FLAG) =>
            cmd = c
            cmd.flags = value
            step = cmd.mainStep
          case Some(c) if c.cmdType == 4 =>
            if (cmd.isIdle) cf(BUSY_FLAG | CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG)
            else cf(BUSY_FLAG)
            cmd = IdleCommand
            step = NoStep
            println("Command Interrupted!!")
          case Some(c) =>
            println(s"Command $c ignored")
          case None =>
            println(s"Command value $value unrecognized")
        }
        println(s"WD1770: command $cmd")
        //clk.schedule(new ClockEvent("WD1770",clk.currentCycles + 100, cycles => cmd = None))
      case 1 =>
        track = value
      case 2 =>
        sector = value
      case 3 =>
        dr = value
        cf(DRQ_FLAG)
    }
  }
  // ==============================================================================
  
  protected def saveState(out:ObjectOutputStream) {
    // TODO
  }
  protected def loadState(in:ObjectInputStream) {
    // TODO
  }
  protected def allowsStateRestoring(parent:JFrame) = true
}