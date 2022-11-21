package ucesoft.cbm.peripheral.drive

import ucesoft.cbm.CBMComponentType.Type
import ucesoft.cbm.cpu.RAMComponent
import ucesoft.cbm.formats.MFM
import ucesoft.cbm.{CBMComponentType, ChipID, Clock}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Properties

class WD1770(rwh:RWHeadController,override val startAddress:Int,wd1772:Boolean = false) extends RAMComponent {
  val componentID = "WD1770"
  val componentType: Type = CBMComponentType.INTERNAL
  val isRom = false
  val length = 4  
  val name: String = componentID
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
  
  @inline private def sf(flag:Int) : Unit = {
    status |= flag
  }
  @inline private def cf(flag:Int) : Unit = {
    status &= ~flag & 0xFF
  }
  @inline private def isf(flag:Int) = (status & flag) == flag 
  
  private abstract class Step(name:String) {
    var lastStep : Step = _
    def apply() : Unit
    
    override def toString: String = name
  }
  private object NoStep extends Step("Idle") { def apply() : Unit = {} }
  private case class Command(cmdType : Int,mask : Int,value : Int,cmd:Int,label:String,mainStep:Step = NoStep) {
    val isIdle = false
    var flags = 0
    def is(flag:Int): Boolean = (flags & flag) == flag
    
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
      Command(2,0xE0,0x80,READ_SECTOR_CMD,"READ_SECTOR",TypeIIMainStep),
      Command(2,0xE0,0xA0,WRITE_SECTOR_CMD,"WRITE_SECTOR",TypeIIMainStep),
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
  private[this] var crc = 0
  private[this] final val DEBUG = false
  
  @inline private def setDirection(): Unit = direction = 1
  @inline private def clearDirection(): Unit = direction = 0
  
  override def getProperties: Properties = {
    properties.setProperty("Command",cmd.toString)
    properties.setProperty("Sector",sector.toString)
    properties.setProperty("Track",track.toString)
    properties.setProperty("Data",dr.toString)
    properties.setProperty("Status",stat.toString)
    properties.setProperty("Step",step.toString)
    super.getProperties
  }
  
  final def clock() : Unit = {
    if (!cmd.isIdle) step()
  }
  // ==============================================================================
  private abstract class CountIndexPulses(name:String) extends Step(name) {
    protected var indexPulses = 0
    private var lastIndexPulse = false
    def apply() : Unit = {
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
    if (DEBUG) println(s"Begin waiting $qt pulses")
    override def apply : Unit = {
      super.apply
      if (indexPulses == qt) {
        if (setDataMark) sf(DATA_MARK_FLAG)
        step = nextStep
      }
    }
  }
  private class WaitCycles(_cycles:Int,nextStep:Step) extends Step("WaitCycles") {
    private var cycles = _cycles
    def apply() : Unit = {
      cycles -= 1
      if (cycles <= 0) {
        step = nextStep
        //sf(DATA_MARK_FLAG)
      }
    }
    
    override def toString = s"$name(${_cycles})"
  }
  private object CommandCompleted extends Step("CommandCompleted") {
    def apply() : Unit = {
      cf(BUSY_FLAG)
      //step = new WaitMotorOff
      step = NoStep
      cmd = IdleCommand
      rwh.setWriting(false)
      if (status != 0) if (DEBUG) println(s"Command completed: status=$status")
    }
  }
  private class WaitMotorOff extends CountIndexPulses("WaitMotorOff") {
    override def apply : Unit = {
      super.apply
      if (indexPulses == 10) {
        step = NoStep
        cmd = IdleCommand
        cf(MOTORON_FLAG)
        if (DEBUG) println("Motor off")
      }
    }
  }
  private class FindIDField(maxIndexPulses:Int,findStep:Step,trackToVerify:Int = -1,sectorToVerify:Int = -1) extends CountIndexPulses("FindIDField") {
    private var A1FEcount = 0
    private var idCount = 0
    private var crc = 45616 // 45616 = crc of A1 x 3, FE x 1
    private var crcToCheck = 0
    
    override def apply : Unit = {
      super.apply
      if (indexPulses >= maxIndexPulses) {
        sf(RECORD_NOT_FOUND_FLAG)
        step = CommandCompleted
        if (DEBUG) println(s"Max index pulses..$maxIndexPulses")
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
            if (DEBUG) println(s"idfield($idCount) = $byte")
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
    private def verifyCrc() : Unit = {
      if (crc == crcToCheck) {
        cf(CRC_ERROR_FLAG)
        step = findStep
        step.lastStep = this
      }
      else {
        sf(CRC_ERROR_FLAG)
      }
    }
    
    override def toString = s"$name($A1FEcount,$idCount,$crc,$crcToCheck)"
  }
  
  private class FindDataField(foundStep:Step,notFoundStep:Step) extends Step("FindDataField") {
    private var byteCount = 0
    private var A1FBcount = 0
    def apply() : Unit = {
      if (rwh.getLastByteReady) { // ok, byte ready
        val byte = rwh.getLastRead
        byteCount += 1
        if (byteCount > 43) step = notFoundStep
        else {
          if (A1FBcount < 3) {
            if (byte == MFM.SYNC_MARK) A1FBcount += 1            
            else A1FBcount = 0
          }
          else
          if (A1FBcount == 3) {
            if (byte == MFM.SYNC_MARK_DATA_NEXT) { 
              step = foundStep // ok, Data Address Mark found
              // TODO put record record type in status bit 5
            }
            else A1FBcount = 0
          }
        }
      }
    }
  }
  // ========================= Type I =============================================    
  private object TypeIMainStep extends Step("TypeIMainStep") {
    def apply() : Unit = {
      sf(BUSY_FLAG)
      cf(CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG)
      step = if (!cmd.is(CMD_H) && !isf(MOTORON_FLAG)) {
        //rwh.setMotor(true) MO line is not connected
        sf(MOTORON_FLAG)
        if (DEBUG) println("Motor on")
        new WaitIndexPulses(6,step_1)      
      }
      else step_1
    }
    
    private val step_1 = new Step("TypeIMainStep_1") {
      def apply() : Unit = {
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
      def apply() : Unit = {
        if (DEBUG) println(s"step_1_1 $track $dr $direction")
        if (track == dr) {
          step = if (cmd.cmd == RESTORE_CMD) new WaitCycles(100,step_2) else step_2
        }
        else {
          if (dr > track) setDirection else clearDirection
          if (direction == 1) track += 1 else track -= 1
          track &= 0xFF
          
          if (rwh.getTrack == 0 && direction == 0) {
            track = 0
            step = if (cmd.cmd == RESTORE_CMD) new WaitCycles(100,step_2) else step_2
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
      def apply() : Unit = {
        step = if (cmd.is(CMD_V)) new FindIDField(6,CommandCompleted,track) else CommandCompleted
      }
    }
  }
  // ========================= Type II ============================================
  private object TypeIIMainStep extends Step("TypeIIMainStep") {
    def apply() : Unit = {
      sf(BUSY_FLAG)
      cf(CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG | DATA_MARK_FLAG | WP_FLAG | LOSTDATA_TRACK0_FLAG)
      
      step = if (!cmd.is(CMD_H) /*&& !rwh.isMotorOn*/) {
        // rwh.setMotor(true) MO line is not connected
        sf(MOTORON_FLAG)
        if (DEBUG) println("Motor on")
        new WaitIndexPulses(6,step_1)      
      }
      else step_1            
    }
    
    private val step_1 = new Step("TypeIIMainStep_1") {
      def apply() : Unit = {
        step = if (cmd.is(CMD_E)) new WaitCycles(millis2clocks(30),step_2) else step_2
      }
    }
    
    private val step_2 : Step = new Step("TypeIIMainStep_2") {
      def apply() : Unit = {
        step = cmd.cmd match {
          case READ_SECTOR_CMD =>
            new FindIDField(5,step_3)
          case WRITE_SECTOR_CMD =>
            if (rwh.isWriteProtected) {
              sf(WP_FLAG)
              CommandCompleted
            }
            else {
              new FindIDField(5,step_3_1)
            }
        }
      }
    }
    
    private val step_3_1 = new Step("TypeIIMainStep_3_1") { // read sector
      def apply() : Unit = {
        // ok, ID FIELD found, check track & sector
        if (track != idfield(0) || sector != idfield(2)) step = step.lastStep // go back to FindIDField
        else {
          if (DEBUG) println(s"Write sector: found track $track sector $sector.")
          step_7.gapCount = 0
          step = step_7
        }
      }
    }
    
    private val step_3 = new Step("TypeIIMainStep_3") { // read sector
      def apply() : Unit = {
        // ok, ID FIELD found, check track & sector
        if (track != idfield(0) || sector != idfield(2)) step = step.lastStep // go back to FindIDField
        else {
          if (DEBUG) println(s"Read sector: found track $track sector $sector.")
          step = new FindDataField(step_4,step.lastStep)
        }
      }
    }
    
    private val step_4 = new Step("TypeIIMainStep_4") {
      def apply() : Unit = {
        if (rwh.getLastByteReady) { // ok, first byte ready
          val byte = rwh.getLastRead
          sf(DRQ_FLAG)
          dr = byte & 0xFF
          step_5.byteCounter = MFM.SECTOR_SIZE(idfield(3)) - 1
          if (DEBUG) print(s"Going to read ${step_5.byteCounter} bytes: ")
          step = step_5
          crc = MFM.crc(byte,58005) // 58005 crc of A1 x 3, FB x 1
        }
      }
    }
    
    private val step_5 = new Step("TypeIIMainStep_5") {
      var byteCounter = 0      
      def apply() : Unit = {
        if (rwh.getLastByteReady) { // ok, first byte ready
          val byte = rwh.getLastRead
          if (DEBUG) print("'" + byte.toChar + "' ")
          crc = MFM.crc(byte,crc)
          dr = byte & 0xFF
          if (isf(DRQ_FLAG)) sf(LOSTDATA_TRACK0_FLAG)
          else sf(DRQ_FLAG)
          byteCounter -= 1
          if (byteCounter == 0) {
            if (DEBUG) println
            step_6.dataCrc = 0
            step_6.crcCount = 0
            step = step_6          
          }
        }
      }
    }
    
    private val step_6 = new Step("TypeIIMainStep_6") {
      var dataCrc = 0
      var crcCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) { // ok, first byte ready
          val byte = rwh.getLastRead
          dataCrc = (dataCrc << 8) | byte
          crcCount += 1
          if (crcCount == 2) { // ok, crc ready to be checked
            crcCount = 0
            if ((dataCrc & 0xFFFF) == crc) {
              if (!cmd.is(CMD_M)) step = CommandCompleted
              else {
                sector += 1
                step = step_2
              }
            }
            else {
              sf(CRC_ERROR_FLAG)
              step = CommandCompleted
              if (DEBUG) println(s"CRC error: dataCrc=$dataCrc crc=$crc")
            }
          }
        }
      }
    }
    
    private val step_7 = new Step("TypeIIMainStep_7") {
      var gapCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) {
          if (DEBUG) println("Read GAP " + rwh.getLastRead)
          gapCount += 1
          if (gapCount == 22) {
            gapCount = 0
            if (isf(DRQ_FLAG)) {
              sf(LOSTDATA_TRACK0_FLAG)
              step = CommandCompleted
            }
            else {
              rwh.setWriting(true)
              rwh.setNextToWrite(0)
              sf(DRQ_FLAG)
              step_8.zeroCount = 0
              step = step_8
            }
          }
        }
      }
    }
    
    private val step_8 = new Step("TypeIIMainStep_8") {
      var zeroCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) {
          zeroCount += 1
          if (zeroCount == 12) {
            if (DEBUG) println("12 0 written")
            rwh.setNextToWrite(MFM.SYNC_MARK)
            step_8_1.dataFieldCount = 0
            step = step_8_1
          }
        }
      }
    }
    
    private val step_8_1 = new Step("TypeIIMainStep_8_1") {
      var dataFieldCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) {
          dataFieldCount += 1
          if (dataFieldCount < 3) rwh.setNextToWrite(MFM.SYNC_MARK)
          else
          if (dataFieldCount == 3) rwh.setNextToWrite(MFM.SYNC_MARK_DATA_NEXT)
          else {
            if (DEBUG) println("DATA Field written")
            step_9.dataCount = MFM.SECTOR_SIZE(idfield(3))
            step = step_9
            crc = 58005 // 58005 crc of A1 x 3, FB x 1
            crc = MFM.crc(dr, crc)
            rwh.setNextToWrite(dr)
            sf(DRQ_FLAG)
            if (DEBUG) println(s"Going to write ${step_9.dataCount} bytes ...")
            if (DEBUG) println(s"[${step_9.dataCount}]Writing $dr status=$status")            
          }
        }
      }
    }
    
    private val step_9 = new Step("TypeIIMainStep_9") {
      var dataCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) {
          dataCount -= 1
          
          var nextToWrite = dr
          if (dataCount > 0 && isf(DRQ_FLAG)) {
            sf(LOSTDATA_TRACK0_FLAG)
            nextToWrite = 0
            if (DEBUG) println("LOST DATA")
          }
          if (dataCount > 0) {
            rwh.setNextToWrite(nextToWrite)
            if (DEBUG) println(s"[$dataCount]Writing $nextToWrite status=$status")
            sf(DRQ_FLAG)
            crc = MFM.crc(dr, crc)
          }
          else {
            rwh.setNextToWrite(crc >> 8)
            step_10.crcCount = 0
            step = step_10
          }
        }
      }
    }
    
    private val step_10 = new Step("TypeIIMainStep_10") {
      var crcCount = 0
      def apply() : Unit = {
        if (rwh.getLastByteReady) {
          crcCount += 1
          crcCount match {
            case 1 =>
              rwh.setNextToWrite(crc & 0xFF)
            case 2 =>
              step = CommandCompleted
              rwh.setWriting(false)
          }
        }
      }
    }
  }
  // ========================= Type III ===========================================
  private object TypeIIIMainStep extends Step("TypeIIIMainStep") {
    def apply() : Unit = {
      sf(BUSY_FLAG)
      cf(CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG | LOSTDATA_TRACK0_FLAG)
      
      step = if (!cmd.is(CMD_H) /*&& !rwh.isMotorOn*/) {
        // rwh.setMotor(true) MO line is not connected
        sf(MOTORON_FLAG)
        if (DEBUG) println("Motor on")
        new WaitIndexPulses(6,step_1)      
      }
      else step_1            
    }
    
    private val step_1 = new Step("TypeIIIMainStep_1") {
      def apply() : Unit = {
        step = if (cmd.is(CMD_E)) new WaitCycles(millis2clocks(30),step_2) else step_2
      }
    }
    
    private val step_2 = new Step("TypeIIIMainStep_2") {
      def apply() : Unit = {
        step = cmd.cmd match {
          case READ_ADDRESS_CMD => 
            new FindIDField(6,step_3)
          case READ_TRACK_CMD => 
            step_5.indexHoleFound = false
            step_5
          case WRITE_TRACK_CMD =>
            if (rwh.isWriteProtected) {
              sf(WP_FLAG)
              CommandCompleted
            }
            else {
              sf(DRQ_FLAG)
              step_8.byteCount = 0
              step_8
            }
        }
      }
    }
    
    private val step_3 = new Step("TypeIIIMainStep_3") {      
      def apply() : Unit = {
        sector = idfield(0) // track
        dr = idfield(0)
        sf(DRQ_FLAG)
        step_4.byteIndex = 0
        step = step_4
      }
    }
    
    private val step_4 = new Step("TypeIIIMainStep_4") {
      var byteIndex = 0
      def apply() : Unit = {
        if (!isf(DRQ_FLAG)) { // ok, byte read from computer
          byteIndex += 1
          if (byteIndex == 6) {
            step = CommandCompleted
            byteIndex = 0
          }
          else {
            dr = idfield(byteIndex)
            sf(DRQ_FLAG)
          }
        }
      }
    }
    
    private val step_5 = new Step("TypeIIIMainStep_5") {
      var indexHoleFound = false
      def apply() : Unit = {
        if (!indexHoleFound) {
          if (rwh.isOnIndexHole) indexHoleFound = true // wait the first index hole          
        }
        else          
        if (!rwh.isOnIndexHole) step = step_6 // wait the first non-index hole byte        
      }
    }
    
    private val step_6 = new Step("TypeIIIMainStep_6") {
      def apply() : Unit = {
        if (rwh.getLastByteReady) { // ok, first byte ready
          val byte = rwh.getLastRead
          sf(DRQ_FLAG)
          dr = byte & 0xFF
          step = step_7
        }
      }
    }
    
    private val step_7 = new Step("TypeIIIMainStep_7") {
      def apply() : Unit = {
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
    
    private val step_8 = new Step("TypeIIIMainStep_8") {
      var byteCount = 0
      def apply() : Unit = {
        if (rwh.getByteReadySignal == 0) { // ok, byte ready
          rwh.resetByteReadySignal
          byteCount += 1
          if (byteCount == 3) {
            byteCount = 0
            if (isf(DRQ_FLAG)) {
              sf(LOSTDATA_TRACK0_FLAG)
              step = CommandCompleted
            }
            else {
              step_9.indexHoleStatus = 0
              step = step_9
            }
          }
        }
      }
    }
    
    private val step_9 = new Step("TypeIIIMainStep_9") {
      var indexHoleStatus = 0
      def apply() : Unit = {
        indexHoleStatus match {
          case 0 =>
            if (rwh.isOnIndexHole) indexHoleStatus += 1
          case 1 =>
            if (!rwh.isOnIndexHole) indexHoleStatus += 1
          case 2 =>
            if (DEBUG) println("Hole REACHED, start formatting track...")
            step_10.crcWriting = 0
            step = step_10
            rwh.setWriting(true)
            rwh.setNextToWrite(dr)
        }       
      }
    }
    
    private val step_10 = new Step("TypeIIIMainStep_10") {
      var crcWriting,lastWritten = 0
      def apply() : Unit = {
        if (rwh.getByteReadySignal == 0) { // ok, byte written
          rwh.resetByteReadySignal
          if (rwh.isOnIndexHole) step = CommandCompleted
          else
          if (crcWriting == 2) {
            rwh.setNextToWrite(crc & 0xFF)
            crcWriting = 0
          }
          else {
            var byteToWrite = dr
            if (isf(DRQ_FLAG)) {
              sf(LOSTDATA_TRACK0_FLAG)
              byteToWrite = 0
              if (DEBUG) println("LOST BYTE")
            }
            sf(DRQ_FLAG)
            byteToWrite = byteToWrite match {
              case 0xF5 =>
                if (lastWritten != MFM.SYNC_MARK) {
                  crc = 0xFFFF
                  crcWriting = 1
                }
                MFM.SYNC_MARK
              case 0xF6 => 
                MFM.SYNC_INDEX_MARK 
              case 0xF7 => 
                crcWriting = 2
                crc >> 8
              case _ => byteToWrite
            }
            if (crcWriting == 1) crc = MFM.crc(byteToWrite,crc)
            lastWritten = byteToWrite
            rwh.setNextToWrite(byteToWrite)
          }
        }
      }
    }    
  }
  // ==============================================================================
  
  def reset : Unit = {
    cmd = IdleCommand
    track = 0
    sector = 0
    dr = 0
    status = 0
    direction = 1
    step = NoStep
    crc = 0
  }
  def init : Unit = {}
  
  // ==============================================================================
  @inline private def stat : Int = {
    var st = status// | (if (rwh.isMotorOn) MOTORON_FLAG else 0)
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
  def write(address: Int, value: Int, chipID: ChipID.ID = ChipID.CPU) : Unit = {
    address & 3 match {
      case 0 =>
        decodeCommand(value) match {
          case Some(c) if c.cmdType == 4 =>
            if (cmd.isIdle) cf(BUSY_FLAG | CRC_ERROR_FLAG | DRQ_FLAG | RECORD_NOT_FOUND_FLAG)
            else cf(BUSY_FLAG)
            cmd = IdleCommand
            step = NoStep
            if (DEBUG) println("Command Interrupted!!")
          case Some(c) if !isf(BUSY_FLAG) =>
            cmd = c
            cmd.flags = value
            step = cmd.mainStep          
          case Some(c) =>
            if (DEBUG) println(s"Command $c ignored")
          case None =>
            if (DEBUG) println(s"Command value $value unrecognized")
        }
        if (DEBUG) println(s"WD1770: command $cmd")
      case 1 =>
        track = value
        if (DEBUG) println("WDD1770 set track to " + track)
      case 2 =>
        sector = value
        if (DEBUG) println("WDD1770 set sector to " + sector)
      case 3 =>
        dr = value
        if (DEBUG) println(s"DR="+dr.toHexString)
        cf(DRQ_FLAG)
    }
  }
  // ==============================================================================
  
  protected def saveState(out:ObjectOutputStream) : Unit = {
    if (!cmd.isIdle) throw new IllegalStateException("Cannot save state while the drive attached is running")
  }
  protected def loadState(in:ObjectInputStream) : Unit = {}
    
  protected def allowsStateRestoring = true
}