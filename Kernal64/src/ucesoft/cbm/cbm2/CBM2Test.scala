package ucesoft.cbm.cbm2

import ucesoft.cbm.cbm2.IEEE488Connectors._
import ucesoft.cbm.cpu.CPU6510_CE
import ucesoft.cbm.misc.{AbstractDriveLedListener, BasicListExplorer, DriveLed, Switcher, TestCart}
import ucesoft.cbm.peripheral.EmptyConnector
import ucesoft.cbm.peripheral.bus.{IECBus, IEEE488Bus}
import ucesoft.cbm.peripheral.c2n.Datassette
import ucesoft.cbm.peripheral.cia.CIA
import ucesoft.cbm.peripheral.crtc.CRTC6845
import ucesoft.cbm.peripheral.drive.{C1541, DriveLedListener, IEEE488Drive}
import ucesoft.cbm.peripheral.keyboard.BKeyboard
import ucesoft.cbm.peripheral.mos6525.MOS6525
import ucesoft.cbm.peripheral.mos6551.ACIA6551
import ucesoft.cbm.peripheral.sid.SID
import ucesoft.cbm.peripheral.vic.Display
import ucesoft.cbm.{ChipID, Clock, ClockEvent, Log}

import java.awt.datatransfer.DataFlavor
import java.awt.{BorderLayout, Dimension, FlowLayout, Toolkit}
import java.io.{File, PrintWriter, StringWriter}
import javax.swing.{JButton, JFrame, JPanel, JToggleButton}

object CBM2Test {
  private var cpu : CPU6510_CE = _
  private var sid : SID = _
  private var ciaieee,ciaip : CIA = _
  private var tpiIeee : MOS6525 = _
  private var tpiKb : MOS6525 = _
  private var crt : CRTC6845 = _
  private var datassette : Datassette = _
  private val bus = new IEEE488Bus
  private var acia : ACIA6551 = _

  private var c1541 : C1541 = _

  private val _50_60_CYCLES = 2000000 / 50 // 50Hz

  def main(args:Array[String]): Unit = {
    init()
  }

  private def init(): Unit = {
    val sw = new StringWriter
    Log.setOutput(new PrintWriter(sw))
    Log.setInfo
    val clk = Clock.setSystemClock(Some(errorHandler _))(mainLoop _)

    val model : CBM2Model = _610PAL

    val _charROM = java.nio.file.Files.readAllBytes(new File(s"/Users/ealeame/OneDrive - Ericsson AB/CBM-II/roms/${model.charROMName}").toPath).map(_.toInt & 0xFF)
    val basicROM = java.nio.file.Files.readAllBytes(new File(s"/Users/ealeame/OneDrive - Ericsson AB/CBM-II/roms/${model.basicROMName}").toPath).map(_.toInt & 0xFF)
    val kernal = java.nio.file.Files.readAllBytes(new File("/Users/ealeame/OneDrive - Ericsson AB/CBM-II/roms/kernal").toPath).map(_.toInt & 0xFF)
    val charROM = Array.ofDim[Int](8192)
    System.arraycopy(_charROM,0,charROM,0,4096)
    System.arraycopy(charROM,2048,charROM,4096,2048)
    for(i <- 0 until 2048) {
      charROM(i + 2048) = charROM(i) ^ 0xFF
      charROM(i + 6144) = charROM(i + 4096) ^ 0xFF
    }
    /*
      Initial char layout:
      0       2048     4096
      |--------|--------|
          B1       B2
      Final char layout:
      0       2048     4096     6144     8192
      |--------|--------|--------|--------|
          B1      Inv B1    B2      Inv B2
     */
    val frame = new JFrame()
    val display = new Display(CRTC6845.SCREEN_WIDTH,CRTC6845.SCREEN_HEIGHT,"CBM-II",frame)
    display.setRenderingHints(java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    display.setPreferredSize(new Dimension(704,534))
    frame.setFocusTraversalKeysEnabled(false)

    val keyb = new BKeyboard(BKeyboard.DEF_CBM2_KEYMAPPER)
    frame.addKeyListener(keyb)

    val mmu = new CBM2MMU
    //mmu.setROM6000(java.nio.file.Files.readAllBytes(new File("""C:\\Users\\ealeame\\OneDrive - Ericsson AB\\CBM-II\\software\\handic_calcresult.bin""").toPath).map(_.toInt & 0xFF))

    cpu = new CPU6510_CE(mmu,ChipID.CPU)
    mmu.setCPU(cpu)
    mmu.setBasicROM(basicROM)
    mmu.setKernalROM(kernal)
    mmu.setModel(model)
    cpu.initComponent
    val irq = new Switcher("IRQ",low => cpu.irqRequest(low))
    sid = new SID()
    sid.setCPUFrequency(2000000)
    sid.initComponent


    val ciaConnectorA = new CIAIEEE488ConnectorA(bus)

    ciaieee = new CIA("CIAIEEE",
      0xDC00,
      ciaConnectorA,
      new CIAIEEE488ConnectorB,
      low => tpiIeee.setInterruptPin(MOS6525.INT_I2,if (low) 0 else 1),
      _ => {}
    )
    ciaieee.initComponent
    ciaip = new CIA("CIAIP",
      0xDB00,
      EmptyConnector,
      EmptyConnector,
      low => tpiIeee.setInterruptPin(MOS6525.INT_I3,if (low) 0 else 1),
      _ => {}
    )
    ciaip.initComponent

    datassette = new Datassette(ciaieee.setFlagLow _)

    val iec = new IECBus

    tpiIeee = new MOS6525(
      "tpiIeee",
      new IEEE488InterfaceA(bus,ciaConnectorA),
      //new IECInterfaceB(iec,ciaieee.setFlagLow _),
      new IEEE488InterfaceB(bus,datassette),
      new MOS6525.PortC {
        override def read(): Int = {
          println("6525 IEEE READ C")
          (tpiIeee.regs(MOS6525.PRC) & tpiIeee.regs(MOS6525.DDRC)) | (0xFF & ~tpiIeee.regs(MOS6525.DDRC))
        }
        override def write(value: Int): Unit = {
          println(s"6525 IEEE C# write $value")
        }
        override def setCA(bit: Int): Unit = crt.setCharAddressMask(bit << 12)
        override def setCB(bit: Int): Unit = {
          println(s"6525 IEEE CB = $bit")
        }
      },
      _irq => {
        //println(s"6525 IEEE IRQ ${_irq}")
        irq.setLine(0x20,_irq) // MASTER IRQ
      }
    )

    tpiKb = new MOS6525(
      "tpiKb",
      new MOS6525.PortAB {
      override def read(): Int = (tpiKb.regs(MOS6525.PRA) & tpiKb.regs(MOS6525.DDRA)) | (0xFF & ~tpiKb.regs(MOS6525.DDRA))
      override def write(value: Int): Unit = {
        //println(s"6525 KB A# write $value")
        keyb.selectHighColAddress(value)
      }
    },
      new MOS6525.PortAB {
        override def read(): Int = (tpiKb.regs(MOS6525.PRB) & tpiKb.regs(MOS6525.DDRB)) | (0xFF & ~tpiKb.regs(MOS6525.DDRB))
        override def write(value: Int): Unit = {
          //println(s"6525 KB B# KB write $value DDRB=${tpiKb.regs(MOS6525.DDRB).toHexString}")
          keyb.selectLowColAddress(value)
        }
      },
      new MOS6525.PortC {
        override def read(): Int = {
          //println(s"KEYB READ: DDRC=${tpiKb.regs(MOS6525.DDRC).toHexString}")
          (keyb.read() & 0x3F) | (if (model.isPAL) 0x00 else 0x40) | (if (model.lowProfile) 0x00 else 0x80)
        }
        override def write(value: Int): Unit = println(s"6525 KB C# KB write $value")
        override def setCA(bit: Int): Unit = println(s"6525 KB CA = $bit")
        override def setCB(bit: Int): Unit = println(s"6525 KB CB = $bit")
      },
      _ => {}
    )

    // 1541
    /*
    c1541 = new C1541(1,iec,new DriveLedListener {
      override def writeMode(enabled: Boolean): Unit = {}
      override def setPowerLedMode(on: Boolean): Unit = {}
      override def turnPower(on: Boolean): Unit = {}
      override def turnOn(): Unit = println("DRIVE LED ON")
      override def turnOff(): Unit = println("DRIVE LED OFF")
      override def isOn: Boolean = false
      override def moveTo(track: Int, sector: Option[Int], halfTrack: Boolean): Unit = println(s"DRIVE track=$track sector=${sector.getOrElse(0)}")
    })
    ROM.props = new Properties()
    c1541.initComponent()
    c1541.runningListener = x => {}
    */

    val driveLed = new DriveLed(8)
    val driveLedListener = new AbstractDriveLedListener(driveLed) {}

    // TEST Device
    val device = new IEEE488Drive("8050",8,bus,driveLedListener)
    /*,new DriveLedListener {
      override def writeMode(enabled: Boolean): Unit = {}
      override def setPowerLedMode(on: Boolean): Unit = {}
      override def turnPower(on: Boolean): Unit = {}
      override def turnOn(): Unit = println("Drive ON")
      override def turnOff(): Unit = println("Drive OFF")
      override def isOn: Boolean = false
      override def moveTo(track: Int, sector: Option[Int], halfTrack: Boolean): Unit = println(s"Drive ($track,${sector.getOrElse(0)})")
    })*/
    device.initComponent

    //val printer = new IEEE488MPS803("MPS803",4,bus,new MPS803GFXDriver(new MPS803ROM))
    //printer.setActive(true)

    // 50/60 Hz source
    clk.schedule(new ClockEvent("50_60Hz",clk.nextCycles,_50_60_Hz _))

    crt = new CRTC6845(mmu.getCRTCRam,charROM,16/*,blank => tpiIeee.setInterruptPin(MOS6525.INT_I0,if (blank) 1 else 0)*/)
    crt.setDisplay(display)
    crt.initComponent
    crt.setClipping(model.crtClip._1,model.crtClip._2,model.crtClip._3,model.crtClip._4)

    acia = new ACIA6551(irq => tpiIeee.setInterruptPin(MOS6525.INT_I4,if (irq) 0 else 1))

    mmu.setIO(crt,ciaieee,ciaip,tpiKb,tpiIeee,sid,acia)

    TestCart.setCartLocation(0xFDAFF)
    TestCart.enabled = true

    /*
    val traceDialog = TraceDialog.getTraceDialog("CPU Debugger", frame, mmu, cpu)
    traceDialog.forceTracing(true)
    traceDialog.setVisible(true)
    Log.setOutput(traceDialog.logPanel.writer)
    Log.info(sw.toString)
     */

    val warp = new JToggleButton("Warp")
    val paste = new JButton("Paste")
    val list = new JButton("List")
    warp.setFocusable(false)
    paste.setFocusable(false)
    list.setFocusable(false)
    val dummy = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummy.add(warp)
    dummy.add(paste)
    dummy.add(list)
    warp.addActionListener(_ => clk.maximumSpeed = warp.isSelected )
    paste.addActionListener(_ => {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val contents = clipboard.getContents(null)
      if (contents.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        val str = contents.getTransferData(DataFlavor.stringFlavor).toString
        BKeyboard.insertTextIntoKeyboardBuffer(str,mmu)
      }
    })
    list.addActionListener(_ => {
      clk.pause()
      BasicListExplorer.listCBM2(mmu,3)
      clk.play()
    })
    val southPanel = new JPanel(new BorderLayout())
    southPanel.add("West",dummy)
    southPanel.add("East",driveLed)

    frame.getContentPane.add("Center",display)
    frame.getContentPane.add("South",southPanel)
    frame.pack()
    frame.setVisible(true)

    clk.setClockHz(2000000) // 2Mhz
    clk.play
    crt.play
  }

  private def _50_60_Hz(cycles:Long):Unit = {
    tpiIeee.setInterruptPin(MOS6525.INT_I0,1)
    tpiIeee.setInterruptPin(MOS6525.INT_I0,0)
    Clock.systemClock.schedule(new ClockEvent("50_60Hz",cycles + _50_60_CYCLES,_50_60_Hz _))
  }

  private def errorHandler(t:Throwable): Unit = {
    t.printStackTrace()
    sys.exit(1)
  }

  private def mainLoop(cycles:Long): Unit = {
    cpu.fetchAndExecute(1)
    ciaieee.clock(false)
    ciaip.clock(false)
    //if ((cycles & 1) == 0) c1541.clock(cycles >> 1)
  }
}
