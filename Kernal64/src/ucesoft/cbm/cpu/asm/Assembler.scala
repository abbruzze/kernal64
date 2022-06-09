package ucesoft.cbm.cpu.asm

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.{RTextScrollPane, SearchContext, SearchEngine}
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.cpu.asm.AsmEvaluator.Evaluator
import ucesoft.cbm.cpu.asm.AsmParser.{Patch, Statement, Virtual}
import ucesoft.cbm.misc.Preferences
import ucesoft.cbm.trace.{BreakSet, NoBreak, TraceDialog, TracingListener}

import java.awt.datatransfer.{Clipboard, DataFlavor}
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Cursor, FlowLayout}
import java.io._
import java.text.SimpleDateFormat
import java.util.{Properties, Scanner}
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.table.AbstractTableModel
import javax.swing.text.DefaultCaret
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.parsing.input.Position

object Assembler {
  private[asm] val symbolTable = new mutable.LinkedHashMap[String,String]()
  private[asm] var importDir = System.getProperty("user.dir")

  private[asm] val FORMAT_BINARY = 0
  private[asm] val FORMAT_PRG = 1

  private[asm] case class CompilationResult(blocks:List[ByteCodeBlock],asmEncoding:AsmEncoding)

  def getAssemblerDialog(parent:JFrame,mem:Memory,traceDialog:TraceDialog) : JDialog = {
    val assembler = new AssemblerPanel(mem,traceDialog)
    val dialog = new JDialog(parent,"Assembler") {
      override def setVisible(b: Boolean): Unit = {
        super.setVisible(b)
        if (b) assembler.reqFocus
      }
    }
    assembler.setParent(dialog)
    dialog.getContentPane.add("Center",assembler)
    dialog.setJMenuBar(assembler.getMenuBar)
    dialog.pack()
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = assembler.close
    })

    dialog
  }

  private def compileAutoImport(implicit log : String => Unit,error : String => Unit) : List[Statement] = {
    val file = new File(new File(importDir),"autoimport.asm")
    val in : Option[(InputStream,String)] = if (file.exists()) Some((new FileInputStream(file),file.getParent)) else {
      val in = ClassLoader.getSystemClassLoader.getResourceAsStream("resources/autoimport.asm")
      if (in != null) Some((in,"jar library")) else None
    }

    in match {
      case Some((in,from)) =>
        val src = Source.fromInputStream(in)
        val sw = new StringReader(src.getLines.mkString("\n"))

        log(s"Loading autoimport.asm from $from ...\n")
        val parser = new AsmParser("autoimport.asm")
        try {
          parser.parseAll(parser.topStatements, sw) match {
            case parser.Success(parsed, _) =>
              parsed
            case parser.NoSuccess(err,input) =>
              error(s"$err at ${input.pos}\n")
              Nil
          }
        }
        finally {
          src.close
        }
      case None =>
        Nil
    }
  }

  private[asm] def compileSource(src:String,
                                 currentFileName:Option[String],
                                 symbolTable:mutable.LinkedHashMap[String,String])
                                (log : String => Unit = s => print(s),
                                 error : String => Unit = s => print(s)) : Option[CompilationResult] = {
    def rowCol(p:Position) : String = s"line ${p.line}, column ${p.column}"

    val prep = new Preprocessor
    log("Preprocessing text ...\n")
    prep.analyze(src,symbolTable.toMap) match {
      case Left(err) =>
        error(s"Preprocessor error: $err")
        None
      case Right(Preprocessor.Result(prepSource,newSet)) =>
        symbolTable.clear()
        symbolTable.addAll(newSet)
        val parser = new AsmParser(currentFileName.getOrElse("noname.asm"))
        val in = new java.io.StringReader(prepSource)

        try {
          val autoImportStatements = compileAutoImport(log,error)
          parser.parseAll(parser.topStatements, in) match {
            case parser.Success(parsed, _) =>
              try {
                val sw = new StringWriter
                log("Start compiling ...\n")
                Evaluator.importDir = importDir
                val cc = new AsmCompiler(new PrintWriter(sw), importDir)
                val lastByteCodeBlocks = cc.compile(autoImportStatements ++ parsed)

                log("Code blocks found:\n")
                log(s"${"=" * 50}\n")
                for ((b, i) <- lastByteCodeBlocks.zipWithIndex) log(f"#$i%03d ${b.name.getOrElse("Unnamed")}%30s ${b.getOrg}%04X ${b.getOrg + b.size - 1}%04X${if (b.orgType == Virtual) " virtual" else if (b.orgType == Patch) " patch" else ""}\n")

                val emitter = new AsmBytecodeEmitter(new PrintWriter(sw), lastByteCodeBlocks)
                val asmEncoding = emitter.encode

                for (l <- sw.toString.split("\n")) log(s"$l\n")

                log("Compilation terminated\n")
                Some(CompilationResult(lastByteCodeBlocks,asmEncoding))
              }
              catch {
                case ce: CompilerException =>
                  ce.statement match {
                    case Some(s) =>
                      error(s"Parsing error at ${rowCol(s.pos)}: ${ce.msg}\n${s.pos.longString}\n")
                    case None =>
                      error(s"Parsing error: ${ce.msg}\n")
                  }
                  None
              }

            case parser.NoSuccess(_err,input) =>
              val err = if (_err.startsWith("end of input")) "Syntax error" else _err
              error(s"$err at ${rowCol(input.pos)}\n${input.pos.longString}\n")
              None
          }
        }
        catch {
          case i:IllegalArgumentException =>
            error(i.getMessage)
            None
        }
    }
  }

  private[asm] def saveAs(asmEncoding:AsmEncoding, fileName:String, format:Int) : Unit = {
    val out = new java.io.FileOutputStream(fileName)
    if (format == FORMAT_PRG) {
      out.write(asmEncoding.org & 0xFF)
      out.write(asmEncoding.org >> 8)
    }
    out.write(asmEncoding.mem)
    out.close()
  }

  def main(args:Array[String]) : Unit = {
    val settings = new Preferences
    importDir = "./"
    val symbols = new mutable.LinkedHashMap[String,String]
    var log : String => Unit = s => print(s)
    var out : Option[String] = None
    var in : InputStream = System.in
    var fileName : Option[String] = None
    var outFormat = FORMAT_BINARY

    settings.add("include-dir","Sets the directory where the include files will be searched in","") { importDir = _ }
    settings.add("silent","Log errors only",false) { sil => if (sil) log = _ => {} }
    settings.add("prg","Writes to the given PRG file","") { prg =>
      out = Some(prg)
      outFormat = FORMAT_PRG
    }
    settings.add("bin","Writes to the given raw file","") { bin =>
      out = Some(bin)
      outFormat = FORMAT_BINARY
    }
    settings.add("D","Defines a symbol. If followed by =<value> defines a string or int symbol, otherwise a boolean symbol","") { sym =>
      sym.split("=") match {
        case Array(symbol,value) =>
          symbols += symbol -> value
        case Array(symbol) =>
          symbols += symbol -> ""
        case _ => throw new IllegalArgumentException(s"Invalid symbol's value $sym")
      }
    }

    if (settings.checkForHelp(args) || args.length == 0) {
      println(s"K64Assembler ver. ${ucesoft.cbm.Version.VERSION} (${ucesoft.cbm.Version.BUILD_DATE})")
      settings.printUsage("file to compile")
      sys.exit(0)
    }

    settings.parseAndLoad(args,new Properties) match {
      case Some(src) =>
        val f = new File(src)
        if (!f.exists() || !f.isFile) {
          println(s"Source file $src not found")
          sys.exit(1)
        }
        in = new FileInputStream(f)
        fileName = Some(src)
      case None =>
    }

    val source = io.Source.fromInputStream(in).getLines().mkString("\n")
    compileSource(source,fileName,symbols)(log,s => println(s)) match {
      case Some(CompilationResult(_,enc)) =>
        out match {
          case Some(target) =>
            saveAs(enc,target,outFormat)
            log(s"Output binary to $target")
          case None =>
        }
      case None =>
    }
  }
}

private object SymbolPanel {
  private var symbolCounter = 0
}

private class SymbolPanel(map:collection.mutable.LinkedHashMap[String,String]) extends JPanel {
  import SymbolPanel._
  private val symbols = new ListBuffer[Array[String]]()

  symbols ++= map.map(kv => Array(kv._1,kv._2))

  private class SymbolDataModel extends AbstractTableModel {
    override def getColumnName(column: Int): String = column match {
      case 0 => "Symbol"
      case 1 => "Value"
    }

    override def getRowCount: Int = symbols.size
    override def getColumnCount: Int = 2

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = symbols(rowIndex)(columnIndex)

    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = true

    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit = symbols(rowIndex)(columnIndex) = aValue.toString
  }

  private val model = new SymbolDataModel
  private val table = new JTable(model)
  private val addButton = new JButton("New symbol")
  private val delButton = new JButton("Delete")
  init

  def init() : Unit = {
    setLayout(new BorderLayout())
    val scrollPane = new JScrollPane(table)
    table.setFillsViewportHeight(true)
    add("Center",scrollPane)

    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    buttonPanel.add(addButton)
    buttonPanel.add(delButton)
    addButton.addActionListener( _ => addSymbol )
    delButton.addActionListener( _ => delSymbols )

    add("North",buttonPanel)
    table.getSelectionModel.addListSelectionListener(e => delButton.setEnabled(table.getSelectedRowCount > 0))
  }

  private def delSymbols() : Unit = {
    for(i <- table.getSelectedRows.sorted.reverse) symbols.remove(i)
    model.fireTableStructureChanged()
  }

  private def addSymbol() : Unit = {
    symbols += Array(s"NewSymbol$symbolCounter","")
    symbolCounter += 1
    model.fireTableStructureChanged()
  }

  def updateSymbols() : Unit = {
    map.clear()
    for(s <- symbols) map += s(0) -> s(1)
  }
}

private class AssemblerPanel(mem:Memory,traceDialog:TraceDialog) extends JPanel with TracingListener {
  import Assembler._

  private case class Break(address:Int,id:AnyRef)

  private val saveAsPRGItem = new JMenuItem("Save as PRG")
  private val saveAsBINItem = new JMenuItem("Save as raw bin file")
  private val uploadInMemoryItem = new JMenuItem("Upload in memory")
  private val compileItem = new JMenuItem("Compile")

  private val clearMessagesButton = new JButton("Clear")
  private val searchField = new JTextField(30)
  private val findNextButton = new JButton("Find next")
  private val findPrevButton = new JButton("Find previous")
  private val clearFoundButton = new JButton("Clear")
  private val regExCheck = new JCheckBox("Regex")
  private val matchCaseCheck = new JCheckBox("Match case")

  private val textArea = new RSyntaxTextArea(20,80)
  private val messageArea = new JTextArea(5,80)
  private var asmEncoding : AsmEncoding = _
  private val df = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss")
  private var currentFileName : Option[String] = None
  private var fileChanged = false
  private var parentDialog : JDialog = _
  private var lastHighlightID : AnyRef = _
  private val breaksID = new collection.mutable.LinkedHashMap[Int,Break]
  private val rsp = new RTextScrollPane(textArea)

  private var lastByteCodeBlocks : List[ByteCodeBlock] = Nil

  def setParent(parent:JDialog) : Unit = this.parentDialog = parent

  init

  def close() : Unit = traceDialog.removeListener(this)

  def getMenuBar : JMenuBar = {
    val menu = new JMenuBar
    val file = new JMenu("File")
    val edit = new JMenu("Edit")
    val search = new JMenu("Search")
    val compile = new JMenu("Compile")
    val debugger = new JMenu("Debugger")

    menu.add(file)
    menu.add(edit)
    menu.add(search)
    menu.add(compile)
    menu.add(debugger)

    val load = new JMenuItem("Load")
    load.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    load.addActionListener( _ => this.load )
    val save = new JMenuItem("Save")
    save.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S,java.awt.event.InputEvent.ALT_DOWN_MASK))
    save.addActionListener( _ => this.save(false) )
    val saveAs = new JMenuItem("Save as")
    saveAs.addActionListener( _ => this.save(true) )

    saveAsPRGItem.addActionListener( _ => saveAsFormat(FORMAT_PRG) )
    saveAsPRGItem.setEnabled(false)
    saveAsBINItem.addActionListener( _ => saveAsFormat(FORMAT_BINARY) )
    saveAsBINItem.setEnabled(false)

    file.add(load)
    file.add(save)
    file.add(saveAs)
    file.add(saveAsPRGItem)
    file.add(saveAsBINItem)

    val gotoItem = new JMenuItem("Go to line ...")
    gotoItem.addActionListener( _ => gotoLine )
    gotoItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G,java.awt.event.InputEvent.CTRL_DOWN_MASK))

    edit.add(gotoItem)

    val searchForward = new JMenuItem("Search forward")
    searchForward.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK))
    searchForward.addActionListener( _ => this.search(true) )
    val searchBackward = new JMenuItem("Search backward")
    searchBackward.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK | java.awt.event.InputEvent.CTRL_DOWN_MASK))
    searchBackward.addActionListener( _ => this.search(false) )

    search.add(searchForward)
    search.add(searchBackward)

    compileItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,java.awt.event.InputEvent.ALT_DOWN_MASK))
    compileItem.addActionListener( _ => this.compile )
    uploadInMemoryItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U,java.awt.event.InputEvent.ALT_DOWN_MASK))
    uploadInMemoryItem.addActionListener( _ => uploadInMemory )
    uploadInMemoryItem.setEnabled(false)
    val includeItem = new JMenuItem("Set include directory")
    includeItem.addActionListener( _ => setImportDir )
    val symbolsItem = new JMenuItem("Symbols ...")
    symbolsItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y,java.awt.event.InputEvent.ALT_DOWN_MASK))
    symbolsItem.addActionListener( _ => openSymbolDialog )
    val breakItem = new JMenuItem("Break on current line")
    breakItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B,java.awt.event.InputEvent.ALT_DOWN_MASK))
    breakItem.addActionListener( _ => setBreak )
    breakItem.setEnabled(false)


    compile.add(compileItem)
    compile.add(uploadInMemoryItem)
    compile.add(includeItem)
    compile.add(symbolsItem)
    compile.add(breakItem)

    val debuggerItem = new JCheckBoxMenuItem("Attach to debugger")
    debuggerItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A,java.awt.event.InputEvent.ALT_DOWN_MASK))
    debuggerItem.addActionListener( _ => {
      attachToDebugger(debuggerItem.isSelected)
      breakItem.setEnabled(debuggerItem.isSelected)
    } )
    val commentedROM = new JMenuItem("Attach to debugger using commented ROM code")
    commentedROM.addActionListener( _ => {
      debuggerItem.setSelected(true)
      attachToDebugger(true)
      breakItem.setEnabled(true)
      findCommentedCode
    })

    debugger.add(debuggerItem)
    debugger.add(commentedROM)

    menu
  }

  private class DNDHanlder(th:TransferHandler) extends TransferHandler {
    override def canImport(support: TransferHandler.TransferSupport): Boolean =
      support.isDataFlavorSupported(DataFlavor.javaFileListFlavor) || th.canImport(support)

    override def importData(support: TransferHandler.TransferSupport): Boolean = {
      if (!canImport(support)) return false
      val t = support.getTransferable
      import scala.jdk.CollectionConverters._
      if (!t.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) th.importData(support)
      else
      t.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]].asScala.headOption match {
        case None =>
          false
        case Some(f) =>
          load(f)
          true
      }
    }

    override def exportToClipboard(comp: JComponent, clip: Clipboard, action: Int): Unit = th.exportToClipboard(comp,clip,action)
  }

  private def search(forward:Boolean) : Unit = {
    val text = searchField.getText
    if (text.length > 0) {
      val context = new SearchContext
      context.setSearchFor(text)
      context.setMatchCase(matchCaseCheck.isSelected)
      context.setRegularExpression(regExCheck.isSelected)
      context.setWholeWord(false)
      //context.setMarkAll(true)
      context.setSearchForward(forward)

      if (!SearchEngine.find(textArea, context).wasFound()) JOptionPane.showMessageDialog(this, "Text not found")
      reqFocus
    }
  }

  private def clearSearch() : Unit = {
    textArea.removeAllLineHighlights()
  }

  def reqFocus() : Unit = {
    textArea.requestFocusInWindow
  }

  private def init() : Unit = {
    setLayout(new BorderLayout)
    textArea.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_ASSEMBLER_6502)
    rsp.setBorder(BorderFactory.createTitledBorder("noname.asm"))
    textArea.setAnimateBracketMatching(true)
    textArea.setBracketMatchingEnabled(true)

    clearMessagesButton.addActionListener( _ => messageArea.setText("") )

    val northPane = new JPanel(new BorderLayout())

    northPane.add("Center",rsp)
    val searchPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    searchPanel.add(searchField)
    searchPanel.add(findNextButton)
    searchPanel.add(findPrevButton)
    searchPanel.add(regExCheck)
    searchPanel.add(matchCaseCheck)
    searchPanel.add(clearFoundButton)
    searchField.addActionListener( _ => search(true) )
    findNextButton.addActionListener( _ => search(true) )
    findPrevButton.addActionListener( _ => search(false) )
    clearFoundButton.addActionListener( _ => clearSearch )
    searchPanel.setBorder(BorderFactory.createTitledBorder("Search"))
    northPane.add("South",searchPanel)

    val southPane = new JPanel(new BorderLayout())
    messageArea.setEditable(false)
    messageArea.setBackground(Color.LIGHT_GRAY)
    messageArea.setForeground(Color.BLACK)
    messageArea.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val sp = new JScrollPane(messageArea)
    sp.setBorder(BorderFactory.createTitledBorder("Messages"))
    southPane.add("Center",sp)

    val buttonSouthPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    buttonSouthPanel.add(clearMessagesButton)
    southPane.add("South",buttonSouthPanel)

    val split = new JSplitPane(JSplitPane.VERTICAL_SPLIT,northPane,southPane)

    add("Center",split)
    textArea.setDragEnabled(true)
    val dnd = new DNDHanlder(textArea.getTransferHandler)
    textArea.setTransferHandler(dnd)
    textArea.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = setFileChanged(true)
      override def removeUpdate(e: DocumentEvent): Unit = setFileChanged(true)
      override def changedUpdate(e: DocumentEvent): Unit = setFileChanged(true)
    })
  }

  private def setFileChanged(changed:Boolean) : Unit = {
    if (fileChanged != changed) {
      fileChanged = changed
      rsp.setBorder(BorderFactory.createTitledBorder(currentFileName.getOrElse("noname.asm") + (if (fileChanged) "*" else "")))
    }
  }

  private def log(msg:String) : Unit = messageArea.append(s"${df.format(new java.util.Date)} $msg")

  private def findCommentedCode() : Unit = {
    Option(JOptionPane.showInputDialog(parentDialog,"Enter a regular expression with a (group) embracing line address","^([a-f|A-F|0-9]{4})")) match {
      case Some(regex) =>
        try {
          val R = regex.r
          val scanner = new Scanner(textArea.getText)
          val bcb = new ByteCodeBlock(0,None)
          var lineCount = 0
          var matches = 0
          while (scanner.hasNextLine) {
            val line = scanner.nextLine()
            lineCount += 1
            R.findAllMatchIn(line).toList match {
              case addr :: Nil =>
                if (addr.groupCount > 0) {
                  bcb.addCommentedAsm(lineCount, Integer.parseInt(addr.group(1), 16),currentFileName.getOrElse("noname.asm"))
                  matches += 1
                }
              case _ =>
            }
          }
          lastByteCodeBlocks = List(bcb)
          JOptionPane.showMessageDialog(this, s"Found $matches matches","Text analyzed",JOptionPane.INFORMATION_MESSAGE)
        }
        catch {
          case e:Exception =>
            JOptionPane.showMessageDialog(this, e.toString,"Regex error",JOptionPane.ERROR_MESSAGE)
        }
      case None =>
    }
  }

  private def setBreak() : Unit = {
    val line = textArea.getCaretLineNumber + 1
    val found = (lastByteCodeBlocks.view map { _.asmStatements } flatten) find { asm =>
      asm.source.line == line
    }

    found match {
      case None =>
      case Some(asm) =>
        breaksID.remove(asm.source.line) match {
          case None =>
            val id = textArea.addLineHighlight(line - 1, Color.RED)
            breaksID += asm.source.line -> Break(asm.pc,id)
          case Some(Break(_,id)) =>
            textArea.removeLineHighlight(id)
        }
        if (breaksID.size > 0) traceDialog.setBrk(BreakSet(breaksID.values map { _.address } toSet))
        else traceDialog.setBrk(NoBreak)
    }

  }

  override def stepInto(pc: Int): Unit = {
    removeLastHighlight
    val found = (lastByteCodeBlocks.view map { _.asmStatements } flatten) find { asm =>
      pc == asm.pc && asm.asm.fileName.getOrElse("") == currentFileName.getOrElse("noname.asm")
    }
    found foreach { asm =>
      highlightAndGo(asm.source.line - 1)
      return
    }
  }

  private def attachToDebugger(attach:Boolean) : Unit = {
    if (attach) {
      traceDialog.addListener(this)
      textArea.setHighlightCurrentLine(false)
      parentDialog.setTitle(parentDialog.getTitle + " attached to debugger")
    }
    else {
      traceDialog.removeListener(this)
      removeLastHighlight
      textArea.setHighlightCurrentLine(true)
      val title = parentDialog.getTitle
      parentDialog.setTitle(title.dropRight(" attached to debugger".length))
    }
  }

  private def openSymbolDialog() : Unit = {
    val dialog = new JDialog(parentDialog,"Symbol table",true)
    val panel = new SymbolPanel(symbolTable)
    dialog.getContentPane.add("Center",panel)
    dialog.setLocationRelativeTo(parentDialog)
    dialog.pack()
    dialog.setVisible(true)
    panel.updateSymbols
  }

  private def save(saveAs:Boolean) : Boolean = {
    def saveFile(fileName:String) : Unit = {
      val out = new PrintWriter(fileName)
      try {
        out.print(textArea.getText)
      }
      finally {
        out.close()
      }
    }

    if (saveAs || !currentFileName.isDefined) {
      val fc = new JFileChooser(importDir)
      fc.setDialogTitle("Choose a file where to save source code")
      fc.showSaveDialog(this) match {
        case JFileChooser.APPROVE_OPTION =>
          saveFile(fc.getSelectedFile.toString)
          currentFileName = Some(fc.getSelectedFile.toString)
          rsp.setBorder(BorderFactory.createTitledBorder(fc.getSelectedFile.getName))
          setFileChanged(false)
          true
        case _ =>
          false
      }
    }
    else {
      saveFile(currentFileName.get)
      setFileChanged(false)
      true
    }
  }

  private def load() : Unit = {
    if (fileChanged) {
      JOptionPane.showConfirmDialog(this,s"File ${currentFileName.getOrElse("noname.asm")} has changed. Do you want to save it ?","Confirm",JOptionPane.YES_NO_CANCEL_OPTION) match {
        case JOptionPane.YES_OPTION =>
          if (!save(false)) return
        case JOptionPane.NO_OPTION =>
        case JOptionPane.CANCEL_OPTION =>
          return
      }
    }
    val fc = new JFileChooser(importDir)
    fc.setDialogTitle("Choose file to load")
    fc.showOpenDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        load(fc.getSelectedFile)
      case _ =>
    }
  }

  private def gotoLine() : Unit = {
    Option(JOptionPane.showInputDialog(parentDialog,"Go to line:","")) match {
      case Some(line) =>
        try {
          val l = line.toInt
          if (l > 0 && l <= textArea.getLineCount) highlightAndGo(l,false)
        }
        catch {
          case _:Exception =>
        }
      case _ =>
    }
  }

  private def removeLastHighlight() : Unit = {
    if (lastHighlightID != null) {
      textArea.removeLineHighlight(lastHighlightID)
      lastHighlightID = null
    }
  }

  private def highlightAndGo(line:Int,highlight:Boolean = true) : Unit = {
    SwingUtilities.invokeLater( () => {
      if (highlight) lastHighlightID = textArea.addLineHighlight(line, Color.GREEN)
      val target = if (line > 10) line - 10 else line
      rsp.getVerticalScrollBar.setValue(textArea.yForLine(target))
      textArea.setCaretPosition(textArea.getLineStartOffset(line - 1))
      textArea.repaint()
    })
  }

  private def load(fileName:File) : Unit = {
    import scala.jdk.CollectionConverters._
    textArea.setText("")
    rsp.setBorder(BorderFactory.createTitledBorder(fileName.getName))
    currentFileName = Some(fileName.toString)
    for(l <- java.nio.file.Files.readAllLines(fileName.toPath).asScala) textArea.append(s"$l\n")
    setFileChanged(false)
  }

  private def saveAsFormat(format:Int) : Unit = {
    val fc = new JFileChooser(importDir)
    fc.setDialogTitle("Choose a file where to save compiled code")
    fc.showSaveDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        saveAs(asmEncoding,fc.getSelectedFile.toString,format)
      case _ =>
    }
  }

  private def setImportDir() : Unit = {
    val fc = new JFileChooser(importDir)
    fc.setDialogTitle("Choose import directory")
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.showOpenDialog(this) match {
      case JFileChooser.APPROVE_OPTION =>
        importDir = fc.getSelectedFile.toString
      case _ =>
    }
  }

  private def uploadInMemory() : Unit = {
    log(f"Uploading from memory ${asmEncoding.org}%04X ...\n")
    for(i <- 0 until asmEncoding.mem.length) {
      mem.write(asmEncoding.org + i,asmEncoding.mem(i) & 0xFF)
    }
    uploadInMemoryItem.setEnabled(false)
  }

  private def compile() : Unit = {
    setCursor(new java.awt.Cursor(Cursor.WAIT_CURSOR))
    uploadInMemoryItem.setEnabled(false)
    saveAsPRGItem.setEnabled(false)
    saveAsBINItem.setEnabled(false)
    compileItem.setEnabled(false)

    new Thread {
      override def run(): Unit = {
        try {
          compileSource(textArea.getText, currentFileName, symbolTable)(log _, log _) match {
            case Some(CompilationResult(blocks, enc)) =>
              lastByteCodeBlocks = blocks
              asmEncoding = enc
              uploadInMemoryItem.setEnabled(true)
              saveAsPRGItem.setEnabled(true)
              saveAsBINItem.setEnabled(true)
            case None =>
          }
        }
        finally {
          compileItem.setEnabled(true)
          setCursor(new java.awt.Cursor(Cursor.DEFAULT_CURSOR))
        }
      }
    }.start()
  }
}
