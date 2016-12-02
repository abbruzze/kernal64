package ucesoft.c64.game

import javax.swing._
import javax.swing.table.AbstractTableModel
import javax.swing.event.ListSelectionListener
import java.awt.Dimension
import java.awt.event.MouseEvent
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import java.awt.FlowLayout
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.BorderLayout
import java.awt.Image
import java.awt.Graphics
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.event.ListSelectionEvent
import java.awt.GridLayout
import scala.util.Success
import scala.util.Failure
import java.awt.Cursor
import scala.concurrent.Future
import java.awt.Insets
import javax.swing.table.DefaultTableCellRenderer
import java.awt.Component
import java.awt.Color
import java.util.concurrent.atomic.AtomicInteger
import ucesoft.c64.formats.ZIP
import java.awt.event.MouseAdapter

object GameUI {
  private val providerCache = new collection.mutable.HashMap[String,JDialog]
  
  private val COL_WIDTHS = Array(100,50,20,50)
  
  private class ResImage(image:Image,maxW:Int = Int.MaxValue,maxH:Int = Int.MaxValue) extends JPanel {
    private val (ow,oh) = (image.getWidth(null),image.getHeight(null))
    private val (w,h) = {
      var w = ow
      var h = oh
      if (w > maxW || h > maxH) {
        var ratio = w.toDouble / h
        w = ow min maxW
        h = (w / ratio).toInt min maxH                
      }
      (w,h)
    }
    setPreferredSize(new Dimension(w,h))
    
    override def paintComponent(g:Graphics) {      
      g.drawImage(image,0,0,w,h,null)
    }
  } 
  
  private class GameTableModel(var games:Array[Game]) extends AbstractTableModel {
    private val COL_NAMES = Array("Name","Genre","Date","Author")    
    def getRowCount = games.length
    def getColumnCount = 4
    def getValueAt(r:Int,c:Int) = c match {
      case 0 => games(r).name
      case 1 => games(r).genre
      case 2 => games(r).date
      case 3 => games(r).softwareHouse
    }
    def getGame(r:Int) = games(r)
    override def getColumnName(c:Int) = COL_NAMES(c)
    override def isCellEditable(r:Int,c:Int) = false
    
    def fillWith(newGames:Array[Game]) {
      games = newGames
      fireTableDataChanged
    }
  }
  
  private def createTable(provider:GameProvider,games:Array[Game])(listener:ListSelectionListener) : JTable = {
    val table = new JTable(new GameTableModel(games)) {
      override def getToolTipText(e:MouseEvent) : String = {
        val p = e.getPoint
        val viewRow = rowAtPoint(p)
        val r = if (viewRow >= 0) getRowSorter.convertRowIndexToModel(viewRow) else -1
        val c = columnAtPoint(p)
        val colIndex = columnAtPoint(p)
        val realColumnIndex = convertColumnIndexToModel(colIndex)
        if (r >= 0 && colIndex >= 0) getModel.getValueAt(r,realColumnIndex).toString else ""
      }
    }
    for(i <- 0 to 3) {
      val col = table.getColumnModel.getColumn(i)
      col.setPreferredWidth(COL_WIDTHS(i))
    }
    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    if (listener != null) table.getSelectionModel.addListSelectionListener(listener)
    table.setAutoCreateRowSorter(true)
    table.setPreferredScrollableViewportSize(new Dimension(500, 200))
    table.setFillsViewportHeight(true)
    table.getColumnModel.getColumn(0).setCellRenderer(new DefaultTableCellRenderer {
      override def getTableCellRendererComponent(table:JTable,value:AnyRef,isSelected:Boolean,hasFocus:Boolean,r:Int,c:Int) : Component = {
        val label = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, r, c).asInstanceOf[JLabel]
        val model = table.getModel.asInstanceOf[GameTableModel]
        val game = model.getGame(table.getRowSorter.convertRowIndexToModel(r))
        if (!isSelected) {
          if (provider.repository.existsInCache(game)) label.setForeground(Color.GREEN.darker) else label.setForeground(Color.BLACK)
        }
        label
      }
    })
    table
  }
  
  private def createTableFilter(filterCols: Int => String) : RowFilter[GameTableModel,Int] = new RowFilter[GameTableModel,Int]{
    override def include(entry:RowFilter.Entry[_ <: GameTableModel,_ <: Int]) : Boolean = {
      var included = true
      for(i <- 0 to 3) {
        val value = entry.getModel.getValueAt(entry.getIdentifier,i).toString.toUpperCase
        val filter = filterCols(i).toUpperCase
        if (!filter.isEmpty) included &= value.indexOf(filter) != -1
      }
      included
    }    
  }
  
  private def createRandomTableFilter(size:Int,totalSize:Int) : RowFilter[GameTableModel,Int] = new RowFilter[GameTableModel,Int] {
    private val rnd = new util.Random
    private val generate : Set[Int] = {
      val set = new collection.mutable.HashSet[Int]
      while (set.size < size) {
        val n = rnd.nextInt(totalSize) + 1
        if (!set.contains(n)) set += n
      }
      set.toSet
    }
    override def include(entry:RowFilter.Entry[_ <: GameTableModel,_ <: Int]) : Boolean = generate contains entry.getIdentifier
  }
  
  private def createFilterPanel(table:JTable) : JPanel with DocumentListener = new JPanel with Function1[Int,String] with DocumentListener {
    val filterTextFields = Array.fill(4)(new JTextField(10))
    val fields = Array("Name: ","Genre: ","Year: ","Software House: ")
    
    for(tf <- filterTextFields) tf.getDocument.addDocumentListener(this)
    
    def changedUpdate(e:DocumentEvent) = updateFilter
    def insertUpdate(e:DocumentEvent) = updateFilter
    def removeUpdate(e:DocumentEvent) = updateFilter
    
    private def updateFilter {
      table.getRowSorter.asInstanceOf[DefaultRowSorter[GameTableModel,Int]].setRowFilter(createTableFilter(this))
    }
    
    def apply(col:Int) = filterTextFields(col).getText
    
    setLayout(new FlowLayout)
    for(i <- 0 to 3) {
      add(new JLabel(fields(i)))
      add(filterTextFields(i))
    }
    val resetButton = new JButton("Clear filters")
    add(resetButton)
    resetButton.addActionListener(new ActionListener {
      def actionPerformed(e:ActionEvent) = filterTextFields foreach { _.setText("") }
    })
    setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,"Filters"))
  }
  
  def createHeaderPanel(provider:GameProvider) : JPanel = {
    val header = new JPanel(new BorderLayout)    
    val rPanel = new JPanel(new FlowLayout)
    provider.repository.getIcon match {
      case Some(icon) =>
        val im = new ResImage(icon.getImage)
        im.setToolTipText(provider.url.getOrElse("http://notavailable").toString)
        im.addMouseListener(new MouseAdapter {
          override def mouseClicked(e:MouseEvent) {
            provider.url match {
              case Some(url) => java.awt.Desktop.getDesktop.browse(url.toURI)
              case None =>
            }
          }
        })
        im.setBorder(BorderFactory.createLoweredSoftBevelBorder)
        rPanel.add(im)
      case _ =>
    }
    rPanel.add(new JLabel(s"<html>Provider name: <b>${provider.name}</b></html>"))
    rPanel.add(new JLabel(s"<html>Version: <b>${provider.version}</b></html>"))
    header.add("Center",rPanel)
    header.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,"Provider"))
    header
  }
  
  private class GamesDialog(menu:Option[JCheckBoxMenuItem],parent:JFrame,provider:GameProvider,player:GamePlayer) extends JDialog(parent,provider.name,false) 
                                                                                        with ListSelectionListener { 
    import concurrent.ExecutionContext.Implicits.global
    private var tableModel : GameTableModel = _
    private var scrollPane : JScrollPane = _
    private val loadButton : AbstractButton = button("Load game","Download game in cache",false) { loadGame }
    private val playButton : AbstractButton = button("Play!","Load & run the game",false) { playGame }
    private val attachDeviceButton : AbstractButton = button("Attach device","Attach device. In case of cartridge it run it as well",false) { attachGame }
    private var table : JTable = _
    private val downloadProgress = new JProgressBar
    private val downloadCount = new AtomicInteger(0)
    private val gameListModel = new DefaultListModel[ZIP.ArchiveEntry]
    private val gameList = new JList(gameListModel)
    private val gameIcon = new JLabel
    private var filterPanel : JPanel with DocumentListener = _
    
    init
    
    private def init {
      setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      addWindowListener(new WindowAdapter {
        override def windowClosed(e:WindowEvent) {
          menu match {
            case Some(menu) =>
              menu.setState(false)
            case None =>
          }          
        }
      })
      
      val content = getContentPane
      content.setLayout(new BorderLayout)
      // header
      content.add("North",createHeaderPanel(provider))
      val tablePanel = new JPanel(new BorderLayout)
      val games = provider.repository.load.toArray
      // table & filters
      table = createTable(provider,games)(this)
      tableModel = table.getModel.asInstanceOf[GameTableModel]
      filterPanel = createFilterPanel(table)
      tablePanel.add("North",new JScrollPane(filterPanel,ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED))
      scrollPane = new JScrollPane(table)
      tablePanel.add("Center",scrollPane)
      updateGamesCount
      // table buttons
      val tableButtons = new JPanel(new GridLayout(0,4))
      tableButtons.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,"Actions"))
      tablePanel.add("South",tableButtons)
      val syncButton = button("Sync","Synchronize with remote repository") { sync }
      tableButtons.add(syncButton)
      tableButtons.add(button("Clear cache","Clear the local cache of games") { clearCache })      
      tableButtons.add(loadButton)
      tableButtons.add(button("10 random","Select 10 random games",toggle=true) { random10 })
      val downloadPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
      downloadPanel.setBorder(BorderFactory.createRaisedSoftBevelBorder)
      downloadPanel.add(downloadProgress)
      // game panel
      val gamePanel = new JPanel(new BorderLayout)
      val gameIconPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
      gameIconPanel.add(gameIcon)
      gameIcon.setPreferredSize(provider.gameIconPreferredSize)
      gamePanel.add("North",gameIconPanel)
      gameIcon.setBorder(BorderFactory.createLineBorder(Color.black,2,true))
      val gameListPanel = new JPanel(new BorderLayout)
      gameListPanel.add("Center",new JScrollPane(gameList))
      gameList.addListSelectionListener(new ListSelectionListener {
        def valueChanged(e:ListSelectionEvent) {
          if (e.getValueIsAdjusting || gameList.getSelectedIndex == -1) {
            playButton.setEnabled(false)
            attachDeviceButton.setEnabled(false)
            return
          }
          playButton.setEnabled(true)
          attachDeviceButton.setEnabled(true)
        }
      })
      gameListPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,"Game contents"))
      gamePanel.add("Center",gameListPanel)
      // play buttons
      val playButtonPanel = new JPanel
      playButtonPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,"Play"))
      playButtonPanel.add(playButton)
      playButtonPanel.add(attachDeviceButton)
      gamePanel.add("South",playButtonPanel)
      val centerPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,tablePanel,gamePanel)
      content.add("South",downloadPanel)
      content.add("Center",centerPanel)
      
      // check empty repository
      if (tableModel.getRowCount == 0) swing {
        JOptionPane.showConfirmDialog(this,s"The repository seems to be empty. Do you want to synchronize now ?","Empty repository",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            sync(syncButton)
          case _ =>
        }
      }
      else
      if (provider.existsNewerVersion) swing {
        JOptionPane.showConfirmDialog(this,s"A new version exists: ${provider.version} (old one is ${provider.repository.currentVersion}). Do you want to synchronize now ?","Empty repository",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
          case JOptionPane.YES_OPTION =>
            sync(syncButton)
          case _ =>
        }
      }
    }
    
    private def button(label:String,toolTip:String,enabled:Boolean=true,toggle:Boolean=false)(ac: AbstractButton => Unit) : AbstractButton = {
      val b = if (toggle) new JToggleButton(label) else new JButton(label)
      b.setToolTipText(toolTip)
      b.setEnabled(enabled)
      b.addActionListener(new ActionListener { def actionPerformed(e:ActionEvent) = ac(b) })
      b
    }
    
    private def updateGamesCount {
      scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedSoftBevelBorder,s"Games [${tableModel.getRowCount}]"))
    }
    
    def valueChanged(e:ListSelectionEvent) {
      var row = table.getSelectedRow
      if (e.getValueIsAdjusting || row == -1) {
        loadButton.setEnabled(false)
        return
      }
      row = table.getRowSorter.convertRowIndexToModel(row)
      val game = tableModel.getGame(row)
      loadButton.setEnabled(!provider.repository.existsInCache(game))
      updateGameInfo(game)
    }
    
    private def updateGameInfo(game:Game) {
      import provider.repository._
      
      getIconFor(game) match {
        case Some(icon) =>
          gameIcon.setIcon(icon)
        case None =>
          gameIcon.setIcon(null)
      }
      gameListModel.clear
      if (existsInCache(game)) {
        for(e <- getArchiveItemsFor(game)) gameListModel.addElement(e)
      }
    }
    
    private def swing(action : => Unit) = SwingUtilities.invokeLater(new Runnable { def run = action })    
    
    // =================================================
    private class SyncDialog extends JDialog(GamesDialog.this,"Synchronizing ...",true) with GameLoadingProgressListener {
      private val progress = new JProgressBar
      
      progress.setStringPainted(true)
      val panel = new JPanel(new GridLayout(3,1)) {
        override def getInsets = new Insets(5,10,5,10)
      }
      
      getContentPane.setLayout(new BorderLayout)
      getContentPane.add("Center",panel)
      val msg = new JLabel("Downloading, please wait ...")
      panel.add(new JLabel(s"<html><b>Synchronizing ${provider.name}'s games</b></html>"))
      panel.add(progress)
      panel.add(msg)
      val buttonPanel = new JPanel(new FlowLayout)
      getContentPane.add("South",buttonPanel)
      val cancelButton = button("Cancel","Cancel synchronization") { cancelSync }
      cancelButton.setCursor(new Cursor(Cursor.DEFAULT_CURSOR))
      buttonPanel.add(cancelButton)
      pack
      setResizable(false)
      setLocationRelativeTo(GamesDialog.this)     
      setCursor(new Cursor(Cursor.WAIT_CURSOR))
      
      private def cancelSync(cancelButton:AbstractButton) {
        cancelButton.setEnabled(false)
        msg.setText("Canceling operation ...")
        provider.interrupt
      }
      
      def update(perc:Int) = swing {
        progress.setValue(perc)
      }
    }
    
    private def clearCache(syncButton:AbstractButton) {
      JOptionPane.showConfirmDialog(this,s"Are you sure you want to clear the cache ?","Cache",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE) match {
        case JOptionPane.YES_OPTION =>
          provider.repository.clearCache
          tableModel.fireTableDataChanged
        case _ =>
      }
    }
    
    private def sync(syncButton:AbstractButton) {
      JOptionPane.showConfirmDialog(this,s"Are you sure you want to delete the current ${provider.name}'s archive and download a new one ?","Synchronization",JOptionPane.YES_NO_OPTION,JOptionPane.WARNING_MESSAGE) match {
        case JOptionPane.YES_OPTION =>          
          val constraint : Option[SyncConstraint]= provider.syncConstraints match {
            case Nil => None
            case list =>
              val values = list map { e => e.asInstanceOf[Object] } toArray
              
              JOptionPane.showInputDialog(this,"Select how do you want to synchronize:","Synchronization filters",JOptionPane.INFORMATION_MESSAGE,null,values,values(0)) match {
                case null =>
                  return
                case c:SyncConstraint =>
                  Some(c)
              }
          }
          syncButton.setEnabled(false)
          provider.repository.clearCache
          val syncDialog = new SyncDialog
          provider.setProgressListener(syncDialog)
          setCursor(new Cursor(Cursor.WAIT_CURSOR))
          val games = provider.games(constraint)                
          games.onComplete { 
            case Success(games) =>
              endDownload
              syncDialog.dispose
              provider.repository.save(games)
              tableModel.fillWith(games.toArray)
              setCursor(new Cursor(Cursor.DEFAULT_CURSOR))
              JOptionPane.showMessageDialog(this,"Synchronization completed successfully!",s"Synchronization",JOptionPane.INFORMATION_MESSAGE)
              syncButton.setEnabled(true)
              updateGamesCount
            case Failure(t) =>
              endDownload
              syncDialog.dispose
              t.printStackTrace
              JOptionPane.showMessageDialog(this,t.toString,s"Error while synchronizing with ${provider.name}'s server",JOptionPane.ERROR_MESSAGE)
              setCursor(new Cursor(Cursor.DEFAULT_CURSOR))
              syncButton.setEnabled(true)            
          }
          beginDownload
          syncDialog.setVisible(true)
        case _ =>
      }
    }
    
    private def beginDownload {
      downloadCount.incrementAndGet
      downloadProgress.setIndeterminate(true)
    }
    private def endDownload {
      if (downloadCount.decrementAndGet == 0) downloadProgress.setIndeterminate(false)
    }
    
    private def loadGame(loadButton:AbstractButton) {
      val row = table.getRowSorter.convertRowIndexToModel(table.getSelectedRow)
      val game = tableModel.getGame(row)
      Future {
        beginDownload
        provider.repository.saveInCache(game) 
      } onComplete {
        case Failure(t) => swing {
          endDownload
          JOptionPane.showMessageDialog(this,t.toString,s"Error while loading game ${game.name}",JOptionPane.ERROR_MESSAGE)
        }          
        case Success(_) =>
          if (downloadCount.decrementAndGet == 0) downloadProgress.setIndeterminate(false)
          val row = table.getRowSorter.convertRowIndexToModel(table.getSelectedRow)
          val gameNow = tableModel.getGame(row)
          if (game == gameNow) updateGameInfo(game)
      }
    }
    
    private def playGame(playButton:AbstractButton) {
      val game = gameListModel.getElementAt(gameList.getSelectedIndex)
      provider.repository.extractEntry(game) match {
        case Some(file) =>
          player.play(file)
        case None =>
          JOptionPane.showMessageDialog(this,s"Cannot play game ${game.name}",s"Error",JOptionPane.ERROR_MESSAGE)
      }
    }
    private def attachGame(playButton:AbstractButton) {
      val game = gameListModel.getElementAt(gameList.getSelectedIndex)
      provider.repository.extractEntry(game) match {
        case Some(file) =>
          player.attachDevice(file)
        case None =>
          JOptionPane.showMessageDialog(this,s"Cannot attach game ${game.name}",s"Error",JOptionPane.ERROR_MESSAGE)
      }
    }
    private def random10(randomButton:AbstractButton) { 
      val button = randomButton.asInstanceOf[JToggleButton]
      if (button.isSelected) table.getRowSorter.asInstanceOf[DefaultRowSorter[GameTableModel,Int]].setRowFilter(createRandomTableFilter(10,tableModel.games.length))
      else filterPanel.changedUpdate(null)
    }
  }  
  
  def getUIFor(cb:JCheckBoxMenuItem,parent:JFrame,provider:GameProvider,player:GamePlayer) : JDialog = {
    providerCache get provider.name match {
      case Some(d) => d
      case None =>
        val dialog = new GamesDialog(Some(cb),parent,provider,player)
        providerCache += provider.name -> dialog
        dialog.pack
        dialog.setLocationByPlatform(true)
        dialog
    }
  }  
}