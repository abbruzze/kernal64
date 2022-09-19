package ucesoft.cbm.trace

import ucesoft.cbm.{C128Model, C64Model, CBMComponent, CBMComputerModel, CBMIIModel, VIC20Model}
import ucesoft.cbm.cpu.{Memory, RAMComponent}

import java.awt.event.MouseEvent
import java.awt.{BorderLayout, Component, FlowLayout}
import java.util.{EventObject, Properties}
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeCellEditor, DefaultTreeCellRenderer, DefaultTreeModel}

object InspectPanel {
  def getInspectDialog(f: JFrame, root: CBMComponent,model:CBMComputerModel) = new InspectPanelDialog(f,root,model)
}

class InspectPanelDialog(f: JFrame,root: CBMComponent,model:CBMComputerModel) extends JDialog(f, "Inspect panel") {
  private[this] var panel = new InspectPanel(root,model)
  
  override def setVisible(visible: Boolean) : Unit = {
    super.setVisible(visible)
    panel.enableUpdating(visible)
  }
  
  getContentPane.add("Center", panel)
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  pack()
  
  def updateRoot() : Unit = {
    getContentPane.remove(panel)
    panel = new InspectPanel(root,model)
    getContentPane.add("Center", panel)
    revalidate()
  }
}

private[trace] class InspectPanel(root: CBMComponent,model:CBMComputerModel) extends JPanel with Runnable with ChangeListener {
  private[this] val treeRoot = createTree(root)
  private[this] val lock = new Object
  private[this] val tree = new JTree(treeRoot)
  private[this] val spin = new JSpinner
  private[this] var panelVisible = false
  private[this] var sleepPeriod = 1000

  spin.setValue(sleepPeriod)
  spin.addChangeListener(this)
  setLayout(new BorderLayout)
  val logo = model match {
    case C64Model => "/resources/logo_bar_mini.png"
    case C128Model => "/resources/logo_bar_mini_128.png"
    case VIC20Model => "/resources/logo_bar_mini_vic20.png"
    case CBMIIModel => "/resources/logo_bar_mini_cbm2.png"
  }
  add("North", new JLabel(new ImageIcon(getClass.getResource(logo))))
  add("Center", new JScrollPane(tree))
  val southPanel = new JPanel(new FlowLayout)
  southPanel.add(new JLabel("Refresh period in millis:"))
  southPanel.add(spin)
  add("South", southPanel)
  tree.setCellRenderer(new Renderer)
  tree.setCellEditor(new Editor(tree, tree.getCellRenderer.asInstanceOf[DefaultTreeCellRenderer]))
  tree.setEditable(true)
  new Thread(this).start()
  
  private[this] class MemoryTreeNode(o:Object) extends DefaultMutableTreeNode(o) {
    override def setUserObject(o:Object) : Unit = {
      val addressValue = o.toString split "="
      val node = getUserObject.asInstanceOf[MemoryNode]
      node.address = Integer.parseInt(addressValue(0),16)
      if (addressValue.length > 1) node.mem.write(node.address,Integer.parseInt(addressValue(1),16))
    }
  }

  private[this] class Renderer extends DefaultTreeCellRenderer {
    private val memoryPanel = new MemoryPanel
    override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
      value.asInstanceOf[DefaultMutableTreeNode].getUserObject match {
        case mn: MemoryNode =>
          memoryPanel.setAddressValue(mn.address, mn.mem.read(mn.address))
          memoryPanel.setOpaque(sel)
          memoryPanel
        case _ =>
          super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
      }
    }
  }

  private[this] class Editor(tree: JTree, renderer: DefaultTreeCellRenderer) extends DefaultTreeCellEditor(tree, renderer) {
    override def isCellEditable(event: EventObject): Boolean = {
      if (event.isInstanceOf[MouseEvent] && event.asInstanceOf[MouseEvent].getClickCount == 2) {
        val me = event.asInstanceOf[MouseEvent]
        val treePath = tree.getPathForLocation(me.getX,me.getY)
        val selectedNode = treePath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        if (selectedNode != null)
          selectedNode.getUserObject match {
            case mn: MemoryNode => true
            case _ => false
          }
        else false
      } else false
    }
  }

  private[this] class MemoryPanel extends JPanel {
    private val addressLabel = new JLabel("0")
    private val valueLabel = new JLabel("0")
    add(new JLabel("Address:"))
    add(addressLabel)
    add(new JLabel("Value:"))
    add(valueLabel)
    setOpaque(false)

    def setAddressValue(address: Int, value: Int) : Unit = {
      addressLabel.setText(Integer.toHexString(address))
      valueLabel.setText(Integer.toHexString(value))
    }
  }

  private[this] class PropNode(var props: Properties, key: String) {
    override def toString: String = key + " = " + props.getProperty(key)
  }
  private[this] class ComponentNode(val node: CBMComponent) {
    override def toString: String = node.componentID
  }
  private[this] class MemoryNode(val mem: Memory, var address: Int) {
    override def toString = "Insert an hex address [=<new value>]"
  }

  def enableUpdating(enabled: Boolean) : Unit = {
    panelVisible = enabled
    if (enabled) lock.synchronized {
      lock.notify()
    }
  }

  def stateChanged(e: ChangeEvent) : Unit = {
    sleepPeriod = spin.getValue.asInstanceOf[Int]
  }

  def run() : Unit = {
    while (true) {
      if (!panelVisible) lock.synchronized {
        while (!panelVisible) lock.wait()
      }
      Thread.sleep(sleepPeriod)
      SwingUtilities.invokeLater(new Runnable {
        def run(): Unit = updateTree(treeRoot)
      })
    }
  }

  private def createTree(node: CBMComponent): DefaultMutableTreeNode = {
    val treeNode = new DefaultMutableTreeNode(new ComponentNode(node))
    treeNode.add(new DefaultMutableTreeNode(node.componentType))
    import scala.jdk.CollectionConverters._
    val properties = node.getProperties
    properties.asScala foreach { p => treeNode.add(new DefaultMutableTreeNode(new PropNode(properties, p._1))) }
    node match {
      case m: RAMComponent =>
        if (!m.isRom) treeNode.add(new MemoryTreeNode(new MemoryNode(m,m.startAddress)))
      case _ =>
    }
    node.components foreach { child => treeNode.add(createTree(child)) }
    treeNode
  }

  private def updateTree(node: DefaultMutableTreeNode) : Unit = {
    import scala.jdk.CollectionConverters._
    val props = node.getUserObject.asInstanceOf[ComponentNode].node.getProperties
    node.children.asScala foreach { c =>
      val child = c.asInstanceOf[DefaultMutableTreeNode]
      child.getUserObject match {
        case pNode: PropNode =>
          pNode.props = props
          tree.getModel.asInstanceOf[DefaultTreeModel].nodeChanged(child)
        case _: ComponentNode =>
          updateTree(child)
        case _ =>
      }
    }
  }
}