package ucesoft.cbm.trace

import javax.swing._
import ucesoft.cbm.CBMComponent
import javax.swing.tree.DefaultMutableTreeNode
import java.awt.BorderLayout
import java.util.Properties
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.FlowLayout
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import ucesoft.cbm.cpu.Memory
import ucesoft.cbm.cpu.RAMComponent
import javax.swing.tree.DefaultTreeCellEditor
import java.util.EventObject
import java.awt.event.MouseEvent

object InspectPanel {
  def getInspectDialog(f: JFrame, root: CBMComponent) = {
    val panel = new InspectPanel(root)
    val dialog = new JDialog(f, "Inspect panel") {
      override def setVisible(visible: Boolean) {
        super.setVisible(visible)
        panel.enableUpdating(visible)
      }
    }
    dialog.getContentPane.add("Center", panel)
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.pack
    dialog
  }
}

class InspectPanel(root: CBMComponent) extends JPanel with Runnable with ChangeListener {
  private[this] val treeRoot = createTree(root)
  private[this] val lock = new Object
  private[this] val tree = new JTree(treeRoot)
  private[this] val spin = new JSpinner
  private[this] var visible = false
  private[this] var sleepPeriod = 1000

  spin.setValue(sleepPeriod)
  spin.addChangeListener(this)
  setLayout(new BorderLayout)
  add("North", new JLabel(new ImageIcon(getClass.getResource("/resources/logo_bar_mini.png"))))
  add("Center", new JScrollPane(tree))
  val southPanel = new JPanel(new FlowLayout)
  southPanel.add(new JLabel("Refresh period in millis:"))
  southPanel.add(spin)
  add("South", southPanel)
  tree.setCellRenderer(new Renderer)
  tree.setCellEditor(new Editor(tree, tree.getCellRenderer.asInstanceOf[DefaultTreeCellRenderer]))
  tree.setEditable(true)
  new Thread(this).start
  
  private[this] class MemoryTreeNode(o:Object) extends DefaultMutableTreeNode(o) {
    override def setUserObject(o:Object) {
      val addressValue = o.toString split "="
      val node = getUserObject.asInstanceOf[MemoryNode]
      node.address = Integer.parseInt(addressValue(0),16)
      if (addressValue.length > 1) node.mem.write(node.address,Integer.parseInt(addressValue(1),16))
    }
  }

  private[this] class Renderer extends DefaultTreeCellRenderer {
    private val memoryPanel = new MemoryPanel
    override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean) = {
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
    override def isCellEditable(event: EventObject) = {
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

    def setAddressValue(address: Int, value: Int) {
      addressLabel.setText(Integer.toHexString(address))
      valueLabel.setText(Integer.toHexString(value))
    }
  }

  private[this] class PropNode(var props: Properties, key: String) {
    override def toString = key + " = " + props.getProperty(key)
  }
  private[this] class ComponentNode(val node: CBMComponent) {
    override def toString = node.componentID
  }
  private[this] class MemoryNode(val mem: Memory, var address: Int) {
    override def toString = "Insert an hex address [=<new value>]"
  }

  private def enableUpdating(enabled: Boolean) {
    visible = enabled
    if (enabled) lock.synchronized {
      lock.notify
    }
  }

  def stateChanged(e: ChangeEvent) {
    sleepPeriod = spin.getValue.asInstanceOf[Int]
  }

  def run {
    while (true) {
      if (!visible) lock.synchronized {
        while (!visible) lock.wait
      }
      Thread.sleep(sleepPeriod)
      SwingUtilities.invokeLater(new Runnable {
        def run = updateTree(treeRoot)
      })
    }
  }

  private def createTree(node: CBMComponent): DefaultMutableTreeNode = {
    val treeNode = new DefaultMutableTreeNode(new ComponentNode(node))
    treeNode.add(new DefaultMutableTreeNode(node.componentType))
    import collection.JavaConversions._
    val properties = node.getProperties
    properties foreach { p => treeNode.add(new DefaultMutableTreeNode(new PropNode(properties, p._1))) }
    node match {
      case m: RAMComponent =>
        if (!m.isRom) treeNode.add(new MemoryTreeNode(new MemoryNode(m,m.startAddress)))
      case _ =>
    }
    node.components foreach { child => treeNode.add(createTree(child)) }
    treeNode
  }

  private def updateTree(node: DefaultMutableTreeNode) {
    import collection.JavaConversions._
    val props = node.getUserObject.asInstanceOf[ComponentNode].node.getProperties
    node.children foreach { c =>
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