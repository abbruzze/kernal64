package ucesoft.cbm.misc

import java.awt.BorderLayout
import javax.swing.{JPanel, JScrollPane, JTable}

class SettingsPanel(settings:Settings) extends JPanel {

  init

  private def init : Unit = {
    setLayout(new BorderLayout)
    val data : Array[Array[Object]] = settings.getSettings.sortBy(_.name) map { s => Array[Object](s"--${s.name}",s.description) } toArray
    val cols : Array[Object] = Array("Setting","Description")
    val table = new JTable(data,cols)
    val sp = new JScrollPane(table)
    add("Center",sp)
  }
}
