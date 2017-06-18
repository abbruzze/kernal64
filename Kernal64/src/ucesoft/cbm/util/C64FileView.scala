package ucesoft.cbm.util

import javax.swing.filechooser.FileView
import java.io.File
import javax.swing.ImageIcon

class C64FileView extends FileView {
  private val icon = new ImageIcon(getClass.getResource("/resources/commodore_file.png"))
  
  override def getTypeDescription(f:File) = {
    if (f.getName.toUpperCase.endsWith(".D64") || f.getName.toUpperCase.endsWith(".D71")) "Commodore 64 disk image"
    else
    if (f.getName.toUpperCase.endsWith(".G64")) "Commodore 64 GCR disk image"
    else
    if (f.getName.toUpperCase.endsWith(".T64")) "Commodore 64 tape image"
    else
    if (f.getName.toUpperCase.endsWith(".TAP")) "Commodore 64 raw tape image"
    else
    if (f.getName.toUpperCase.endsWith(".PRG")) "Commodore 64 program file"
    else
    if (f.getName.toUpperCase.endsWith(".CRT")) "Commodore 64 cartridge file"
    else
    if (f.getName.toUpperCase.endsWith(".REU")) "Commodore 64 ram expansion unit file"
    else null
  }
  override def getIcon(f:File) = {
    val name = f.getName.toUpperCase
    if (name.endsWith(".D64") || 
        name.endsWith(".D71") ||
        name.endsWith(".G64") ||
        name.endsWith(".T64") || 
        name.endsWith(".PRG") || 
        name.endsWith(".CRT") ||
        name.endsWith(".REU") ||
        name.endsWith(".TAP")) icon
    else null
  }
}