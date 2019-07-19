package qq.util

import qq.editor.objects.DefaultColors

/**
 * Plain label style for document content, just text
 */
class PlainLabel(title0: String) extends swing.Label(title0) {
  this.foreground = DefaultColors.textText
  this.background = DefaultColors.text
//  this.font = new java.awt.Font("SansSerif", java.awt.Font.PLAIN, this.font.getSize)
}