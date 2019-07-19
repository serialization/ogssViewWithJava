package qq.util

import qq.editor.objects.DefaultColors

/**
 * Plain button style for document content, looks like a hyper link
 */
class PlainButton() extends swing.Button() {
  def this(a: swing.Action) = {this; action = a}
  this.border = swing.Swing.EmptyBorder(0)
  this.contentAreaFilled = false
  this.foreground = DefaultColors.textHighlight
  this.background = DefaultColors.text
//  this.font = new java.awt.Font("SansSerif", java.awt.Font.PLAIN, this.font.getSize)
}
