package qq.util

import qq.editor.objects.DefaultColors

/**
 * Functions I miss in swing.Swing :)
 *
 *  Well, originally only HBox and VBox, but then the white background was added in a rush…
 */
object Swing {
  def HBox(textStyle: Boolean, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Horizontal) {
    if (textStyle) background = DefaultColors.text
    contents ++= xs
  }
  def HBox(textStyle: Boolean, vAlignment: Float, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Horizontal) {
    if (textStyle) background = DefaultColors.text
    xs.foreach { _.peer.setAlignmentY(vAlignment) }
    contents ++= xs
  }
  def HBoxT(xs: swing.Component*) = HBox(true, xs: _*)
  def HBoxD(xs: swing.Component*) = HBox(false, xs: _*)
  def HBoxT(vAlignment: Float, xs: swing.Component*) = HBox(true, vAlignment, xs: _*)
  def HBoxD(vAlignment: Float, xs: swing.Component*) = HBox(false, vAlignment, xs: _*)
  def VBox(textStyle: Boolean, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Vertical) {
    if (textStyle) background = DefaultColors.text
    contents ++= xs
  }
  def VBox(textStyle: Boolean, hAlignment: Float, xs: swing.Component*): swing.BoxPanel = new swing.BoxPanel(swing.Orientation.Vertical) {
    if (textStyle) background = DefaultColors.text
    xs.foreach { _.peer.setAlignmentX(hAlignment) }
    contents ++= xs
  }
  def VBoxT(xs: swing.Component*) = VBox(true, xs: _*)
  def VBoxD(xs: swing.Component*) = VBox(false, xs: _*)
  def VBoxT(hAlignment: Float, xs: swing.Component*) = VBox(true, hAlignment, xs: _*)
  def VBoxD(hAlignment: Float, xs: swing.Component*) = VBox(false, hAlignment, xs: _*)
}
