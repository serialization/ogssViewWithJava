package qq.util.binding

import qq.util.Swing.HBoxD
import qq.util.Swing.VBoxD
import swing.Swing.HGlue
import swing.Swing.RigidBox
import javax.swing.JPanel
import swing.Label

/*
 * Puts a label above an unlabled edit control.
 */
class LabeledEdit[T](val inner: EditControl[T])
    extends swing.Component with swing.Container.Wrapper with swing.SequentialContainer.Wrapper {

  private def leftColumnWidth = 25
  private def maxLeftColumnLabelWidth = leftColumnWidth - 2

  override lazy val peer = new JPanel with SuperMixin

  val label = new Label(inner.property.name) { tooltip = inner.property.description }

  private val orientation =
    if (label.preferredSize.getWidth < maxLeftColumnLabelWidth)
      swing.Orientation.Horizontal
    else
      swing.Orientation.Vertical

  peer.setLayout(new javax.swing.BoxLayout(peer, orientation.id))

  if (orientation == swing.Orientation.Vertical) {
    contents += HBoxD(
      label,
      HGlue)
    contents += inner

  } else {
    contents += VBoxD(
      HBoxD(
        label,
        HGlue),
      RigidBox(new java.awt.Dimension(leftColumnWidth, 0)))
    contents += inner
  }

  override def enabled: Boolean = inner.enabled
  override def enabled_=(x: Boolean): Unit = inner.enabled_=(x)
}