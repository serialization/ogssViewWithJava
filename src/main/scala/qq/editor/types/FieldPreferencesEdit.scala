package qq.editor.types

import ogss.common.java.api;
import qq.util.binding._;

/**
 * Swing Ui element for editing [[qq.editor.FieldPreferences]]
 */
class FieldPreferencesEdit(
  val file: qq.editor.File,
  val skillType: api.Access[_],
  val field: api.FieldDeclaration[_])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val fs = file.fieldPreferences(field)

  val hideEd = new CheckBoxEdit(fs.prefHide)
  val hideNullEd = new CheckBoxEdit(fs.prefHideNull)
  val showInParentEd = new CheckBoxEdit(fs.prefShowInParent)
  val fixDirEd = new CheckBoxEdit(fs.prefFixedEdgeDirection)
  val dirEd = new TextEdit(fs.prefEdgeDirection,qq.util.Vector.parse(_))

  /**
   * Enable and disable controls according to their value
   */
  private def endis: Unit = {
    hideNullEd.enabled = !fs.prefHide()
    showInParentEd.enabled = !fs.prefHide()
    fixDirEd.enabled = !fs.prefHide() && !fs.prefShowInParent()
  }
  /* variants of endis that take the right parameters (needs to be
   * referenced from this to stay alive long enough) */
  private val endisb = (_: Boolean) => endis

  fs.prefHide.onChange.weak += endisb
  fs.prefHideNull.onChange.weak += endisb
  fs.prefShowInParent.onChange.weak += endisb
  fs.prefFixedEdgeDirection.onChange.weak += endisb

  contents ++= Seq(
      hideEd,
      qq.util.Swing.HBoxD(
        swing.Swing.RigidBox(new scala.swing.Dimension(15,0)),
        qq.util.Swing.VBoxD(
        hideNullEd,
        showInParentEd,
        fixDirEd,
        dirEd
        )
      ))
   endis
}