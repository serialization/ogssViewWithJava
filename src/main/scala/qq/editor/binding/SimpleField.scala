package qq.editor.binding

import ogss.common.java.api;
import ogss.common.java.internal;

/** Property for a ground type field for use by edit components.
 *  Generates undoable UserEdit to update the file and monitors Edits to update its own state */
class SimpleField[O <: internal.Obj, F](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends SkillFieldProperty[F](owner0, field.name, field.get(obj)) {

  def groundType = field.`type`

  description = s"${field.`type`} ${field.name} in ${file.idOfObj(obj)}"

  restrictions ++= Restrictions(field)
  restrictions ++= Restrictions(file, field.`type`)

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { x ⇒
    if (x.obj == obj && x.isInstanceOf[qq.editor.SimpleFieldEdit[_, _]]) {
      val y = x.asInstanceOf[qq.editor.SimpleFieldEdit[O, F]]
      if (y.field == field) {
        this.assignUnchecked(y.newValue)
      }
    }
  }
  file.onEdit.weak += fileEditHandler

  private def selfChangeHandler(x: F): Unit = {
    new qq.editor.UserSimpleFieldEdit(file, pool, obj, field, x)
  }
  onChange.strong += selfChangeHandler

}