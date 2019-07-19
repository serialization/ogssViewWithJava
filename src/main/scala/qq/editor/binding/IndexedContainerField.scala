package qq.editor.binding

import ogss.common.java.api;
import ogss.common.java.internal.Obj;
import ogss.common.java.internal.fieldTypes.SingleArgumentType;
import ogss.common.java.internal.FieldType;
import scala.collection.mutable.Buffer;

/** Property for an element of a list &c. field for use by edit components.
 *  Generates undoable UserEdit to update the file and monitors Edits to update its own state */
class IndexedContainerField[O <: Obj, C[F] <: Buffer[F], F](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[F]],
  val index: Int)
    extends SkillFieldProperty[F](owner0, index.toString(), field.get(obj)(index)) {

  description = s"${groundType} ${field.name}($index) in ${file.idOfObj(obj)}"  
  
  def groundType = field.`type`.asInstanceOf[SingleArgumentType[_,F]].base

  restrictions ++= Restrictions(field)
  restrictions ++= Restrictions(file, field.`type`.asInstanceOf[SingleArgumentType[_,_]].base.asInstanceOf[FieldType[F]]) 

  /**
   * when obj.get(field)(index) is the last element and is removed, this object
   *  disables itself so that it can do no harm. Its owner will remove it.
   */
  private var disabled = false

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { x ⇒
    if (!disabled && x.obj == obj && x.isInstanceOf[qq.editor.IndexedContainerEdit[_, _, _]]) {
      val y = x.asInstanceOf[qq.editor.IndexedContainerEdit[O, C[F], F]]
      if (y.field == field) {
        y match {
          case ins: qq.editor.IndexedContainerInsert[O, C[F], F] ⇒
            if (ins.index == index) {
              this.assignUnchecked(ins.value)
            } else if (ins.index < index) {
              this.assignUnchecked(field.get(obj)(index))
            }
          case del: qq.editor.IndexedContainerRemove[O, C[F], F] ⇒
            if (del.index <= index) {
              if (index >= field.get(obj).size) {
                disabled = true
              } else {
                this.assignUnchecked(field.get(obj)(index))
              }
            }
          case mod: qq.editor.IndexedContainerModify[O, C[F], F] ⇒
            if (mod.index == index) {
              this.assignUnchecked(mod.newValue)
            }
        }
      }
    }
  }
  file.onEdit.weak += fileEditHandler

  private def selfChangeHandler(x: F): Unit = {
    new qq.editor.UserIndexedContainerModify(file, pool, obj, field, index, x)
  }
  onChange.strong += selfChangeHandler

}