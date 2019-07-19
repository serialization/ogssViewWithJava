package qq.editor.binding

import ogss.common.java.api
import ogss.common.java.internal
import ogss.common.java.internal.FieldDeclaration
//import ogss.common.java.restrictions.CheckableFieldRestriction
//import ogss.common.java.internal.UserType
//import ogss.common.java.api.RestrictionCheckFailed
import qq.util.binding.Restriction
import scala.collection.JavaConverters._;

/** Make [[qq.util.binding.Restriction]]s for SKilL fields */
object Restrictions {
  /** internal representation of checkable field restrictions of field x */
  def apply[E, T](x: api.FieldDeclaration[T]): Iterator[Restriction[E]] = {
    x.asInstanceOf[FieldDeclaration[T, _]].restrictions.iterator.asScala.flatMap {
//      case cr: CheckableFieldRestriction[E] ⇒
//        Some(new Restriction[E]() {
//          override val conditionMessage = "unknown"
//          override val test: E ⇒ Boolean = validationMessage(_).isEmpty
//          override def validationMessage(x: E): Option[String] = {
//            try {
//              cr.check(x)
//              None
//            } catch {
//              case e: RestrictionCheckFailed ⇒ Some(e.msg)
//            }
//          }
//        })
      case _ ⇒ None
    }
  }
  /** restrict values to τ and sub-type for user types */
  def apply[T](file: qq.editor.File, τ: api.FieldType[T]): Iterator[Restriction[T]] = {
//    if (τ.isInstanceOf[UserType[_]]) {
//      Iterator(Restriction(
//        { (x: T) ⇒
//          x == null || {
//            val t = file.s.pool(x.asInstanceOf[internal.Obj])
//            t == τ || file.superTypes(t).contains(τ)
//          }
//        },
//        "object must have type " + τ + " or sub-type"))
//    } else { Iterator() }
    Iterator()
  }

}