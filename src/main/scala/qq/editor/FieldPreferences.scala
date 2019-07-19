package qq.editor

import ogss.common.java.api;
import java.util.prefs.Preferences;
import ogss.common.java.internal;
import ogss.common.java.internal.fieldTypes;
import scala.collection.mutable;
import qq.util.Vector;
import qq.util.binding._;

/** Persistently stored preferences for a field in a skill user type. 
 * */
class FieldPreferences[T, U <: internal.Obj](
    /** The field this is about */
    val field: api.FieldDeclaration[T],
    /** The type this belongs to */
    val containingType: TypePreferences[U]) extends PropertyOwner {

  private val prefs = Preferences.userRoot().node(s"/qq/skilledit/fieldsettings/${containingType.typ.name}/${field.name}");  
  
  /* defaults */
  private val (_hide, _inParent) = field.`type`.asInstanceOf[internal.FieldType[_]] match {
    case u: internal.Pool[T]                    ⇒ (false, false)
    case c: fieldTypes.SingleArgumentType[_, T]       ⇒ (false, false)
    case m: fieldTypes.MapType[_, _]                  ⇒ (false, false)
    case s: internal.StringPool                     ⇒ (false, false)
    case b: fieldTypes.BoolType                       ⇒ (false, true)
    case _: internal.AnyRefType                       ⇒ (false, false)
    case f32 : fieldTypes.F32                         ⇒ (false, true)
    case f64 : fieldTypes.F64                         ⇒ (false, true)
    case i8  : fieldTypes.I8                          ⇒ (false, true)
    case i16 : fieldTypes.I16                         ⇒ (false, true)
    case i32 : fieldTypes.I32                         ⇒ (false, true)
    case i64 : fieldTypes.I64                         ⇒ (false, true)
    case v64 : fieldTypes.V64                         ⇒ (false, true)
  }

  /** User preference: hide this field */
  var prefHide: Property[Boolean] = new Property(this, "Always hide", prefs.getBoolean("hide",  _hide))
  /** User preference: hide if zero/empty/null */
  var prefHideNull: Property[Boolean] = new Property(this, "Hide empty/null values", prefs.getBoolean("hideEmpty",  true))
  /** User preference: show inside parent node */
  var prefShowInParent: Property[Boolean] = new Property(this, "Show inside parent", prefs.getBoolean("showInParent",  _inParent))
  /** User preference: keep edge direction stable */
  var prefFixedEdgeDirection: Property[Boolean] = new Property(this, "Keep edge direction stable", prefs.getBoolean("stableDirection",  false))
  /** User preference: edge direction (is changed by programme unless prefFixedEdgeDirection)*/
  var prefEdgeDirection: Property[Vector] = new Property(this, "Edge direction", Vector.parse(prefs.get("idealDirection","(0,0)"))) {
    restrictions += Restriction(_.abs <= 1, "length must not exceed one")
  }

  prefHide.onChange.strong += (prefs.putBoolean("hide", _))
  prefHideNull.onChange.strong += (prefs.putBoolean("hideEmpty", _))
  prefShowInParent.onChange.strong += (prefs.putBoolean("showInParent", _))
  prefFixedEdgeDirection.onChange.strong += {x =>
    prefs.putBoolean("stableDirection", x)
    if (x) prefs.put("idealDirection", prefEdgeDirection().toString)}
  prefEdgeDirection.onChange.strong += (x => if (prefFixedEdgeDirection()) prefs.put("idealDirection", x.toString))
  
  
  /** true if the value of this field in object o is null, empty collection, zero, or empty string
   *  @todo If the field has a default restriction: hide if value==default, instead?*/
  private def hasNullValueIn(o: internal.Obj): Boolean = {
    field.`type`.asInstanceOf[internal.FieldType[_]] match {
      case u: internal.Pool[T]                   ⇒ field.get(o) == null
      case c: fieldTypes.SingleArgumentType[_, T] ⇒ field.get(o).asInstanceOf[Iterable[T]].size == 0
      case m: fieldTypes.MapType[_, _]                 ⇒ field.get(o).asInstanceOf[mutable.HashMap[_, _]].size == 0
      case s: internal.StringPool                    ⇒ field.get(o).asInstanceOf[String] == ""
      case b: fieldTypes.BoolType                         ⇒ !field.get(o).asInstanceOf[Boolean]
      case _: internal.AnyRefType                      ⇒ field.get(o) == null
      case f32 : fieldTypes.F32                              ⇒ field.get(o).asInstanceOf[Float] == 0.0f
      case f64 : fieldTypes.F64                              ⇒ field.get(o).asInstanceOf[Double] == 0.0
      case i8  : fieldTypes.I8                               ⇒ field.get(o).asInstanceOf[Byte] == 0
      case i16 : fieldTypes.I16                              ⇒ field.get(o).asInstanceOf[Short] == 0
      case i32 : fieldTypes.I32                              ⇒ field.get(o).asInstanceOf[Int] == 0
      case i64 : fieldTypes.I64                              ⇒ field.get(o).asInstanceOf[Long] == 0l
      case v64 : fieldTypes.V64                              ⇒ field.get(o).asInstanceOf[Long] == 0l
    }
  }

  /** returns whether and how this field should be shown in object o*/
  def visibilityIn(o: internal.Obj): FieldVisibility = {
    if (prefHide() /* user says hide */
      || (hasNullValueIn(o) && prefHideNull())) { /* or it;s null/empty…*/
      HideVisibility
    } else if (prefShowInParent()) {
      ShowInParentVisibility
    } else {
      ShowAsNodeVisibility
    }
  }
  
  
  // check whether it still exists after save
  var isDeleted = false
  def checkDeleted() : Unit = {
    def typeIsDeleted[T](τ: internal.FieldType[T]): Boolean = {
      τ match {
        case u: internal.Pool[_] => containingType.containingFile.typePreferences(u).isDeleted
        case c: fieldTypes.SingleArgumentType[_,_] => typeIsDeleted(c.base)
        case m: fieldTypes.MapType[k,v] => typeIsDeleted(m.keyType) || typeIsDeleted(m.valueType)
        case _ => false
      }
    }
    isDeleted = typeIsDeleted(field.`type`.asInstanceOf[internal.FieldType[_]])
  }

}