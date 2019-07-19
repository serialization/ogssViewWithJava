package qq.editor.objects

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.internal.FieldType;
import ogss.common.java.internal.fieldTypes._;
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.SkillFieldProperty

import scala.collection.JavaConverters._;

/**
 * swing UI element for modifying a field of the SKilL file.
 * 
 * @param The [[qq.editor.Page]] in which this UI element is shown 
 * @param pool a user type
 * @param obj an object in `pool`
 * @param field a field of `obj`
 * 
 * TODO Should be possible to reuse [[ElementFieldEdit]] for ground types
 * */

class FieldEdit[F, O <: internal.Obj](
  val page: qq.editor.Page,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val restrictions = field.asInstanceOf[internal.FieldDeclaration[F,O]].restrictions.asScala
  field.`type`.asInstanceOf[FieldType[_]] match {
    case _: internal.AnyRefType
      | _: internal.Pool[_] ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      contents += new ReferenceEdit(p.asInstanceOf[SkillFieldProperty[internal.Obj]], page)
    case c: ListType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.base))//, restrictions))
    case c: ArrayType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.base))//, restrictions))
    case c: SetType[e] ⇒ 
      contents += new SetContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[HashSet[e]]])       
    case m: MapType[k, v] ⇒
            contents += new MapContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[HashMap[k,v]]])
    case _:BoolType | _:internal.StringPool | _:I8 | _:I16 | _:I32 | _:I64 | _:V64 | _:F32 | _:F64 ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val editField = if ( field.`type`.asInstanceOf[FieldType[_]].isInstanceOf[internal.StringPool]) 
        new qq.util.binding.TextEdit(p.asInstanceOf[SkillFieldProperty[String]],
        {x ⇒ val s = x.trim()
            if (s == "(null)") null 
            else if (s.head == '"' && s.last == '"')
              scala.StringContext.treatEscapes(s.tail.dropRight(1))
            else throw new qq.util.binding.RestrictionException("expected string in double quotation marks")
        },
        {(x:String)  ⇒ if (x == null) "(null)"
           else ""+scala.reflect.runtime.universe.Literal(scala.reflect.runtime.universe.Constant(x))
        })
      else p.defaultEditor
      val ed = new qq.util.binding.LabeledEdit(editField)
      contents += new qq.util.ExpandableNode(ed, false)
    
  }

}