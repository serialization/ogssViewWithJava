package qq.editor.types

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.internal.FieldType;
import ogss.common.java.internal.fieldTypes;
import qq.editor.objects.DefaultColors

/**
 * A swing Ui element for displaying a field type on a page with user types turned into links (i.e.\
 * [[TypeNameControl]]s.
 */
class FieldTypeControl(
    val page: TypePage,
    val file: qq.editor.File,
    val fieldType: api.FieldType[_])
    extends swing.BoxPanel(swing.Orientation.Horizontal) {

  background = DefaultColors.text

  contents ++=
    (fieldType.asInstanceOf[FieldType[_]] match {
      case u: internal.Pool[_] ⇒ Seq(new TypeNameControl(page, u))
      case c: fieldTypes.ListType[_] ⇒ Seq(
        new qq.util.PlainLabel("list<"),
        new FieldTypeControl(page, file, c.base),
        new qq.util.PlainLabel(">"))
      case c: fieldTypes.SetType[_] ⇒ Seq(
        new qq.util.PlainLabel("set<"),
        new FieldTypeControl(page, file,c.base),
        new qq.util.PlainLabel(">"))
      case c: fieldTypes.ArrayType[_] ⇒ Seq(
        new FieldTypeControl(page, file,c.base),
        new qq.util.PlainLabel("[]"))
      case m: fieldTypes.MapType[_, _] ⇒ Seq(
        new qq.util.PlainLabel("map<"),
        new FieldTypeControl(page, file,m.keyType),
        new qq.util.PlainLabel(", "),
        new FieldTypeControl(page, file,m.valueType),
        new qq.util.PlainLabel(">"))
      case _ ⇒ Seq(new qq.util.PlainLabel(fieldType.toString()))
    })
    peer.setMaximumSize(peer.getMinimumSize)

}