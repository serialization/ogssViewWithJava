package qq.editor.types

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.restrictions;
import qq.util.Swing.VBoxT
import qq.util.Swing.HBoxT
import swing.Swing.HGlue
import swing.Swing.RigidBox
import scala.swing.Dimension
import qq.editor.objects.DefaultColors

import scala.collection.JavaConverters._

/** show the fields of one type and edit their preferences. If page != null make hyperlinks to other types*/
class TypeEdit(val page: qq.editor.types.TypePage,
               val file: qq.editor.File,
               val skillType: api.Access[_ <: internal.Obj]) extends qq.util.VScrollPane {

  background = DefaultColors.text

  /** displays one field. the field preferences can be shown and hidden */
  class Field(val page: qq.editor.types.TypePage,
              val file: qq.editor.File,
              val skillType: api.Access[_ <: internal.Obj],
              val field: api.FieldDeclaration[_])
      extends qq.util.ExpandableNode({
        /* constants need a = value part */
        /* constanten wurden abgeschafft!
        val constantValuePart = field.t match {
          case internal.fieldTypes.ConstantI8(i)  ⇒ " = " + i
          case internal.fieldTypes.ConstantI16(i) ⇒ " = " + i
          case internal.fieldTypes.ConstantI32(i) ⇒ " = " + i
          case internal.fieldTypes.ConstantI64(i) ⇒ " = " + i
          case _                                  ⇒ "" /* non-constants don't */
        }*/
        /* main part is type and name; FieldTypeControl makes user types in the type clickable */
        val ftc = new FieldTypeControl(page, file, field.`type`)
        val ftcw = ftc.preferredSize.width
        val colWidth = qq.editor.Main.preferences.typeColumnWidth()
        var typeAndName = if (ftcw > colWidth) {
          VBoxT(
            HBoxT(ftc, HGlue),
            HBoxT(RigidBox(new Dimension(colWidth, 0)),
              new qq.util.PlainLabel(" " + field.name),// + constantValuePart),
              HGlue))
        } else {
          HBoxT(
            ftc,
            RigidBox(new Dimension(colWidth - ftcw, 0)),
            new qq.util.PlainLabel(" " + field.name),// + constantValuePart),
            HGlue)
        }
        /* when there are field restrictions, make a vbox with restrictions followed by type and name */
        var f2 = field.asInstanceOf[internal.FieldDeclaration[_, _]]
        if (f2.restrictions.size == 0) {
          typeAndName
        } else {
          new swing.BoxPanel(swing.Orientation.Vertical) {
            background = DefaultColors.text
            contents ++= f2.restrictions.asScala.map { x ⇒
              HBoxT(
                new qq.util.PlainLabel(
                  /* non-null has no nice toString */
                  if (x.isInstanceOf[restrictions.NonNull[_]]) {
                    "@NonNull"
                  } else {
                    "@" + x.toString
                  }),
                HGlue)
            }
            contents += typeAndName
          }
        }
      },
        new FieldPreferencesEdit(file, skillType, field) {
        border = swing.Swing.BeveledBorder(swing.Swing.Raised)
      }, true) {
  }

  private val inner = new swing.BoxPanel(swing.Orientation.Vertical) {
    background = DefaultColors.text
    def addFieldsOfType(τ: api.Access[_ <: internal.Obj]): Unit = {
      if (file.parentType.contains(τ)) {
        addFieldsOfType(file.parentType(τ))
      }
      contents += new qq.util.ExpandableNode(
        HBoxT(
          new qq.util.PlainLabel("" + τ.fields.asScala.length + " fields from "),
          new TypeNameControl(page, τ),
          HGlue), true) {

        if (τ.fields.asScala.length > 0) {
          subPart = new swing.BoxPanel(swing.Orientation.Vertical) {
            background = DefaultColors.text
            contents ++= τ.fields.asScala.
                  filter(!file.fieldPreferences(_).isDeleted).
                  map(x ⇒ new Field(page, file, skillType, x))
          }
          if (τ == skillType) expand
        }
      }
    }
    //for (
    //  r ← …type restictions…
    //) {
    //  contents += HBoxT(
    //   new qq.util.PlainLabel("@" + r.toString()),
    //    HGlue)
    //}
    file.parentType.get(skillType) match {
      case Some(parentType) ⇒
        contents += HBoxT(
          new TypeNameControl(page, skillType),
          new qq.util.PlainLabel(" : "),
          new TypeNameControl(page, parentType),
          new qq.util.PlainLabel(" {"),
          HGlue)
      case None ⇒
        contents += HBoxT(
          new TypeNameControl(page, skillType),
          new qq.util.PlainLabel(" {"),
          HGlue)
    }
    addFieldsOfType(skillType)
    contents += HBoxT(
      new qq.util.PlainLabel("}"),
      HGlue)

  }

  contents = inner
}