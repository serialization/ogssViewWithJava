package qq.editor.objects

import ogss.common.java.api;
import ogss.common.java.internal
import ogss.common.java.internal.FieldType;
import ogss.common.java.internal.fieldTypes._;
import ogss.common.java.restrictions._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.PromptSkillFieldProperty
import qq.editor.Page
import swing.Action
import swing.Label
import swing.Button
import swing.Swing.HGlue
import swing.Swing.VGlue
import qq.util.Swing.VBoxD
import qq.util.Swing.HBoxD

/**
 * Functions to create a new value of a ground type; either default or ask the user
 */
object NewValue {
  /** default value of type `τ` considering defaults and minima from `restrictions`. */
  def default[T](τ: api.FieldType[T]): T = {//, restrictions: HashSet[FieldRestriction[_]]): T = {
    // default?
//    restrictions.foreach {
//      case r: DefaultRestriction[T] ⇒ return r.value;
//      case _                        ⇒ ()
//    }
//    // range?
//    restrictions.foreach {
//      case Range.RangeI8(min, max)  ⇒ if (0 < min) return min.asInstanceOf[T];
//      case Range.RangeI16(min, max) ⇒ if (0 < min) return min.asInstanceOf[T];
//      case Range.RangeI32(min, max) ⇒ if (0 < min) return min.asInstanceOf[T];
//      case Range.RangeI64(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
//      case Range.RangeF32(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
//      case Range.RangeF64(min, max) ⇒ if (0L < min) return min.asInstanceOf[T];
//      case _                        ⇒ ()
//    }

    τ.asInstanceOf[FieldType[T]] match {
      case _:BoolType  ⇒ false.asInstanceOf[T]
      case _:I8        ⇒ 0.toByte.asInstanceOf[T]
      case _:I16       ⇒ 0.toShort.asInstanceOf[T]
      case _:I32       ⇒ 0.asInstanceOf[T]
      case _:I64 | _:V64 ⇒ 0L.asInstanceOf[T]
      case _:F64       ⇒ 0.0.asInstanceOf[T]
      case _:F32       ⇒ 0.0f.asInstanceOf[T]
      case _: internal.AnyRefType | _: internal.Pool[_] ⇒
        null.asInstanceOf[T]
      case _: internal.StringPool ⇒ "".asInstanceOf[T]
//      case τ: ConstantLengthArray[e] ⇒
//        val x = new ArrayBuffer[e](τ.length)
//        for (_ ← 0 until τ.length) {
//          x += default(τ.groundType, restrictions)
//        }
//        x.asInstanceOf[T]
      case _: ListType[e] ⇒
        (new ListBuffer[e]()).asInstanceOf[T]
      case _: ArrayType[e] ⇒
        (new ArrayBuffer[e]()).asInstanceOf[T]
      case _: SetType[e] ⇒
        (new HashSet[e]()).asInstanceOf[T]
      case _: MapType[k, v] ⇒
        (new HashMap[k, v]()).asInstanceOf[T]
//      case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
//        | ConstantV64(_) ⇒
//        throw new Exception(s"constant field type $τ does not have a default value")
    }
  }

  /** page for the main tabbed pane that is used to prompt the user for primitive type values.
   *  
   *  references are prompted in an [[ObjectPage]]. */
  class PromptPage[T](file0: qq.editor.File,
                      preferences0: qq.editor.EditorPreferences,
                      val prompt: String,
                      val τ: api.FieldType[T],
                      //val restrictions: HashSet[FieldRestriction[_]],
                      val onSelect: T ⇒ Unit,
                      val onCancel: Unit ⇒ Unit)
      extends qq.editor.Page(file0, preferences0) {

    def viewMenuItems = Seq()
    def typeMenuItems = Seq()
    def objectMenuItems = Seq()

    val objSelectErrorLabel = new swing.Label("-") { foreground = java.awt.Color.red; visible = false }

    val p = new PromptSkillFieldProperty(null, prompt, default(τ), τ)//, restrictions), τ)
    val ed = new ElementFieldEdit(this, τ.asInstanceOf[FieldType[T]], p)

    val accept = Action("Ok") {
      try {
        ed.editField.componentToProperty()
        onSelect(p())
        tabbedPane.removePage(index)
      } catch {
        case e: Exception ⇒
          objSelectErrorLabel.text = qq.util.binding.ValidationExceptionMessage(e)
          objSelectErrorLabel.visible = true
      }
    }
    val cancel = Action("Cancel") {
      try {
        onCancel(())
        tabbedPane.removePage(index)
      } catch {
        case e: Exception ⇒
          objSelectErrorLabel.text = qq.util.binding.ValidationExceptionMessage(e)
          objSelectErrorLabel.visible = true
      }
    }

    title = "New Value"
    content = VBoxD(
      HBoxD(HGlue, new Label(s"<html><h1>$title</h1></html>"), HGlue),
      VGlue,
      HBoxD(HGlue, ed, HGlue),
      VGlue,
      HBoxD(HGlue, objSelectErrorLabel, HGlue, new Button(accept), new Button(cancel)))
  }

  /** Ask the user to enter or select a value of type `τ`.
   *  
   *  @param prompt text for the user
   *  @param restrictions field restrictions that the value has to fulfil
   *  @param onSelect closure that uses the selected value
   *  @param onCancel closure called when the user cancels the selection*/
  def promptInPage[T](τ: api.FieldType[T],
                      prompt: String,
                      page: Page,
                      //restrictions: HashSet[FieldRestriction[_]],
                      onSelect: T ⇒ Unit,
                      onCancel: Unit ⇒ Unit): Unit = {

    τ.asInstanceOf[FieldType[T]] match {
      case _: internal.AnyRefType | _: internal.Pool[_] ⇒
        val sel = τ match {
          case u: api.Access[_] ⇒ qq.editor.Main.newObjectTab(u)
          case _                ⇒ qq.editor.Main.newObjectTab()
        }
        sel.select(prompt, onSelect, _ ⇒ onCancel(()))
      case _ ⇒
        val sel = new PromptPage(page.file, page.preferences, prompt, τ, onSelect, onCancel) //restrictions,
        page.tabbedPane.addPage(sel)
        sel.show()
    }

  }

}