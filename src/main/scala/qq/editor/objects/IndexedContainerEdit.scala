package qq.editor.objects

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.internal.fieldTypes.SingleArgumentType;
import scala.collection.mutable.Buffer;
import qq.util.Swing.HBoxD;
/** Swing Ui element for modifying the contents of a list or array field.
 * 
 * @param getNewElement closure that returns a value that is used for newly inserted elements
 * @param canResize whether entries can be added to or removed from the container */
class IndexedContainerEdit[E, C[E] <: Buffer[E], O <: internal.Obj](
  val page: qq.editor.Page,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[E]],
  val getNewElement: (() ⇒ E) = null,
  val canResize: Boolean = true)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  private var firstIndex = 0
  private val pageSize = qq.editor.Main.preferences.editCollectionPageSize()

  /** label showing field name */
  private val nameLbl = new swing.Label(field.name)

  /** label showing number of elements when collapsed*/
  private val countLbl = new swing.Label("-")
  /** label showing indices of visible elements when expanded */
  private val shownLbl = new swing.Label("-")
  /** next page action */
  private val pgDnAct = new swing.Action("Next Page") {
    icon = new qq.icons.ForwardIcon(true, true)
    override def apply() {
      val n = field.get(obj).size
      if (firstIndex + pageSize < n) {
        firstIndex += pageSize
        updateHeadValues
        refillLower
      }
    }
  }
  private val pgUpAct = new swing.Action("Previous Page") {
    icon = new qq.icons.BackIcon(true, true)
    override def apply() {
      val n = field.get(obj).size
      if (firstIndex > 0) {
        firstIndex -= pageSize min firstIndex
        updateHeadValues
        refillLower
      }
    }
  }

  private val pgUpBtn = new qq.util.PlainButton(pgUpAct) {
    text = ""
    this.disabledIcon = new qq.icons.BackIcon(false, true)
  }
  private val pgDnBtn = new qq.util.PlainButton(pgDnAct) {
    text = ""
    this.disabledIcon = new qq.icons.ForwardIcon(false, true)
  }

  /** update number of elements and current position in header */
  private def updateHeadValues(): Unit = {
    val n = field.get(obj).size
    countLbl.text = "" + n + " elements"
    shownLbl.text = "" + firstIndex + " to " + ((firstIndex + pageSize - 1) min (n - 1)) + " of " + n
    pgUpAct.enabled = firstIndex > 0
    pgDnAct.enabled = firstIndex + pageSize < n /* when length is variable there is also a last line for appending, but we will make the page too long when necessary instead of moving it to a separate page */
  }

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { e ⇒
    if (e.obj == obj) {
      e match {
        case e: qq.editor.IndexedContainerEdit[E, C[E], O] ⇒
          if (e.field == field) {
            e match {
              case e: qq.editor.IndexedContainerInsert[E, C[E], O] ⇒
                updateHeadValues
                if (e.index < firstIndex + pageSize) {
                  refillLower
                }
              case e: qq.editor.IndexedContainerRemove[E, C[E], O] ⇒
                updateHeadValues
                if (e.index < firstIndex + pageSize) {
                  refillLower
                }
              case _ ⇒ ()
            }
          }
        case _ ⇒ ()
      }
    }
  }

  page.file.onEdit.weak += fileEditHandler

  private val head = HBoxD()

  private def switchHeadStyle(expanded: Boolean) = {
    head.contents.clear()
    head.contents ++= Seq(nameLbl, swing.Swing.HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
    head.contents.foreach(_.tooltip = s"${field.`type`} ${field.name}")
  }
  private val en = new qq.util.ExpandableNode(head,false) {
    override def onCollapse() = { switchHeadStyle(false) }
    override def onExpand() = { switchHeadStyle(true) }
  }

  private val lowerPart = new swing.BoxPanel(swing.Orientation.Vertical)
  private def refillLower(): Unit = {
    lowerPart.contents.clear()
    lowerPart.contents ++= firstIndex.until((firstIndex + pageSize) min field.get(obj).size).map { i ⇒
      val fed = new ElementFieldEdit(
        page,
        field.`type`.asInstanceOf[SingleArgumentType[_, _]].base,
        new qq.editor.binding.IndexedContainerField(null, page.file, pool, obj, field, i))
      if (canResize) {
        val aa = new swing.Action("add") {
          icon = new qq.icons.AddListItemIcon(true)
          override def apply() {
            new qq.editor.UserIndexedContainerInsert(page.file, pool, obj, field, i, getNewElement())
          }
        }
        val ra = new swing.Action("remove") {
          icon = new qq.icons.RemoveListItemIcon(true)
          override def apply() {
            new qq.editor.UserIndexedContainerRemove[O, C[E], E](page.file, pool, obj, field, i)
          }
        }
        qq.util.Swing.HBoxD(0.0f,
          fed,
          new qq.util.PlainButton(ra) { text = "" },
          new qq.util.PlainButton(aa) { text = "" })
      } else {
        fed
      }
    }
    if (canResize && firstIndex + pageSize >= field.get(obj).size) {
      /* add a row for inserting at the end; if the fields end at a page break,
       * the last page will be too long (due to this append line), but that's,
       * I think, less bad then having the append-line on its own page */
      val aa = new swing.Action("add") {
        icon = new qq.icons.AddListItemIcon(true)
        override def apply() {
          new qq.editor.UserIndexedContainerInsert(page.file, pool, obj, field, field.get(obj).size, getNewElement())
        }
      }
      lowerPart.contents += qq.util.Swing.HBoxD(0.0f,
        new swing.Label(s"end of ${field.name}"),
        swing.Swing.HGlue,
        new qq.util.PlainButton(aa) { text = "" })
    }

  }
  en.subPart = lowerPart
  contents += en

  updateHeadValues
  refillLower
  if (field.get(obj).size > 0 && field.get(obj).size <= qq.editor.Main.preferences.editCollectionSmall()) {
    en.expand()
  } else {
    en.collapse()
  }

}