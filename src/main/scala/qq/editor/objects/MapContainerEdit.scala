package qq.editor.objects

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.internal.fieldTypes.MapType
import ogss.common.java.internal.FieldType
import scala.collection.mutable.HashMap;
import qq.util.Swing.HBoxD
import qq.util.Swing.VBoxD
import swing.Swing.HGlue
import qq.util.FlattenedMap

/** Swing Ui element for modifying the contents of a map field. */
class MapContainerEdit[K, V, C[K, V] <: HashMap[K, V], O <: internal.Obj](
  val page: qq.editor.Page,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[K, V]])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val skillType = field.`type`.asInstanceOf[MapType[K, V]]
  val groundTypes = FlattenedMap.typeList(skillType)
  val elType = groundTypes.last

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
      val n = FlattenedMap.size(field.get(obj), skillType)
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
      val n = FlattenedMap.size(field.get(obj), skillType)
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
    val n = FlattenedMap.size(field.get(obj), skillType)
    countLbl.text = "" + n + " elements"
    shownLbl.text = "" + firstIndex + " to " + ((firstIndex + pageSize - 1) min (n - 1)) + " of " + n
    pgUpAct.enabled = firstIndex > 0
    pgDnAct.enabled = firstIndex + pageSize < n /* when length is variable there is also a last line for appending, but we will make the page too long when necessary instead of moving it to a separate page */
  }

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { e ⇒
    if (e.obj == obj) {
            e match {
        case e: qq.editor.MapEdit[C[K, V], O] ⇒
          if (e.field == field) {
            e match {
              case _: qq.editor.MapInsert[C[K, V], O] | _: qq.editor.MapRemove[C[K, V], O]⇒
                updateHeadValues
                refillLower
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
    head.contents ++= Seq(nameLbl, HGlue)
    if (expanded) {
      head.contents ++= Seq(pgUpBtn, shownLbl, pgDnBtn)
    } else {
      head.contents += countLbl
    }
    head.contents.foreach(_.tooltip = s"${field.`type`} ${field.name}")
  }
  private val en = new qq.util.ExpandableNode(head, false) {
    override def onCollapse() = { switchHeadStyle(false) }
    override def onExpand() = { switchHeadStyle(true) }
  }

  private val lowerPart = new swing.BoxPanel(swing.Orientation.Vertical)
  private def refillLower(): Unit = {
    lowerPart.contents.clear()
    lowerPart.contents ++= FlattenedMap.keys(field.get(obj), skillType).toSeq.sortBy(x ⇒ if (x == null) "" else x.toString).drop(firstIndex).take(pageSize).map { key ⇒
      val keysbox = VBoxD()
      for ((k, t) ← key.zip(groundTypes)) {
        keysbox.contents += qq.util.Swing.HBoxD(new GroundValueLabel(page, t, k), swing.Swing.HGlue)
      }
      val fprop = qq.editor.binding.MapContainerField(null, page.file, pool, obj, field, key, elType)
      val valed = new ElementFieldEdit(
        page,
        elType,
        fprop,
        false)
      val ra = new swing.Action("remove") {
        icon = new qq.icons.RemoveListItemIcon(true)
        override def apply() {
          new qq.editor.UserMapRemove[O, K, V, C](page.file, pool, obj, field, key)
        }
      }
      HBoxD(0.0f,
        VBoxD(0.0f,
          HBoxD(0.0f, //new swing.Label("⋅"),
            keysbox),
          HBoxD(0.0f, //new swing.Label("↦"),
            valed)),
        new qq.util.PlainButton(ra) { text = "" })
    }
    /* add a row for inserting at the end; if the fields end at a page break,
       * the last page will be too long (due to this append line), but that's,
       * I think, less bad then having the append-line on its own page */
    val aa = new swing.Action("add") {
      icon = new qq.icons.AddListItemIcon(true)
      override def apply() {
        def selectKeys(todo: Seq[api.FieldType[_]], done: Seq[Any], onOk: Seq[Any] => Unit, onCancel: Unit => Unit): Unit = {
           if (todo.size == 0) {
             onOk(done)
           } else {
             NewValue.promptInPage[Any](todo.head.asInstanceOf[FieldType[Any]], s"Key ${done.size + 1} for new entry in $field",
                 page, //scala.collection.mutable.HashSet(),
                 { case x: Any => selectKeys(todo.tail, done :+ x, onOk, onCancel)},
                 onCancel)
           }
        }
        selectKeys(groundTypes.dropRight(1), Seq(), 
          keys => {
            new qq.editor.UserMapInsert(page.file, pool, obj, field, keys, NewValue.default(groundTypes.last))//, scala.collection.mutable.HashSet()))
            page.tabbedPane.addPage(page)
          },
          _ => page.tabbedPane.addPage(page)
        )
        page.tabbedPane.removePage(page.index)
          
      }
    }
    lowerPart.contents += qq.util.Swing.HBoxD(0.0f,
      new swing.Label(if (firstIndex + pageSize >= FlattenedMap.size(field.get(obj), skillType)) s"end of ${field.name}" else ""),
      swing.Swing.HGlue,
      new qq.util.PlainButton(aa) { text = "" })

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

