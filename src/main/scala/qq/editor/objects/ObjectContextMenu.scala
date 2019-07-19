package qq.editor.objects

import ogss.common.java.internal

/** Popup menu for a object in a editor page. Is used by non-null references.
 *  
 *  TODO (also) providing a list of MenuItems would make ReferenceEdit nicer. */
object ObjectContextMenu {
  def apply(o: internal.Obj, page: qq.editor.Page): swing.PopupMenu = {
    new swing.PopupMenu() {
      page match {
        case page: ObjectPage ⇒
          contents += new swing.MenuItem(swing.Action("Go to") {
            page.goTo(o)
          })
        case _ ⇒ ()

      }
      contents ++= Seq(
        new swing.MenuItem(swing.Action("Open in new page") {
          qq.editor.Main.newObjectTab(o)
        }),
        new swing.MenuItem(swing.Action("Show/hide fields") {
          val τ = page.file.s.pool(o)
          val frame = new swing.Frame() {
            title = s"Field preferences of ${τ.name}"
            contents = new qq.editor.types.TypeEdit(null, page.file, τ)
          }
          frame.visible = true
        }))
    }
  }
}