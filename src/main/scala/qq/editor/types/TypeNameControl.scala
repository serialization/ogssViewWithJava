package qq.editor.types

import ogss.common.java.api;
import ogss.common.java.internal;
import qq.editor.objects.DefaultColors

/**
 * A control for displaying a type name on a page with user types turned into links and
 * a context menu for actions with this type.
 */
class TypeNameControl(val page: qq.editor.types.TypePage,
                      val τ: api.Access[_ <: internal.Obj]) extends swing.BoxPanel(swing.Orientation.Horizontal) {

  background = DefaultColors.text

  if (page != null) {
    contents += new qq.util.PlainButton(swing.Action(τ.name) {
      page.goTo(τ)
    }) {
      this.peer.setComponentPopupMenu(
        new swing.PopupMenu() {
          contents ++= Seq(
            new swing.MenuItem(swing.Action("Go to") { page.goTo(τ) }),
            new swing.MenuItem(swing.Action("Open in new tab") {
              qq.editor.Main.newTypeTab(τ)
            }),
            new swing.MenuItem(swing.Action("Show instances") {
              qq.editor.Main.newObjectTab(τ)
            }))
        }.peer)
    }
  } else {
    contents += new qq.util.PlainLabel(τ.name)
  }

}