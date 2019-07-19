package qq.graph

import qq.util.HtmlEscape
import ogss.common.java.api
import ogss.common.java.internal
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import swing.Swing.LineBorder
import swing.Swing.CompoundBorder
import swing.Swing.EmptyBorder
import qq.editor.objects.DefaultColors

/** common Ui elements for the graph nodes*/
object UIElements {

  def valueShortString[T](v: T) = {
    val asString = if (v == null) "⊥" else v.toString()
    val n = qq.editor.Main.preferences.graphMaxStringLength()
    if (asString.length() > n) {
      asString.take((n - 1) / 2) + "…" + asString.takeRight(n / 2)
    } else {
      asString
    }
  }
  def value[T](g: Graph, n: AbstractNode, v: T) = {
    val asString = if (v == null) "⊥" else v.toString()
    new qq.util.PlainLabel(HtmlEscape(n.name(g))) {
      tooltip = HtmlEscape(asString)
    }
  }
  def nil(g: Graph) = new swing.Label("⊥")
  /** border width of node in graph (root bold is set in preferences) */
  private def borderWidth(g: Graph, n: AbstractNode) = {
    if (g.viewer.root == n && g.preferences.rootBold()) 2 else 1
  }

  /**
   * boarder is colored like text for ordinary nodes and like selection for selected node
   */
  private def borderColour(g: Graph, n: AbstractNode) = {
    if (g.viewer.root == n) {
      DefaultColors.textHighlight
    } else {
      DefaultColors.textText
    }
  }

  def skillObject(g: Graph, node: AbstractNode, o: internal.Obj) = {
    val button = new qq.util.PlainButton(
      new swing.Action(node.name(g)) {
        override def apply() = {
          g.viewer.expandCollapse(node)
        }
      })
    button.peer.setComponentPopupMenu(qq.editor.objects.ObjectContextMenu(o, g.viewer.page).peer)
    if (g.viewer.expandedNodes.contains(node)) {
      val τ = g.file.s.pool(o)
      var allFields : Seq[ogss.common.java.api.FieldDeclaration[_]] = Seq();
      val it = τ.allFields
      while (it.hasNext()){
        allFields = allFields :+ it.next();
      }
      
      val innerFields = for (f ← allFields if !g.file.fieldPreferences(f).isDeleted && g.file.fieldPreferences(f).visibilityIn(o).showInParent) yield {
        val text = valueShortString(f.get(o))
        qq.util.Swing.HBoxT(new qq.util.PlainLabel(f.name + " = " + text), swing.Swing.HGlue)
      }

      if (innerFields.size == 0) {
        button.border = CompoundBorder(
          LineBorder(borderColour(g, node), borderWidth(g, node)),
          EmptyBorder(0, 2, 0, 2))
        button
      } else {
        val head = qq.util.Swing.HBoxT(button, swing.Swing.HGlue)
        head.border = CompoundBorder(
          swing.Swing.MatteBorder(0, 0, borderWidth(g, node), 0, borderColour(g, node)),
          EmptyBorder(0, 2, 0, 2))
        val whole = qq.util.Swing.VBoxT((head +: innerFields): _*)
        whole.border = LineBorder(borderColour(g, node), borderWidth(g, node))
        whole
      }
    } else {
      button
    }
  }
  private def container(g: Graph, node: AbstractNode, o: internal.Obj, f: api.FieldDeclaration[_], small: Boolean) = {

    val lm = new swing.Action("list members") {
      override def apply() {
        val page = qq.editor.Main.newObjectTab()
        page.find(g.file.idOfObj(o) + " " + f.name + " ?member")
        page.show()
      }
    }

    val button = new qq.util.PlainButton(
      new swing.Action(node.name(g)) {
        override def apply() = {
          if (small) {
            g.viewer.expandCollapse(node)
          } else {
            lm()
          }
        }
      }) {
      peer.setComponentPopupMenu(new swing.PopupMenu() { contents += new swing.MenuItem(lm) }.peer)
    }

    button.border = CompoundBorder(
      LineBorder(DefaultColors.textText),
      CompoundBorder(
        LineBorder(DefaultColors.text),
        CompoundBorder(
          LineBorder(DefaultColors.textText),
          EmptyBorder(0, 2, 0, 2))))
    button
  }

  def list[E, C[E] <: Buffer[E]](g: Graph, node: AbstractNode, o: internal.Obj, f: api.FieldDeclaration[C[E]]) = {
    val base = container(g, node, o, f, f.get(o).size <= g.viewer.page.preferences.graphCollectionSmall())
    base
  }
  def set[E, C[E] <: HashSet[E]](g: Graph, node: AbstractNode, o: internal.Obj, f: api.FieldDeclaration[C[E]]) = {
    val base = container(g, node, o, f, f.get(o).size <= g.viewer.page.preferences.graphCollectionSmall())
    base
  }
  def map[K, V, C[K, V] <: HashMap[K, V]](g: Graph, node: AbstractNode, o: internal.Obj, f: api.FieldDeclaration[C[K, V]]) = {
    import qq.util.FlattenedMap.size
    val base = container(g, node, o, f, size(f.get(o), f.`type`.asInstanceOf[ogss.common.java.internal.fieldTypes.MapType[K, V]]) <= g.viewer.page.preferences.graphCollectionSmall())
    base
  }
}