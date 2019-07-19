package qq.editor.types

import scala.collection.mutable
import scala.collection.JavaConverters._

import ogss.common.java.api
import ogss.common.java.internal
import qq.editor.objects.DefaultColors
import qq.util.Swing.HBoxT

/** UI-Element displaying the type hierearchy as tree view.*/
class TypeTree(val page: qq.editor.types.TypePage)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  background = DefaultColors.text

  /** node control by type for unfold/highlight */
  val nodes: mutable.Map[api.Access[_], TypeTreeNode] = new mutable.HashMap()

  class TypeTreeNode(τ: api.Access[_ <: internal.Obj])
      extends qq.util.ExpandableNode(
        HBoxT(), true) {

    val nameLabel = new TypeNameControl(page, τ)
    val countsLabel = new qq.util.PlainLabel(" " + τ.asInstanceOf[internal.Pool[_]].staticInstances.asScala.size + " (" + τ.size + ")") {
      tooltip = "objects in this pool (including subpools)"
    }

    node.asInstanceOf[swing.BoxPanel].contents ++= Seq(
      nameLabel, swing.Swing.HGlue, countsLabel)

    nodes(τ) = this

    if (page.file.childTypes.contains(τ)) {
      subPart = new swing.BoxPanel(swing.Orientation.Vertical) {
        background = DefaultColors.text
        contents ++= page.file.childTypes(τ).filter(!page.file.typePreferences(_).isDeleted).map(new TypeTreeNode(_))
      }
    }

    def highlight_=(l: Boolean) = {
      val colour = if (l) DefaultColors.textHighlightText else DefaultColors.text
      node.background = colour
      nameLabel.background = colour
      countsLabel.background = colour
    }

  }
  val typeTree = new swing.BoxPanel(swing.Orientation.Vertical) {
    /* toSeq keeps the order stable; there's probably some concurrency going on in the
     * map of sets */
    background = DefaultColors.text
    contents ++= page.file.rootTypes.toSeq.map(new TypeTreeNode(_))
    contents += swing.Swing.VGlue
  }
  val scrollContainer = new qq.util.VScrollPane() {
    contents = typeTree
  }

  var selected: TypeTreeNode = null
  /** select type `τ` in the hierarchy, expand levels and scroll if necessary */
  def select(τ: api.Access[_ <: internal.Obj]): Unit = {
    if (selected != null) {
      selected.highlight_=(false)
    }

    /* expand all parents */
    for (x <- page.file.superTypes(τ)) nodes(x).expand

    val node = nodes(τ).node.peer
    val nodePos = javax.swing.SwingUtilities.convertRectangle(node.getParent, node.getBounds, typeTree.peer)
    typeTree.peer.scrollRectToVisible(nodePos)
    selected = nodes(τ)
    selected.highlight_=(true)

  }

  contents += scrollContainer
}