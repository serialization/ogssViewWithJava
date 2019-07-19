package qq.editor.objects

import java.awt.RenderingHints

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import ogss.common.java.api
import ogss.common.java.internal
import qq.util.Vector

/** Show obj and neighbourhood as graph */
class ObjectGraph[O <: internal.Obj](
  val page: ObjectPage,
  val obj: O)
    extends swing.Panel() {
  peer.setLayout(null)
  peer.setMinimumSize(new swing.Dimension(50, 50))
  peer.setPreferredSize(new swing.Dimension(50, 50))
  peer.setSize(new swing.Dimension(50, 50))
  background = java.awt.SystemColor.text

  var graph = new qq.graph.Graph(page.file, this, page.preferences.graphLayout)

  val root = new qq.graph.SkillObjectNode(obj)
  val visibleNodes = new HashSet[qq.graph.AbstractNode]
  val expandedNodes = new HashSet[qq.graph.AbstractNode]
  /** paths through which a node is expanded. must be complete for collapsing */
  private val epathsToNode = new HashMap[qq.graph.AbstractNode, Set[Seq[api.FieldDeclaration[_]]]]
  /** paths through which a node is visible. only five shortest paths */
  private val vpathsToNode = new HashMap[qq.graph.AbstractNode, Set[Seq[api.FieldDeclaration[_]]]]
  /** app π to vpaths for node n if it is shortest, remove longer paths if possible*/
  private def addVpath(n: qq.graph.AbstractNode, π: Seq[api.FieldDeclaration[_]]): Unit = {
    vpathsToNode.getOrElseUpdate(n, Set())
    if (vpathsToNode(n).size == 0) {
      vpathsToNode(n) += π
    } else {
      if (π.size < vpathsToNode(n).head.size) {
        vpathsToNode(n) = Set(π)
      } else if (π.size == vpathsToNode(n).head.size) {
        if (vpathsToNode(n).size < 5) {
          vpathsToNode(n) += π
        }
      }
    }
  }
  private def addEpath(n: qq.graph.AbstractNode, π: Seq[api.FieldDeclaration[_]]): Unit = {
    epathsToNode.getOrElseUpdate(n, Set())
    epathsToNode(n) += π
    addVpath(n, π)
  }

  val expandWithoutPath = new HashSet[qq.graph.AbstractNode]

  // once shown, nodes stay at their position
  val clampedNodes = new HashMap[qq.graph.AbstractNode, Vector]

  /**
   * paths to expanded nodes. Only those which start with fields of obj are releavant and got from
   *  the preferences of the type of obj and super types
   */
  private def expandPrefs(): Set[Seq[api.FieldDeclaration[_]]] = {
    val τ = page.file.s.pool(obj)
    (for (
      τ2 <- τ +: page.file.superTypes(τ);
      path <- page.file.typePreferences(τ2).expanded
    ) yield path).toSet
  }
  /** add paths to expanded nodes. Store at the type that contains the first field */
  private def expandPrefs_add(πs: Set[Seq[api.FieldDeclaration[_]]]): Unit = {
    for (π <- πs if π.size > 0) {
      val typePreferences = page.file.fieldPreferences(π.head).containingType
      typePreferences.expanded += π
    }
  }
  /** remove paths to expanded nodes. They are stored at the type that contains the first field */
  private def expandPrefs_remove(πs: Set[Seq[api.FieldDeclaration[_]]]): Unit = {
    for (π <- πs if π.size > 0) {
      val typePreferences = page.file.fieldPreferences(π.head).containingType
      typePreferences.expanded.remove(π)
    }
  }
  def expandCollapse(n: qq.graph.AbstractNode) {
    if (!vpathsToNode.contains(n) || vpathsToNode(n).size == 0) {
      // members of containers have no path
      if (expandWithoutPath.contains(n)) {
        expandWithoutPath -= n
      } else {
        expandWithoutPath += n
      }
    } else {
      if (expandedNodes.contains(n)) {
        expandPrefs_remove(epathsToNode(n))
        expandWithoutPath -= n
      } else {
        expandPrefs_add(vpathsToNode(n).take(5)) // prevent exponential growth
      }
    }
    updateLayout
    repaint
  }

  def updateLayout: Unit = {
    if (page.file.deletedObjects.contains(root.skillObject)) {
      // clear all if deleted
      graph = new qq.graph.Graph(page.file, this, page.preferences.graphLayout)
      peer.removeAll()
      return
    }
    val t0 = System.nanoTime()
    visibleNodes.clear()
    expandedNodes.clear()
    vpathsToNode.clear()
    epathsToNode.clear()
    visibleNodes += root
    expandedNodes += root
    addEpath(root, Seq())
    val t1 = System.nanoTime()
    // follow paths to expanded nodes
    val edgesOnPaths = new HashSet[qq.graph.AbstractEdge]()
    for (path ← expandPrefs) {
      def expandPath(o: internal.Obj,
        pathToO: Seq[api.FieldDeclaration[_]],
        pathToDo: Seq[api.FieldDeclaration[_]]): Boolean = {
        if (o != null) {
          if (pathToDo.size == 0) {
            val node = new qq.graph.SkillObjectNode(o)
            visibleNodes += node
            expandedNodes += node
            addEpath(node, pathToO)
            true
          } else {
            val field = pathToDo.head
            val node = new qq.graph.SkillObjectNode(o)
            val toNext = node.edgeForField(graph.file, field)
            toNext match {
              case Some(edge) ⇒
                edge.getTo match {
                  case qq.graph.SkillObjectNode(p) ⇒
                    if (expandPath(p, pathToO :+ field, pathToDo.tail)) {
                      visibleNodes += node
                      addVpath(node, pathToO)
                      edgesOnPaths += edge
                      true
                    } else {
                      false
                    }
                  case next ⇒
                    // last step can lead to a collections (size == 1)
                    visibleNodes += node
                    addVpath(node, pathToO)
                    visibleNodes += next
                    expandedNodes += next
                    addEpath(next, pathToO :+ field)
                    edgesOnPaths += edge
                    true

                }
              case None ⇒ false // path leads nowhere (null or hidden or wrong type)
            }
          }
        } else {
          false
        }
      }
      expandPath(obj, Seq(), path)
    }
    expandedNodes ++= expandWithoutPath
    // go to fields of expanded nodes
    val t2 = System.nanoTime()
    graph = new qq.graph.Graph(page.file, this, page.preferences.graphLayout)
    // fix the position of clamped nodes first, then other nodes can get a better initial position
    if (page.preferences.graphLayout.rootAtCentre()) {
      // clamp root to center
      clampedNodes(root) = qq.util.Vector(size) / 2
      graph.addNode(root)
      graph.nodes(root).clampedAt = Some(clampedNodes(root))
      graph.nodes(root).move(0.0f)
    }
    for (node ← visibleNodes) {
      graph.addNode(node)
      if (clampedNodes.contains(node)) {
        graph.nodes(node).clampedAt = Some(clampedNodes(node))
        graph.nodes(node).move(0.0f)
      }
    }
    for (node ← expandedNodes) {
      node.getOutEdge(page.file).foreach { e ⇒
        graph.addEdge(e)
        e match {
          case e: qq.graph.SkillFieldEdge[_] ⇒
            val to = e.getTo
            if (vpathsToNode.contains(node)) {
              vpathsToNode(node).foreach(path => addVpath(to, path :+ e.field))
            }
          case _ ⇒ () // no paths for container members
        }
      }
    }
    // clamp rest
    for (v ← graph.nodes.keys) {
      if (clampedNodes.contains(v)) {
        graph.nodes(v).clampedAt = Some(clampedNodes(v))
      }
    }

    // fill in all edges – is too much :)
    /*for (node ← graph.nodes.keys) {
      for (edge ← node.getOutEdge(page.file)) {
        if (graph.nodes.contains(edge.getTo)) graph.addEdge(edge)
      }
    }*/
    for (e <- edgesOnPaths) {
      graph.addEdge(e)
    }

    val t3 = System.nanoTime()
    graph.placeNodes(size)
    val t4 = System.nanoTime()
    // add UI elements and store positions
    clampedNodes.clear()
    peer.removeAll()
    graph.nodes.values.foreach { x ⇒
      x.edgesOut.values.foreach { y ⇒
        peer.add(y.uiElementAtFrom.peer)
        peer.add(y.uiElementAtTo.peer)
      }
      peer.add(x.uiElement.peer)
      clampedNodes(x.data) = x.pos
    }
    val t5 = System.nanoTime()
    //println(s"${graph.nodes.size} nodes ${graph.nodes.values.map(_.edgesOut.size).sum} edges follow paths: ${t2 - t1} ns, place: ${(t4 - t3) / 1E6} ms, total ${(t5 - t0) / 1E6} ms E=${graph.energy} E2=${graph.energyHu} stepsize=${graph.graphInfo.stepOfStep.last}")

  }

  override def paintComponent(g: swing.Graphics2D) {
    super.paintComponent(g)

    // enable antialising for text and shapes
    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    // Start by erasing this Canvas
    g.setColor(DefaultColors.text)
    g.clearRect(0, 0, bounds.width, bounds.height)
    g.fillRect(0, 0, bounds.width, bounds.height)
    g.setColor(DefaultColors.textText)

    for (c ← graph.nodes.values) {
      //g.drawString((c.force).toString(), c.pos.x.toInt, c.pos.y.toInt - 10)
      for (e ← c.edgesOut.values) {
        e.draw(g)
        e.updateToolTip
      }
      c.uiElement.peer.setSize(c.uiElement.preferredSize)
      c.uiElement.peer.setLocation(c.left, c.top)
      c.uiElement.peer.revalidate()
    }
    /* for (i ← 0.until(graph.graphInfo.energyOfStep.size)) {
      g.drawString("x", 10 + i, 10 + 10 * graph.graphInfo.energyOfStep(i) / graph.nodes.size)
      g.drawString("o", 10 + i, 10 + graph.graphInfo.stepOfStep(i))
      g.drawString("-", 10 + i, 10 + 10 * graph.graphInfo.energyHuOfStep(i))
    }*/
  }

  listenTo(this)
  reactions += {
    case e: swing.event.UIElementResized ⇒
      clampedNodes.clear()
      updateLayout
      repaint

  }

  // simple solution to dealing with edits: rebuilt when any object we show changes 
  val onEdit = (ed: qq.editor.Edit[_]) ⇒ {
    if (visibleNodes.contains(new qq.graph.SkillObjectNode(ed.obj.asInstanceOf[internal.Obj]))) {
      updateLayout
      repaint
    }
  }

  page.file.onEdit.weak += onEdit

  // don't updateLayout when created, it is done in resize, when the size is known

}
