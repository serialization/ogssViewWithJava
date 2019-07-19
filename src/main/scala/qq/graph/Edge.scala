package qq.graph

import scala.collection.mutable.HashSet
import qq.util.Vector
import java.awt.geom.AffineTransform

/**
 * a edge in the graph as drawn. There is at most one edge between two nodes. All
 *  edges that should connect those two nodes in the underlying data
 *  are added to the data and reverseData collections
 */
class Edge(
    val graph : Graph,
    val from : Node,
    val to : Node,
    data0 : AbstractEdge) {

  /** all abstract edges represented by this drawn link */
  val data : HashSet[AbstractEdge] = HashSet(data0)
  /** abstract edges represented by a edge in the opposite direction of this one */
  val reverseData : HashSet[AbstractEdge] = HashSet()

  val uiElementAtFrom = new swing.Label("  ")
  val uiElementAtTo = new swing.Label("  ")
  def updateToolTip : Unit = {
    // two tool tips with all edges neatly on top of each other
    uiElementAtFrom.tooltip = s"""<html><table><tr>
      <td valign="middle">${from.data.name(graph)}</td>
      <td valign="middle"><center>${
      (data.map(x ⇒ x.textLabel(graph.file) + " " + x.toDecoration.symbol) ++
        reverseData.map(x ⇒ x.toDecoration.reverseSymbol + " " + x.textLabel(graph.file))).mkString("<br>")
    }</center></td>
      <td valign="middle">${to.data.name(graph)}</td></tr></html>"""
    uiElementAtTo.tooltip = s"""<html><table><tr>
      <td valign="middle">${to.data.name(graph)}</td>
      <td valign="middle"><center>${
      (reverseData.map(x ⇒ x.textLabel(graph.file) + " " + x.toDecoration.symbol) ++
        data.map(x ⇒ x.toDecoration.reverseSymbol + " " + x.textLabel(graph.file))).mkString("<br>")
    }</center></td>
      <td valign="middle">${from.data.name(graph)}</td></tr></html>"""
    uiElementAtFrom.peer.setSize(uiElementAtFrom.preferredSize)
    uiElementAtTo.peer.setSize(uiElementAtTo.preferredSize)
    // invisible labels that show the tool tips at the endpoints of the edges
    val fromCenter = from.pos + from.toBorder(to.pos - from.pos)
    val toCenter = to.pos + to.toBorder(from.pos - to.pos)
    uiElementAtFrom.peer.setLocation(
      fromCenter.x.toInt - uiElementAtFrom.preferredSize.width / 2,
      fromCenter.y.toInt - uiElementAtFrom.preferredSize.height / 2)
    uiElementAtTo.peer.setLocation(
      toCenter.x.toInt - uiElementAtTo.preferredSize.width / 2,
      toCenter.y.toInt - uiElementAtTo.preferredSize.height / 2)

  }

  def r : Vector = to.pos - from.pos
  /** calculate the direction stabilising force due to this edge and add it to from and to */
  def calculateForce() : Unit = {
    def Fp(p : Vector) = (p - r * (p * r) / (r * r)) * graph.preferences.c4()
    def apply(F : Vector) = if (F.isFinite()) {
      val ff = F * F
      from.force -= F
      to.force += F
      from.energy += ff
      to.energy += ff
    }
    // when many edges for the same field meet at one node we make the force
    // weaker so that the nodes at the other end don't get pushed together too much.
    // thank data and reverseDate that's a lot of cases…

    // e and f represent the same field
    def sameField(e : AbstractEdge, f : AbstractEdge) = {
      e.isInstanceOf[SkillFieldEdge[_]] &&
        f.isInstanceOf[SkillFieldEdge[_]] &&
        e.asInstanceOf[SkillFieldEdge[_]].field ==
        f.asInstanceOf[SkillFieldEdge[_]].field
    }
    /* number of edges from source that represent the same field as e */
    def sameSourceEdgeCount(source : Node, e : AbstractEdge) = {
      if (graph.preferences.scaleDirectionWhenConflict()) {
        source.edgesOut.values.count(_.data.exists(sameField(e, _))) +
          source.edgesIn.values.count(_.reverseData.exists(sameField(e, _)))
      } else {
        1
      }
    }
    /* number of edges to target that represent the same field as e */
    def sameTargetEdgeCount(target : Node, e : AbstractEdge) = {
      if (graph.preferences.scaleDirectionWhenConflict()) {
        target.edgesOut.values.count(_.reverseData.exists(sameField(e, _))) +
          target.edgesIn.values.count(_.data.exists(sameField(e, _)))
      } else {
        1
      }
    }

    for (e ← data) {
      val scale = if (e.isInstanceOf[SkillFieldEdge[_]]) {
        sameSourceEdgeCount(from, e) + sameTargetEdgeCount(to, e) - 1
      } else {
        1
      }
      apply(Fp(e.idealDirection(graph.file)) / scale)
    }
    for (e ← reverseData) {
      val scale = if (e.isInstanceOf[SkillFieldEdge[_]]) {
        sameSourceEdgeCount(to, e) + sameTargetEdgeCount(from, e) - 1
      } else {
        1
      }
      apply(-Fp(e.idealDirection(graph.file)) / scale)
    }

  }

  /**
   * print a block of strings
   */
  private def putsstr(g : swing.Graphics2D, ss : Seq[String], width : Float, toDirection : Boolean) = {
    val savedTransformation = g.getTransform

    // calculate label distance
    val direction = if (toDirection) -1 else 1
    val h = 1.5 * g.getFontMetrics.getHeight * direction
    val begin = (width / 2 - 3) * direction

    // set initial position relative to the center of the line
    g.translate(begin, h * (if (!toDirection) .75 else .5))

    for (s ← ss) {
      g.drawString(s, if (toDirection) 0 else -g.getFontMetrics.stringWidth(s), 0)
      g.translate(0, h)
    }

    g.setTransform(savedTransformation)
  }

  /**
   * draw this edge, add decoration and textLabels from data and reverse data if feasible,
   * otherwise provide interactive means to get to them
   */
  def draw(g : swing.Graphics2D) {
    // save transformation to reset changes made in the body of this method 
    val savedTransformation = g.getTransform

    val fromLabels = reverseData.iterator.map(_.textLabel(graph.file)).toSeq
    val fromDecorations = reverseData.iterator.map(_.toDecoration).toSeq.distinct
    val toLabels = data.iterator.map(_.textLabel(graph.file)).toSeq
    val toDecorations = data.iterator.map(_.toDecoration).toSeq.distinct

    if (from == to) {
      // self edge
      g.translate(from.pos.x.toInt + from.width / 2, from.pos.y.toInt)
      g.drawOval(-10, -20, 40, 40)
      g.translate(30, 0)
      putsstr(g, fromLabels ++ toLabels, 0, true)
    } else {
      // regular edge
      val f = from.pos + from.toBorder(to.pos - from.pos)
      val t = to.pos + to.toBorder(from.pos - to.pos)
      g.translate(f.x.toInt, f.y.toInt)
      val delta = t - f
      val φ = delta.direction

      // create decorations
      g.rotate(φ)
      var width = delta.abs.toInt
      for (d ← fromDecorations) {
        d.draw(g)
        width -= d.width
        g.translate(d.width, 0)
      }
      g.translate(width, 0)
      g.rotate(math.Pi)
      for (d ← toDecorations) {
        d.draw(g)
        width -= d.width
        g.translate(d.width, 0)
      }
      // draw line
      g.rotate(math.Pi)
      g.drawLine(0, 0, -width, 0)

      // set transformation such that center is the center of the line and direct causes text to be readable
      var flipped = false;
      locally {
        val trans = savedTransformation.clone().asInstanceOf[AffineTransform]
        val mid = (t + f) * .5f
        trans.translate(mid.x, mid.y)
        trans.rotate(φ)
        // ensure that rotations points upwards
        locally {
          val data = new Array[Double](6)
          trans.getMatrix(data)
          if (data(3) < 0) {
            trans.rotate(math.Pi)
            flipped = true
          }
        }

        g.setTransform(trans)
      }

      // create toLabels above and fromLabels below      
      putsstr(g, toLabels, width, !flipped)
      putsstr(g, fromLabels, width, flipped)
    }
    g.setTransform(savedTransformation)
  }

  // TODO decorations and stack labels of parallel edges in postscript
  def toPs() : String = {
    val fromLabels = reverseData.iterator.map(_.textLabel(graph.file)).toSeq
    val fromDecorations = reverseData.iterator.map(_.toDecoration).toSeq.distinct
    val toLabels = data.iterator.map(_.textLabel(graph.file)).toSeq
    val toDecorations = data.iterator.map(_.toDecoration).toSeq.distinct

    " matrix currentmatrix\n" +
      (if (from == to) {
        " " + (from.pos.x.toInt + from.width / 2) + " top " + from.pos.y.toInt + " sub translate\n" +
          "                           0.0 -10.0 moveto\n" +
          "   5.0 -20.0  30.0 -20.0  30.0   0.0 curveto\n" +
          "  30.0  20.0   5.0  20.0   0.0  10.0 curveto\n" +
          " stroke\n" +
          fromLabels.map("  33.0 0.0 moveto (" + _ + ") show\n").mkString("") +
          toLabels.map("  33.0 0.0 moveto (" + _ + ") show\n").mkString("")
      } else {
        val f = from.pos + from.toBorder(to.pos - from.pos)
        val t = to.pos + to.toBorder(from.pos - to.pos)
        val d = t - f
        val fromBelow = from.intersectsTopOrBottom(d) ^ (d.y < 0)
        val toBelow = to.intersectsTopOrBottom(d) ^ (d.y > 0)
        val φ = -d.direction
        " " + f.x + " top " + f.y + " sub translate\n" +
          (if (φ.abs <= math.Pi / 2) {
            // write in direction of edge
            " " + φ.toDegrees + " rotate\n" +
              toLabels.map(" " + d.abs + " 0 moveto (" + _ + ") " + (if (toBelow) "showtr" else "showbr") + "\n").mkString("") +
              fromLabels.map(" 0 0 moveto (" + _ + ") " + (if (fromBelow) "showtl" else "showbl") + "\n").mkString("") +
              " 0 0 moveto " + d.abs + " 0 lineto stroke\n"
          } else {
            // write in opposite direction
            " " + (φ + math.Pi).toDegrees + " rotate\n" +
              toLabels.map(" " + (-d.abs) + " 0 moveto (" + _ + ") " + (if (toBelow) "showtl" else "showbl") + "\n").mkString("") +
              fromLabels.map(" 0 0 moveto (" + _ + ") " + (if (fromBelow) "showtr" else "showbr") + "\n").mkString("") +
              " 0 0 moveto " + (-d.abs) + " 0 lineto stroke\n"
          })
      }) +
      " setmatrix "

  }

}