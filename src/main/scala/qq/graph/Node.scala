package qq.graph

import qq.util.Vector
import scala.collection.mutable.HashMap

/** a node in a laid out graph */
class Node(val graph: Graph,
           /** the thing that is represented by this node */
           val data: AbstractNode) {

  val edgesOut: HashMap[Node, Edge] = HashMap()
  val edgesIn: HashMap[Node, Edge] = HashMap()
  def degree = edgesOut.size + edgesIn.size - (if (connectedTo(this)) 2 else 0)
  def connectedTo(r: Node) = edgesOut.contains(r) || edgesIn.contains(r)

  val uiElement = data.getUiElement(graph)
  var pos: Vector = new Vector(0f, 0f)
  def width: Int = uiElement.peer.getPreferredSize.getWidth.toInt
  def height: Int = uiElement.peer.getPreferredSize.getHeight.toInt
  def left: Int = (pos.x - width / 2).toInt
  def top: Int = (pos.y - height / 2).toInt
  var clampedAt: Option[Vector] = None

  var energy: Float = 0
  var force: Vector = new Vector(0f, 0f)

  def resetAccumulators: Unit = {
    energy = 0
    force = new Vector(0f, 0f)
  }
  def move(stepSize: Float): Unit = {
    clampedAt match {
      case Some(x) ⇒ pos = x
      case None    ⇒ if (force.isFinite() && !force.isZero()) pos += force.norm * stepSize
    }
  }

  /** @retval true if line though centre in direction @c d intersects top or bottom edge of bounds. */
  def intersectsTopOrBottom(d: Vector): Boolean = {
    d.x.abs * height < width * d.y.abs
  }
  /** x.toborder(r) = r * k for some k and x.pos+x.toBorder(r) is on the border of the rectangular node x */
  def toBorder(r: Vector): Vector = {
    if (r.x == 0 && r.y == 0 || width == 0 || height == 0) return new Vector(0f, 0f)
    if (r.y.abs * width > r.x.abs * height) { // w/h > x/y
      // pos+xr intersects top or bottom
      val h = math.signum(r.y) * height / 2
      new Vector(r.x / r.y * h, h) // scale with h/y because y component of new vector must be h
    } else {
      // pos+xr intersects left or right
      val w = math.signum(r.x) * width / 2
      new Vector(w, r.y / r.x * w)
    }
  }
  /**
   * calculate forces between this and r. will only be called once for each pair,
   *  therefore the accumulators of r are also written. overlapRemoval is a number
   *  0 .. 1 that specified how strongly the distance is corrected for removing
   *  overlap of the node's bounding boxes
   */
  def calculateForce(r: Node, overlapRemoval: Float, bounds: java.awt.Dimension) = {
    val p = graph.preferences
    if (r == this) {
      // distance to border
      val (dl, dr, dt, db) = (left, bounds.width - left - width, top, bounds.height - height - top)
      def Fᵣ(x: Int) = { val xx = (x.toFloat - p.margin()).max(p.ε()); p.c3() / xx / xx }
      val F = new Vector(Fᵣ(dl) - Fᵣ(dr), Fᵣ(dt) - Fᵣ(db))
      force += F
      energy += F * F
    } else {
      val deg = (degree.min { r.degree })
      val l = p.c2()// does not really work well: if (deg <= 3) p.c2() else p.c2() * math.sqrt(deg / 3)
      val δ = (r.pos - pos).max(p.ε())
      val δ2 = {
        // distance not covered by labels
        val δ2 = δ - (toBorder(δ) - r.toBorder(-δ)) * overlapRemoval
        // use epsilon when small or negative
        if (δ2 * δ < 0 || δ2.abs < p.ε()) δ.max(p.ε()) else δ2
      }
      val F = if (connectedTo(r)) {
        δ.norm * p.c1() * math.log(δ2.abs / l).toFloat
      } else {
        -δ.norm * p.c3() / (δ2 * δ2)
      }
      force += F
      energy += F * F
      r.force -= F
      r.energy += F * F

    }
  }
  
  // TODO ps export: attributes inside node labels 
  def toPs(g: Graph): String = {
    " " + (pos.x - width / 2) + " top " + (pos.y - height / 2) + " sub moveto\n" +
      " " + (pos.x + width / 2) + " top " + (pos.y - height / 2) + " sub lineto\n" +
      " " + (pos.x + width / 2) + " top " + (pos.y + height / 2) + " sub lineto\n" +
      " " + (pos.x - width / 2) + " top " + (pos.y + height / 2) + " sub lineto\n" +
      " " + (pos.x - width / 2) + " top " + (pos.y - height / 2) + " sub lineto\n" +
      " stroke\n" +
      " " + pos.x + " top " + pos.y + " sub moveto (" + data.name(g) + ") showc\n"
  }

}