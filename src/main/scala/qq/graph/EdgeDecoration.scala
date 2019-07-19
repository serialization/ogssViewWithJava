package qq.graph

import java.awt.Graphics2D

/** Decoration at the end of an edge (arrows) */
abstract class EdgeDecoration {
  def width: Int
  /* draw decoration, (0,0) is where the edge meets the node, (width,0) is where the line continues*/
  def draw(g: Graphics2D): Unit
  def symbol: String = "→"
  def reverseSymbol: String = "←"
}

object NoDecoration extends EdgeDecoration {
  override def width = 0
  override def draw(g: Graphics2D) = {}
}

object SmallArrowDecoration extends EdgeDecoration {
  override def width = 5
  override def draw(g: Graphics2D) = {
    g.drawLine(0, 0, width, 0)
    g.fillPolygon(
      Array(0,5,3,5),
      Array(0,3,0,-3),
      4
    )
  }
}