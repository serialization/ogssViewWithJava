package qq.icons

import javax.swing._
import java.awt._

class RemoveListItemIcon(val rollover: Boolean = false)
    extends Icon {
  def getIconWidth = 9
  def getIconHeight = 9
  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
    val gg = g.asInstanceOf[Graphics2D]
    val prevs = gg.getStroke
    if (rollover) {
      gg.setColor(Color.red)
    } else {
      gg.setColor(Color.gray)
    }
    gg.setStroke(new BasicStroke(2))
    gg.drawLine(0, getIconHeight / 2, getIconWidth - 2, getIconHeight / 2)
    gg.setStroke(prevs)
  }
}
class AddListItemIcon(val rollover: Boolean = false)
    extends Icon {
  def getIconWidth = 9
  def getIconHeight = 9
  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
    val gg = g.asInstanceOf[Graphics2D]
    val prevs = gg.getStroke
    if (rollover) {
      gg.setColor(Color.green)
    } else {
      gg.setColor(Color.gray)
    }
    gg.setStroke(new BasicStroke(2))
    gg.drawLine(getIconWidth / 2, 0, getIconWidth / 2, getIconHeight - 2)
    gg.drawLine(0, getIconHeight / 2, getIconWidth - 2, getIconHeight / 2)
    gg.setStroke(prevs)
  }
}

class ForwardIcon(val enabled: Boolean = false, val small: Boolean = false)
    extends Icon {
  def getIconWidth = if (small) 9 else 20
  def getIconHeight = if (small) 9 else 20
  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
    val gg = g.asInstanceOf[Graphics2D]
    val prevs = gg.getStroke
    if (enabled) {
      gg.setColor(Color.blue)
    } else {
      gg.setColor(Color.gray)
    }
    val prevt = gg.getTransform
    gg.translate(x, y)
    gg.setStroke(new BasicStroke(2))
    gg.drawLine(0, getIconHeight / 3, getIconWidth - getIconWidth / 3, getIconHeight / 3)
    gg.drawLine(0, getIconHeight - getIconHeight / 3, getIconWidth - getIconWidth / 3, getIconHeight - getIconHeight / 3)
    gg.drawLine(0, getIconHeight / 3, 0, getIconHeight - getIconHeight / 3)
    gg.drawLine(getIconWidth - getIconWidth / 3, getIconHeight / 3, getIconWidth - getIconWidth / 3, 0)
    gg.drawLine(getIconWidth - getIconWidth / 3, getIconHeight - getIconHeight / 3, getIconWidth - getIconWidth / 3, getIconHeight)
    gg.drawLine(getIconWidth - getIconWidth / 3, 0, getIconWidth, getIconHeight / 2)
    gg.drawLine(getIconWidth - getIconHeight / 3, getIconHeight, getIconWidth, getIconHeight / 2)
    gg.setStroke(prevs)
    gg.setTransform(prevt)
  }
}

class BackIcon(val enabled: Boolean = false, val small: Boolean = false)
    extends Icon {
  def getIconWidth = if (small) 9 else 20
  def getIconHeight = if (small) 9 else 20
  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
    val gg = g.asInstanceOf[Graphics2D]
    val prevs = gg.getStroke
    if (enabled) {
      gg.setColor(Color.blue)
    } else {
      gg.setColor(Color.gray)
    }
    gg.setStroke(new BasicStroke(2))
    val prevt = gg.getTransform
    gg.translate(x, y)
    gg.drawLine(getIconWidth , getIconHeight / 3, getIconWidth / 3, getIconHeight / 3)
    gg.drawLine(getIconWidth , getIconHeight - getIconHeight / 3, getIconWidth / 3, getIconHeight - getIconHeight / 3)
    gg.drawLine(getIconWidth , getIconHeight / 3, getIconWidth, getIconHeight - getIconHeight / 3)
    gg.drawLine(getIconWidth / 3, getIconHeight / 3, getIconWidth / 3, 0)
    gg.drawLine(getIconWidth / 3, getIconHeight - getIconHeight / 3, getIconWidth / 3, getIconHeight)
    gg.drawLine(getIconWidth / 3, 0, 0, getIconHeight / 2)
    gg.drawLine(getIconHeight / 3, getIconHeight, 0, getIconHeight / 2)
    gg.setStroke(prevs)
    gg.setTransform(prevt)
  }
}
