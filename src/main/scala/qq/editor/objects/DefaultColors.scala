package qq.editor.objects

import java.awt.Color
import javax.swing.UIManager

/**
 * overrides broken awt SystemColor
 */
object DefaultColors {
  // background
  val text = getColor("text")

  // foreground
  val textText = getColor("textText")

  // selections and links text
  val textHighlight = getColor("textHighlight")

  // selections and links
  val textHighlightText = getColor("textHighlightText")

  /**
   * workaround for broken color management; apparently actual instances seem to break some of their methods
   */
  private def getColor(name: String): Color = {
    val c = UIManager.getDefaults().getColor(name);
    new Color(c.getRed, c.getGreen, c.getBlue)
  }
}