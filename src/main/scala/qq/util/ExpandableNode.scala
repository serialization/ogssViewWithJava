package qq.util

/**
 * A Swing UI-element that can be expanded and collapsed: used for building tree-views
 */
class ExpandableNode(val node: swing.Component, textStyle: Boolean)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  def this(node0: swing.Component, subPart0: swing.Component, textStyle0: Boolean, expanded: Boolean = false) = {
    this(node0, textStyle0)
    subPart = subPart0
    if (expanded) expand
  }

  if (textStyle) background = java.awt.SystemColor.text
  /** sub class may override in order to react when the thing is collapsed */
  def onCollapse(): Unit = {}
  /** sub class may override in order to react when the thing is expanded */
  def onExpand(): Unit = {}

  private var subPart_ : swing.Component = null
  private var lazySubPart_ : Unit ⇒ swing.Component = null

  def subPart: swing.Component = subPart_
  def subPart_=(x: swing.Component): Unit = {
    lazySubPart_ = null
    subPart_ = x
    subBox.contents.clear
    subBox.contents += spacer
    if (x != null) {
      subBox.contents += subPart
    } else {
      erbtn.action = nullAction
    }
    collapse
  }
  def lazySubPart: Unit ⇒ swing.Component = lazySubPart_
  def lazySubPart_=(x: Unit ⇒ swing.Component): Unit = {
    subPart_ = null
    lazySubPart_ = x
    subBox.contents.clear
    subBox.contents += spacer
    if (x != null) {
    } else {
      erbtn.action = nullAction
    }
    collapse
  }

  final def expand(): Unit = {
    if (lazySubPart != null) {
      subPart_ = lazySubPart(())
      subBox.contents.clear
      subBox.contents += spacer
      subBox.contents += subPart
    }
    erbtn.action = if (subPart != null) collapseAction else nullAction
    erbtn.text = ""
    subBox.visible = subPart != null
    onExpand
  }
  final def collapse(): Unit = {
    erbtn.action = if (subPart != null || lazySubPart != null) expandAction else nullAction
    erbtn.text = ""
    subBox.visible = false
    if (lazySubPart != null) subPart_ = null // forget sub pane, recreate later
    onCollapse
  }
  private val expandAction: swing.Action = new swing.Action("expand") {
    icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
    override def apply(): Unit = {
      expand
    }
  }
  private val collapseAction: swing.Action = new swing.Action("collapse") {
    icon = javax.swing.UIManager.getIcon("Tree.expandedIcon")
    override def apply(): Unit = {
      collapse
    }
  }
  private val nullAction = swing.Action("") {}

  private val erbtn = new swing.Button() {
    this.border = swing.Swing.EmptyBorder(0)
    this.contentAreaFilled = false
    if (textStyle) background = java.awt.SystemColor.text
    this.preferredSize = new java.awt.Dimension(15, 15)
    this.focusable = false
  }

  private val spacer = swing.Swing.RigidBox(new java.awt.Dimension(15, 0))
  private val subBox = Swing.HBox(textStyle, spacer)
  contents ++= Seq(
    Swing.HBox(textStyle, 0.0f, erbtn, node),
    subBox)

}
