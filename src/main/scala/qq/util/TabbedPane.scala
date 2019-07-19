package qq.util

object TabbedPane {
  /** Extends [[swing.TabbedPane.Page]] with its index and the tab component of the java peer. */
  class Page(parent0: TabbedPane, title0: String, content0: swing.Component, tip0: String)
      extends swing.TabbedPane.Page(parent0, title0, content0, tip0) {
    var tabbedPane = parent0
    val page = this
    def this(title0: String, content0: swing.Component, tip0: String) =
      this(null, title0, content0, tip0)
    def this(title0: String, content0: swing.Component) =
      this(title0, content0, "")
    val tabComponent = new swing.BoxPanel(swing.Orientation.Horizontal) {
      contents += new swing.Label(" " + title0)
      opaque = false
      listenTo(mouse.clicks)
      reactions += {
        case x: swing.event.MousePressed ⇒
          tabbedPane.peer.setSelectedComponent(content.peer)
      }
    }
    def updateTabComponent(): Unit = {
      tabComponent.contents.clear
      tabComponent.contents += new swing.Label(title)
    }
    private def updateTabComponent_(): Unit = if (tabComponent != null) updateTabComponent()
    override def title: String = super.title
    override def title_=(x: String): Unit = { super.title = x; updateTabComponent_ }
    /// TODO why does peer.indexOfComponent(pg.content.peer) below work and this doesn't?
    override def index: Int = if (tabbedPane != null) tabbedPane.peer.indexOfComponent(content.peer) else 0;
    def show(): Unit = tabbedPane.peer.setSelectedIndex(index)
  }
  /** Page in a tabbed pane that has a close button */
  class ClosablePage(parent0: TabbedPane, title0: String, content0: swing.Component, tip0: String)
      extends Page(parent0, title0, content0, tip0) {
    def this(title0: String, content0: swing.Component, tip0: String) =
      this(null, title0, content0, tip0)
    def this(title0: String, content0: swing.Component) =
      this(title0, content0, "")
    override def updateTabComponent(): Unit = {
      tabComponent.contents.clear
      tabComponent.contents += new swing.Label(title + " ") { opaque = false }
      tabComponent.contents += new PlainButton(swing.Action("close") { tabbedPane.removePage(index); onClose }) {
        this.icon = javax.swing.UIManager.getIcon("InternalFrame.closeIcon")
        this.text = ""
      }
    }
    def onClose(): Unit = {}
  }
}

/** Extends [[swing.TabbedPane]] by publishing a notification when a new page is selected. */
class TabbedPane extends swing.TabbedPane {

  /* TODO howto override pages?—f… that pages thing anyway: it doesn't store the pages, apply
   * makes new ones from peer. so we use out own pages list, don't access scala.swing.pages, otherwise :(…
   */
  private val origPages: scala.collection.mutable.Set[TabbedPane.Page]= new scala.collection.mutable.HashSet()
  def addPage(pg: TabbedPane.Page) {
    pg.tabbedPane = this
    origPages += pg
    pages += pg
    peer.setTabComponentAt(pg.index, pg.tabComponent.peer)
  }
  def removePage(n: Int) {
    origPages.retain(_.index != n)
    pages.remove(n)
  }

  val onPageChanged: qq.util.binding.Event[TabbedPane.Page] = new qq.util.binding.Event()

  private def theTabbedPane = this /* java event only tells us peer */
  this.peer.addChangeListener(new javax.swing.event.ChangeListener() {
    override def stateChanged(x: javax.swing.event.ChangeEvent) = {
      val i = theTabbedPane.peer.getSelectedIndex
      theTabbedPane.origPages.find(_.index == i) match {
        case Some(p) ⇒
          if (p.isInstanceOf[TabbedPane.Page])
            onPageChanged.fire(p.asInstanceOf[TabbedPane.Page])
        case None ⇒ 
            onPageChanged.fire(null)
      }
    }
  })
}
