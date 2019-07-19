package qq.editor.types

import ogss.common.java.api;
import ogss.common.java.internal;
import scala.collection.mutable;
import scala.swing;

/**
 * A page displaying data about a type ([[TypeEdit]]), with an optional type tree for navigation on the
 * left ([[TypeTree]]).
 */
class TypePage(file0: qq.editor.File, preferences0: qq.editor.EditorPreferences)
    extends qq.editor.Page(file0, preferences0) {

  /** types tree visible*/
  def treeVisible: Boolean = treeVisibleModel.isSelected()
  val treeVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()
  treeVisibleModel.setSelected(true)

  /** graph visible */
  def graphVisible: Boolean = graphVisibleModel.isSelected()
  val graphVisibleModel = new javax.swing.JToggleButton.ToggleButtonModel()

  /** the type that is currently shown */
  var currentType: api.Access[_ <: internal.Obj] = null

  /** previously shown types (for back navigation) */
  val previousType: mutable.Stack[api.Access[_ <: internal.Obj]] = new mutable.Stack()

  /** previously previously shown types :) (for forward navigation) */
  val nextType: mutable.Stack[api.Access[_ <: internal.Obj]] = new mutable.Stack()

  /** show a type (internal, for goTo, goBack, goForward) */
  private def _goTo(t: api.Access[_ <: internal.Obj]): Unit = {
    currentType = t
    title = t.name
    typeEdit.contents.clear()
    typeEdit.contents += new TypeEdit(this, file, t)
    typeTree.select(t)
    goBack.enabled = previousType.length > 0
    goForward.enabled = nextType.length > 0
    newObjectOfThisType.enabled = true
    showObjectsOfThisType.enabled = true
  }

  /** show a type and update navigation */
  def goTo(t: api.Access[_ <: internal.Obj]): Unit = {
    nextType.clear()
    if (currentType != null && t != currentType) previousType.push(currentType)
    _goTo(t)
  }
  /** show previously shown type */
  val goBack = new swing.Action("Back") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("alt LEFT"))
    mnemonic = swing.event.Key.B.id
    icon = new qq.icons.BackIcon(true)
    smallIcon = new qq.icons.BackIcon(true, true)
    enabled = false
    override def apply() = {
      if (previousType.length > 0) {
        val t = previousType.pop()
        nextType.push(currentType)
        _goTo(t)
      }
    }
  }
  /** show next type */
  val goForward = new swing.Action("Forward") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("alt RIGHT")) //IRL, alt right is backwards
    mnemonic = swing.event.Key.F.id
    icon = new qq.icons.ForwardIcon(true)
    smallIcon = new qq.icons.ForwardIcon(true, true)
    enabled = false
    override def apply() = {
      if (nextType.length > 0) {
        val t = nextType.pop()
        previousType.push(currentType)
        _goTo(t)
      }
    }
  }

  val toggleTreeVisible = new swing.Action("Show Tree") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl T"))
    mnemonic = swing.event.Key.T.id
    override def apply() = {
      updateVisibility
    }
  }
  val toggleGraphVisible = new swing.Action("Show Graph") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl G"))
    mnemonic = swing.event.Key.G.id
    override def apply() = {
      updateVisibility
    }
  }

  val showObjectsOfThisType = new swing.Action("Show Objects of Current Type") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl F"))
    mnemonic = swing.event.Key.O.id
    enabled = false
    override def apply() = {
      qq.editor.Main.newObjectTab(currentType)
    }
  }

  val newObjectOfThisType = new swing.Action("Create new Object of Current Type") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl N"))
    mnemonic = swing.event.Key.N.id
    enabled = false
    override def apply() = {
      val c = new qq.editor.UserCreateObject(file, currentType)
      qq.editor.Main.newObjectTab(c.obj)
    }
  }

  val gotoParentType = new swing.Action("Go to Parent Type") {
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl P"))
    mnemonic = swing.event.Key.P.id
    override def apply() = {
      file.parentType.get(currentType) match {
        case Some(τ) ⇒ goTo(τ)
        case None    ⇒ ()
      }
    }
  }

  /* menu entries for the main window */
  override def viewMenuItems = Seq(
    new swing.MenuItem(goBack),
    new swing.MenuItem(goForward),
    new swing.CheckMenuItem("") {
      action = toggleTreeVisible
      peer.setModel(treeVisibleModel)
    }/*, type graph currently not available
    new swing.CheckMenuItem("") {
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    }*/)
  override def typeMenuItems = Seq(
    new swing.MenuItem(gotoParentType))
  override def objectMenuItems = Seq(
    new swing.MenuItem(showObjectsOfThisType),
    new swing.MenuItem(newObjectOfThisType))
  /* the layout */
  val toolBar = qq.util.Swing.HBoxD(
    new swing.Button(goBack) {
      text = ""
      icon = new qq.icons.BackIcon(true)
      disabledIcon = new qq.icons.BackIcon(false)
    },
    new swing.Button(goForward) {
      text = ""
      icon = new qq.icons.ForwardIcon(true)
      disabledIcon = new qq.icons.ForwardIcon(false)
    },
    new swing.ToggleButton("") {
      action = toggleTreeVisible
      peer.setModel(treeVisibleModel)
    },
    /* type graph currently not available
    new swing.ToggleButton("") {
    
      action = toggleGraphVisible
      peer.setModel(graphVisibleModel)
    },*/
    scala.swing.Swing.HGlue)
  val typeTree = new TypeTree(this)
  val typeEdit = qq.util.Swing.HBoxT()
  val typeGraph = new swing.Label("Todo graph")

  val mainContent = qq.util.Swing.HBoxD()

  def updateVisibility: Unit = {
    mainContent.contents.clear()
    if (treeVisible) {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeTree, new swing.SplitPane(swing.Orientation.Vertical) {
            contents_=(typeEdit, typeGraph)
          })
        }
      } else {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeTree, typeEdit)
        }
      }
    } else {
      if (graphVisible) {
        mainContent.contents += new swing.SplitPane(swing.Orientation.Vertical) {
          contents_=(typeEdit, typeGraph)
        }
      } else {
        mainContent.contents += typeEdit
      }
    }
    mainContent.revalidate()
  }
  updateVisibility
  title = "Types"
  content = qq.util.Swing.VBoxD(toolBar, mainContent)
}