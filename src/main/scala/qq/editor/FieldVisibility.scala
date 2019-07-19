package qq.editor

/** The way in which a field is shown */
sealed abstract class FieldVisibility {
  val showAsNode: Boolean
  val showInParent: Boolean
}

/** Field is hidden */
case object HideVisibility extends FieldVisibility {
  override val showAsNode: Boolean = false
  override val showInParent: Boolean = false
}

/** Field is shown as a separate node in the graph */
case object ShowAsNodeVisibility extends FieldVisibility {
  override val showAsNode: Boolean = true
  override val showInParent: Boolean = false
}

/** Field is shown inside the label of the node for the containing object */
case object ShowInParentVisibility extends FieldVisibility {
  override val showAsNode: Boolean = false
  override val showInParent: Boolean = true
}