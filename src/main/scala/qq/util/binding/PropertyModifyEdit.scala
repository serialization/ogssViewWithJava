package qq.util.binding

import javax.swing.undo._;

/** Undoable edit for the modifications of [[Property]]s by [[EditControl]]s*/
class PropertyModifyEdit[T](val property: Property[T], var oldValue: T, var newValue: T)
    extends UndoableEdit {

  /** if @c l and @c r can be merged into one edit, *both* are modified accordingly and
   *  @c true is returned. Common implementation for ::addEdit() and ::replaceEdit() */
  private def mergeEdits(l: UndoableEdit, r: UndoableEdit): Boolean = {
    if (!l.isInstanceOf[PropertyModifyEdit[T]]) return false
    if (!r.isInstanceOf[PropertyModifyEdit[T]]) return false
    val ll = l.asInstanceOf[PropertyModifyEdit[T]]
    val rr = r.asInstanceOf[PropertyModifyEdit[T]]
    if (ll.property != rr.property) return false
    if (ll.newValue != rr.oldValue) return false // WTF
    ll.newValue = rr.newValue
    rr.oldValue = ll.oldValue
    return true
  }
  override def addEdit(next: UndoableEdit): Boolean = {
    mergeEdits(this, next)
  }
  override def replaceEdit(previous: UndoableEdit): Boolean = {
    mergeEdits(previous, this)
  }
  override def canRedo(): Boolean = property() == oldValue
  def redo(): Unit = {
    property := newValue
  }
  override def canUndo(): Boolean = property() == newValue
  def undo(): Unit = {
    property := oldValue
  }
  override def die(): Unit = {

  }
  private def name: String = property.name
  private def oldValText: String = if (oldValue == null) "(null)" else oldValue.toString()
  private def newValText: String = if (newValue == null) "(null)" else newValue.toString()
  override def getPresentationName(): String = {
    if (oldValue == newValue) {
      "edited " + name + " but didn't change anything in the end" // TODO✓ should not happen: can a undoable edit commit suicide while sitting in the undo manager's queue
    } else {
      "changed " + name + " from " + oldValText + " to " + newValText
    }
  }
  override def getRedoPresentationName(): String = {
    if (oldValue == newValue) {
      "look at " + name + " but then decide to leave it unchanged" // TODO✓  should not happen: can a undoable edit commit suicide while sitting in the undo manager's queue
    } else {
      "change " + name + " to " + newValText
    }
  }
  override def getUndoPresentationName(): String = {
    if (oldValue == newValue) {
      "leave " + name + " unchanged" // TODO✓ should not happen, prevented elsewhere@ can a undoable edit commit suicide while sitting in the undo manager's queue
    } else {
      "change " + name + " back to " + oldValText
    }
  }
  override def isSignificant(): Boolean = oldValue != newValue


}