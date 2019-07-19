package qq.util

import javax.swing.undo;

/**
 * like javax.swing.UndoManager but comes with two actions for undo and
 *  redo which get their captions and enabledness updated as it is appropriate
 */
class UndoManager extends undo.UndoManager {

  val undoAction = new swing.Action("nothing to undo") {
    def apply = if (canUndo()) {
      undo
      updateActions
    }
    enabled = false
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl Z"))
    mnemonic = swing.event.Key.U.id
  }
  val redoAction = new swing.Action("nothing to redo") {
    def apply = if (canRedo()) {
      redo
      updateActions
    }
    enabled = false
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke("ctrl Y"))
    mnemonic = swing.event.Key.R.id
  }
  override def addEdit(e: javax.swing.undo.UndoableEdit): Boolean = {
    val result = super.addEdit(e)
    if (result) {
      updateActions
    }
    result
  }
  private def updateActions(): Unit = {
    undoAction.enabled = canUndo
    undoAction.title = if (this.editToBeUndone() != null) "Undo: " + this.getUndoPresentationName else "nothing to undo"
    redoAction.enabled = canRedo
    redoAction.title = if (this.editToBeRedone() != null) "Redo: " + this.getRedoPresentationName else "nothing to redo"
  }
}
