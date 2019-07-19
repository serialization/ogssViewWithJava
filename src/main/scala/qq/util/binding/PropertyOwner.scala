package qq.util.binding

import scala.collection.mutable;

/**
 * Provide a editor UI element and undo manager for a group of [[Property]]s. Mostly useless for skill field editors
 */
trait PropertyOwner {
  private def propertyOwner = this
  
  var properties: List[Property[_]] = Nil
  
  val onAnyPropertyChange: Event[Unit] = new Event
  def doOnAnyPropertyChange: Unit = onAnyPropertyChange.fire(())
  /**
   * for undoing edits made to the associated properties
   */
  val undoManager: javax.swing.undo.UndoManager = null

  /**
   * Swing UI element for editing all owned [[Property]]s
   */
  def propertyPage() = {
    val box = qq.util.Swing.VBoxD()
    box.contents ++= properties.map { x => new LabeledEdit(x.defaultEditor)}
    box
  }
}
