package qq.util.binding

import qq.util.Neq

/** A ui-element used for editing a [[Property]]. */
abstract class EditControl[T](val property: Property[T]) extends swing.Component {
  /**
   * whether editing updates the property immediately or update has to be called
   * explicitly (e.g. from an Apply button)
   */
  var updateImmediately: Boolean = true
  /**
   * Right(x) if the currently shown value is valid, Left(message) otherwise with message telling why not
   */
  val editValue: Observable[Either[List[String], T]]
  /**
   * @retval true iff the value shown in this component was changed but not yet
   *    written back to the property
   */
  lazy val isModified: Observable[Boolean] = editValue.map({ case Left(_) ⇒ true; case Right(x) ⇒ x != property() })
  /**
   * @retval true iff value shown by the component can be written back to the property
   */
  lazy val isValid: Observable[Boolean] = editValue.map(_.isInstanceOf[Right[_, _]])
  lazy val validationMessages: Observable[List[String]] = editValue.map({ case Left(xs) ⇒ xs; case Right(_) ⇒ Nil })
  /**
   * initialise the value shown by this component to the value of the property
   */
  def propertyToComponent(value: T): Unit
  /**
   * set property to the value of the editor (i.e. ::editValue). Nop unless ::isModified, exception unless ::isValid.
   */
  def componentToProperty(): Unit = {
    property.synchronized {
      editValue() match {
        case Right(x) ⇒
          val old = property()
          if (Neq(x, old)) {
            property := x
            if (property.owner != null && property.owner.undoManager != null) {
              property.owner.undoManager.addEdit(new PropertyModifyEdit(property, old, x))
            }
          }
        case Left(es) ⇒ throw new IllegalStateException("can not update " + property.name + " because: " + es.mkString(", "))
      }
    }
  }

  /* update UI when property changes */
  private val ptc = propertyToComponent(_)
  property.onChange.weak += ptc
}