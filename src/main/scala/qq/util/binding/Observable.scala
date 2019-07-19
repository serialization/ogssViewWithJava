package qq.util.binding

import scala.collection.mutable
/**
 * Something that can change; a read-only [[Property]].
 */
abstract class Observable[T]() {
  def apply(): T
  def doOnChange(value: T) = {
    onChange.fire(value)
  }
  /**
   * functions to be called when then property changes
   */
  val onChange: Event[T] = new Event;

  /**
   * return a new observable, the value of which is derived from the value of this one by applying
   * a function
   */
  def map[U](f: T ⇒ U): Observable[U] = {
    new DerivedObservable(this, f)
  }

  private class DerivedObservable[U, V](base: Observable[U], f: U ⇒ V) extends Observable[V] {
    private val onChangeHandler: U ⇒ Unit = x ⇒ doOnChange(f(x))
    override def apply() = f(base())
    base.onChange.weak += onChangeHandler
  }

}