package qq.util.binding

import scala.collection.mutable;

/**
 * Somethign that can happen.
 * Stores pointers to event handler functions and allows call them with a parameter
 */
class Event[T] {
  /**
   * call all registered handlers with parameter @c value
   */
  def fire(value: T) = {
    strongHandlers.foreach(_(value))
    /* make a copy of the current handlers; they may want to add other handlers :)*/
    val wh = weakHandlers.keys.toSeq
    wh.foreach(_(value))
  }
  object weak {
    /**
     * register @c handler as handler for this event via a weak pointer. Use when the event
     * will outlive the consumer. Make sure to keep strong reference to the handler somewhere;
     * this precludes use of anonymous functions.
     * 
     * A def seems not to qualify as strong reference, unfortunately. Probably there's
     * something going on with patching in the this parameter or whatever, so it's best
     * do declare weak event handlers as val with function type.
     */
    def +=(handler: T ⇒ Unit) = weakHandlers += handler -> (())
    /**
     * unregister @c handler as weak handler for this event
     */
    def -=(handler: T ⇒ Unit) = weakHandlers -= handler
  }
  object strong {
    /**
     * register @c handler as handler for this event via a strong pointer. Don't forget to remove
     * the handler if the event outlives the consumer; otherwise it is safe to add anonymous functions, here.
     */
    def +=(handler: T ⇒ Unit) = strongHandlers += handler
    /**
     * unregister @c handler as strong handler for this event
     */
    def -=(handler: T ⇒ Unit) = strongHandlers -= handler
  }
  private val weakHandlers: mutable.WeakHashMap[T ⇒ Unit, Unit] = mutable.WeakHashMap()
  private val strongHandlers: mutable.Set[T ⇒ Unit] = mutable.Set()
}
   
