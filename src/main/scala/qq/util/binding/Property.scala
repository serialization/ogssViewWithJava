package qq.util.binding

import scala.collection.mutable;
import ogss.common.java.api
import qq.util.Neq

/**
 * Something that can be changed; an [[Observable]] with assignment.
 *
 * Adapted from schauderhaft blog (blog.schauderhaft.de/2011/05/01/binding-scala-objects-to-swing-components/)
 *
 * Used for binding GUI elements to actual data.
 */
class Property[T](val owner: PropertyOwner, val name: String, var value: T)
    extends Observable[T]() {

  if (owner != null) {
    owner.properties = owner.properties :+ this
    onChange.strong += (_ ⇒ owner.doOnAnyPropertyChange)
  }

  var description: String = name
  def apply(): T = this.synchronized { value }

  /**
   * Set the value of the property to \c newValue. \throws RestrictionException when
   * newValue violates any restriction.
   */
  def :=(newValue: T): Unit = this.synchronized {
    validationMessages(newValue) match {
      case x :: xs ⇒
        throw new RestrictionException(x)
      case Nil ⇒
        if (Neq(value, newValue)) {
          value = newValue
          doOnChange(newValue)
        }
    }
  }
  /**
   * Set the value of the property to \c newValue without checking restrictions
   */
  def assignUnchecked(newValue: T): Unit = this.synchronized {
    if (Neq(value, newValue)) {
      value = newValue
      doOnChange(newValue)
    }
  }
  /** Restrictions on the value of this property. */
  val restrictions: mutable.ListBuffer[Restriction[T]] = mutable.ListBuffer()

  /**
   * @retval Right(()) when @c value satisfies all ::restrictions set for this property
   *  @retval Left(errormessage) with the error message of one of the restrictions violated otherwise
   */
  def validationMessages(value: T): List[String] = this.synchronized {
    restrictions.toList.flatMap(_.validationMessage(value))
  }

  def defaultEditor: EditControl[T] = {
    value match {
      case _: Boolean ⇒ new BoolEdit(this.asInstanceOf[Property[Boolean]]).asInstanceOf[EditControl[T]]
      case _: Byte ⇒ new TextEdit(this.asInstanceOf[Property[Byte]], _.toByte).asInstanceOf[EditControl[T]]
      case _: Short ⇒ new TextEdit(this.asInstanceOf[Property[Short]], _.toShort).asInstanceOf[EditControl[T]]
      case _: Int ⇒ new TextEdit(this.asInstanceOf[Property[Int]], _.toInt).asInstanceOf[EditControl[T]]
      case _: Long ⇒ new TextEdit(this.asInstanceOf[Property[Long]], _.toLong).asInstanceOf[EditControl[T]]
      case _: Float ⇒ new TextEdit(this.asInstanceOf[Property[Float]], _.toFloat).asInstanceOf[EditControl[T]]
      case _: Double ⇒ new TextEdit(this.asInstanceOf[Property[Double]], _.toDouble).asInstanceOf[EditControl[T]]
      case _: String ⇒ new TextEdit(this.asInstanceOf[Property[String]],
        x ⇒ (if (x == "(null)") null else x), (x: String) ⇒ (if (x == null) "(null)" else x)).asInstanceOf[EditControl[T]]
    }
  }
}