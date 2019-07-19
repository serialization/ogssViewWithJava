package qq.util.binding

/** Restricts the valid values of a [[Property]]. */
abstract class Restriction[T] {
  /** @retval Right(()) if @c value satisfies the restriction,
   *  @retval Left(conditionMessage) otherwise */
  def validationMessage(value: T): Option[String] = if (test(value)) None else Some(conditionMessage)
  /** predicate that evaluates to true for all values that satisfy the restriction */
  val test: T => Boolean
  /** human readable version of the condition that this restriction is enforcing */
  val conditionMessage: String
}

/** A [[Restriction]] was violated; `message` can be shown to the user. */
class RestrictionException(message: String) extends scala.IllegalArgumentException(message) {
  
}

object Restriction {
  def apply[T](test0: T => Boolean, message0: String) = {
    new Restriction[T] {
      val test = test0
      val conditionMessage = message0
    }
  }
  def positive[T](implicit num: Numeric[T]): Restriction[T] =
    Restriction(num.gt(_, num.zero),"must be positive")
  def positiveOrZero[T](implicit num: Numeric[T]): Restriction[T] =
    Restriction(num.gteq(_, num.zero),"must be positive or zero ")
  def min[T](lowerBound: T)(implicit ord: Ordering[T]): Restriction[T] =
    Restriction(ord.gteq(_, lowerBound), "must be at least "+ lowerBound)
  def max[T](upperBound: T)(implicit ord: Ordering[T]): Restriction[T] = 
    Restriction(ord.lteq(_, upperBound), "must be at most "+ upperBound)

}