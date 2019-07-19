package qq.util.binding

/** nice messages for usual errors during validation and conversion from string */
object ValidationExceptionMessage {
  def apply(e: Exception): String = e match {
    case e: RestrictionException => e.getMessage
    case e: java.lang.NumberFormatException => "Invalid number format"
    case e => e.toString()
  }
  
  
}