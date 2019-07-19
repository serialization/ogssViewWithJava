package qq.editor.queries

/** Thrown by queries when they would have to build the full Cartesian product of two sets */
class ExpensiveQueryException(message: String, cause: Throwable) extends Exception(message) {
  if (cause != null) initCause(cause)
  def this(message: String) = this(message, null)
  def this() = this(null, null)
}