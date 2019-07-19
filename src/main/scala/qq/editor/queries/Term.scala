package qq.editor.queries

/** The terms in the queries, i.e.\ constants and variable. */
abstract class Term {
  def canBeSubject: Boolean
  def canBeObject: Boolean
  def apply(assignment: Map[String, Any]): Any
  def definedIn(assignment: Map[String, Any]): Boolean
}

class VarTerm(val variable: String)
    extends Term {
  override def canBeSubject = true
  override def canBeObject = true
  override def apply(assignment: Map[String, Any]) = assignment(variable)
  override def definedIn(assignment: Map[String, Any]) = assignment.contains(variable)
}

class ConstTerm(val value: Any)
    extends Term {
  override def canBeSubject = value.isInstanceOf[ogss.common.java.internal.Obj]
  override def canBeObject = true
  override def apply(assignment: Map[String, Any]) = value
  override def definedIn(assignment: Map[String, Any]) = true
}