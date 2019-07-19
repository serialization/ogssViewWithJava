package qq.editor.queries

import ogss.common.java.internal;

/** A query returning a specific object */
class IdQuery(
    val file0: qq.editor.File,
    val variable: String,
    val obj: internal.Obj) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = {
    Seq(Map(variable -> obj)).toIterator
  }
  override def prepare(assigned: Seq[String]) = ()
  override def find(assignment: Map[String, Any]) = {
    /* we must ensure (outside) that b contains variable: we want to disallow the generation of full cartesian products */
    if (assignment(variable).isInstanceOf[internal.Obj] && obj == assignment(variable)) {
      Iterator(assignment)
    } else {
      Iterator()
    }
  }

  override def costSizeEstimate = {
    /* just return one thing */
    (1.0, 1.0)
  }
  override def costSizeEstimate(assigned: Seq[String]) = {
    if (!assigned.contains(variable)) {
      (Double.PositiveInfinity, Double.PositiveInfinity)
    } else {
      /* check whether bound object is this, assume to filter out 9 of 10 */
      (1.0, 0.1)
    }
  }
}