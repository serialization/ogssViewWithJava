package qq.editor.queries

import scala.collection.JavaConverters._

/** Search query returning all objects */
class AllObjectQuery(
              val file0: qq.editor.File,
                val variable: String) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = { 
    for (
      p <- file.s.allTypes.asScala.toIterator if !file.typePreferences(p).isDeleted  ;
      o ← p.typeOrderIterator.asScala if !file.deletedObjects.contains(o)
    ) yield Map(variable -> o)
  }
  override def prepare(assigned: Seq[String]) = ()
  override def find(assigment: Map[String, Any]) = {
      Iterator(assigment)
  }

  override def costSizeEstimate = {
    /* iterate over n instances and return all of them */
    val n =  file.rootTypes.map(_.size.toDouble).sum
    (n, n)
  }
  override def costSizeEstimate(assigned: Seq[String]) = {
    if (!assigned.contains(variable)) {
      (Double.PositiveInfinity, Double.PositiveInfinity)
    } else {
      /* check whether bound value is an instance, assume to filter out 9 of 10 */
      (1.0, 0.1)
    }
  }
}

object AllObjectQuery {
  def apply(file: qq.editor.File,
            s: Term) = { 
    s match {
      case v: VarTerm => new AllObjectQuery(file, v.variable)
      case _ => throw new Exception("Variable expected")
    }
  }
}
