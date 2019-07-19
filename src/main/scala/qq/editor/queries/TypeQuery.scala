package qq.editor.queries

import ogss.common.java.api;
import ogss.common.java.internal;
import scala.collection.JavaConverters._;

/** Search query returning all members of type `pool` including subtypes. */
class TypeQuery(val file0: qq.editor.File,
                val variable: String,
                val pool: api.Access[_ <: internal.Obj]) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = { 
    for (
      o ← pool.typeOrderIterator.asScala if !file.deletedObjects.contains(o)
    ) yield Map(variable -> o)
  }
  
  override def prepare(assigned: Seq[String]) = ()
  override def find(assigment: Map[String, Any]) = {
    /* we must ensure (outside) that b contains variable: we want to disallow the generation of full cartesian products */
    if (assigment(variable).isInstanceOf[internal.Obj]
      && (file.superTypes(file.s.pool(assigment(variable).asInstanceOf[internal.Obj])).contains(pool))
        || file.s.pool(assigment(variable).asInstanceOf[internal.Obj]) == pool) {
      Iterator(assigment)
    } else {
      Iterator()
    }
  }

  override def costSizeEstimate = {
    /* iterate over n instances and return all of them */
    val n = pool.size
    (n.toDouble, n.toDouble)
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

object TypeQuery {
  def apply(file: qq.editor.File,
            s: Term,
            pool: api.Access[_ <: internal.Obj]) = { 
    s match {
      case v: VarTerm => new TypeQuery(file, v.variable, pool)
      case _ => throw new Exception("Variable expected")
    }
  }
}
