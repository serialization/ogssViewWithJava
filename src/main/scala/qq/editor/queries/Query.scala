package qq.editor.queries

import ogss.common.java.api;

/**
 * A query that will return a iterable list of assignments of variables to objects or values that satisfy query.
 *
 * Queries will be used to
 * * list all objects in a pool
 * * list all values of a collection
 * * trivial case: retrieve an individual object
 *
 * in the end, it should become a sparql-like query language, but we will see how far we get
 */
abstract class Query(
    /**the file that is queried */
    val file: qq.editor.File) {
  /** the names of the variables that are bound by this query */
  def variables: Seq[String]
  /** return all variable bindings that satisfy this query */
  def find(): Iterator[Map[String, Any]]
  /** return all variable bindings that extend bound and satisfy this query */
  def find(assignment: Map[String, Any]): Iterator[Map[String, Any]]
  /** return a tuple (a,b) where a is an estimate of the run-time and b of the size of the results of find */
  def costSizeEstimate(): Tuple2[Double, Double]
  /** return a tuple (a,b) where a is an estimate of the run-time and b of the size of the results of find which extend a binding that binds the given variables */
  def costSizeEstimate(assigned: Seq[String]): Tuple2[Double, Double]
  /** prepare for find(a), i.e. do things you don't want to do for ecery element */
  def prepare(assigned: Seq[String]): Unit
}

object Query {
  def parse(file: qq.editor.File, x: String): Query = {
    import qq.editor.queries.parser._;
    if (x == "") new AllObjectQuery(file, "object")
    else Parser(file, x)
  }

}