package qq.editor.queries

/** A query returning the join of two queries.
 *  Returns  all assignments of variables
 *  which extend assignments from the results sets of both queries. 
 *  
 *  Joins must be cheap, i.e. there must be some way to filter before building
 *  large Cartesian products which our simple minded algorithm must be able to find.
 *  */
class JoinQuery(
  file0: qq.editor.File,
  val l: Query,
  val r: Query)
    extends Query(file0) {

  def variables = (l.variables ++ r.variables).distinct

  private def costLRRLSize() = {
    val (cl, sl) = l.costSizeEstimate()
    val (clrpe, slrpe) = r.costSizeEstimate(l.variables)
    val (cr, sr) = r.costSizeEstimate()
    val (crlpe, srlpe) = l.costSizeEstimate(r.variables)
    val clr = cl + clrpe * sl
    val crl = cr + crlpe * sr
    if (clr < crl)
      (clr, crl, sl * slrpe)
    else
      (clr, crl, sr * srlpe)
  }
  private def costLRRLSize(assigned: Seq[String]) = {
    val (cl, sl) = l.costSizeEstimate(assigned)
    val (clrpe, slrpe) = r.costSizeEstimate((l.variables ++ assigned).distinct)
    val (cr, sr) = r.costSizeEstimate(assigned)
    val (crlpe, srlpe) = l.costSizeEstimate((r.variables ++ assigned).distinct)
    val clr = cl + clrpe * sl
    val crl = cr + crlpe * sr
    if (clr < crl)
      (clr, crl, sl * slrpe)
    else
      (clr, crl, sr * srlpe)
  }

  def costSizeEstimate() = {
    val (clr, crl, s) = costLRRLSize()
    (clr.min(crl), s)
  }

  def costSizeEstimate(assigned: Seq[String]) = {
    val (clr, crl, s) = costLRRLSize(assigned)
    (clr.min(crl), s)
  }

  override def find() = {
    val (clr, crl, s) = costLRRLSize()
    if (clr < crl) {
      r.prepare(l.variables)
      val ls = l.find()
      ls.flatMap(r.find(_))
    } else if (!crl.isInfinite()) {
      l.prepare(r.variables)
      val rs = r.find()
      rs.flatMap(l.find(_))
    } else {
      throw new ExpensiveQueryException
    }
  }

  var lR: Boolean = false
  def prepare(assigned: Seq[String]) = {
    val (clr, crl, s) = costLRRLSize()
    if (clr < crl) {
      lR = true
      l.prepare(assigned)
      r.prepare((assigned ++ l.variables).distinct)
    } else if (!crl.isInfinite()) {
      lR = false
      r.prepare(assigned)
      l.prepare((assigned ++ r.variables).distinct)
    } else {
      throw new ExpensiveQueryException
    }
  }

  override def find(assignment: Map[String, Any]) = {
    if (lR) {
      l.find(assignment).flatMap(r.find(_))
    } else {
      r.find(assignment).flatMap(l.find(_))
    }
  }
}