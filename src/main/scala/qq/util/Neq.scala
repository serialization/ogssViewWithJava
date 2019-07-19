package qq.util

/* ≠ that gets rid of the special treatment of floats: 0.0==-0.0 and NaN!=NaN, but Neq(0.0,-0.0) and ¬Neq(x,x) even if x is a NaN. */
object Neq {
  def apply[T](l: T, r: T): Boolean = {
    if (l == null) {
      r != null
    } else {
      !l.equals(r)
    }
  }  
  
}