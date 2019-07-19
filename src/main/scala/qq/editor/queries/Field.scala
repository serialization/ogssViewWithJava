package qq.editor.queries

import ogss.common.java.api;
import ogss.common.java.internal;
import scala.collection.JavaConverters._;


/**
 * A field as used in the queries. The user can leave the type unspecified, in which
 * case a field may correspond to different fields in different user types
 */
abstract class Field {
  /** all field declarations this field can refer to, and the pool they belong to */
  def apply(): Iterator[Tuple2[api.Access[_], api.FieldDeclaration[_]]]
  /**
   * all field declarations this field can refer to, and the pool they belong to
   * when this field should be a field of object o (may be empty)
   */
  def apply(o: internal.Obj): Iterator[Tuple2[api.Access[_], api.FieldDeclaration[_]]]
}

/* All fields with the given name in any type. */
class UnspecificField(
  val file: qq.editor.File,
  val name: String)
    extends Field {

  override def apply() = {
    file.fieldsByName(name).toIterator
  }

  def apply(o: internal.Obj) = {
    val relevantTypes = file.s.pool(o) +: file.superTypes(file.s.pool(o))
    this().filter(x ⇒ relevantTypes.contains(x._1))
  }
}

/** The field with the given name in the given type */
class SpecificTypeField(
  val file: qq.editor.File,
  val typeName: String,
  val name: String)
    extends Field {

  override def apply() = {
    val pool = file.s.pool(typeName).asInstanceOf[api.Access[_<:internal.Obj]]
    val field = pool.fields.asScala.find(_.name == name).get
    Iterator((pool, field))
  }

  def apply(o: internal.Obj) = {
    val relevantTypes = file.s.pool(o) +: file.superTypes(file.s.pool(o))
    this().filter(x ⇒ relevantTypes.contains(x._1))
  }
}  
  