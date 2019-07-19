package qq.util

import ogss.common.java.internal.FieldType
import ogss.common.java.internal.fieldTypes._
import scala.collection.mutable.HashMap

/** functions for treating a nested map as map from tupels to result type */
object FlattenedMap {
  /** flatten the nested map type into a list of ground types */
  def typeList(τ: MapType[_, _]): Seq[FieldType[_]] = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        τ.keyType +: typeList(τ2)
      case _ ⇒
        Seq(τ.keyType, τ.valueType)
    }
  }
  /** number of entries in a map (complete ones) */
  def size(m: HashMap[_, _], τ: MapType[_, _]): Int = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        m.map(e ⇒ size(e._2.asInstanceOf[HashMap[_, _]], τ2)).sum
      case _ ⇒
        m.size
    }
  }
  /** enumerate all key tuples */
  def keys(m: HashMap[_, _], τ: MapType[_, _]): Iterable[Seq[Any]] = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        m.flatMap(e ⇒ keys(e._2.asInstanceOf[HashMap[_, _]], τ2).map(f ⇒ e._1 +: f))
      case _ ⇒
        m.keys.map(Seq(_))
    }
  }

  def get[K,V](m: HashMap[K, V], τ: MapType[K, V], key: Seq[Any]): Any = {
    τ.valueType match {
      case τ2: MapType[l, w] ⇒
        get(m(key.head.asInstanceOf[K]).asInstanceOf[HashMap[l, w]], τ2, key.tail)
      case _ ⇒
        m(key.head.asInstanceOf[K])
    }
  }
  def contains[K,V](m: HashMap[K, V], τ: MapType[K, V], key: Seq[Any]): Boolean = {
    τ.valueType match {
      case τ2: MapType[l, w] ⇒
        m.contains(key.head.asInstanceOf[K]) && contains(m(key.head.asInstanceOf[K]).asInstanceOf[HashMap[l, w]], τ2, key.tail)
      case _ ⇒
        m.contains(key.head.asInstanceOf[K])
    }
  }
  def set(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any], value: Any): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        set(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail, value)
      case _ ⇒
        m(key.head) = value
    }
  }
  def insert(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any], value: Any): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        if (!m.contains(key.head)) {
          m(key.head) = new HashMap()
        }
        insert(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail, value)
      case _ ⇒
        m(key.head) = value
    }
  }
  def remove(m: HashMap[Any, Any], τ: MapType[_, _], key: Seq[Any]): Unit = {
    τ.valueType match {
      case τ2: MapType[_, _] ⇒
        remove(m(key.head).asInstanceOf[HashMap[Any, Any]], τ2, key.tail)
        if (m(key.head).asInstanceOf[HashMap[Any, Any]].size == 0) {
          m.remove(key.head) 
        }
      case _ ⇒
        m.remove(key.head)
    }
  }
}