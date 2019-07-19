package qq.graph

import ogss.common.java.api
import ogss.common.java.internal
import ogss.common.java.internal.FieldType
import ogss.common.java.internal.fieldTypes._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.util.Vector
import qq.editor.File
import qq.util.FlattenedMap

/** the thing a link in the graph represents */
abstract class AbstractEdge {
  /** start node in the abstract sense (before it has a position…) */
  def getTo: AbstractNode
  /** end node */
  def getFrom: AbstractNode
  /** label for the edge */
  def textLabel(file: File): String
  /** prefereed direction of the edge in `file` */
  def idealDirection(file: File): Vector
  /** decoration at the to-end of the edge */
  def toDecoration: EdgeDecoration = SmallArrowDecoration
}

/** edge representing a skill field */
case class SkillFieldEdge[T](
    val from: internal.Obj,
    val field: api.FieldDeclaration[T])
    extends AbstractEdge {
  
  val to = field.get(from)
  
  override def hashCode = 17 + 31 * java.util.Objects.hash(from, field) + (if(to != null) to.hashCode() else 0)
  override def equals(that: Any) = that match {
    case that: SkillFieldEdge[T] =>
      from == that.from && field == that.field && to == that.to
    case _ => false
  }
  
  override def getFrom = new SkillObjectNode(from)
  override def getTo = field.`type`.asInstanceOf[FieldType[_]] match {
    case u: internal.Pool[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[t]) else new NullNode(from, field)
    case a: internal.AnyRefType => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new NullNode(from, field)
    case c: ArrayType[e] => new ListNode(from, field.asInstanceOf[api.FieldDeclaration[Buffer[e]]])
    case c: ListType[e] => new ListNode(from, field.asInstanceOf[api.FieldDeclaration[Buffer[e]]])
    case c: SetType[e] => new SetNode(from, field.asInstanceOf[api.FieldDeclaration[HashSet[e]]])
    case c: MapType[k,v] => new MapNode(from, field.asInstanceOf[api.FieldDeclaration[HashMap[k,v]]])
    case _:I8| _:I16| _:I32| _:I64| _:V64| _:F32| _:F64| _:internal.StringPool| _:BoolType =>
      new ValueNode(from, field)
  }
  override def textLabel(file: File) = field.name
  
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
  
 }
  
/** edge from a list node to the list member */
case class ListMemberEdge[E, C[E] <: Buffer[E]](
    val from: internal.Obj,
    val field: api.FieldDeclaration[C[E]],
    val index: Int)
    extends AbstractEdge {
  
  override def getFrom = new ListNode(from, field)
  override def getTo = field.`type`.asInstanceOf[SingleArgumentType[_,_]].base match {
    case u: internal.Pool[t] => if (field.get(from)(index) != null) new SkillObjectNode(field.get(from)(index).asInstanceOf[t]) else new ListNullNode(from, field, index)
    case a: internal.AnyRefType => if (field.get(from)(index) != null) new SkillObjectNode(field.get(from)(index).asInstanceOf[internal.Obj]) else new ListNullNode(from, field, index)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case _:I8| _:I16| _:I32| _:I64| _:V64| _:F32| _:F64| _:internal.StringPool| _:BoolType =>
      new ListValueNode(from, field, index)
  }
    
    
  override def textLabel(file: File) = index.toString()
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}

/** edge from a set node to the set member */
case class SetMemberEdge[E, C[E] <: HashSet[E]](
    val from: internal.Obj,
    val field: api.FieldDeclaration[C[E]],
    val index: E)
    extends AbstractEdge {
  
  override def getFrom = new SetNode(from, field)
  override def getTo = field.`type`.asInstanceOf[SingleArgumentType[_,_]].base match {
    case u: internal.Pool[t] => if (index != null) new SkillObjectNode(index.asInstanceOf[internal.Obj]) else new SetNullNode(from, field)
    case a: internal.AnyRefType => if (index != null) new SkillObjectNode(index.asInstanceOf[internal.Obj]) else new SetNullNode(from, field)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case _:I8| _:I16| _:I32| _:I64| _:V64| _:F32| _:F64| _:internal.StringPool| _:BoolType =>
      new SetValueNode(from, field, index)
  }
  override def textLabel(file: File) = ""
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}

/** edge from a map node to the map member */
case class MapMemberEdge[K, V, C[K,V] <: HashMap[K,V]](
    val from: internal.Obj,
    val field: api.FieldDeclaration[C[K,V]],
    val index: Seq[Any])
    extends AbstractEdge {
  
  private def fieldType = field.`type`.asInstanceOf[MapType[K,V]]
  private def to = FlattenedMap.get(field.get(from), fieldType, index) 
  private def valueGroundType = FlattenedMap.typeList(fieldType).last

  override def getFrom = new MapNode(from, field)
  override def getTo = valueGroundType match {
    case u: internal.Pool[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new MapNullNode(from, field, index)
    case a: internal.AnyRefType => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new MapNullNode(from, field, index)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case _:I8| _:I16| _:I32| _:I64| _:V64| _:F32| _:F64| _:internal.StringPool| _:BoolType =>
      new MapValueNode(from, field, index)
  }
  override def textLabel(file: File) = index.map {
    case o: internal.Obj => file.idOfObj(o)
    case x => x
         }.mkString(", ")
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}