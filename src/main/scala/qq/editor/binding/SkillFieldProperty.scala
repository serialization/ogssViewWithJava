package qq.editor.binding

/** Base class for all properties that allow editor UI elements to bind to (elements of) fields in as Skill file.
 *  
 * Property is only extended by exporting the groundType of the (element of the) field.
 * This in necessary because one (or I) can't tell the nulls of different SKilL user types apart with
 * the Scala type system. */
abstract class SkillFieldProperty[T](
    owner0: qq.util.binding.PropertyOwner,
    name0: String,
    init0: T) extends qq.util.binding.Property[T](owner0, name0, init0) {

  /** SKilL field type of the field or member of a container field that is modified by this property. */
  def groundType: ogss.common.java.api.FieldType[T]
}

class PromptSkillFieldProperty[T](
    owner0: qq.util.binding.PropertyOwner,
    name0: String,
    init0: T,
    val groundType0: ogss.common.java.api.FieldType[T]) extends SkillFieldProperty[T](owner0, name0, init0) {

  def groundType = groundType0

}

