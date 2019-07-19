package qq.editor

import ogss.common.java.api;
import ogss.common.java.internal;
import scala.collection.mutable;
import scala.collection.mutable.HashMap;
import scala.collection.mutable.HashSet;
import scala.collection.JavaConverters._ 

/** Represents a ogss file while opened in the editor */
class File(
    /** The name of the ogss file this is about (may change with save as) */
    var fileName: java.nio.file.Path) {

  private def pathAndName = {
    val dirnamepattern = "^(?:(.*)[\\\\/])?([^\\\\/]+)$".r
    fileName.toAbsolutePath().toString() match { case dirnamepattern(d, n) ⇒ (d, n) }
  }

  def fileNameOnly: String = pathAndName._2
  /** Window title: filename, path, programme name, and a plus sign when modified (like gvim) */
  def windowTitle: String = {
    val (d, n) = pathAndName
    n + (if (isModified) " + " else "") + " (" + d + ") – OGSS edit"
  }
  /** the ogss state for file [[fileName]] */
  val s: empty.OGFile = empty.OGFile.open(fileName, api.Mode.Read, api.Mode.Write)
  
  /** Undo/redo queue of the changes that were applied to the file after it was opened or saved */
  val undoManager = new qq.util.UndoManager()

  /** true if file was modified, i.e. there is something to save */
  def isModified: Boolean = {
    /* TODO either we clear the undo queue when saving, or we need to add a dummy save event */
    undoManager.canUndo()
  }
  /** event that fires when the value of modified changes, probably a lot more often */
  val onModifiednessChange: qq.util.binding.Event[Boolean] = new qq.util.binding.Event()

  /**
   * event that fires whenever the file is edited
   */
  val onEdit: qq.util.binding.Event[qq.editor.Edit[_]] = new qq.util.binding.Event;

  /**
   * Perform an [[qq.editor.Edit]] and notify the rest of the programme about it
   */
  def modify_(e: qq.editor.Edit[_]): Unit = {
    e.doIt()
    onEdit.fire(e)
    onModifiednessChange.fire(isModified)
  }

  /**
   * Perform a [[qq.editor.UserEdit]], add it to the undo-queue, and notify the rest of
   * the programme about the change.
   *
   * This should only be called from the constructors of UserEdits (should probably be protected, somehow)
   */
  def modify(e: qq.editor.UserEdit[_]): Unit = {
    val e2 = e.toEdit
    e2.doIt()
    undoManager.addEdit(e)
    onEdit.fire(e2)
    onModifiednessChange.fire(isModified)
  }

  /**
   * Set of deleted objects.
   *
   * We mark objects as deleted by adding them to a deleted object queue. This
   * allows us to undelete them in a way such that all other edits in the undo/redo
   * queue stay valid.
   *
   * The objects are deleted from the ogss state before it is saved to file
   */
  val deletedObjects: mutable.HashSet[internal.Obj] = new mutable.HashSet()

  /**
   * Set of objects which have fields which violate a restriction.
   *
   * When deleting causes null references in non-null restricted fields, we add
   * the affected field to this list.
   *
   * Deletions are the only things that can violate restrictions in the ogss state.
   * All other modifications check the restrictions during the creation of the
   * user edit and reject the modification.
   */
  val validationErrors: mutable.HashSet[Tuple2[internal.Obj, api.FieldDeclaration[_]]] = new mutable.HashSet()

  /**
   * List of newly created objects.
   *
   * created objects do not have a SkillId; we give them temporary negative ones;
   * item 0 has ID -1 and so on
   */
  val createdObjects: mutable.ListBuffer[internal.Obj] = new mutable.ListBuffer()
  /** Lookup table: surrogate SkillIDs for newly created objects */
  val createdObjectId: mutable.HashMap[internal.Obj, Int] = new mutable.HashMap()
  /** add a newly created object to [[createdObjects]] and [[createdObjectId]]*/
  def registerCreatedObject(o: internal.Obj): Unit = {
    createdObjects.synchronized {
      val id = -1 - createdObjects.size
      createdObjects += o
      createdObjectId(o) = id
    }
  }

  /* some auxiliary functions about types */

  /** parentType(a) = b if a is directly derived from b, a ∉ Dom(parentType) if a is a root type */
  val parentType: HashMap[api.Access[_ <: internal.Obj], api.Access[_ <: internal.Obj]] = new HashMap()
  /** a ∈ childTypes(b) if a is directly derived from b */
  val childTypes: HashMap[api.Access[_ <: internal.Obj], Seq[api.Access[_ <: internal.Obj]]] = new HashMap()
  /** a ∈ rootTypes if ~(∃x)(x = parentType(a)) */
  val rootTypes: HashSet[api.Access[_ <: internal.Obj]] = new HashSet()

  /** Update [[parentType]], [[childTypes]], and [[rootTypes]]. Necessary after save. */
  private def updateTables(): Unit = {
    parentType.clear()
    for (t ← s.allTypes.asScala) {
      if (t.superType() != null) {       
        parentType(t) = s.pool(t.superType().name()).asInstanceOf[api.Access[_ <: internal.Obj]]
      }
    }
    childTypes.clear()

    childTypes ++= s.allTypes.asScala.toSeq.groupBy(parentType.getOrElse(_, null))
    rootTypes.clear()
    for (t ← s.allTypes.asScala if !(parentType contains t)) rootTypes += t
  }
  updateTables

  /** baseType(a) = b if b ∈ parentType*(a) ∩ rootTypes */
  def baseType(a: api.Access[_ <: internal.Obj]) = a.asInstanceOf[internal.Pool[_]].basePool.asInstanceOf[api.Access[_]]
  /** parentType+ */
  def superTypes(a: api.Access[_ <: internal.Obj]): List[api.Access[_ <: internal.Obj]] = {
    if (parentType.contains(a)) {
      val p = parentType(a)
      p :: superTypes(p)
    } else {
      Nil
    }
  }

  /** Get the ogss object of a given type with the given ogss ID. */
  def objOfId[T <: B, B <: internal.Obj](pool: api.Access[T], id: Int): internal.Obj = {
    var o = if (id > 0) {
      val bp = pool.asInstanceOf[internal.Pool[T]].basePool
      if (id > bp.size) {
        throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
      }
      bp.get(id - 1)
    } else if (id < 0) {
      createdObjects(-1 - id)
    } else {
      throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
    }
    // why cast required now but not previously??
    if (s.pool(o.asInstanceOf[internal.Obj]) == pool || superTypes(s.pool(o.asInstanceOf[internal.Obj])).contains(pool)) {
      o.asInstanceOf[internal.Obj]
    } else {
      throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
    }
  }

  /** Get the ogss object identified by string with format `type#ID`. */
  def objOfId(x: String): internal.Obj = {
    val xt = x.trim()
    if (x.trim().equals("(null)")) return null
    val xts = xt.split("#")
    if (xts.size != 2) throw new qq.util.binding.RestrictionException("format error, expected format type#number")
    val pn = xts(0)
    val id = try Integer.parseInt(xts(1)) catch {
      case _: java.lang.NumberFormatException ⇒ throw new qq.util.binding.RestrictionException("format error, expected format type#number")
    }
    val pool = try { s.pool(pn) } catch {
      case e: java.util.NoSuchElementException ⇒
        throw new qq.util.binding.RestrictionException(s"Unknown type in $pn#$id")
    }
    objOfId(s.pool(pn), id)
  }

  /**
   * Get a string, `type#ID` or `(null)`, that identifies ogss object `o`.
   *  @param stropped add apostrophes to the type name (make sure, that it is not mistaken for a keyword when used in the serch function)
   */
  def idOfObj(o: internal.Obj, stropped: Boolean = false): String = {
    if (o == null) {
      "(null)"
    } else if (o.ID == -1) {
      if (stropped) {
        s"'${s.pool(o).name()}'#${createdObjectId(o)}"
      } else {
        s"${s.pool(o).name()}#${createdObjectId(o)}"
      }
    } else {
//      if (stropped) {
        s"'${s.pool(o).name()}'#${o.ID}"
//      } else {
//        o.prettyString(s)
//      }
    }
  }
  /** field definition and type that it belongs to for each field name */
  val fieldsByName: Map[String, Seq[Tuple2[api.Access[_], api.FieldDeclaration[_]]]] = 
    // (for (t ← s; f ← t.fields) yield (f.name, (t, f))).groupBy(_._1).mapValues(_.map(_._2))
    
     (for (t ← s.allTypes().asScala.toSeq; f ← t.fields.asScala) yield (f.name, (t, f))) // seq? of tuples (name, (type, fielddecl.)) // jedertyp mit all seinen feldern vertuplen. 
       .groupBy(_._1)  // Map ( name -> Seq[(name, (type, fielddecl.))] )
       .mapValues(x => x.map(_._2)) // Map (name -> Seq[(type, fielddecl.)])
     

  /* type and field preferences for this file */
  /** Preferences for all user types */
//  val typePreferences: HashMap[api.Access[internal.Obj], TypePreferences[internal.Obj]] = new HashMap()
  val typePreferences: HashMap[api.Access[_], TypePreferences[_]] = new HashMap()

  for (t ← s.allTypes().asScala) typePreferences(t.asInstanceOf[api.Access[internal.Obj]]) = new TypePreferences(t.asInstanceOf[api.Access[internal.Obj]], this)

  /** Preferences for all FieldDeclarations */
  val fieldPreferences: Map[api.FieldDeclaration[_], FieldPreferences[_, _]] =
    (for (t ← typePreferences.values; fd ← t.typ.fields.asScala) yield (fd, t.fields(fd).asInstanceOf[FieldPreferences[_, internal.Obj]])).toMap

  /** Save changes to disk */
  def flush(): Unit = {
    // finally delete the deleted objects (no undo after this)
    deletedObjects.foreach(s.delete(_))
    s.flush()
    deletedObjects.clear()
    // created objects have a real ID, now. Remove surrogate IDs
    createdObjectId.clear()
    createdObjects.clear()
    // hash codes of Access[]es have changed
    updateTables
    val tss = new mutable.ListBuffer[TypePreferences[_]]()
    for (t ← typePreferences.values) tss += t
    typePreferences.clear()
    for (ts ← tss) { println(ts.typ); typePreferences(ts.typ) = ts }
    for (fs ← fieldPreferences.values) fs.checkDeleted()
  }
}