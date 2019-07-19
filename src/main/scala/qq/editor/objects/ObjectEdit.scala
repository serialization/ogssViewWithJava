package qq.editor.objects

import ogss.common.java.api;
import ogss.common.java.internal;
import ogss.common.java.internal.fieldTypes;
import swing.Label;
import qq.util.Swing.VBoxD;
import scala.collection.JavaConverters._ 

/** Edit the fields of \c obj. Can be used as the expandable pane below a reference field */
class ObjectEdit[P <: internal.Obj](
  val page: qq.editor.Page,
  val obj: P)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val pool: api.Access[P] = page.file.s.pool(obj).asInstanceOf[api.Access[P]]

  val it = pool.allFields()
  while (it.hasNext()){
    var f = it.next()
    if (!page.file.fieldPreferences(f).isDeleted) {
      contents += new FieldEdit(page, pool, obj, f)
    }
  }
  
}
/** Top level version of ObjectEdit, comes with scroll bars, title, and detects whether the object was deleted. */
class TopObjectEdit[P <: internal.Obj](
  val page: ObjectPage,
  val obj: P)
    extends qq.util.VScrollPane {
  
  
  def updateContents(): Unit = {
    if (page.file.deletedObjects.contains(obj)) {
      contents = VBoxD(
          new Label(page.file.idOfObj(obj)),
          new Label("this object has been deleted"), 
          swing.Swing.VGlue)
    }  else {
      contents = VBoxD(
          new Label(page.file.idOfObj(obj)),
          new ObjectEdit(page, obj),
          swing.Swing.VGlue)
    }
  }
  // refresh contents when this object is deleted or re-created
  val onEditHandler = (e: qq.editor.Edit[_]) => e match {
    case c: qq.editor.CreateObject[_] if c.obj == obj => updateContents
    case d: qq.editor.DeleteObject[_] if d.obj == obj => updateContents
    case _ => ()
  }
  page.file.onEdit.weak += onEditHandler
  
  updateContents
}