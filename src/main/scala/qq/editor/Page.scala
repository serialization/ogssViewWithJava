package qq.editor

import scala.swing;

/** A page that displays something from file.
 *  
 *  Extends a page of a tabbed pane with menu items for some of the menus of the application.
 *  
 *  @param preferences global preferences of the editor (TODO when it needs the preferences and the file, why not pass the whole Main?) */
abstract class Page(val file: File,
     val preferences: EditorPreferences) extends qq.util.TabbedPane.ClosablePage("", new swing.Label("")) {
  /** menu items that this page adds to the view menu */
  def viewMenuItems: Seq[swing.MenuItem]
  /** menu items that this page adds to the type menu */
  def typeMenuItems: Seq[swing.MenuItem]
   /** menu items that this page adds to the object menu */
  def objectMenuItems: Seq[swing.MenuItem]
   
}