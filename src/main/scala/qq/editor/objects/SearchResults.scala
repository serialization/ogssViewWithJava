package qq.editor.objects

import scala.collection.mutable;
import ogss.common.java.api;
import ogss.common.java.internal;

/** Swing UI element for entering a search query and displaying a list of search results.
 *  
 *  Selecting a search result opens it in `page`; the search result is intended to be part of `page`. */
class SearchResults(val page: ObjectPage)
    extends swing.BoxPanel(swing.Orientation.Vertical)
    with qq.util.binding.PropertyOwner {

  val queryText = new qq.util.binding.Property[String](this, "Query", "")
  val queryEdit = queryText.defaultEditor

  val queryError = new swing.Label() { visible = false }

  var query: qq.editor.queries.Query = null
  var resultsIterator: Iterator[Map[String, Any]] = null
  var resultsRetrieved: mutable.ArrayBuffer[Map[String, Any]] = null

  var displayOffset: Int = 0
  var pageLength: Int = 50

  var resultsTable: swing.Table = null
  val resultsPart = new swing.BoxPanel(swing.Orientation.Vertical)

  /**
   * read enough elements from resultsIterator to show current page
   */
  private def readResults() = {
    while (resultsIterator.hasNext
      && resultsRetrieved.size < displayOffset + pageLength) {
      resultsRetrieved += resultsIterator.next
    }
  }

  val searchAction = new swing.Action("Search") {
    override def apply(): Unit = {
      try {
        query = qq.editor.queries.Query.parse(page.file, queryText())
        queryError.visible = false
        resultsIterator = query.find()
        resultsRetrieved = new mutable.ArrayBuffer()
        displayOffset = 0
        showPage
        if (resultsRetrieved.size > 0) {
          // open first result automatically
          resultsTable.peer.setRowSelectionInterval(0, 0)
        }
        if (resultsRetrieved.size == 1) {
          /** hide list of search results when there is only one result.
           *  
           *  TODO: is nice when opened from programme, but when the user typed the
           *    query themselves, at least the query should stay visible (but with
           *    the current layout, the query can not stay visible without the list
           *    of results)*/
          page.searchVisibleModel.setSelected(false)
          page.updateVisibility
        }
      } catch {
        case e: Exception ⇒
//          val text = e.getMessage //stiesssh : here happens NPE
//          queryError.text = if (text != null) text else e.toString()
//          queryError.visible = true
          throw e;
      }
      queryError.revalidate()
    }
  }
  queryText.onChange.strong += (_ ⇒  searchAction())

  val searchButtonAction = new swing.Action("Search") {
    override def apply(): Unit = {
      if (queryEdit.isModified()) {
        // when called from button: update property from text field and return; search is then called through property change handler
        queryEdit.componentToProperty()
        
      } else {
        // re-run search
        searchAction()
      }
    }
  }
  
  val lblPagePos = new swing.Label("-") //indeed the page position at the bottom of the left most panel

  def showPage: Unit = {
    readResults
    if (resultsTable != null) { deafTo(resultsTable.selection) }

    val visibleData = resultsRetrieved
      .drop(displayOffset)
      .take(pageLength)
    resultsTable = new swing.Table(
      visibleData
        .map { x ⇒
          query.variables.map { v ⇒
            if (x(v).isInstanceOf[internal.Obj])
              page.file.idOfObj(x(v).asInstanceOf[internal.Obj])
            else if (x(v) == null) "(null)" else x(v).toString().asInstanceOf[Any] // no boolean checkbox magic
          }.toArray
        }.toArray,
      query.variables) {
      //  model = new javax.swing.table.DefaultTableModel() {
      //    override def isCellEditable(row: Int, col: Int) = false
      //  }
    }
    listenTo(resultsTable.selection)

    resultsPart.contents.clear()
    resultsPart.contents += new swing.ScrollPane(resultsTable)

    lblPagePos.text = s"$displayOffset to ${displayOffset + visibleData.size - 1}"
    pgUpAct.enabled = displayOffset > 0
    pgDnAct.enabled = displayOffset + pageLength <= resultsRetrieved.size || resultsIterator.hasNext
  }

  reactions += {
    case swing.event.TableRowsSelected(source, range, false) ⇒
      val i = source.selection.rows.leadIndex
      if (i >= 0) {
        val row = resultsRetrieved(displayOffset + i)
          .values
          .filter(_.isInstanceOf[internal.Obj])
          .map(_.asInstanceOf[internal.Obj])
        page.goTo(new page.View(row.head, row.drop(1)))
      }
  }

  private val pgDnAct = new swing.Action("Next Page") {
    icon = new qq.icons.ForwardIcon(true, false)
    override def apply() {
      if (resultsIterator.hasNext) {
        displayOffset += pageLength
        showPage
      }
    }
  }
  private val pgUpAct = new swing.Action("Previous Page") {
    icon = new qq.icons.BackIcon(true, false)
    override def apply() {
      if (displayOffset > 0) {
        displayOffset -= displayOffset min pageLength
        showPage
      }
    }
  }

  private val pgUpBtn = new qq.util.PlainButton(pgUpAct) {
    text = ""
    this.disabledIcon = new qq.icons.BackIcon(false, false)
  }
  private val pgDnBtn = new qq.util.PlainButton(pgDnAct) {
    text = ""
    this.disabledIcon = new qq.icons.ForwardIcon(false, false)
  }

  pgUpAct.enabled = false
  pgUpAct.enabled = false

  import qq.util.Swing._;
  import swing.Swing.HGlue
  contents += VBoxD(
    HBoxD(queryEdit, new swing.Button(searchButtonAction)),
    queryError,
    HBoxD(HGlue, lblPagePos, HGlue, pgUpBtn, pgDnBtn),
    resultsPart)
}