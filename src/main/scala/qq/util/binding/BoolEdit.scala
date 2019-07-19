package qq.util.binding

/**
 * Unlabled edit control for a boolean property with two radio buttons. Looks better
 * then a check box when combined with text edits
 */
class BoolEdit(p: Property[Boolean])
    extends EditControl[Boolean](p) with swing.Container.Wrapper with swing.SequentialContainer.Wrapper {
  override lazy val peer = {
    val p = new javax.swing.JPanel with SuperMixin
    val l = new javax.swing.BoxLayout(p, swing.Orientation.Horizontal.id)
    p.setLayout(l)
    p
  }
  
  val bg = new swing.ButtonGroup
  val cbFalse = new swing.RadioButton("")
  cbFalse.action = swing.Action("false") {editValue.doOnChange(editValue())}
  val cbTrue = new swing.RadioButton("")
  cbTrue.action = swing.Action("true") {editValue.doOnChange(editValue())}
  bg.buttons ++= Seq(cbFalse, cbTrue)
  contents ++= Seq(cbFalse, cbTrue, swing.Swing.HGlue)
  
  def propertyToComponent(x: Boolean): Unit = {
    if (cbTrue.selected != x || cbFalse.selected == x) {
      cbTrue.selected = x
      cbFalse.selected = !x
      editValue.doOnChange(Right(x))
    }
  }
  val editValue = new Observable[Either[List[String], Boolean]] {
    override def apply() = {
      val x = cbTrue.selected
      property.validationMessages(x) match {
        case Nil =>
          Right(x)
        case xs =>
          Left(xs)
      }
    }
  }
  /* update property when UI changes (only if updateImmediately is set) */
  editValue.onChange.strong += {
    case Right(x) =>
      if (updateImmediately) componentToProperty
    case Left(_) => ()
  }
  
  private val normalBackground = background
  isValid.onChange.strong += (vld => background = if (vld) normalBackground else java.awt.Color.red)

  validationMessages.onChange.strong += (vms => tooltip = vms.take(2).mkString("; "))
  
  override def enabled: Boolean = cbFalse.enabled
  override def enabled_=(x: Boolean): Unit = {cbFalse.enabled_=(x);cbTrue.enabled_=(x)}
  
  
  propertyToComponent(p())
}