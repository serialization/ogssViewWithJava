package qq.util.binding

/**
 * Labled edit control for a boolean property using a check box
 */
class CheckBoxEdit(p: Property[Boolean])
    extends EditControl[Boolean](p) with swing.Container.Wrapper with swing.SequentialContainer.Wrapper {
  override lazy val peer = {
    val p = new javax.swing.JPanel with SuperMixin
    val l = new javax.swing.BoxLayout(p, swing.Orientation.Horizontal.id)
    p.setLayout(l)
    p
  }
  
  val cb = new swing.CheckBox(p.name) {
    tooltip = p.description 
  }
  cb.action = swing.Action(p.name) {editValue.doOnChange(editValue())}
  contents ++= Seq(cb, swing.Swing.HGlue)
  
  def propertyToComponent(x: Boolean): Unit = {
    if (cb.selected != x) {
      cb.selected = x
      editValue.doOnChange(Right(x))
    }
  }
  val editValue = new Observable[Either[List[String], Boolean]] {
    override def apply() = {
      val x = cb.selected
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

  validationMessages.onChange.strong += (vms => tooltip = if (vms.length == 0) p.description else vms.take(2).mkString("; "))
  
  override def enabled: Boolean = cb.enabled
  override def enabled_=(x: Boolean): Unit = cb.enabled_=(x)
  
  
  propertyToComponent(p())
}