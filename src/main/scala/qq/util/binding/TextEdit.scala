package qq.util.binding

import java.awt.Color.red

/**
 * Unlabled edit control for everything that can be converted to text and back.
 */
class TextEdit[T](p: Property[T],
                  val fromString: String ⇒ T,
                  /** to string function that can cope with nulls */
                  val ntoString: T ⇒ String)
    extends EditControl[T](p) with swing.Container.Wrapper with swing.SequentialContainer.Wrapper {

  def this(p: Property[T], fromString0: String ⇒ T) =
    this(p, fromString0, _.toString())

  override lazy val peer = {
    val p = new javax.swing.JPanel with SuperMixin
    val l = new javax.swing.BoxLayout(p, swing.Orientation.Vertical.id)
    p.setLayout(l)
    p
  }

  val tf = new swing.TextField(ntoString(property())) {
    peer.setMaximumSize(new java.awt.Dimension(Integer.MAX_VALUE, peer.getPreferredSize().height));
  }
  tf.action = swing.Action("") { editValue.doOnChange(editValue()) }

  val ef = new swing.Label("") {
    visible = false
    this.foreground = red
  }
  contents += tf
  contents += ef

  def propertyToComponent(x: T): Unit = {
    val asString = ntoString(x)
    if (tf.text != asString) {
      tf.text = asString
      editValue.doOnChange(Right(x))
    }
  }

  val editValue = new Observable[Either[List[String], T]] {
    override def apply() =
      try {
        val asT = fromString(tf.text)
        property.validationMessages(asT) match {
          case Nil ⇒
            Right(asT)
          case xs ⇒
            Left(xs)
        }
      } catch {
        case e: Exception ⇒
          Left(List(ValidationExceptionMessage(e)))
      }
  }

  /* update property when UI changes (only if updateImmediately is set) */
  editValue.onChange.strong += {
    case Right(x) ⇒
      if (updateImmediately) componentToProperty
    case Left(_) ⇒ ()
  }

  private val normalBackground = tf.background
  isValid.onChange.strong +=
    ((vld: Boolean) ⇒ tf.background = if (vld) normalBackground else red)

  validationMessages.onChange.strong += ((vms: List[String]) ⇒ {
    ef.visible = vms.size != 0
    ef.text = vms.mkString("\n")
  })

  listenTo(keys)
  listenTo(tf.keys)
  reactions += {
    case scala.swing.event.KeyPressed(_, scala.swing.event.Key.Escape, _, _) ⇒
      propertyToComponent(p())
  }

  editValue.doOnChange(editValue()) // check restrictions 
}