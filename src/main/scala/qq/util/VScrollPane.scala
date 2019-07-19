package qq.util

/** A [[swing.ScrollPane]] that only scrolls vertically */
class VScrollPane
extends swing.ScrollPane() {
    protected trait DontHScroll extends javax.swing.JScrollPane {
      override def getMinimumSize() = new java.awt.Dimension(
          getViewport.getView.getMinimumSize.width +
          getHorizontalScrollBar.getPreferredSize.width + 
          getInsets.left + getInsets.right +
          getViewport.getInsets.left + getViewport.getInsets.right +
          getViewport.getView.asInstanceOf[java.awt.Container].getInsets.left + getViewport.getView.asInstanceOf[java.awt.Container].getInsets.right +
          8 // TODO why 8
          , super.getMinimumSize.height)
    }
    override lazy val peer = new javax.swing.JScrollPane with DontHScroll with SuperMixin 
    horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never  
}
