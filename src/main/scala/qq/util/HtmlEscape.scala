package qq.util

/** render a string as HTML so that swing labels and tooltips don't do anything stupid with it*/
object HtmlEscape {
  def apply(x: String) = {
    "<html>" + (x.flatMap { case '&' =>  "&amp;"
    case '<' => "&lt;"
    case '>' => "&gt;"
    case x => x.toString
    }) + "</html>"
  }
}