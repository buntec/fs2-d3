package d3.selection

import scalajs.js

object window {

  def defaultView(node: js.Any): js.Any = {
    val n = node.asInstanceOf[js.Dynamic]
    if (n.ownerDocument != null && n.ownerDocument.defaultView != null) {
      n.ownerDocument.defaultView
    } else if (n.document != null && n != null) {
      node
    } else {
      n.defaultView
    }
  }

}
