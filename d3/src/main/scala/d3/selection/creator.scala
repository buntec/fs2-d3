package d3.selection

import org.scalajs.dom

object creator {

  def apply(name: String, parent: dom.Node) = {
    namespace(name) match {
      case Left(name0) =>
        creatorInherited(name0, parent)
      case Right(ns) =>
        creatorFixed(ns, parent)
    }
  }

  private def creatorFixed(ns: namespace.Namespaced, parent: dom.Node) = {
    parent.ownerDocument.createElementNS(ns.space, ns.local)
  }

  private def creatorInherited(name: String, parent: dom.Node) = {
    val document = parent.ownerDocument
    val uri = parent.namespaceURI
    if (
      uri == namespaces.xhtml && document.documentElement.namespaceURI == namespaces.xhtml
    ) {
      document.createElement(name)
    } else {
      document.createElementNS(uri, name)
    }
  }

}
