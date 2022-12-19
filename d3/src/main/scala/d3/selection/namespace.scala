package d3.selection

object namespace {

  case class Namespaced(space: String, local: String)

  def apply(name: String): Either[String, Namespaced] = {
    val i = name.indexOf(":")
    val (prefix, name0) = if (i >= 0) {
      val prefix = name.take(i)
      (prefix, if (prefix != "xmlns") name.drop(i) else name)
    } else {
      (name, name)
    }
    namespaces.map.get(prefix) match {
      case None     => Left(name0)
      case Some(ns) => Right(Namespaced(ns, name0))
    }
  }

}
