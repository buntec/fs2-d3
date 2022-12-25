package d3.transition

object interpolate {

  def apply(a: String, b: String): Double => String = {

    val fallback = (t: Double) => if (t < 1) a else b

    b.toDoubleOption match {
      case Some(_) =>
        d3.interpolate.number(a, b).getOrElse(fallback)
      case None => {
        d3.color.fromString(b) match {
          case Some(_) => d3.interpolate.rgb(a, b).getOrElse(fallback)
          case None    => d3.interpolate.string(a, b)
        }
      }
    }

  }

}
