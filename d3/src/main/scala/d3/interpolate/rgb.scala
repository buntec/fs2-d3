package d3.interpolate

import d3.color.Color

object rgb {

  def apply(start: Color.Rgb, end: Color.Rgb): Double => Color.Rgb = {
    val interp = color.gamma(1)
    val r = interp(start.r, end.r)
    val g = interp(start.g, end.g)
    val b = interp(start.b, end.b)
    val opacity = color.nogamma(start.opacity, end.opacity)
    (t: Double) => {
      Color.Rgb(r(t), g(t), b(t), opacity(t))
    }
  }

}
