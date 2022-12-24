package d3.color

sealed trait Color

object Color {

  case class Hsl(h: Double, s: Double, l: Double, opacity: Double) extends Color

  case class Rgb(r: Double, g: Double, b: Double, opacity: Double) extends Color

}
