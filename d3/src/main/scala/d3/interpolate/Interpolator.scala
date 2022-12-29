package d3.interpolate

import d3.color.Color

trait Interpolator[A] {

  def apply(a0: A, a1: A): Double => A

  final def imap[B](f: B => A, g: A => B): Interpolator[B] =
    (b0: B, b1: B) => {
      val h = apply(f(b0), f(b1))
      (t: Double) => g(h(t))
    }

}

object Interpolator {

  def apply[A: Interpolator]: Interpolator[A] = implicitly[Interpolator[A]]

  implicit val doubleInterpolator: Interpolator[Double] =
    (a0: Double, a1: Double) => number(a0, a1)

  implicit val colorInterpolator: Interpolator[Color] =
    (a0: Color, a1: Color) => rgb.apply(a0.rgb, a1.rgb)

  implicit val stringInterpolator: Interpolator[String] =
    (a0: String, a1: String) => string(a0, a1)

}
