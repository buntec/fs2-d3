/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package d3.color

sealed trait Color {

  def clamped: Color

  def rgb: Color.Rgb

  override def toString(): String = rgb.toString

}

object Color {

  case class Hsl(h: Double, s: Double, l: Double, opacity: Double)
      extends Color {

    override def clamped: Color =
      Hsl(clamph(h), clampt(s), clampt(l), clampa(opacity))

    override def rgb: Rgb = {
      val h = this.h % 360 + (if (this.h < 0) 360 else 0)
      val s = if (h.isNaN || this.s.isNaN) 0 else this.s
      val l = this.l
      val m2 = l + (if (l < 0.5) l else 1 - l) * s
      val m1 = 2 * l - m2
      new Rgb(
        hsl2rgb(if (h >= 240) h - 240 else h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(if (h < 120) h + 240 else h - 120, m1, m2),
        opacity
      )
    }

  }

  case class Rgb(r: Double, g: Double, b: Double, opacity: Double)
      extends Color {

    override def rgb: Rgb = this

    override def clamped: Color =
      Rgb(
        clampi(r).toDouble,
        clampi(g).toDouble,
        clampi(b).toDouble,
        clampa(opacity)
      )

    override def toString(): String = {
      val a = clampa(opacity)
      if (a == 1)
        s"rgb(${clampi(r)}, ${clampi(g)}, ${clampi(b)})"
      else
        s"rgba(${clampi(r)}, ${clampi(g)}, ${clampi(b)}, ${a})"
    }

  }

}
