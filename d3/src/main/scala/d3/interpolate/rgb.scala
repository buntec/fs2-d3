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

/*
 * Copyright 2010-2021 Mike Bostock
 *
 * Permission to use, copy, modify, and/or distribute this software for any purpose
 * with or without fee is hereby granted, provided that the above copyright notice
 * and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

package d3.interpolate

import cats.syntax.all._
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

  def apply(start: String, end: String): Option[Double => String] = {
    (d3.color.fromString(start), d3.color.fromString(end)).tupled.map {
      case (start0, end0) =>
        val fn = apply(start0.rgb, end0.rgb)
        (t: Double) => fn(t).toString
    }
  }

}
