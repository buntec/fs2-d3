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

object color {

  def linear(a: Double, d: Double): Double => Double = { (t: Double) =>
    a + t * d
  }

  def exponential(a: Double, b: Double, y: Double): Double => Double = {
    val a0 = Math.pow(a, y)
    val b0 = Math.pow(b, y) - a
    val y0 = 1 / y
    (t: Double) => {
      Math.pow(a0 + t * b0, y0)
    }
  }

  def hue(a: Double, b: Double): Double => Double = {
    val d = b - a;
    if (d != 0 && !d.isNaN) {
      linear(a, if (d > 180 || d < -180) d - 360 * Math.round(d / 360) else d)
    } else {
      constant(if (a.isNaN()) b else a)
    }
  }

  def gamma(y: Double): (Double, Double) => Double => Double =
    if (y == 1.0) { (a: Double, b: Double) =>
      nogamma(a, b)
    } else { (a: Double, b: Double) =>
      {
        if (b - a != 0 && !(b - a).isNaN) exponential(a, b, y)
        else constant(if (a.isNaN) b else a)
      }
    }

  def nogamma(a: Double, b: Double): Double => Double = {
    val d = b - a;
    if (d != 0 && !d.isNaN) linear(a, d) else constant(if (a.isNaN) b else a)
  }

  def constant(a: Double): Double => Double = (_: Double) => a

}
